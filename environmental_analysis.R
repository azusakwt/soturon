# NEP と環境要因の関係
# Greg Nishihara
# 2019-01-16


# 図の寸法 ----
# A5寸法
WIDTH = 297/2
HEIGHT = 210/2

# パッケージの読み込み ----
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(mgcv) # GAM 解析用パッケージ
library(lemon)
library(ggfortify)
library(nlstools) # 非線形モデルの当てはめに役立つパッケージ

# データの読み込み ----
kamigoto = read_csv("Modified_data/kamigoto_production.csv")
mushima = read_csv("Modified_data/mushima_production.csv")
df1 = bind_rows(kamigoto,mushima)

# cem = read_csv("Modified_data/CEM_fixed.csv")
# cku = read_csv("Modified_data/CKU_fixed.csv")
light = read_csv("Modified_data/light_calibrate.csv")
temperature = read_csv("Modified_data/temperature.csv")

light = light %>% 
  group_by(location, Date) %>% 
  mutate(ppfd = ifelse(ppfd < 0, 0, ppfd)) %>% 
  summarise(ppfd = sum(ppfd*60) * 10e-6) %>% 
  mutate(month = month(Date)) 

temperature = temperature %>% 
  group_by(location, Date) %>% 
  summarise(temperature = mean(temperature, na.rm=T)) %>% 
  mutate(month = month(Date)) 

light = 
  light %>% 
  ungroup() %>% 
  mutate(location = recode(location,
                           "tainoura" = 1, 
                           "arikawaamamo" = 2,
                           "arikawagaramo" = 3,
                           "mushima2" = 4,
                           "mushima3" = 5)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2,3,4,5),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Zostera)",
                                     "Arikawa (Sargassum)",
                                     "Mushima (Old port)" ,
                                     "Mushima (New port)" ))) 
temperature = 
  temperature %>% 
  ungroup() %>% 
  mutate(location = recode(location,
                           "tainoura" = 1, 
                           "arikawaamamo" = 2,
                           "arikawagaramo" = 3,
                           "mushima2" = 4,
                           "mushima3" = 5)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2,3,4,5),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Zostera)",
                                     "Arikawa (Sargassum)",
                                     "Mushima (Old port)" ,
                                     "Mushima (New port)" ))) 

# write_csv(df1, "../soturon_2019/Modified_data/prymary_production.csv")

df1 = 
  df1 %>% 
  ungroup() %>% 
  mutate(location = recode(location,
                           "Tainoura (Isoyake)" = 1, 
                           "Arikawa (Zostera)" = 2,
                           "Arikawa (Sargassum)" = 3,
                           "Mushima (Old port)" = 4,
                           "Mushima (New port)" = 5)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2,3,4,5),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Zostera)",
                                     "Arikawa (Sargassum)",
                                     "Mushima (Old port)" ,
                                     "Mushima (New port)" ))) %>% 
  select(-X1)

# アマモ場と六島のデータを外します。----

alldata = 
  full_join(df1, light, by = c("location","month", "Date")) %>% 
  full_join(temperature, by = c("location", "month", "Date")) %>% 
  filter(!str_detect(location, "Zostera")) %>% 
  filter(!str_detect(location, "Mushima"))

alldata = 
  alldata %>% 
  mutate(log_ppfd = log(ppfd)) %>% 
  drop_na()

# 光合成光曲線の解析 ----
# 箇々では NEP の方を解析しています。

dset01 = alldata %>% filter(str_detect(key, "NEP"))

# 光合成光曲線の関数

pecurve = function(x, pmax, alpha, rd) {
  pmax * (1-exp(-alpha / pmax * x)) + rd
}

pecurve2 = function(x, w, pmax, alpha, rd, dpmax, dalpha, drd) {
  PMAX =  pmax + dpmax*(w == 2)
  ALPHA = alpha + dalpha*(w == 2)
  RD = rd + drd*(w == 2)
  
  PMAX * (1- exp(-ALPHA/PMAX * x)) + RD
}

# variable = 6 は dset01 の ppfd の行です。
preview(value ~ pecurve(ppfd, pmax, alpha, rd),
        start = list(pmax = 10, alpha = 1, rd = 5),
        data = dset01,
        variable = 6)

# factor をリセット
dset01 = dset01 %>% mutate(location = factor(location))
dset01 = dset01 %>% mutate(idx = as.numeric(location))


# 
preview(value ~
          pecurve2(ppfd, idx, pmax, alpha, rd,
                   dpmax, dalpha, drd),
        start = list(pmax = 12, alpha = 0.5, rd = 1,
                     dpmax = 1, dalpha = 0.1, drd = 0.1),
        data = dset01,
        variable = 6)

nullmodel = 
  nls(value ~ 
        pecurve(ppfd, pmax, alpha, rd),
      start = list(pmax = 10,alpha = 0.5, rd = 5),
      data = dset01)

fullmodel = 
  nls(value ~ 
        pecurve2(ppfd, idx, pmax, alpha, rd,
                 dpmax, dalpha, drd),
      start = list(pmax = 12, alpha = 0.5, rd = 1,
                   dpmax = 1, dalpha = 0.1, drd = 0.1),
        data = dset01)

anova(nullmodel, fullmodel, test = "F")
summary(fullmodel)

dset01_nd = 
  dset01 %>% 
  expand(ppfd = seq(0, 30, length = 101), idx)

dset01_nd  = 
  dset01_nd %>% 
  mutate(fit = predict(fullmodel, newdata = dset01_nd)) 

dset01_nd = dset01_nd %>% 
  mutate(location = factor(idx,
                           levels = 1:2,
                           labels = c("Tainoura (Isoyake)",
                                      "Arikawa (Sargassum)")))

dset01 %>%  
  ggplot() +
  geom_point(aes(x = ppfd, 
                 y = value, color = location)) +
  geom_line(aes(x = ppfd, y = fit),
            data = dset01_nd) +
  facet_rep_wrap("location")

dset01 %>% pull(ppfd) %>% range()
