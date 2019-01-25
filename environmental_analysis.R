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
library(glue)

# データの読み込み ----
# kamigoto & mushima のサンプリング日と nutrient のサンプリン日は
# 異なるので，そのまま full_join() できない。
kamigoto = read_csv("Modified_data/primary_production_0m+1m.csv")
mushima = read_csv("Modified_data/mushima_production.csv")
nutrient = read_csv("~/Lab_Data/nutrient/no2no3po4_all_station.csv")

nutrient_summary =
  nutrient %>% 
  rename(Date = date) %>% 
  mutate(variable = as.character(glue("{station} ({variable})")),
         month = month(Date),
         conc = ifelse(conc < 0, 0, conc)) %>% 
  group_by(variable, Date, Nutrient) %>% 
  summarise(conc = mean(conc)) %>% 
  mutate(month = month(Date)) %>% 
  ungroup() %>% 
  filter(str_detect(variable, "Isoyake|Sargassum"))

df1 = bind_rows(kamigoto, mushima)

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


df1 = kamigoto
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
                                     "Mushima (New port)" )))

# アマモ場と六島のデータを外します。----

alldata = 
  full_join(df1, light, by = c("location","month", "Date")) %>% 
  full_join(temperature, by = c("location", "month", "Date")) %>% 
  filter(!str_detect(location, "Zostera")) %>% 
  filter(!str_detect(location, "Mushima"))

alldata = 
  alldata %>% 
  mutate(log_ppfd = log(ppfd)) %>% 
  drop_na() %>% 
  select(-`0m`, -`1m`)

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
        start = list(pmax = 15, alpha = 3, rd = 2),
        data = dset01,
        variable = 6)

# factor をリセット
dset01 = dset01 %>% mutate(location = factor(location))
dset01 = dset01 %>% mutate(idx = as.numeric(location))


# 
preview(value ~
          pecurve2(ppfd, idx, pmax, alpha, rd,
                   dpmax, dalpha, drd),
        start = list(pmax = 15, alpha = 3, rd = 2,
                     dpmax = 1, dalpha = 0.1, drd = 0.1),
        data = dset01,
        variable = 6)

nullmodel = 
  nls(value ~ 
        pecurve(ppfd, pmax, alpha, rd),
      start = list(pmax = 15,alpha = 3, rd = 2),
      data = dset01)

fullmodel = 
  nls(value ~ 
        pecurve2(ppfd, idx, pmax, alpha, rd,
                 dpmax, dalpha, drd),
      start = list(pmax = 15, alpha = 3, rd = 2,
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

write_csv(dset01, "./Modified_data/data_for_nls_model.csv")

# 水温------------------------------------------------
xlabel = ""
ylabel = expression(Temperature~(degree*C))
gtitle = ""
alldata %>%
  group_by(location, Date) %>% 
  summarise(temperature = mean(temperature)) %>% 
  mutate(month = month(Date)) %>% 
  group_by(location, month) %>% 
  summarise(temp_mean = mean(temperature),
            temp_sd = sd(temperature),
            n = length(temperature)) %>% 
  mutate(l95 = temp_mean - temp_sd/ sqrt(n -1),
         u95 = temp_mean + temp_sd/ sqrt(n -1)) %>% 
  ggplot() +
  geom_point(aes(x = month, y = temp_mean, color =location), position = position_dodge(0.1))+
  geom_line(aes(x = month, y = temp_mean, color =location) , position = position_dodge(0.1))+
  geom_errorbar(aes(x = month, 
                    ymin = l95, 
                    ymax = u95,
                    color =location), 
                width = 0,
                position = position_dodge(0.1))+
  scale_x_continuous(name = xlabel,
                     labels = month.abb[1:12],
                     breaks = 1:12) +
  scale_y_continuous(name = ylabel,
                     limits = c(10, 30)) +
  scale_color_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.line = element_line())

ggsave(filename = "水温.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")

alldata %>%
  mutate(month = month(Date)) %>% 
  ggplot() +
   geom_boxplot(aes(x = month,
                   y = temperature,
                   fill = location,
                   group = interaction(location, month))) +
  scale_x_continuous(xlabel, 
                     minor_breaks = 1:12,
                     breaks = c(1, 5, 8, 12),
                     labels = month.abb[c(1, 5, 8, 12)]) +
  scale_y_continuous(name = ylabel,
                     limits = c(10, 30),
                     breaks = c(10, 15, 20, 25, 30)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  guides(color = FALSE, fill = FALSE) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.line = element_line())+
  facet_wrap("location", nrow = 1)

ggsave(filename = "水温ボックス.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")


