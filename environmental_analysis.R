# NEP と環境要因の関係
# Greg Nishihara
# 2019-01-16


# 図の寸法
# A5寸法
WIDTH = 297/2
HEIGHT = 210/2
# パッケージの読み込み
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(mgcv) # GAM 解析用パッケージ
library(lemon)

#データの読み込み
kamigoto = read_csv("Modified_data/kamigoto_production.csv")
mushima = read_csv("Modified_data/mushima_production.csv")
df1 = bind_rows(kamigoto,mushima)

# cem = read_csv("Modified_data/CEM_fixed.csv")
# cku = read_csv("Modified_data/CKU_fixed.csv")
light = read_csv("Modified_data/light_calibrate.csv")
temperature = read_csv("Modified_data/kamigoto_temperature.csv")

light = light %>% 
  group_by(location, Date) %>% 
  mutate(ppfd = ifelse(ppfd < 0, 0, ppfd)) %>% 
  summarise(ppfd = sum(ppfd*60) * 10e-6) %>% 
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




alldata = 
  full_join(df1, light, by = c("location","month", "Date")) %>% 
  filter(!str_detect(location, "Zostera"))

alldata %>% filter(str_detect(key, "NEP")) %>% 
ggplot() +
  geom_point(aes(x = ppfd, y = value, color = location)) +
  geom_smooth(method = "nls",
              formula = y~pecurve(x, pmax, alpha, rd),
              method.args = list(start = list(pmax = 10, rd = -5, alpha = 5)))
  facet_rep_wrap("location")

