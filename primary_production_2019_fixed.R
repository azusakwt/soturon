#一次生産比較
#河手梓
#2019-01-09


# 図の寸法
# A5寸法
WIDTH = 297/2
HEIGHT = 210/2

# 図のテーマ
source("theme_kawate.R")

# パッケージの読み込み
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(mgcv) # GAM 解析用パッケージ
library(lemon)

#データの読み込み
df1 = read_csv("Modified_data/kamigoto_production.csv")
light = read_csv("Modified_data/light_calibrate.csv")
df1 = 
  df1 %>% 
  ungroup() %>% 
  mutate(location = recode(location,
                           "Tainoura (Isoyake)" = 1, 
                           "Arikawa (Zostera)" = 2,
                           "Arikawa (Sargassum)" = 3)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2,3),
                           label = c("Tainoura",
                                     "Arikawa (Zostera)",
                                     "Arikawa"))) 

df1 = df1 %>% filter(!str_detect(location, "Zostera")) 
df1 = df1 %>% mutate(location = factor(location)) # 外した要因のレベルも外す
df1 %>% pull(location)

df1 = df1 %>% spread(position, value) %>% 
  mutate(value = `1m`+`0m`)
df1
# write_csv(df1,"../soturon_2019/Modified_data/primary_production_0m+1m.csv")

light = light %>% group_by(location, Date) %>% summarise(ppfd = 60*sum(ppfd)/10^6) %>% 
  ungroup() %>% 
  filter(str_detect(location, "tainoura|arikawa")) %>% 
  mutate(location = recode(location,
                           "tainoura" = 1, 
                           "arikawaamamo" = 2,
                           "arikawagaramo" = 3)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2,3),
                           label = c("Tainoura",
                                     "Arikawa (Zostera)",
                                     "Arikawa"))) 

df2 = full_join(df1 %>% filter(str_detect(key, "NEP")) %>% drop_na() %>% 
                  select(Date, location, value),
                light, by = c("Date", "location") ) %>% 
  filter(str_detect(location, "Tainoura|Sarg"))

df2 %>% mutate(month = month(Date)) %>% 
  ggplot() +
  geom_point(aes(x = ppfd, y = value)) +
  facet_grid(month ~ location)


# 解析（案）-------------------------------------------------------------------------------------------------
library(mgcv) # GAM 解析用パッケージ
# GEPとRP は正の値しか取れないので，Gamma 分布
# NEP は 正規分布，このとき，family の定義は不要。
# 一般化加法モデル

df1 %>% group_by(location) %>% nest()
df1 %>% 
  group_by(location, key, month) %>% 
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  ggplot(aes(x = month, y = mean, color = location)) +
  geom_point() + 
  geom_line()+
  geom_smooth(method = "gam", formula = y ~ s(x))  + # デフォルトは正規分布
  facet_grid(location ~ key)


df1  %>% filter(str_detect("GEP", key)) %>% pull(value) %>% range(na.rm = T)
df1 %>% 
  filter(str_detect(location, "Arik|Taino")) %>% 
  group_by(location, key, month) %>% 
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% filter(key == "NEP") %>% 
  ungroup() %>% 
  write_csv(path = "/home/gnishihara/Data/all_nutrient_analysis/all_nutrient_analysis/nep.csv")

#GEP-----------------------------------------------------------------
#作図
xlabel = ""
ylabel = expression("GEP"~(g~O[2]~m^{-3}~day^{-1}))
gtitle = "Gross Ecosystem Production"

dset01 = 
  df1  %>% 
  filter(str_detect("GEP", key)) 


dset01 %>% 
  ggplot(aes(x = month,
             y = value, 
             color = location,
             fill = location)) +
  # geom_point(aes(group = month), alpha = 0.5) +
  geom_boxplot(aes(group = month), 
               alpha = 0.5,
               size = rel(0.2)) +
  geom_smooth(fill = "black",
              color = "black",
              method = "gam", 
              size = rel(0.4),
              alpha = 0.25,
              formula = y ~ s(x, bs = "cc"),
              method.args = 
                list(family = Gamma(link = "log")))  + 
  scale_x_continuous(xlabel, 
                     minor_breaks = 1:12,
                     breaks = c(1, 5, 8, 12),
                     labels = month.abb[c(1, 5, 8, 12)]) +
  scale_y_continuous(ylabel) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.text.x = element_text(size = rel(0.8))) +
  facet_wrap("location", nrow = 1)




# Gamma 分布のデフォルトのリンク関数は 1/y
# URL: http://hosho.ees.hokudai.ac.jp/~kubo/log/2009/kubolog20091212.html
# URL: https://logics-of-blue.com/平滑化スプラインと加法モデル/ 

gam00 = gam(value ~ s(month, bs = "cc"), 
            family = Gamma(link = "log"), 
            data = dset01)   # 帰無仮説：location の影響はない
gam01 = gam(value ~ s(month, bs = "cc") + location, 
            family = Gamma(link = "log"),
            data = dset01) # 対立仮設：location の影響はある
gam02 = gam(value ~ s(month, bs = "cc", by = location) + location, 
            family = Gamma(link = "log"),
            data = dset01) # 対立仮設：location とスムーズの綜合作用の影響はある

# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, gam02, test = "F")
summary(gam02)

# Parametric coefficient の Estimate は リンクスケールの平均値。

# GAM のモデルと ggplot のモデルは同じ？

# モデル期待値の計算
# dset01 %>% pull(location)
dset01_nd = 
  dset01 %>% 
  expand(month = seq(1, 12, length = 51), location) 

# type = "response" にすると，自然のスケールの期待値が返ってくる
dset01_nd = 
  dset01_nd %>% 
  mutate(fit01 = predict(gam01, newdata = dset01_nd, type = "response")) %>% 
  mutate(fit02 = predict(gam02, newdata = dset01_nd, type = "response"))

# 信頼区間も必要なら，次のようにする

dset01_nd = 
  dset01_nd %>% 
  mutate(fit01 = predict(gam01, newdata = dset01_nd)) %>% 
  mutate(fit02 = predict(gam02, newdata = dset01_nd)) %>% 
  mutate(se01 = predict(gam01, newdata = dset01_nd, se.fit = TRUE)$se.fit) %>% 
  mutate(se02 = predict(gam02, newdata = dset01_nd, se.fit = TRUE)$se.fit) %>% 
  mutate(l95_01 = exp(fit01 - 1.96*se01), u95_01 = exp(fit01 + 1.96*se01)) %>% 
  mutate(l95_02 = exp(fit02 - 1.96*se02), u95_02 = exp(fit02 + 1.96*se02)) %>% 
  mutate(fit01 = exp(fit01), fit02 = exp(fit02)) %>% 
  select(-se01, -se02)

ggplot() +
  geom_boxplot(aes(x = month,
                   y = value, 
                   color = location,
                   fill = location, 
                   group = month),
               data = dset01,
               outlier.shape = NA,
               alpha = 0.5,
               size = rel(0.2)) +
  geom_line(aes(x = month, 
                y = fit02),
            data = dset01_nd) +
  geom_ribbon(aes(x = month, 
                  ymin = l95_02, 
                  ymax = u95_02,
                  group = "GAM02"), 
              alpha = 0.5,
              data = dset01_nd)+
  scale_x_continuous(xlabel, 
                     minor_breaks = 1:12,
                     breaks = c(1, 5, 8, 12),
                     labels = month.abb[c(1, 5, 8, 12)]) +
  scale_y_continuous(ylabel) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  guides(color = F, fill = F) +
  theme(axis.text.x = element_text(size = rel(0.8))) +
  facet_wrap("location", nrow = 1)

ggsave(filename = "GAM入り年間総一次生産量(0m+1m).png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")


#RPの解析-----------------------------------
xlabel = ""
ylabel = expression("ER"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = "Respiration Production"

dset02 = 
  df1  %>% 
  filter(str_detect("RP", key)) %>% 
  # spread(position, value) %>% 
  mutate(value = `1m`+`0m`)

dset02 %>% 
  ggplot(aes(x = month,
             y = value, 
             color = location,
             fill = location)) +
  geom_boxplot(aes(group = month), 
               alpha = 0.5,
               size = rel(0.2)) +
  geom_smooth(aes(linetype = "GAM"),
              color = "black", fill = "black",
              method = "gam", 
              size = rel(0.4),
              alpha = 0.25,
              formula = y ~ s(x, bs = "cc"))  + 
  scale_x_continuous(xlabel, 
                     minor_breaks = 1:12,
                     breaks = c(1, 5, 8, 12),
                     labels = month.abb[c(1, 5, 8, 12)]) +
  scale_y_continuous(ylabel) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # facet_rep_wrap("location", nrow = 1)+
  facet_wrap("location", nrow = 1)+
  theme_kawate(base_size = 16) 

ggsave(filename = "呼吸量.png",
       width = WIDTH,
       height = HEIGHT,
       dpi = 600,
       units = "mm")
# Gamma 分布のデフォルトのリンク関数は 1/y
# URL: http://hosho.ees.hokudai.ac.jp/~kubo/log/2009/kubolog20091212.html
# URL: https://logics-of-blue.com/平滑化スプラインと加法モデル/ 

gam00 = gam(value ~ s(month, bs = "cc"), 
            family = Gamma(link = "log"), 
            data = dset02)   # 帰無仮説：location の影響はない
gam01 = gam(value ~ s(month, bs = "cc") + location, 
            family = Gamma(link = "log"),
            data = dset02) # 対立仮設：location の影響はある
gam02 = gam(value ~ s(month, bs = "cc", by = location) + location, 
            family = Gamma(link = "log"),
            data = dset02)
#

# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, gam02, test = "F")
summary(gam02)


#NEP-----------------------------------------------
xlabel = ""
ylabel = expression("NEP"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = ""

dset03 = 
  df1  %>% 
  filter(str_detect("NEP", key))

dset03 %>% 
  ggplot(aes(x = month,
             y = value, 
             color = location,
             fill = location)) +
  geom_boxplot(aes(group = month), 
               alpha = 0.5,
               size = rel(0.2)) +
  geom_smooth(aes(linetype = "GAM"),
              color = "black", fill = "black",
              method = "gam", 
              size = rel(0.4),
              alpha = 0.25,
              formula = y ~ s(x, bs = "cc"))  + 
  scale_x_continuous(xlabel, 
                     minor_breaks = 1:12,
                     breaks = c(1, 5, 8, 12),
                     labels = month.abb[c(1, 5, 8, 12)]) +
  scale_y_continuous(ylabel) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  # facet_rep_wrap("location", nrow = 1)+
  facet_wrap("location", nrow = 1)+
  theme_kawate(base_size = 16) 

ggsave(filename = "GAM入り年間純一次生産量.png", 
       width = WIDTH,
       height = HEIGHT,
       dpi = 600,
       units = "mm")

gam00 = gam(value ~ s(month, bs = "cc"),
            data = dset03)   # 帰無仮説：location の影響はない
gam01 = gam(value ~ s(month, bs = "cc") + location, 
            data = dset03) # 対立仮設：location の影響はある
gam02 = gam(value ~ s(month, bs = "cc", by = location) + location, 
            data = dset03) # 対立仮設：location の影響はある
# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, gam02, test = "F")
summary(gam02)

# 各月の計算
dset03 %>% 
  group_by(location, month) %>% 
  drop_na() %>% 
  summarise_at(vars(value), funs(mean, sd, min, max)) %>% 
  arrange( location) %>% 
  print(n = Inf)


dset02 %>% 
  group_by(location, month) %>% 
  drop_na() %>% 
  summarise_at(vars(value), funs(mean, sd, min, max)) %>% 
  arrange( location) %>% 
  print(n = Inf)

