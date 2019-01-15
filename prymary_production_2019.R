#一次生産比較
#河手梓
#2019-01-09


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
                                     "Mushima (New port)" ))) 


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

#GEP-----------------------------------------------------------------
#作図
xlabel = ""
ylabel = expression("GEP"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = "Gross Ecosystem Production"

p1 =
df1  %>% 
  filter(str_detect("GEP", key)) %>% 
  filter(!str_detect(location, "Zostera")) %>% 
  ggplot(aes(x = month,
             y = value, 
             color = location)) +
  geom_point() +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cc"),
              method.args = list(family = Gamma(link = "log")))  + 
  scale_x_continuous(xlabel, 
                     breaks = 1:12,
                     labels = month.abb[1:12]) +
  scale_y_continuous(ylabel) +
  scale_color_brewer(palette = "Dark2") +
  guides(color = FALSE) +
  facet_rep_grid(location ~ .) +
  theme(axis.line = element_line())


ggsave(filename = "年間総一次生産量.png", 
       plot = p1,
       width = WIDTH,
       height = HEIGHT,
       units = "mm")

#解析
df1  %>% 
  filter(str_detect("GEP", key)) %>% 
  filter(month > 5) %>% 
  ggplot(aes(x = month,
             y = value, 
             color = key)) +
  geom_point() +
  geom_smooth(method = "glm", 
              formula = y ~ x,
              method.args = list(family = Gamma(link = "log")))  + 
  scale_x_continuous(xlabel, 
                     breaks = 1:12,
                     labels = month.abb[1:12]) +
  scale_y_continuous(ylabel) +
  guides(color = FALSE) +
  facet_grid(location ~ .)

dset02 = 
  df1  %>% 
  filter(str_detect("GEP", key)) %>% 
  filter(month > 5)

# 一年分のデータ解析
dset01 = 
  df1  %>% 
  filter(str_detect("RP", key)) %>% 
  filter(!str_detect(location, "Zostera"))

# Gamma 分布のデフォルトのリンク関数は 1/y
# URL: http://hosho.ees.hokudai.ac.jp/~kubo/log/2009/kubolog20091212.html
# URL: https://logics-of-blue.com/平滑化スプラインと加法モデル/ 

gam00 = gam(value ~ s(month, bs = "cc"), 
            family = Gamma(link = "log"), 
            data = dset01)   # 帰無仮説：location の影響はない
gam01 = gam(value ~ s(month, bs = "cc") + location, 
            family = Gamma(link = "log"),
            data = dset01) # 対立仮設：location の影響はある
#

# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, test = "F")
summary(gam01)

# Gamma 分布のデフォルトのリンク関数は 1/y
# URL: http://hosho.ees.hokudai.ac.jp/~kubo/log/2009/kubolog20091212.html
glm00 = glm(value ~ I(month - 6),
            family = Gamma(link = "log"), 
            data = dset02)   # 帰無仮説：location の影響はない
glm01 = glm(value ~ I(month - 6) + location, 
            family = Gamma(link = "log"),
            data = dset02) # 対立仮設：location の影響はある
glm02 = glm(value ~ I(month - 6) * location, 
            family = Gamma(link = "log"),
            data = dset02) # 対立仮設：location の影響はある
# F検定をつかって，二つのモデルの比較

anova(glm00, glm01, glm02, test = "F")
summary(glm02)

coefficients(glm02)
exp(coefficients(glm02) [1])
exp(coefficients(glm02) [1] + coefficients(glm02) [3])#Zostera
exp(coefficients(glm02) [1] + coefficients(glm02) [4])#Sargassum
exp(coefficients(glm02) [1] + coefficients(glm02) [5])#Old port
exp(coefficients(glm02) [1] + coefficients(glm02) [6])#New port

(coefficients(glm02) [2]) # Isoyake
(coefficients(glm02) [2] + coefficients(glm02) [7]) # Zostera
(coefficients(glm02) [2] + coefficients(glm02) [8]) # Sargassum
(coefficients(glm02) [2] + coefficients(glm02) [9]) # Old port
(coefficients(glm02) [2] + coefficients(glm02) [10]) # New port

dset01 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))


# Generalized Linear Hypothesis Testing (GLHT)
library(multcomp)

summary(glm02) # location間の比較は Wald's test (検出力低い)
summary(glm02)$coef

# 各パラメータを０と比較するとき
K = rbind("Tainoura Intercept"   = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          "Tainoura Slope"       = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
          "Zostera Intercept"    = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
          "Sargassum Intercept"  = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
          "Old port Intercept"   = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
          "New port Intercept"   = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
          "Zostera Slope"        = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
          "Sargassum Slope"      = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
          "Old port Slope"       = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
          "New port Slope"       = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
glht(glm02, linfct = K) %>% summary()


# Tukey は切片の比較をする
glht(glm02, mcp(location = "Tukey")) %>% summary()
K = rbind("Zostera - Isoyake"       = c(0, 0,  1,  0,  0, 0, 0, 0, 0, 0),
          "Sargassum - Isoyake"     = c(0, 0,  0,  1,  0, 0, 0, 0, 0, 0),
          "Old port - Isoyake"      = c(0, 0,  0,  0,  1, 0, 0, 0, 0, 0),
          "New port - Isoyake"      = c(0, 0,  0,  0,  0, 1, 0, 0, 0, 0),
          "Sargassum - Zostera"     = c(0, 0, -1,  1,  0, 0, 0, 0, 0, 0),
          "Old port - Zostera"      = c(0, 0, -1,  0,  1, 0, 0, 0, 0, 0),
          "New port - Zostera"      = c(0, 0,  1,  0,  0, 1, 0, 0, 0, 0),
          "Old port - Sargassum"    = c(0, 0,  0, -1,  1, 0, 0, 0, 0, 0),
          "New port - Sargassum"    = c(0, 0,  0, -1,  0, 1, 0, 0, 0, 0),
          "New port - Old port"     = c(0, 0,  0,  0, -1, 1, 0, 0, 0, 0))
glht(glm02, linfct = K) %>% summary()


# Tukey で角度を比較

K = rbind("Zostera - Isoyake"       = c(0, 0, 0, 0, 0, 0,  1,  0,  0, 0),
          "Sargassum - Isoyake"     = c(0, 0, 0, 0, 0, 0,  0,  1,  0, 0),
          "Old port - Isoyake"      = c(0, 0, 0, 0, 0, 0,  0,  0,  1, 0),
          "New port - Isoyake"      = c(0, 0, 0, 0, 0, 0,  0,  0,  0, 1),
          "Sargassum - Zostera"     = c(0, 0, 0, 0, 0, 0, -1,  1,  0, 0),
          "Old port - Zostera"      = c(0, 0, 0, 0, 0, 0, -1,  0,  1, 0),
          "New port - Zostera"      = c(0, 0, 0, 0, 0, 0, -1,  0,  0, 1),
          "Old port - Sargassum"    = c(0, 0, 0, 0, 0, 0,  0, -1,  1, 0),
          "New port - Sargassum"    = c(0, 0, 0, 0, 0, 0,  0, -1,  0, 1),
          "New port - Old port"     = c(0, 0, 0, 0, 0, 0,  0,  0, -1, 1))
glht(glm02, linfct = K) %>% summary()

#RPの解析-----------------------------------
xlabel = ""
ylabel = expression("RP"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = "Respiration Production"

p2 = df1  %>% 
  filter(str_detect("RP", key)) %>% 
  filter(!str_detect(location, "Zostera")) %>% 
  ggplot(aes(x = month,
             y = value, 
             color = location)) +
  geom_point() +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cc"),
              method.args = list(family = Gamma(link = "log")))  + 
  scale_x_continuous(xlabel, 
                     breaks = 1:12,
                     labels = month.abb[1:12]) +
  scale_y_continuous(ylabel) +
  scale_color_brewer(palette = "Dark2") +
  guides(color = FALSE) +
  facet_rep_grid(location ~ .) +
  theme(axis.line = element_line())

ggsave(filename = "年間呼吸量.png", 
       plot = p2,
       width = WIDTH,
       height = HEIGHT,
       units = "mm")


# 一年分のデータ解析
dset01 = 
  df1  %>% 
  filter(str_detect("RP", key)) %>% 
  filter(!str_detect(location, "Zostera"))

# Gamma 分布のデフォルトのリンク関数は 1/y
# URL: http://hosho.ees.hokudai.ac.jp/~kubo/log/2009/kubolog20091212.html
# URL: https://logics-of-blue.com/平滑化スプラインと加法モデル/ 

gam00 = gam(value ~ s(month, bs = "cc"), 
            family = Gamma(link = "log"), 
            data = dset01)   # 帰無仮説：location の影響はない
gam01 = gam(value ~ s(month, bs = "cc") + location, 
            family = Gamma(link = "log"),
            data = dset01) # 対立仮設：location の影響はある
#

# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, test = "F")
summary(gam01)

#切片の確認
exp(coefficients(gam01) [1])　　
exp(coefficients(gam01) [1] + coefficients(gam01) [2])

dset01 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))



# アマモもいれる (データ解析)

df1  %>% 
  filter(str_detect("RP", key)) %>% 
  filter(month > 5) %>% 
  ggplot(aes(x = month,
             y = value, 
             color = key)) +
  geom_point() +
  geom_smooth(method = "glm", 
              formula = y ~ x,
              method.args = list(family = Gamma(link = "log")))  + 
  scale_x_continuous(xlabel, 
                     breaks = 1:12,
                     labels = month.abb[1:12]) +
  scale_y_continuous(ylabel) +
  guides(color = FALSE) +
  facet_grid(location ~ .)

dset02 = 
  df1  %>% 
  filter(str_detect("RP", key)) %>% 
  filter(month > 5)

# Gamma 分布のデフォルトのリンク関数は 1/y
# URL: http://hosho.ees.hokudai.ac.jp/~kubo/log/2009/kubolog20091212.html
glm00 = glm(value ~ I(month - 6),
            family = Gamma(link = "log"), 
            data = dset02)   # 帰無仮説：location の影響はない
glm01 = glm(value ~ I(month - 6) + location, 
            family = Gamma(link = "log"),
            data = dset02) # 対立仮設：location の影響はある
glm02 = glm(value ~ I(month - 6) * location, 
            family = Gamma(link = "log"),
            data = dset02) # 対立仮設：location の影響はある
# F検定をつかって，二つのモデルの比較

anova(glm00, glm01, glm02, test = "F")
summary(glm02)

oefficients(glm02)
exp(coefficients(glm02) [1])
exp(coefficients(glm02) [1] + coefficients(glm02) [3])#Zostera
exp(coefficients(glm02) [1] + coefficients(glm02) [4])#Sargassum
exp(coefficients(glm02) [1] + coefficients(glm02) [5])#Old port
exp(coefficients(glm02) [1] + coefficients(glm02) [6])#New port

(coefficients(glm02) [2]) # Isoyake
(coefficients(glm02) [2] + coefficients(glm02) [7]) # Zostera
(coefficients(glm02) [2] + coefficients(glm02) [8]) # Sargassum
(coefficients(glm02) [2] + coefficients(glm02) [9]) # Old port
(coefficients(glm02) [2] + coefficients(glm02) [10]) # New port

dset02 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))


# Generalized Linear Hypothesis Testing (GLHT)
library(multcomp)

summary(glm02) # location間の比較は Wald's test (検出力低い)
summary(glm02)$coef

# 各パラメータを０と比較するとき
K = rbind("Tainoura Intercept"   = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          "Tainoura Slope"       = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
          "Zostera Intercept"    = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
          "Sargassum Intercept"  = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
          "Old port Intercept"   = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
          "New port Intercept"   = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
          "Zostera Slope"        = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
          "Sargassum Slope"      = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
          "Old port Slope"       = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
          "New port Slope"       = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
glht(glm02, linfct = K) %>% summary()


# Tukey は切片の比較をする
glht(glm02, mcp(location = "Tukey")) %>% summary()
K = rbind("Zostera - Isoyake"       = c(0, 0,  1, 0, 0, 0, 0, 0, 0, 0),
          "Sargassum - Isoyake"     = c(0, 0,  0, 1, 0, 0, 0, 0, 0, 0),
          "Old port - Isoyake"      = c(0, 0,  0, 0, 1, 0, 0, 0, 0, 0),
          "New port - Isoyake"      = c(0, 0,  0, 0, 0, 1, 0, 0, 0, 0),
          "Sargassum - Zostera"     = c(0, 0, -1, 1, 0, 0, 0, 0, 0, 0),
          "Old port - Zostera"      = c(0, 0, -1, 0, 1, 0, 0, 0, 0, 0),
          "New port - Zostera"      = c(0, 0,  1, 0, 0, 1, 0, 0, 0, 0),
          "Old port - Sargassum"    = c(0, 0,  0,-1, 1, 0, 0, 0, 0, 0),
          "New port - Sargassum"    = c(0, 0, 0, -1, 0, 1, 0, 0, 0, 0),
          "New port - Old port"     = c(0, 0, 0, 0, -1, 1, 0, 0, 0, 0))
glht(glm02, linfct = K) %>% summary()

#################
# Tukey で角度を比較

K = rbind("Zostera - Isoyake"       = c(0, 0, 0, 0, 0, 0,  1,  0,  0, 0),
          "Sargassum - Isoyake"     = c(0, 0, 0, 0, 0, 0,  0,  1,  0, 0),
          "Old port - Isoyake"      = c(0, 0, 0, 0, 0, 0,  0,  0,  1, 0),
          "New port - Isoyake"      = c(0, 0, 0, 0, 0, 0,  0,  0,  0, 1),
          "Sargassum - Zostera"     = c(0, 0, 0, 0, 0, 0, -1,  1,  0, 0),
          "Old port - Zostera"      = c(0, 0, 0, 0, 0, 0, -1,  0,  1, 0),
          "New port - Zostera"      = c(0, 0, 0, 0, 0, 0, -1,  0,  0, 1),
          "Old port - Sargassum"    = c(0, 0, 0, 0, 0, 0,  0, -1,  1, 0),
          "New port - Sargassum"    = c(0, 0, 0, 0, 0, 0,  0, -1,  0, 1),
          "New port - Old port"     = c(0, 0, 0, 0, 0, 0,  0,  0, -1, 1))
glht(glm02, linfct = K) %>% summary()


#NEP-----------------------------------------------
xlabel = ""
ylabel = expression("NEP"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = "Net Ecosystem Production"

p3 = 
  df1  %>% 
  filter(str_detect("NEP", key)) %>% 
  filter(!str_detect(location, "Zostera")) %>% 
  ggplot(aes(x = month,
             y = value, 
             color = location)) +
  geom_point() +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cc"))  + 
  scale_x_continuous(xlabel, 
                     breaks = 1:12,
                     labels = month.abb[1:12]) +
  scale_y_continuous(ylabel) +
  scale_color_brewer(palette = "Dark2") +
  guides(color = FALSE) +
  facet_rep_grid(location ~ .) +
  theme(axis.line = element_line())

ggsave(filename = "生態系純一次生産量.png", 
       plot = p3,
       width = WIDTH,
       height = HEIGHT,
       units = "mm")



# 一年分のデータ解析
dset01 = 
  df1  %>% 
  filter(str_detect("NEP", key)) %>% 
  filter(!str_detect(location, "Zostera"))

# Gamma 分布のデフォルトのリンク関数は 1/y
# URL: http://hosho.ees.hokudai.ac.jp/~kubo/log/2009/kubolog20091212.html
# URL: https://logics-of-blue.com/平滑化スプラインと加法モデル/ 

gam00 = gam(value ~ s(month, bs = "cc"),
            data = dset01)   # 帰無仮説：location の影響はない
gam01 = gam(value ~ s(month, bs = "cc") + location, 
            data = dset01) # 対立仮設：location の影響はある

# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, test = "F")
summary(gam01)

#切片の確認
coefficients(gam01) [1]　　
coefficients(gam01) [1] + coefficients(gam01) [2]
coefficients(gam01) [1] + coefficients(gam01) [3]
coefficients(gam01) [1] + coefficients(gam01) [4]

dset01 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))



# アマモもいれる (データ解析) -----------

rate_tall  %>% 
  filter(str_detect("NEP", key)) %>% 
  filter(month > 5) %>% 
  ggplot(aes(x = month,
             y = value, 
             color = key)) +
  geom_point() +
  geom_smooth(method = "glm", 
              formula = y ~ x)  + 
  scale_x_continuous(xlabel, 
                     breaks = 1:12,
                     labels = month.abb[1:12]) +
  scale_y_continuous(ylabel) +
  guides(color = FALSE) +
  facet_grid(location ~ .)

dset02 = 
  rate_tall  %>% 
  filter(str_detect("NEP", key)) %>% 
  filter(month > 5)

# Gamma 分布のデフォルトのリンク関数は 1/y
# URL: http://hosho.ees.hokudai.ac.jp/~kubo/log/2009/kubolog20091212.html
glm00 = glm(value ~ I(month - 6),
            data = dset02)   # 帰無仮説：location の影響はない
glm01 = glm(value ~ I(month - 6) + location, 
            data = dset02) # 対立仮設：location の影響はある
glm02 = glm(value ~ I(month - 6) * location, 
            data = dset02) # 対立仮設：location の影響はある
# F検定をつかって，二つのモデルの比較

anova(glm00, glm01, glm02, test = "F")
summary(glm02)

coefficients(glm02)
coefficients(glm02) [1]
coefficients(glm02) [1] + coefficients(glm02) [3]
coefficients(glm02) [1] + coefficients(glm02) [4]

coefficients(glm02) [2] # Isoyake
coefficients(glm02) [2] + coefficients(glm02) [5] # Zostera
coefficients(glm02) [2] + coefficients(glm02) [6] # Sargassum

dset02 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))


summary(glm02) # location間の比較は Wald's test (検出力低い)
summary(glm02)$coef

#各パラメータを０と比較するとき
K = rbind("Tainoura Intercept"   = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
          "Tainoura Slope"       = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
          "Zostera Intercept"    = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
          "Sargassum Intercept"  = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
          "Old port Intercept"   = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
          "New port Intercept"   = c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
          "Zostera Slope"        = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
          "Sargassum Slope"      = c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
          "Old port Slope"       = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
          "New port Slope"       = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
glht(glm02, linfct = K) %>% summary()


# Tukey は切片の比較をする
glht(glm02, mcp(location = "Tukey")) %>% summary()
K = rbind("Zostera - Isoyake"       = c(0, 0,  1, 0, 0, 0, 0, 0, 0, 0),
          "Sargassum - Isoyake"     = c(0, 0,  0, 1, 0, 0, 0, 0, 0, 0),
          "Old port - Isoyake"      = c(0, 0,  0, 0, 1, 0, 0, 0, 0, 0),
          "New port - Isoyake"      = c(0, 0,  0, 0, 0, 1, 0, 0, 0, 0),
          "Sargassum - Zostera"     = c(0, 0, -1, 1, 0, 0, 0, 0, 0, 0),
          "Old port - Zostera"      = c(0, 0, -1, 0, 1, 0, 0, 0, 0, 0),
          "New port - Zostera"      = c(0, 0,  1, 0, 0, 1, 0, 0, 0, 0),
          "Old port - Sargassum"    = c(0, 0,  0,-1, 1, 0, 0, 0, 0, 0),
          "New port - Sargassum"    = c(0, 0, 0, -1, 0, 1, 0, 0, 0, 0),
          "New port - Old port"     = c(0, 0, 0, 0, -1, 1, 0, 0, 0, 0))
glht(glm02, linfct = K) %>% summary()

#################
# Tukey で角度を比較

K = rbind("Zostera - Isoyake"       = c(0, 0, 0, 0, 0, 0,  1,  0,  0, 0),
          "Sargassum - Isoyake"     = c(0, 0, 0, 0, 0, 0,  0,  1,  0, 0),
          "Old port - Isoyake"      = c(0, 0, 0, 0, 0, 0,  0,  0,  1, 0),
          "New port - Isoyake"      = c(0, 0, 0, 0, 0, 0,  0,  0,  0, 1),
          "Sargassum - Zostera"     = c(0, 0, 0, 0, 0, 0, -1,  1,  0, 0),
          "Old port - Zostera"      = c(0, 0, 0, 0, 0, 0, -1,  0,  1, 0),
          "New port - Zostera"      = c(0, 0, 0, 0, 0, 0, -1,  0,  0, 1),
          "Old port - Sargassum"    = c(0, 0, 0, 0, 0, 0,  0, -1,  1, 0),
          "New port - Sargassum"    = c(0, 0, 0, 0, 0, 0,  0, -1,  0, 1),
          "New port - Old port"     = c(0, 0, 0, 0, 0, 0,  0,  0, -1, 1))
glht(glm02, linfct = K) %>% summary()


#################################################
#アマモ場以外で一年間解析その2-----------------------------------------------------------------
dset01 = 
  df1  %>% 
  filter(str_detect("GEP", key)) %>% 
  filter(!str_detect(location, "Zostera"))


glm00 = glm(value ~ I(month),
            family = Gamma(link = "log"), 
            data = dset01)   # 帰無仮説：location の影響はない
glm01 = glm(value ~ I(month) + location, 
            family = Gamma(link = "log"),
            data = dset01) # 対立仮設：location の影響はある
glm02 = glm(value ~ I(month) * location, 
            family = Gamma(link = "log"),
            data = dset01) # 対立仮設：location の影響はある
# F検定をつかって，二つのモデルの比較

anova(glm00, glm01, glm02, test = "F")
summary(glm02)

coefficients(glm02)
exp(coefficients(glm02) [1])
exp(coefficients(glm02) [1] + coefficients(glm02) [3])#Sargassum
exp(coefficients(glm02) [1] + coefficients(glm02) [4])#Old port
exp(coefficients(glm02) [1] + coefficients(glm02) [5])#New port

(coefficients(glm02) [2]) # Isoyake
(coefficients(glm02) [2] + coefficients(glm02) [6]) # Sargassum
(coefficients(glm02) [2] + coefficients(glm02) [7]) # Old port
(coefficients(glm02) [2] + coefficients(glm02) [8]) # New port

dset01 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))


# Generalized Linear Hypothesis Testing (GLHT)
library(multcomp)

summary(glm02) # location間の比較は Wald's test (検出力低い)
summary(glm02)$coef

# 各パラメータを０と比較するとき
K = rbind("Tainoura Intercept"   = c(1, 0, 0, 0, 0, 0, 0, 0),
          "Tainoura Slope"       = c(0, 1, 0, 0, 0, 0, 0, 0),
          "Sargassum Intercept"  = c(0, 0, 1, 0, 0, 0, 0, 0),
          "Old port Intercept"   = c(0, 0, 0, 1, 0, 0, 0, 0),
          "New port Intercept"   = c(0, 0, 0, 0, 1, 0, 0, 0),
          "Sargassum Slope"      = c(0, 0, 0, 0, 0, 1, 0, 0),
          "Old port Slope"       = c(0, 0, 0, 0, 0, 0, 1, 0),
          "New port Slope"       = c(0, 0, 0, 0, 0, 0, 0, 1))
glht(glm02, linfct = K) %>% summary()


# Tukey は切片の比較をする
glht(glm02, mcp(location = "Tukey")) %>% summary()
K = rbind("Sargassum - Isoyake"     = c(0, 0,  1,  0, 0, 0, 0, 0),
          "Old port - Isoyake"      = c(0, 0,  0,  1, 0, 0, 0, 0),
          "New port - Isoyake"      = c(0, 0,  0,  0, 1, 0, 0, 0),
          "Old port - Sargassum"    = c(0, 0, -1,  1, 0, 0, 0, 0),
          "New port - Sargassum"    = c(0, 0, -1,  0, 1, 0, 0, 0),
          "New port - Old port"     = c(0, 0,  0, -1, 1, 0, 0, 0))
glht(glm02, linfct = K) %>% summary()

#傾き比較
glht(glm02, mcp(location = "Tukey")) %>% summary()
K = rbind("Sargassum - Isoyake"     = c(0, 0,  0,  0, 0,  1,  0, 0),
          "Old port - Isoyake"      = c(0, 0,  0,  0, 0,  0,  1, 0),
          "New port - Isoyake"      = c(0, 0,  0,  0, 0,  0,  0, 1),
          "Old port - Sargassum"    = c(0, 0,  0,  0, 0, -1,  1, 0),
          "New port - Sargassum"    = c(0, 0,  0,  0, 0, -1,  0, 1),
          "New port - Old port"     = c(0, 0,  0,  0, 0,  0, -1, 1))
glht(glm02, linfct = K) %>% summary()


