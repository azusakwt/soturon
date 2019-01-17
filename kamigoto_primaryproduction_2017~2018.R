# 卒論用
# 河手梓
# 2019年01月07日
# 
# パッケージの読み込み
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(mgcv) # GAM 解析用パッケージ
library(lemon)

#マイ関数------------------------------
my_date_format = function() {
  # 年・月の軸のフォーマットを定義
  function(x)
  {
    m <- format(x,"%m")
    y <- format(x,"%Y")
    ifelse(duplicated(y),
           paste0(m,"月"),
           paste0(m,"月\n", y, "年"))
  }
}

month_labels = function() {
  function(x) {
    paste0(x, "月")
  }
}

# 図の寸法
# A5寸法
WIDTH = 297/2
HEIGHT = 210/2

# 溶存酸素のデータの読み込み --------------------------------------------------------------------------------------
fnames = dir("~/Lab_Data/kawatea/Modified_Data/",pattern = "kamigoto_oxygen",full=T)
oxygen_file = data_frame(file = fnames)
oxygen = oxygen_file %>% 
  mutate(oxygen = map(file,
                      read_csv))
oxygen %>% slice(1) %>% unnest()
# 光データの読み込み--------------------------------------------------------------------------------------

light = read_csv("Modified_data/light_calibrate.csv")


#----------------------------------------------
oxygen =
  oxygen %>% unnest() %>% 
  select(-file)

oxygen %>% pull(location) %>% unique()
light %>% pull(location) %>% unique()

tmp %>% pull(position) %>% unique()
light %>% pull(position) %>% unique()


tmp =
  inner_join(oxygen %>% filter(str_detect(position, "[(0m)(1m)]")), 
             light  %>% filter(str_detect(position, "0m")), 
             by = c("datetime", "location","Date")) 

tmp %>% filter(str_detect(location, "tainoura"))

tmp  = tmp %>% mutate(ppfd = ifelse(ppfd < 1, 0, ppfd)) 

# tmp %>% filter(ppfd < 10) %>%  pull(ppfd) %>% unique()

tmp = tmp %>% mutate(light_group = ifelse(near(ppfd, 0), F, T))

oxygen %>% 
  group_by(Date, location, position) %>% nest() %>% 
  slice(1:10) %>% unnest() %>% 
  ggplot(aes(x = datetime, y=oxygen, color = position)) +
  geom_line()

tmp = tmp %>%
  select(Date, location, datetime, position.x,
         rate, oxygen, temperature, ppfd, H, light_group) %>% 
  rename(position = position.x)

tmp = 
  tmp %>% 
  group_by(Date, location, position, light_group) %>%
  summarise(rate_sum = sum(rate* 1),
            duration_hours = length(rate) / 6) 

tmp

tmp =
  tmp %>% 
  mutate(rate_sum = ifelse(!light_group, rate_sum/duration_hours*24, rate_sum)) %>% 
  select(-duration_hours)

# location 要因を英語から日本語へ変換する

tmp

tmp = 
  tmp %>% 
  ungroup() %>% 
  mutate(location = recode(location,
                           tainoura = 1, 
                           arikawaamamo = 2,
                           arikawagaramo = 3)) %>%
  mutate(location = factor(location, 
                           levels = c(1,2,3),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Zostera)",
                                     "Arikawa (Sargassum)"))) 


# 生産量の計算---------------------------------------------
rate = 
  tmp %>% 
  spread(key = light_group, value = rate_sum) %>% 
  rename(RP = `FALSE`, NEP = `TRUE`) %>% 
  mutate(RP = -RP)

# rate %>% filter(RP <= 0 ,location == "有川（アマモ場）") %>% 
#   print(n = Inf)

ggplot(rate)+
  geom_point(aes(x = Date,y = RP,color = position))+
  facet_grid(. ~ location)

# RP <= 0, GEP <= 0 のデータを解析から外します

rate = rate %>%
  mutate(RP = ifelse(RP <= 0, NA, RP)) %>% 
  mutate(GEP = NEP + RP) %>%
  mutate(GEP = ifelse(GEP <= 0, NA, GEP)) %>% 
  drop_na() 
  

rate_tall = 
  rate %>% 
  gather(key = "key", value = "value", -Date, -location,-position)

rate_tall = rate_tall %>% mutate(month = month(Date))

# write_csv(rate_tall,"../soturon_2019/Modified_data/kamigoto_production.csv")

p1 = 
  rate_tall %>% filter(str_detect("GEP", key)) %>% 
  ggplot() +
  geom_point(aes(x = Date, y = value)) +
  geom_line(aes(x = Date, y = value)) +
  scale_x_date("Date") +
  scale_y_continuous("GEP") +
  facet_grid(. ~ location)

p1

p2 = 
  rate_tall  %>% filter(str_detect("GEP", key)) %>% 
  mutate(month = factor(month)) %>% 
  ggplot() +
  geom_boxplot(aes(x=month, y = value, fill = location)) +
  facet_grid(. ~ location)
p2

# 図 (GEP) ------------------------------

# 軸ラベル
xlabel = ""
ylabel = expression("GEP"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = "生態系総一次生産速度"

#季節変動をみる--------------------------
# rate_tall_label=expand.grid(month = unique(rate_tall$month),
# location = unique(rate_tall$location))


# 解析（案）-------------------------------------------------------------------------------------------------
library(mgcv) # GAM 解析用パッケージ
# GEPとRP は正の値しか取れないので，Gamma 分布
# NEP は 正規分布，このとき，family の定義は不要。
# 一般化加法モデル


rate_tall %>% 
  group_by(location, key, month) %>% 
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  ggplot(aes(x = month, y = mean, color = location)) +
  geom_point() + 
  geom_line()+
  geom_smooth(method = "gam", formula = y ~ s(x))  + # デフォルトは正規分布
  facet_grid(location ~ key)


rate_tall  %>% filter(str_detect("GEP", key)) %>% pull(value) %>% range(na.rm = T)
rate_tall  %>% filter(str_detect("RP", key)) %>% pull(value) %>% range(na.rm = T)
rate_tall  %>% filter(str_detect("NEP", key)) %>% pull(value) %>% range(na.rm = T)

xlabel = ""
ylabel = expression("GEP"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = "生態系総一次生産速度"
gtitle = "Gross Ecosystem Production"

rate_tall  %>% 
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

# 一年分のデータ解析
dset01 = 
  rate_tall  %>% 
  filter(str_detect("GEP", key)) %>% 
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

rate_tall  %>% 
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
  rate_tall  %>% 
  filter(str_detect("GEP", key)) %>% 
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

coefficients(glm02)
exp(coefficients(glm02) [1])
exp(coefficients(glm02) [1] + coefficients(glm02) [3])
exp(coefficients(glm02) [1] + coefficients(glm02) [4])

(coefficients(glm02) [2])
(coefficients(glm02) [2] + coefficients(glm02) [5])
(coefficients(glm02) [2] + coefficients(glm02) [6])

dset01 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))


# Generalized Linear Hypothesis Testing (GLHT)
library(multcomp)

summary(glm02) # location間の比較は Wald's test (検出力低い)
summary(glm02)$coef

# 各パラメータを０と比較するとき
K = rbind("Tainoura Intercept"  = c(1, 0, 0, 0, 0, 0),
          "Tainoura Slope"      = c(0, 1, 0, 0, 0, 0),
          "Zostera Intercept"   = c(0, 0, 1, 0, 0, 0),
          "Sargassum Intercept" = c(0, 0, 0, 1, 0, 0),
          "Zostera Slope"       = c(0, 0, 0, 0, 1, 0),
          "Sargassum Slope"     = c(0, 0, 0, 0, 0, 1))
glht(glm02, linfct = K) %>% summary()


n = c(10, 20, 30)
contrMat(n, type = "Tukey")

# Tukey は切片の比較をする
glht(glm02, mcp(location = "Tukey")) %>% summary()
K = rbind("Zostera - Isoyake"      = c(0, 0,  1, 0, 0, 0),
          "Sargassum - Isoyake"    = c(0, 0,  0, 1, 0, 0),
          "Sargassum - Zostera"    = c(0, 0, -1, 1, 0, 0))
glht(glm02, linfct = K) %>% summary()

#################
# Tukey で角度を比較

K = rbind("Zostera - Isoyake"   = c(0, 0, 0, 0,  1,  0),
          "Sargassum - Isoyake" = c(0, 0, 0, 0,  0,  1),
          "Sargassum - Zostera" = c(0, 0, 0, 0, -1,  1))
glht(glm02, linfct = K) %>% summary()

#RPの解析-----------------------------------

rate_tall %>% 
  group_by(location, key, month) %>% 
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>% 
  ggplot(aes(x = month, y = mean, color = location)) +
  geom_point() + 
  geom_line()+
  geom_smooth(method = "gam", formula = y ~ s(x))  + # デフォルトは正規分布
  facet_grid(location ~ key)


xlabel = ""
ylabel = expression("RP"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = "Respiration Production"

rate_tall  %>% 
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

# 一年分のデータ解析
dset01 = 
  rate_tall  %>% 
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

rate_tall  %>% 
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
  rate_tall  %>% 
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

coefficients(glm02)
exp(coefficients(glm02) [1])
exp(coefficients(glm02) [1] + coefficients(glm02) [3])
exp(coefficients(glm02) [1] + coefficients(glm02) [4])

(coefficients(glm02) [2]) # Isoyake
(coefficients(glm02) [2] + coefficients(glm02) [5]) # Zostera
(coefficients(glm02) [2] + coefficients(glm02) [6]) # Sargassum

dset02 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))


# Generalized Linear Hypothesis Testing (GLHT)
library(multcomp)

summary(glm02) # location間の比較は Wald's test (検出力低い)
summary(glm02)$coef

# 各パラメータを０と比較するとき
K = rbind("Tainoura Intercept"  = c(1, 0, 0, 0, 0, 0),
          "Tainoura Slope"      = c(0, 1, 0, 0, 0, 0),
          "Zostera Intercept"   = c(0, 0, 1, 0, 0, 0),
          "Sargassum Intercept" = c(0, 0, 0, 1, 0, 0),
          "Zostera Slope"       = c(0, 0, 0, 0, 1, 0),
          "Sargassum Slope"     = c(0, 0, 0, 0, 0, 1))
glht(glm02, linfct = K) %>% summary()


# Tukey は切片の比較をする
glht(glm02, mcp(location = "Tukey")) %>% summary()
K = rbind("Zostera - Isoyake"      = c(0, 0,  1, 0, 0, 0),
          "Sargassum - Isoyake"    = c(0, 0,  0, 1, 0, 0),
          "Sargassum - Zostera"    = c(0, 0, -1, 1, 0, 0))
glht(glm02, linfct = K) %>% summary()

#################
# Tukey で角度を比較

K = rbind("Zostera - Isoyake"   = c(0, 0, 0, 0,  1,  0),
          "Sargassum - Isoyake" = c(0, 0, 0, 0,  0,  1),
          "Sargassum - Zostera" = c(0, 0, 0, 0, -1,  1))
glht(glm02, linfct = K) %>% summary()


#NEP-----------------------------------------------
xlabel = ""
ylabel = expression("NEP"~(g~O[2]~m^{-2}~day^{-1}))
gtitle = "Net Ecosystem Production"

rate_tall  %>% 
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

# 一年分のデータ解析
dset01 = 
  rate_tall  %>% 
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

# 各パラメータを０と比較するとき
K = rbind("Tainoura Intercept"  = c(1, 0, 0, 0, 0, 0),
          "Tainoura Slope"      = c(0, 1, 0, 0, 0, 0),
          "Zostera Intercept"   = c(0, 0, 1, 0, 0, 0),
          "Sargassum Intercept" = c(0, 0, 0, 1, 0, 0),
          "Zostera Slope"       = c(0, 0, 0, 0, 1, 0),
          "Sargassum Slope"     = c(0, 0, 0, 0, 0, 1))
glht(glm02, linfct = K) %>% summary()


# Tukey は切片の比較をする
glht(glm02, mcp(location = "Tukey")) %>% summary()
K = rbind("Zostera - Isoyake"      = c(0, 0,  1, 0, 0, 0),
          "Sargassum - Isoyake"    = c(0, 0,  0, 1, 0, 0),
          "Sargassum - Zostera"    = c(0, 0, -1, 1, 0, 0))
glht(glm02, linfct = K) %>% summary()

#################
# Tukey で角度を比較

K = rbind("Zostera - Isoyake"   = c(0, 0, 0, 0,  1,  0),
          "Sargassum - Isoyake" = c(0, 0, 0, 0,  0,  1),
          "Sargassum - Zostera" = c(0, 0, 0, 0, -1,  1))
glht(glm02, linfct = K) %>% summary()



