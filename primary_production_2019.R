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

df1 %>% spread(key, value)

# 一年分のデータ解析
dset01 = 
  df1  %>% 
  filter(str_detect("GEP", key)) %>% 
  filter(!str_detect(location, "Zostera"))
# アマモなしの図
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

# むりやり，Z検定
# str(summary(gam01)) # オブジェクトの構造がみれる
Estimate = summary(gam02)$p.table[, "Estimate"]
SE = summary(gam01)$p.table[, "Std. Error"]
Estimate[2:4] = Estimate[2:4] + Estimate[1]

# Estimate の高い純からペアを組んで比較する
Estimate %>% sort(index.return = T)

# 新港 - 有川
zval1 = (Estimate[4] - Estimate[2]) / sqrt(SE[4]^2 + SE[2]^2) # Z値
2 * pnorm(abs(zval), lower.tail = F) # P値

# 有川 - 六島旧港
zval2 = (Estimate[2] - Estimate[3]) / sqrt(SE[2]^2 + SE[3]^2) # Z値
2 * pnorm(abs(zval), lower.tail = F) # P値

# 六島旧港 - 鯛ノ浦
zval3 = (Estimate[3] - Estimate[1]) / sqrt(SE[3]^2 + SE[1]^2) # Z値
2 * pnorm(abs(zval), lower.tail = F) # P値

tibble(compare = c("新港 - 有川", "有川 - 旧港", "旧港 - 鯛ノ浦"),
       zval = c(zval1, zval2, zval3)) %>% 
  mutate(pval = 2 * pnorm(abs(zval), lower.tail = F))

# location の順序を帰る

levels = dset01 %>% pull(location) %>% levels()
levels = levels[-2]

dset01 = 
  dset01 %>% 
  mutate(location = factor(location, levels = levels[c(4,2,3,1)]))

# GAM のモデルと ggplot のモデルは同じ？

# モデル期待値の計算
# dset01 %>% pull(location)
dset01_nd = 
  dset01 %>% 
  expand(month = seq(1, 12, length = 51), location) %>% 
  filter(!str_detect(location, "Zostera"))
  
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

ggsave(filename = "GAM入り年間総一次生産量.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")


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
gam02 = gam(value ~ s(month, bs = "cc", by = location) + location, 
            family = Gamma(link = "log"),
            data = dset01)
#

# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, gam02, test = "F")
summary(gam02)

#切片の確認
exp(coefficients(gam01) [1])　　
exp(coefficients(gam01) [1] + coefficients(gam01) [2])

dset01 %>% 
  group_by(location) %>% 
  summarise(mean = mean(value))

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
  facet_rep_wrap("location", nrow = 1) +
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
gam02 = gam(value ~ s(month, bs = "cc", by = location) + location, 
            data = dset01) # 対立仮設：location の影響はある
# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, gam02, test = "F")
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

df1  %>% 
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
  df1  %>% 
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


