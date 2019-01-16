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
library(ggfortify)

#データの読み込み
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



alldata = 
  full_join(df1, light, by = c("location","month", "Date")) %>% 
  full_join(temperature, by = c("location", "month", "Date")) %>% 
  filter(!str_detect(location, "Zostera"))

alldata = 
  alldata %>% 
  mutate(log_ppfd = log(ppfd)) %>% 
  drop_na()


##########################################
dset01 = alldata %>% filter(str_detect(key, "NEP"))

pecurve = function(x, pmax, alpha, rd) {
  pmax * (1-exp(-alpha / pmax * x)) + rd
}

library(nlstools)

# preview(value ~ pecurve(ppfd, pmax, alpha, rd),
#         start = list(pmax = 10, alpha = 1, rd = 5),
#         data = dset01, 
#         variable = 6)

dset01 = dset01 %>% mutate(idx = as.numeric(location))

# 
# preview(value ~ 
#           pecurve(ppfd, pmax[1], alpha[1], rd[1])*(idx == 1) +
#           pecurve(ppfd, pmax[2], alpha[2], rd[2])*(idx == 3) +
#           pecurve(ppfd, pmax[3], alpha[3], rd[3])*(idx == 4) +
#           pecurve(ppfd, pmax[4], alpha[4], rd[4])*(idx == 5) ,
#         start = list(pmax = c(40,25,20,20), 
#                      alpha = c(0.5,0.5,0.5,0.5), 
#                      rd = c(1,1,1,1)),
#         data = dset01, 
#         variable = 6)

nullmodel = 
  nls(value ~ 
        pecurve(ppfd, pmax, alpha, rd),
      start = list(pmax = 10,alpha = 0.5, rd = 5),
      data = dset01)

fullmodel = 
  nls(value ~ 
          pecurve(ppfd, pmax[1], alpha[1], rd[1])*(idx == 1) +
          pecurve(ppfd, pmax[2], alpha[2], rd[2])*(idx == 3) +
          pecurve(ppfd, pmax[3], alpha[3], rd[3])*(idx == 4) +
          pecurve(ppfd, pmax[4], alpha[4], rd[4])*(idx == 5) ,
        start = list(pmax = c(10,15,5,20), 
                     alpha = c(0.5,0.5,0.5,0.5), 
                     rd = c(5,5,5,5)),
        data = dset01)

anova(nullmodel, fullmodel, test = "F")
summary(fullmodel)

CFS = coefficients(summary(fullmodel))
Estimate = CFS[1:4,"Estimate"]
SE = CFS[1:4, "Std. Error"]


# Estimate の高い純からペアを組んで比較する
Estimate %>% sort(index.return = T)

# 新港 - 有川
zval1 = (Estimate[4] - Estimate[2]) / sqrt(SE[4]^2 + SE[2]^2) # Z値

# 有川 - 鯛ノ浦
zval2 = (Estimate[2] - Estimate[1]) / sqrt(SE[2]^2 + SE[1]^2) # Z値

# 鯛ノ浦 - 六島旧港
zval3 = (Estimate[1] - Estimate[3]) / sqrt(SE[1]^2 + SE[3]^2) # Z値

tibble(compare = c("新港 - 有川", "有川 - 鯛ノ浦", "鯛ノ浦 - 旧港"),
       zval = c(zval1, zval2, zval3)) %>% 
  mutate(pval = 2 * pnorm(abs(zval), lower.tail = F))

CFS = coefficients(summary(fullmodel))
Estimate = CFS[5:8,"Estimate"]
SE = CFS[5:8, "Std. Error"]


# Estimate の高い純からペアを組んで比較する
Estimate %>% sort(index.return = T)

# 新港 - 旧港
zval1 = (Estimate[4] - Estimate[3]) / sqrt(SE[4]^2 + SE[3]^2) # Z値

# 旧港 - 有川
zval2 = (Estimate[3] - Estimate[2]) / sqrt(SE[3]^2 + SE[2]^2) # Z値

# 有川 - 鯛ノ浦
zval3 = (Estimate[2] - Estimate[1]) / sqrt(SE[2]^2 + SE[1]^2) # Z値

tibble(compare = c("新港 - 旧港", "旧港 - 有川浦", "有川 - 鯛ノ浦"),
       zval = c(zval1, zval2, zval3)) %>% 
  mutate(pval = 2 * pnorm(abs(zval), lower.tail = F))




dset01_nd = 
  dset01 %>% 
  expand(ppfd = seq(0, 80, length = 101), idx)

dset01_nd  = 
  dset01_nd %>% 
  mutate(fit = predict(fullmodel, newdata = dset01_nd)) 

dset01_nd = dset01_nd %>% 
  mutate(location = factor(idx,
                           levels = 1:5,
                           labels = c("Tainoura (Isoyake)",
                                      "Arikawa (Zostera)",
                                      "Arikawa (Sargassum)",
                                      "Mushima (Old port)" ,
                                      "Mushima (New port)" )))

dset01 %>%  
  ggplot() +
  geom_point(aes(x = ppfd, 
                 y = value, color = location)) +
  geom_line(aes(x = ppfd, y = fit),
            data = dset01_nd) +
  facet_rep_wrap("location")






# ここから下は検討中

dset01 = alldata %>% filter(str_detect(key, "GEP"))

m0 = glm(value ~ 1, data = dset01)
m1 = glm(value ~ ppfd + location, data = dset01)
m2 = glm(value ~ ppfd + temperature + location, data = dset01)
m3 = glm(value ~ ppfd * temperature * location, data = dset01)

anova(m0, m1, m2, m3, test = "F")
summary(m3)
plot(m3)

# 主成分解析
# https://logics-of-blue.com/principal-components-analysis/

dset01 = dset01 %>% mutate(log_ppfd = log(ppfd))


m0 = glm(value ~ 1, data = dset01)
m1 = glm(value ~ log_ppfd + location, data = dset01)
m2 = glm(value ~ log_ppfd + temperature + location, data = dset01)
m3 = glm(value ~ log_ppfd * temperature * location, data = dset01)

anova(m0, m1, m2, m3, test = "F")
summary(m3)
plot(m3)

dset01_mat = 
  dset01 %>% 
  select(temperature, log_ppfd) %>% 
  as.matrix() 

pout = princomp(dset01_mat, cor = TRUE)
plot(pout)
biplot(pout)

length(pout$scores[,1])

dset01 = 
  dset01 %>% 
  mutate(comp1 = pout$scores[,1],
         comp2 = pout$scores[,2])

dset01 %>% pull(value) %>% range()

ggplot(dset01) +
  geom_point(aes(x = comp2, 
                 y = value,
                 color = location)) +
  geom_smooth(aes(x = comp2, y = value, group = location),
              method = "glm",
              formula = y~x)  +
  facet_wrap("location")


m0 = glm(value ~ (comp1 + comp2) , data = dset01)
m1 = glm(value ~ (comp1 + comp2) * location, data = dset01)
anova(m0, m1, test = "F")
summary(m1)

