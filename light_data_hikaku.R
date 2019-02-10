#光データ
#河手　梓
#2019-01-07

# パッケージの読み込み
library(tidyverse)
library(lubridate)
library(readxl)
library(stringr)
library(glue)

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

# 図のテーマ
# source() は別のRファイルを読み込んで実行する
source("theme_kawate.R")

#言語の変換
Sys.getlocale("LC_TIME")#確認
Sys.setlocale("LC_TIME","en_US.UTF-8")#設定

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
                           label = c("Tainoura",
                                     "Arikawa (Zostera)",
                                     "Arikawa",
                                     "Mushima (Old port)" ,
                                     "Mushima (New port)" ))) 


dset_daily =light %>%  
  filter(!str_detect(location, "Zostera|Mushima")) 

#使う
# 標準偏差は sd()
# 標準誤差は sd() / sqrt(length()-1)
xlabel = ""
ylabel = expression("Daily underwater PPFD"~(mol~m^{-2}~day^{-1}))
gtitle = ""

dset_daily %>%
  mutate(month = month(Date)) %>% 
  group_by(month, location) %>% 
  summarise(daily_ppfd = mean(ppfd),
            daily_ppfd_sd = sd(ppfd),
            n = length(ppfd)) %>% 
  mutate(l95 = daily_ppfd - daily_ppfd_sd/sqrt(n - 1),
         u95 = daily_ppfd + daily_ppfd_sd/sqrt(n - 1)) %>% 
ggplot()+
  geom_point(aes(x = month,
                y = daily_ppfd, 
                color = location),
            position = position_dodge(0.1)) +
  geom_line(aes(x = month,
                y = daily_ppfd, 
                color = location),
            position = position_dodge(0.1)) +
  geom_errorbar(aes(x = month,
                    ymin = l95,
                    ymax = u95, 
                    color = location), width= 0,
                position = position_dodge(0.1)) +
  scale_x_continuous(name = xlabel, 
                     breaks = 1:12,
                     labels = month.abb[1:12]) +
  scale_y_continuous(name = ylabel) +
  scale_color_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.line = element_line())


ggsave(filename = "光環境.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")  

# guides() を消した
# geom_smooth() の aes() の中に linetype = "GAM" を指定した
dset_daily %>% 
  mutate(month = month(Date)) %>% 
  ggplot()+
  geom_boxplot(aes(x = month,
                   y = ppfd,
                   color = location,
                   fill = location,
                   group =  month),
               alpha = 0.5,
               size = rel(0.4)) +
  geom_smooth(aes(x= month, y = ppfd,
                  linetype = "GAM"),
              method = "gam",
              method.args = list(family = Gamma(link = "log")),
              fill = "black",
              color = "black",
              size = rel(0.4),
              alpha = 0.25,
              formula = y ~ s(x)) +
  scale_x_continuous(xlabel, 
                     minor_breaks = 1:12,
                     breaks = c(1, 5, 8, 12),
                     labels = month.abb[c(1, 5, 8, 12)]) +
  scale_y_continuous(name = ylabel) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_kawate(16) +
  facet_wrap("location", nrow = 1)

ggsave(filename = "光環境ボックス.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")  

library(mgcv)

dset03 = 
  dset_daily %>% 
  mutate(month = month(Date))

gam00 = gam(ppfd ~ s(month),
            family = Gamma(link = "log"),
            data = dset03)   # 帰無仮説：location の影響はない
gam01 = gam(ppfd ~ s(month) + location, 
            family = Gamma(link = "log"),
            data = dset03) # 対立仮設：location の影響はある
gam02 = gam(ppfd ~ s(month, by = location) + location, 
            family = Gamma(link = "log"),
            data = dset03) # 対立仮設：location の影響はある
# F検定をつかって，二つのモデルの比較

anova(gam00, gam01, gam02, test = "F")
summary(gam02)





#年間を通しての平均
dset_daily %>%
  group_by(location) %>% 
  summarise(mean(daily_ppfd))


