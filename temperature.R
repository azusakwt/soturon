#水温
#河手梓
#2019-01-11

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


#データの読み込み-----------------------------------
#上五島
oxygen = read_csv("~/Lab_Data/kawatea/Modified_Data/kamigoto_oxygen_all.csv")


temperature_a = 
  oxygen %>% 
  unnest() %>% 
  select(Date, location, position, datetime, temperature) %>% 
  filter(str_detect(position,"0m"))
  
#重複しているデータを削除する
temperature_a = 
  temperature_a %>% group_by(location, Date) %>% nest() %>% 
  mutate(data = map(data, function(X) {
    X %>% distinct()
  })) %>% unnest() 

temperature_a %>% group_by(Date,location) %>% 
  summarise(N = n()) %>% 
  filter(!near(N,144))

#おかしな値を見つける
ggplot(temperature_a)+
  geom_line(aes(x = Date, y= temperature, color = location))

temperature_a %>%
  group_by(location, Date) %>% 
  filter(str_detect(location, "arikawaamamo")) %>%
  # filter(!(Date == as.Date("2018-07-05"))) %>%
  unnest() %>%
  mutate(month_floor = floor_date(Date, "month")) %>%
  filter(month_floor == as.Date("2018-08-01")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = temperature, group = Date))

#おかしな値を削除
temperature_a = temperature_a %>% filter(!(Date == as.Date("2018-04-19") & str_detect(location, "arikawagaramo")))
temperature_a = temperature_a %>% filter(!(Date == as.Date("2018-04-19") & str_detect(location, "tainoura")))
temperature_a = temperature_a %>% filter(!(Date == as.Date("2018-05-16") & str_detect(location, "tainoura")))
temperature_a = temperature_a %>% filter(!(Date == as.Date("2018-05-17") & str_detect(location, "tainoura")))
temperature_a = temperature_a %>% filter(!(Date == as.Date("2018-06-21") & str_detect(location, "tainoura")))
temperature_a = temperature_a %>% filter(!(Date == as.Date("2018-06-21") & str_detect(location, "arikawaamamo")))
temperature_a = temperature_a %>% filter(!(Date == as.Date("2018-06-21") & str_detect(location, "arikawagaramo")))
temperature_a = temperature_a %>% filter(!(Date == as.Date("2018-07-05") & str_detect(location, "tainoura")))

#1日ごとに平均をとった時の図
temperature_a %>% 
  group_by(Date,location) %>% 
  summarise(mean_day_temperature = mean(temperature)) %>% 
  ggplot()+
  geom_line(aes(x = Date, y = mean_day_temperature,color = location))

#月ごと
temperature = temperature_a %>% 
  mutate(month = month(Date)) 

temperature_a %>% 
  group_by(location,month) %>% 
  summarise(mean_temperature = mean(temperature)) %>% 
  ggplot()+
  geom_line(aes(x = month, y = mean_temperature, color = location))

temperature = temperature %>% 
  ungroup() %>%
  mutate(location = recode(location,
                           "tainoura" = 1,
                           "arikawaamamo" = 2,
                           "arikawagaramo" = 3,
                           "mushima2" = 4,
                           "mushima3" = 5))  %>%
  mutate(location = factor(location,
                           levels = c(1,2,3,4,5),
                           label = c("Tainoura",
                                     "Arikawa (Zostera)",
                                     "Arikawa",
                                     "Mushima (Old port)" ,
                                     "Mushima (New port)" ))) %>% 
  group_by(location, Date) %>% 
  summarise(temperature = mean(temperature, na.rm=T)) %>% 
  mutate(month = month(Date)) 

temperature = temperature %>% 
  filter(!str_detect(location, "Zostera"))

xlabel = ""
ylabel = expression(Temperature~(degree*C))
gtitle = ""

temperature %>%
  mutate(month = month(Date)) %>% 
  ggplot() +
  geom_boxplot(aes(x = month,
                   y = temperature,
                   color = location,
                   fill = location,
                   group =  month),
               alpha = 0.5,
               size = rel(0.4)) +
  geom_smooth(aes(x= month, y = temperature,
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
  scale_y_continuous(name = ylabel,
                     limits = c(10, 30),
                     breaks = c(10, 15, 20, 25, 30)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_kawate(16) +
  facet_wrap("location", nrow = 1)

temperature %>% 
  group_by(location, month) %>% 
  summarise_at(vars(temperature), funs(mean, sd, min, max)) %>% 
  arrange(month, location) %>% 
  print(n = Inf)

WIDTH = 297/2
HEIGHT = 210/2

ggsave(filename = "水温ボックス.png", 
       width = WIDTH,
       height = HEIGHT,
       dpi = 600,
       units = "mm")

#六島------------------------------------------------------------------------
fnames = dir("../",pattern = "mushima",full=T)
fnames = grep(x = fnames,pattern = "(\\.){2}//mushima_[0-9]{6}",value = T)
fnames = paste(fnames,"/Modified_data/", sep = "")
oxygen_file = data_frame(file = as.character(dir(fnames, pattern = "oxygen", full.names = T)))
temperature_m = oxygen_file %>% 
  mutate(temperature_m = map(file,
                           read_csv))
temperature_m %>% slice(1) %>% unnest()

temperature_m = 
  temperature_m %>% 
  unnest() %>% 
  select(Date, location, position, datetime, temperature) %>% 
  filter(str_detect(position,"0m"))

temperature_m %>% 
  group_by(Date,location) %>% 
  summarise(N=n()) %>% 
  filter(!near(N,144)) %>% 
  print(n=Inf)

#重複したデータを削除
temperature_m = 
  temperature_m %>% group_by(location, Date) %>% nest() %>% 
  mutate(data = map(data, function(X) {
    X %>% distinct()
  })) %>% unnest() 



#おかしな値を見つける
ggplot(temperature_m)+
  geom_line(aes(x = Date, y= temperature, color = location))

temperature_m %>%
  group_by(location, Date) %>% 
  filter(str_detect(location, "mushima3")) %>%
  # filter(!(Date == as.Date("2018-05-16"))) %>%
  unnest() %>%
  mutate(month_floor = floor_date(Date, "month")) %>%
  filter(month_floor == as.Date("2018-09-01")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = temperature, group = Date))

#おかしな値を削除
temperature_m = temperature_m %>% filter(!(Date == as.Date("2018-05-16") & str_detect(location, "mushima3")))
temperature_m = temperature_m %>% filter(!(Date == as.Date("2018-05-16") & str_detect(location, "mushima2")))
temperature_m = temperature_m %>% filter(!(Date == as.Date("2018-08-07") & str_detect(location, "mushima3")))


ggplot(temperature_m)+
  geom_line(aes(x = Date, y= temperature, color = location))

#1日ごとに平均をとった時の図
temperature_m %>% 
  group_by(Date,location) %>% 
  summarise(mean_day_temperature = mean(temperature)) %>% 
  ggplot()+
  geom_line(aes(x = Date, y = mean_day_temperature,color = location))

#月ごと
temperature_m = temperature_m %>% 
  mutate(month = month(Date)) 

temperature_m %>% 
  group_by(location,month) %>% 
  summarise(mean_temperature = mean(temperature)) %>% 
  ggplot()+
  geom_line(aes(x = month, y = mean_temperature, color = location))

write_csv(temperature,"../soturon_2019/Modified_data/temperature.csv")

temperature = bind_rows(temperature_a,temperature_m)

t_test = temperature %>% 
  group_by(location,Date) %>% 
  drop_na() %>% 
  summarise(temperature_max = sum(temperature))
  
  
ggplot(t_test)+
  geom_line(aes(x = Date, y = temperature_max, color = location))


t_test = t_test %>% 
  mutate(month = month(Date)) 
  

t_test%>% 
  group_by(location,month) %>% 
  summarise(temperature_max = mean(temperature_max)) %>% 
  ggplot()+
  geom_line(aes(x = month, y = temperature_max, color = location))
  
temperature %>% ungroup() %>% 
  filter(Date>"2018-01-01") %>% 
  summarise(temperature = mean(temperature))

temperature %>% filter(temperature>20.3) %>% 
  group_by(location,Date) %>% 
  summarise(temperature = mean(temperature)) %>% 
  filter(Date>"2018-01-01") %>% 
  group_by(location) %>% nest()
