#全データ解析
#河手梓
#2019-01-10

#言語の変換
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

#エクセルファイルの時間データ処理用
fix_xlsx_date = function(x) {
  as.character(glue("{sprintf('%02d', day(x))}/{sprintf('%02d', month(x))}/{year(x)}"))
}
fix_xlsx_time = function(x) {
  as.character(glue("{sprintf('%02d', hour(x))}:{sprintf('%02d', minute(x))}:{sprintf('%02d', second(x))}"))
}

# 図の寸法
# A5寸法
WIDTH = 297/2
HEIGHT = 210/2

#言語の変換
Sys.getlocale("LC_TIME")#確認
Sys.setlocale("LC_TIME","en_US.UTF-8")#設定

fnames = dir("../",pattern = "kamigoto",full=T)
fnames = grep(x = fnames,pattern = "(\\.){2}//kamigoto_[0-9]{6}",value = T)
fnames = paste(fnames,"/Modified_data/", sep = "")

#CEM-------------------------------------------------------------
#読み込み
cem_file = data_frame(file = as.character(dir(fnames, pattern = "CEM", full.names = T)))
cem = cem_file %>% 
  mutate(cem = map(file,
                      read_csv))

cem = cem %>% unnest() %>% 
  select(-file) %>%
  rename(datetime = datetime2) %>%
  mutate(Date = as.Date(datetime))

#1日分のデータは144
#それ以外を見つける
# cem %>% 
#   group_by(location, Date) %>% 
#   summarise(N = n()) %>% 
#   filter(!near(N, 144)) %>% print(n = Inf)#nearは==と同じ

#とりあえず作図
ggplot(cem)+
  geom_line(aes(x = Date, y = mean_speed, color = location))

#tainouraのデータでおかしいものがある
#おかしなデータを確認
cem %>%
  group_by(location, Date) %>% 
  filter(str_detect(location, "tainoura")) %>%
  filter(!(Date == as.Date("2017-10-31"))) %>%
  unnest() %>%
  mutate(month_floor = floor_date(Date, "month")) %>%
  filter(month_floor == as.Date("2017-10-01")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = mean_speed, group = Date))

#おかしなデータ削除
cem = cem %>% filter(!(Date == as.Date("2017-11-18") & str_detect(location, "tainoura")))
cem = cem %>% filter(!(Date == as.Date("2017-10-31") & str_detect(location, "tainoura")))

#月ごとにグループ化できるようにする
cem = cem %>% 
  mutate(month = month(Date))

#作図
xlabel = ""
ylabel = expression("Velocity"~cm~sec^{-1})
gtitle = "Velocity of water"

cem %>% 
  group_by(location,month) %>% 
  drop_na() %>% 
  summarise(speed = mean(mean_speed)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = speed, color = location))+
  geom_line(aes(x = month, y = speed, color = location))+
  scale_x_continuous(name = xlabel,
                     labels = month_labels(),
                     breaks = 1:12,
                     limits = c(1,12)) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave(filename = "流速.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")

#ボックスプロット
cem %>% 
  group_by(location,month) %>% 
  drop_na() %>% 
  summarise(speed = mean(mean_speed)) %>% 
  ggplot()+
  geom_boxplot(aes(x = location,
                   y = speed,
                   fill = location,
                   group = interaction(location)))+
  xlab(xlabel) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave(filename = "流速ボックス.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")

# write_csv(cem, "../soturon_2019/Modified_data/CEM_fixed.csv")
#年間の平均
cem %>% group_by(location) %>% drop_na() %>% 
  summarise(mean(mean_speed))

#CKU-------------------------------------------------------------------
#読み込み
cku_file = data_frame(file = as.character(dir(fnames, pattern = "CKU", full.names = T)))
cku = cku_file %>%
  mutate(cem = map(file,
                   read_csv))

#locationの表記確認・訂正
cku %>% unnest() %>% pull(location) %>% unique()
cku = cku %>%
  unnest() %>%
  mutate(location = recode(location,
                           `ariiawaamamo` = "arikawaamamo",
                           `arikawagaramo` = "arikawaamamo")) %>%
  select(-file)

cku = cku %>% 
  rename(datetime = datetime2) %>% 
  mutate(Date = as.Date(datetime))

#1日分のデータは144のはず
#それ以外があるか確認
# cku %>% 
#   group_by(location, Date) %>% 
#   summarise(N = n()) %>% 
#   filter(!near(N, 144)) %>% print(n = Inf)#nearは==と同じ

#とりあえず作図
ggplot(cku)+
  geom_line(aes(x = Date, y = mean_chla, color = location))

# cem %>%
#   group_by(location, Date) %>% 
#   filter(str_detect(location, "tainoura")) %>%
#   filter(!(Date == as.Date("2017-10-31"))) %>%
#   unnest() %>%
#   mutate(month_floor = floor_date(Date, "month")) %>%
#   filter(month_floor == as.Date("2017-10-01")) %>%
#   ggplot() +
#   geom_line(aes(x = Date, y = mean_speed, group = Date))
# 
# cem = cem %>% filter(!(Date == as.Date("2017-11-18") & str_detect(location, "tainoura")))
# cem = cem %>% filter(!(Date == as.Date("2017-10-31") & str_detect(location, "tainoura")))

#月ごとにグループ化できるようにする
cku = cku %>% 
  mutate(month = month(Date))

#作図
xlabel = ""
ylabel = expression("chla"~μg~L^{-1})
gtitle = "chlorophyll fluorescence"

cku %>% 
  group_by(location,month) %>% 
  drop_na() %>% 
  summarise(chla = mean(mean_chla)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = chla, color = location))+
  geom_line(aes(x = month, y = chla, color = location))+
  scale_x_continuous(name = xlabel,
                     labels = month_labels(),
                     breaks = 1:12,
                     limits = c(1,12)) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 50),
                     breaks = c(0, 10, 20,30, 40, 50)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave(filename = "クロロフィル蛍光.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")

cku %>% 
  group_by(location,month) %>% 
  drop_na() %>% 
  summarise(chla = mean(mean_chla)) %>% 
  ggplot()+
  geom_boxplot(aes(x = location,
                   y = chla,
                   fill = location,
                   group = interaction(location)))+
  xlab(xlabel) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 50),
                     breaks = c(0, 10, 20,30, 40, 50)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave(filename = "クロロフィル蛍光ボックス.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")



#年間の平均
cku %>% group_by(location) %>%drop_na() %>%  
  summarise(mean(mean_chla))

#濁度
xlabel = ""
ylabel = expression("tubidity"~ppm~L^{-1})
gtitle = "Tubidty"

cku %>% 
  group_by(location,month) %>% 
  drop_na() %>% 
  summarise(turbidty = mean(mean_tur)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = turbidty, color = location))+
  geom_line(aes(x = month, y = turbidty, color = location))+
  scale_x_continuous(name = xlabel,
                     labels = month_labels(),
                     breaks = 1:12,
                     limits = c(1,12)) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 70),
                     breaks = c(0, 10, 20,30, 40, 50, 60, 70)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave(filename = "濁度.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")

#年間の平均
cku %>% group_by(location) %>% drop_na() %>% 
  summarise(mean(mean_tur))

# write_csv(cku,"../soturon_2019/Modified_data/CKU_fixed.csv")





