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

fnames = dir("~/Lab_Data/kawatea/CEM/", full = TRUE)

#CEM-------------------------------------------------------------
#読み込み
col_type = "nccnnnnnnnnnnnnnnnnn_"
cem_file = tibble(file = fnames)
cem = cem_file %>% 
  mutate(cem = map(file, read_csv, skip = 36, 
                   locale = locale(encoding = "CP932"),
                   col_type = col_type))

cem =
  cem %>% 
  mutate(file = basename(file)) %>% 
  separate(file, c("x", "y", "location", "position", "z")) %>% 
  unnest() %>% 
  mutate(datetime = ymd_hms(paste(`YYYY/MM/DD`, `hh:mm:ss`))) %>% 
  select(location, datetime,
         speed = starts_with("Velo["),
         velX = starts_with("Vel X["),
         velY = starts_with("Vel Y["),
         temperature = starts_with("Temp[")) %>% 
  filter(between(velX, -250, 250)) %>% 
  filter(between(velY, -250, 250))


kikan = read_xlsx("~/Lab_Data/kawatea/調査機関.xlsx")
kikan = kikan %>% filter(str_detect(location, "garamo|tainoura")) 
kikan = kikan %>% mutate(start_date = start_date + days(2))    

cem = full_join(kikan %>% unnest() %>% group_by(location) %>% nest(),
                cem %>% group_by(location) %>% nest(),
                by = "location")

# 期間範囲毎のフィルター (data.table パッケージのinrange()を使う)
cem = 
  cem %>% 
  mutate(data = map2(data.x, data.y, function(X, Y) {
    Y %>% filter(data.table::inrange(datetime, X$start_date, X$end_date)) %>% 
      mutate(H = hour(datetime) + minute(datetime) / 60) %>% 
      mutate(Date = as.Date(datetime))
  }))


cem = 
  cem %>% unnest(data) %>% 
  mutate(datetime2 = floor_date(datetime, unit = "minutes")) %>% 
  group_by(location, datetime2) %>% 
  summarise(mean_speed = mean(speed),
            sd_speed = sd(speed))

cem = cem %>% rename(datetime = datetime2)
cem = cem %>% mutate(Date = as.Date(datetime))

#とりあえず作図
ggplot(cem)+
  geom_line(aes(x = Date, y = mean_speed, color = location))

#tainouraのデータでおかしいものがある
#おかしなデータを確認
cem %>%
  group_by(location, Date) %>% 
  filter(str_detect(location, "arikawagaramo")) %>%
  # filter(!(Date == as.Date("2017-10-31"))) %>%
  unnest() %>%
  mutate(month_floor = floor_date(Date, "month")) %>%
  filter(month_floor == as.Date("2018-03-01")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = mean_speed, group = Date))

#おかしなデータ削除
cem = cem %>% filter(!(Date == as.Date("2017-11-18") & str_detect(location, "tainoura")))
cem = cem %>% filter(!(Date == as.Date("2017-10-31") & str_detect(location, "tainoura")))
cem = cem %>% filter(!(Date == as.Date("2018-03-19") & str_detect(location, "arikawagaramo")))
cem = cem %>% filter(!(Date == as.Date("2018-03-20") & str_detect(location, "arikawagaramo")))
cem = cem %>% filter(!(Date == as.Date("2018-03-21") & str_detect(location, "arikawagaramo")))
cem = cem %>% filter(!(Date == as.Date("2018-03-05") & str_detect(location, "arikawagaramo")))
cem = cem %>% filter(!(Date == as.Date("2018-03-16") & str_detect(location, "arikawagaramo")))
cem = cem %>% filter(!(Date == as.Date("2018-03-17") & str_detect(location, "arikawagaramo")))

#月ごとにグループ化できるようにする
cem = cem %>% 
  mutate(month = month(Date))

cem = 
  cem %>% 
  ungroup() %>% 
  mutate(location = recode(location,
                           "tainoura" = 1, 
                           "arikawagaramo" = 2)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Sargassum)")))


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
  group_by(location,Date) %>% 
  drop_na() %>% 
  summarise(speed = mean(mean_speed)) %>% 
  mutate(month = month(Date)) %>% 
  ggplot()+
  geom_boxplot(aes(x = month,
                   y = speed,
                   fill = location,
                   group = interaction(location, month))) +
  scale_x_continuous(name = xlabel,
                     labels = month.abb[1:12],
                     breaks = 1:12,
                     limits = c(0,12.5)) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 10),
                     breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())+
  facet_wrap("location", nrow = 1)


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

col_type = "nccnnnnnnnnnnn_"
fnames = dir("~/Lab_Data/kawatea/CKU/", full = TRUE)
cku_file = tibble(file = fnames)
cku = cku_file %>% 
  mutate(ckm = map(file, read_csv, skip = 36, 
                   locale = locale(encoding = "CP932"),
                   col_type = col_type))

cku =
  cku %>% 
  mutate(file = basename(file)) %>% 
  separate(file, c("x", "y", "location", "position", "z")) %>% 
  unnest() %>% 
  mutate(datetime = ymd_hms(paste(`YYYY/MM/DD`, `hh:mm:ss`))) %>% 
  select(location, datetime,
         chla  = `ｸﾛﾛ[   ]`,
         turbidity = starts_with("Turb[")) %>% 
  filter(between(turbidity, 0, 1000)) %>% 
  filter(between(chla, 0, 400))

cku = cku %>% mutate(location = recode(location, arikawaamamo = "arikawagaramo"))

kikan = read_xlsx("~/Lab_Data/kawatea/調査機関.xlsx")
kikan = kikan %>% filter(str_detect(location, "garamo|tainoura")) 
kikan = kikan %>% mutate(start_date = start_date + days(2)) %>% 
  mutate(X = end_date - start_date)

# kikan = kikan %>% filter(X > 5)  %>%
#   mutate(end_date = start_date + days(5)) 


cku = full_join(kikan %>% unnest() %>% group_by(location) %>% nest(),
                cku %>% group_by(location) %>% nest(),
                by = "location")

# 期間範囲毎のフィルター (data.table パッケージのinrange()を使う)
cku = 
  cku %>% 
  mutate(data = map2(data.x, data.y, function(X, Y) {
    Y %>% filter(data.table::inrange(datetime, X$start_date, X$end_date)) %>% 
      mutate(H = hour(datetime) + minute(datetime) / 60) %>% 
      mutate(Date = as.Date(datetime))
  }))


cku = 
  cku %>% unnest(data) %>% 
  mutate(datetime2 = floor_date(datetime, unit = "minutes")) %>% 
  group_by(location, datetime2) %>% 
  summarise(mean_chla = mean(chla),
            sd_chla = sd(chla),
            mean_tur = mean(turbidity),
            sd_tur = mean(turbidity))

cku = cku %>% rename(datetime = datetime2)
cku = cku %>% mutate(Date = as.Date(datetime))

#1日分のデータは144のはず
#それ以外があるか確認
# cku %>% 
#   group_by(location, Date) %>% 
#   summarise(N = n()) %>% 
#   filter(!near(N, 144)) %>% print(n = Inf)#nearは==と同じ

#とりあえず作図
ggplot(cku)+
  geom_line(aes(x = Date, y = mean_chla, color = location)) +
  scale_y_continuous(breaks = seq(0, 100, 10))

cku = cku %>% mutate(mean_chla = ifelse(mean_chla > 15, NA, mean_chla))
cku = cku %>% drop_na()
# cku = cku %>% filter(!(Date == as.Date("2018-03-21") & str_detect(location, "arikawaamamo")))

#月ごとにグループ化できるようにする
cku = cku %>% 
  mutate(month = month(Date))

cku = 
  cku %>% 
  ungroup() %>% 
  mutate(location = recode(location,
                           "tainoura" = 1, 
                           "arikawagaramo" = 2)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Sargassum)")))


#作図

xlabel = ""
ylabel = expression("Chl-a"~(mu*g~L^{-1}))
gtitle = "Chlorophyll fluorescence (mean and 1 standard deviation)"

# ログスケールでの平均値と標準偏差

cku %>% 
  group_by(location, Date, month) %>% 
  summarise(mean_chla = mean(mean_chla)) %>% 
  group_by(location,month) %>% 
  drop_na() %>% 
  summarise(chla = mean(log(mean_chla)),
            chla_sd = sd(log(mean_chla)),
            n = length(mean_chla)) %>% 
  mutate(l95 = chla - chla_sd/ sqrt(n - 1),
         u95 = chla + chla_sd/ sqrt(n - 1)) %>% 
  mutate(chla = exp(chla),
         l95 = exp(l95),
         u95 = exp(u95)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = chla, color = location), 
             position = position_dodge(0.1))+
  geom_line(aes(x = month, y = chla, color = location), 
            position = position_dodge(0.1))+
  geom_errorbar(aes(x = month, 
                    ymin = l95, ymax = u95, 
                    color = location), width = 0, 
                position = position_dodge(0.1))+
  scale_x_continuous(name = xlabel,
                     labels = month.abb[1:12],
                     breaks = 1:12) +
  scale_y_continuous(name = ylabel,
                     limits = c(0,10)) +
  scale_color_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave(filename = "クロロフィル蛍光.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")

cku %>% 
  mutate(month = month(Date)) %>% 
  ggplot()+
  geom_boxplot(aes(x = month,
                   y = mean_chla,
                   fill = location,
                   group = interaction(location, month))) +
  scale_x_continuous(name = xlabel,
                     labels = month.abb[1:12],
                     breaks = 1:12,
                     limits = c(0,12)) +
  xlab(xlabel) +
  # scale_y_continuous(name = ylabel,
  #                    limits = c(0, 50),
  #                    breaks = c(0, 10, 20,30, 40, 50)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())+
  facet_wrap("location", nrow = 1)



ggsave(filename = "クロロフィル蛍光ボックス.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")



#年間の平均
cku %>% group_by(location) %>%drop_na() %>%  
  summarise(mean(mean_chla))

#濁度
xlabel = ""
ylabel = expression("Turbidity"~(mg~L^{-1}))
gtitle = "Turbidity"

cku %>% 
  group_by(location, Date, month) %>% 
  summarise(mean_tur = mean(mean_tur)) %>% 
  group_by(location,month) %>% 
  drop_na() %>% 
  summarise(tur = mean(log(mean_tur)),
            tur_sd = sd(log(mean_tur)),
            n = length(mean_tur)) %>% 
  mutate(l95 = tur - tur_sd/sqrt(n - 1),
         u95 = tur + tur_sd/sqrt(n - 1)) %>% 
  mutate(tur = exp(tur),
         l95 = exp(l95),
         u95 = exp(u95)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = tur, color = location), 
             position = position_dodge(0.1))+
  geom_line(aes(x = month, y = tur, color = location), 
            position = position_dodge(0.1))+
  geom_errorbar(aes(x = month, 
                    ymin = l95, ymax = u95, 
                    color = location), width = 0, 
                position = position_dodge(0.1))+
  scale_x_continuous(name = xlabel,
                     labels = month.abb[1:12],
                     breaks = 1:12) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 30)) +
  scale_color_brewer(name = "", palette = "Dark2") +
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

cku %>% 
  mutate(month = month(Date)) %>% 
  ggplot()+
  geom_boxplot(aes(x = month,
                   y = (mean_tur),
                   fill = location,
                   group = interaction(location, month))) +
  scale_x_continuous(name = xlabel,
                     labels = month.abb[1:12],
                     breaks = 1:12,
                     limits = c(0,12)) +
  xlab(xlabel) +
  # scale_y_continuous(name = ylabel,
  #                    limits = c(0, 900),
  #                    breaks = c(0, 100, 200,300, 400, 500, 600, 700, 800, 900)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())+
  facet_wrap("location", nrow = 1)

ggsave(filename = "濁度ボックス.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")


