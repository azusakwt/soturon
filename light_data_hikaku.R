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


# キャリブレーションモデルの読み込み---------------------------------------------

load("../Light_calibration_20181225/20181218_Odyssey_Calibration_Model.rda", verbose = T)
calib_model_18 = calibration_coefficient_2
load("../Light_calibration_20181225/20181220_Odyssey_Calibration_Model.rda", verbose = T)
calib_model_20 = calibration_coefficient_2
calib_model_18 %>% pull(model)
#光ロガーデータ読み込み--------------------------------------
#拡張子がCSVとXLSXがあるが、時間補正のやり方が違うため、グループ化
light = 
  data_frame(fnames = dir("../", pattern = "Light_[0-9]{2}", 
                          recursive = T,
                          include.dirs = T, 
                          full = T)) %>% 
  filter(str_detect(fnames, "(kamigoto|mushima)_[0-9]{6}")) %>% 
  mutate(filetype = 
           ifelse(str_detect(fnames, "[Xx][Ll][Ss][Xx]"), "xlsx", "csv")) %>% 
  filter(str_detect(fnames, "/Light_")) %>% 
  group_by(filetype) %>% 
  nest() 

light %>% unnest() %>% arrange(fnames) %>% 
  separate(fnames,LETTERS[1:10]) %>% 
  filter(str_detect(G, "mushima")) %>% 
  print(n = Inf)

#時間補正
#CSVとXLSXは異なるやり方
#エクセルはマイ関数を使う
light = 
  light %>% 
  mutate(data = map2(data, filetype, function(data, filetype) {
    col_names = c("n", "DATE", "TIME", "raw", "ppfd")
    if(str_detect(filetype, "xlsx")) {
      data %>% 
        mutate(data = map(fnames, read_xlsx, skip = 9, 
                          col_types = c("numeric", 
                                        "date", 
                                        "date", 
                                        "numeric", 
                                        "numeric"),
                          col_names = col_names)) %>% 
        mutate(data = map(data, function(X) {
          X %>% 
            mutate(DATE = fix_xlsx_date(DATE),
                   TIME = fix_xlsx_time(TIME))
        }))
    } else {
      data %>% mutate(data = map(fnames, read_csv, skip = 9,
                                 col_types = "nccnn",
                                 col_names = col_names))
    }
  })) %>% 
  unnest()



light %>% arrange(fnames) %>% mutate(fnames = basename(fnames)) %>% 
  arrange(fnames) %>% 
  print(n = Inf)



light %>% 
  mutate(survey = str_extract(fnames, "[0-9]{6}")) %>% 
  mutate(fname = basename(fnames)) %>% 
  separate(fname,
           c("type","id","location","position"),
           sep = "_|\\.") %>% 
  select(-filetype) %>% print(n = Inf)


light = 
  light %>% 
  mutate(survey = str_extract(fnames, "[0-9]{6}")) %>% 
  mutate(fname = basename(fnames)) %>% 
  separate(fname,
           c("type","id","location","position"),
           sep = "_") %>% 
  select(-filetype) %>% 
  mutate(id = as.numeric(id))

light %>% pull(position) %>% unique()

# light %>% pull(position)
#名前を統一
#０ｍとsurfaceに変換
light = 
  light %>% 
  mutate(position = recode(position,
                           `0.5m` = "0m",
                           `surfece` = "surface",
                           `surface.CSV` = "surface"))

light %>% pull(position) %>% unique()

light = 
  light %>%
  mutate(data = map(data, function(X) {
    X %>% 
      mutate(datetime = dmy_hms(paste(DATE, TIME))) 
  }))

light = light %>% rename(light = data)

light = light %>% mutate(light = map(light, function(X) {
  X %>% 
    mutate(H = hour(datetime) + minute(datetime)/60,
           Date = as.Date(datetime))
  
}))
light %>% unnest()

light %>% 
  mutate(chk = map_dbl(light, function(X) {
    X %>% 
      group_by(H_floor = floor(H)) %>% 
      summarise(test = sum(ppfd)) %>% 
      filter(near(H_floor, 15)) %>% 
      pull(test)
  })) %>% print(n = Inf)

light = light %>% select(type, survey, id, location, position, light) %>% unnest()
calib_model = bind_rows(calib_model_18,calib_model_20)

dset = left_join(light %>% group_by(id) %>% nest(), 
                 calib_model, by = "id")

dset = 
  dset %>% mutate(data = map2(data, model, function(data, model) {
    data %>% mutate(ppfd = predict(model, newdata = data))
  }))  %>% 
  unnest(data) 

dset %>% 
  group_by(id, type, location, position, Date) %>% 
  summarise(N = n()) %>% 
  filter(!near(N, 144)) %>% print(n = Inf)

# start_time と end_time のフィルターをかける
# Linux のファイルには、一般的に 3 つのタイムスタンプがあります
# atime : 最終アクセス時刻 (access time)
# mtime : 最終変更時刻 (modify time)
# ctime : 最終ステータス変更時刻 (change time)

chousa_kikan = data_frame(fname = dir("../", pattern = "調査", recursive = T, full = T))
chousa_kikan %>% 
  mutate(info = map(fname, file.info)) %>% 
  unnest() %>% 
  arrange(mtime, atime, ctime) %>% 
  print(n = Inf)

filter_file = read_xlsx("../mushima_181219/調査期間(1).xlsx")
filter_file %>% pull(location) %>% unique()

tmp = filter_file %>% 
  filter(str_detect(location, "mushima")) %>% 
  mutate(location = recode(location, `mushima` = "mushima2"))

filter_file = bind_rows(filter_file, tmp) %>% 
  mutate(location = recode(location, `mushima` = "mushima3"))

filter_file %>% arrange(location) %>% print(n = Inf)
filter_file %>% group_by(location, start_date, end_date) %>% nest() %>% select(-data)

filter_file = 
  filter_file %>% 
  distinct() %>% 
  mutate(Date = map2(start_date, end_date, function(s,e) {
    as.Date(seq(s, e, by = "1 day"))
  })) %>% 
  select(location, Date) %>% 
  unnest() %>% 
  arrange(location, Date)

dset %>% pull(location) %>% unique() %>% sort()
filter_file %>% pull(location) %>% unique() %>% sort()

dset = left_join(filter_file, dset, by = c("location", "Date"))
dset = dset %>% drop_na()

# dset %>% group_by(location, id) %>% nest() %>% 
#   mutate(N = map_dbl(data, function(X) {
#     X %>% duplicated() %>% sum()
#   })) %>% print(n = Inf)

dset %>% group_by(location, id) %>% nest()

dset = 
  dset %>% group_by(location, id) %>% nest() %>% 
  mutate(data = map(data, function(X) {
    X %>% distinct()
  })) %>% unnest() 


dset = 
  dset %>%
  group_by(location, id, Date, position) %>% 
  nest() %>% 
  mutate(data = map(data, function(X) {
    X %>% mutate(ppfd = ppfd - min(ppfd)) %>% 
      mutate(ppfd = ifelse(ppfd <1, 0, ppfd))
  })) %>% unnest()

#各日付ごとの一日あたりの総 PPFD
# PPFD: mumol/m2/s から mol/m2/day
# 陸上の最大はだいたい 60 mol/m2/day

dT = 60 * 1 # サンプリングインターバル

dset_daily = 
  dset %>% unnest() %>% 
  group_by(Date, location, position, id) %>% 
  summarise(daily_ppfd = sum(ppfd * dT) * 10e-6) %>%
  drop_na()

dset_daily %>% pull(daily_ppfd) %>% range()

dset_daily %>% pull(daily_ppfd) %>% hist()

# dset %>% 
#   unnest() %>% 
#   group_by(id, type, location, position, Date) %>% 
#   summarise(N = n()) %>% 
#   filter(!near(N, 144)) %>% print(n = Inf)

dset = 
  dset %>% 
  unnest() %>% 
  group_by(id, type, location, position, Date) %>% 
  nest() %>% 
  mutate(N = map_int(data, function(X){
    X %>% summarise(N = length(ppfd)) %>% pull(N)
  })) %>% 
  filter(near(N, 144)) 
dset %>% filter(location=="tainoura")%>% print(n=Inf)

# dset %>% 
#   unnest() %>% 
#   ggplot() +
#   geom_line(aes(x = H, y = ppfd, group = Date)) +
#   facet_grid(location ~ position)


# dset %>% 
#   filter(str_detect(location, "mushima3"),
#          str_detect(position, "surface")) %>% 
#   unnest() %>% 
#   mutate(month_floor = floor_date(Date, "month")) %>% 
#   filter(month_floor == as.Date("2018-04-01")) %>% 
#   filter(Date == as.Date("2018-04-18"))

dset = dset %>% filter(!(Date == as.Date("2018-04-18") & str_detect(location, "mushima3")))

# dset %>% 
#   filter(str_detect(location, "mushima3"),
#          str_detect(position, "surface")) %>% 
#   unnest() %>% 
#   mutate(month_floor = floor_date(Date, "month")) %>% 
#   filter(month_floor == as.Date("2018-04-01")) %>% 
#   ggplot() +
#   geom_line(aes(x = H, y = ppfd, group = Date)) +
#   facet_wrap("Date")

dset_daily %>% 
  mutate(floor_month = floor_date(Date, "month")) %>%
  filter(str_detect(location, "mushima3")) %>% 
  ggplot(aes(x = Date, y = daily_ppfd, color = position)) +
  geom_line() +
  geom_point()+
  facet_wrap("floor_month", scales = "free")

dset %>% filter(str_detect(location, "tainoura")) %>% unnest()

dset %>% unnest() %>% select(location, position, Date, ppfd, datetime, H) %>% 
  write_csv(path = "Modified_data/light_calibrate.csv")
  
dset_daily = dset_daily %>% 
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


dset_daily =
  dset_daily %>% 
  filter(!str_detect(location, "Zostera")) %>% 
  filter(!str_detect(location, "Mushima"))
#作図----------------------------------
#お試し
ggplot(dset_daily)+
  geom_point(aes(x = Date, y= daily_ppfd, color = location))

#月ごとに平均
dset_daily %>%
  mutate(month = month(Date)) %>% 
  group_by(month, location) %>% 
  summarise(daily_ppfd = mean(daily_ppfd)) %>% 
  ggplot()+
  geom_point(aes(x = month, y = daily_ppfd, color = location)) + 
  geom_line(aes(x = month, y = daily_ppfd, color = location)) +
  scale_x_continuous(breaks = 1:12,
                   labels = month.abb[1:12]) +
  scale_color_brewer(palette = "Dark2") +
  guides(color = FALSE) +
  theme(axis.line = element_line())

#使う
xlabel = ""
ylabel = expression("PPFD"~(mol~m^{-1}~day^{-1}))
gtitle = "Daily PPFD"

dset_daily %>%
  mutate(month = month(Date)) %>% 
  group_by(month, location) %>% 
  summarise(daily_ppfd = mean(daily_ppfd)) %>% 
  ggplot()+
  geom_line(aes(x = month,
             y = daily_ppfd, 
             color = location)) +
  scale_x_continuous(name = xlabel,
                     labels = month_labels(),
                     breaks = 1:12,
                     limits = c(1,12)) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 20),
                     breaks = c(0, 5, 10, 15, 20)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())

ggsave(filename = "光環境.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")  

dset_daily %>% 
  mutate(month = month(Date)) %>% 
  ggplot()+
  geom_boxplot(aes(x = month,
                   y = daily_ppfd,
                   fill = location,
                   group = interaction(location, month))) +
  scale_x_continuous(name = xlabel,
                     labels = month_labels(),
                     breaks = 1:12,
                     limits = c(0,12)) +
  scale_y_continuous(name = ylabel,
                     limits = c(0, 20),
                     breaks = c(0, 5, 10, 15, 20)) +
  scale_fill_brewer(name = "", palette = "Dark2") +
  ggtitle(gtitle) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())+
  facet_wrap("location", nrow = 1)


ggsave(filename = "光環境ボックス.png", 
       width = WIDTH,
       height = HEIGHT,
       units = "mm")  

dset_daily %>% 
  geom_boxplot(aes(x = month,
                   y = daily_ppfd,
                   fill = location)) +
  # geom_line(aes(x = month,
  #               y = daily_ppfd, 
  #               color = location)) +
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


#年間を通しての平均
dset_daily %>%
  group_by(location) %>% 
  summarise(mean(daily_ppfd))


