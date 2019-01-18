library(tidyverse)
library(readxl)
library(lubridate)
library(glue)


col_names = c("n", "datetime", "oxygen", "temperature")
col_type_csv = "ncnn"
col_type_xlsx = c("numeric", "text", "numeric", "numeric")
d1 = 
  tibble(fnames = dir("~/Lab_Data/kawatea/Oxygen/", full = TRUE),
       type = str_extract(fnames, "xlsx|csv")) %>% 
  mutate(data = map2(fnames, type, function(X, Y){
    if(str_detect(Y, "xlsx")) {
      read_xlsx(X, col_names = col_names, col_types = col_type_xlsx, skip = 2)
    } else {
      read_csv(X, col_names = col_names, col_types = col_type_csv, skip = 2)
    }
  }))

d1  = d1 %>% 
  mutate(fnames = basename(fnames)) %>% 
  separate(fnames, into = c("V1", "id", "location", "position", "V2")) %>% 
  filter(!str_detect(position, "calibra"))

  
kikan = read_xlsx("~/Lab_Data/kawatea/調査機関.xlsx")
kikan = kikan %>% group_by(location) %>% nest() %>% 
  mutate(location = recode(location, mushima = "mushima2"))

kikan = bind_rows(kikan, 
          kikan %>% 
            filter(str_detect(location, "mushima")) %>%
            mutate(location = recode(location, mushima2 = "mushima3")))
 
# kikan = kikan %>% 
#   mutate(data = map(data, function(X) {
#     X %>% 
#       mutate(start_date = start_date + days(2),
#              out = map2(start_date, end_date, function(x,y) {
#                seq(x, y, by = "1 day") %>% as.Date()
#              }))
#   })) %>% 
#   unnest() %>% unnest() %>% 
#   select(location, Date = out)

kikan = kikan %>% unnest() %>% 
  mutate(start_date = start_date + days(2))

d1 = 
  d1 %>% unnest() %>% 
  mutate(datetime = parse_date_time(datetime, c("mdyT!"), locale = "ja_JP.utf8")) %>% 
  select(-V2, -type, -V1) %>% 
  mutate(datetime = round_date(datetime, "10 mins")) %>% 
  mutate(Date = as.Date(datetime)) %>% 
  group_by(location) %>% nest()

out = full_join(kikan %>% unnest() %>% group_by(location) %>% nest(),
                d1 %>% unnest() %>% group_by(location) %>% nest(),
                by = "location")

out = 
  out %>% 
  mutate(data = map2(data.x, data.y, function(X, Y) {
    Y %>% filter(data.table::inrange(datetime, X$start_date, X$end_date)) %>% 
      mutate(H = hour(datetime) + minute(datetime) / 60)
  }))

out %>% unnest(data) %>% 
  group_by(location, Date, position) %>% 
  nest() %>% 
  mutate(chk = map_dbl(data, nrow)) %>% 
  filter(near(chk, 144))

# Calculate rates with GAM ----
# 10 分程度かかります。

calculate_rate = function(data) {
  out = mgcv::gam(oxygen ~ s(H, bs = "cs", k = 5), data = data)
  y1 = predict(out)
  eps = 1e-3
  y2 = predict(out, newdata = tibble(H = data$H+eps))
  data %>% mutate(rate = (y2-y1)/eps)
}

out = 
  out %>% 
  unnest(data) %>% 
  select(-id, -n) %>% 
  group_by(location, position, Date) %>% 
  nest() %>% 
  mutate(chk = map_dbl(data, nrow)) %>% 
  filter(near(chk, 144)) %>% 
  mutate(rate = map(data, calculate_rate)) 

out %>% unnest(rate) %>% 
  select(-chk) %>% 
  filter(str_detect(location, "arikawa|tainoura")) %>% 
  write_csv("~/Lab_Data/kawatea/Modified_Data/kamigoto_oxygen_all.csv")
  
out %>% unnest(rate) %>% 
  select(-chk) %>% 
  filter(!str_detect(location, "arikawa|tainoura")) %>% 
  write_csv("~/Lab_Data/kawatea/Modified_Data/mushima_oxygen_all.csv")
