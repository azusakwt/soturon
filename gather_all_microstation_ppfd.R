library(tidyverse)
library(readxl)
library(lubridate)
library(glue)


d1 = 
  tibble(fnames = dir("~/Lab_Data/kawatea/Microstation/", full = TRUE),
         type = str_extract(fnames, "xlsx|csv")) %>% 
  mutate(data = map2(fnames, type, function(X, Y){
    if(str_detect(Y, "xlsx")) {
      read_xlsx(X, skip = 1)
    } else {
      read_csv(X, skip = 1)
    }
  }))

d1 = 
  d1 %>% unnest() %>% 
  select(fnames, 
         datetime = contains("GMT"),
         ppfd = contains("PAR"),
         sol = contains("W"))

d1 = 
  d1 %>% 
  mutate(fnames = basename(fnames)) %>% 
  separate(fnames, c("x", "y", "location", "position", "z","w")) %>% 
  select(location, position, datetime:sol) %>% 
  mutate(datetime = parse_date_time(datetime, c("mdyT!"), locale = "ja_JP.utf8"))

d1 %>% 
  mutate(sol = ifelse(is.na(sol), 0, sol),
         ppfd = ifelse(is.na(ppfd), 0, ppfd)) %>% 
  mutate(ppfd_fixed = 4.6*sol + ppfd) %>% 
  mutate(datetime = round_date(datetime, "10 mins")) %>% 
  select(location, position, datetime, ppfd = ppfd_fixed) %>% 
  write_csv(path = "~/Lab_Data/kawatea/Modified_Data/surface_ppfd_all.csv")
