library(tidyverse)
library(lubridate)
library(stringr)
library(glue)



dir("~/", include.dirs = T, pattern = "(kamigoto)|(mushima)_[0-9]{6}$", full = T)



data_frame(folder = dir("~/", include.dirs = T, pattern = "(kamigoto)|(mushima)_[0-9]{6}$", full = T)) %>% 
  mutate(fnames = map(folder, dir, pattern = "csv", recursive = T, full = T)) %>% 
  unnest() %>% 
  filter(str_detect(fnames, "Modified_data")) %>% 
  mutate(idx = str_extract(folder, "[0-9]{6}")) %>% 
  mutate(location = str_extract(folder, "kamigoto|mushima")) %>% 
  mutate(newname = str_replace(fnames, "data/", glue("data/{location}_"))) %>% 
  mutate(newname = str_replace(newname, ".csv", glue("_{idx}.csv"))) %>% 
  mutate(newname = basename(newname)) %>% 
  mutate(newname = glue("/home/kawatea/Lab_Data/kawatea/Modified_Data/{newname}")) %>% 
  # mutate(out = file.copy(from=fnames, to=newname))

