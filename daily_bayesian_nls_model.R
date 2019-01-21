# Non-linear Bayesian Model of P-E Curve

library(magrittr)
library(tidyverse)
library(lubridate)
library(rstan)
library(broom)
library(brms)
library(loo)
library(bayesplot)

# Set these for rstan
set.seed(2019)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)

dset01 = read_csv("~/Lab_Data/kawatea/raw_rate_data.csv")
sppfd = read_csv("~/Lab_Data/kawatea/Modified_Data/surface_ppfd_all.csv")

sppfd = 
  sppfd %>% group_by(location, position) %>% nest() %>% 
  filter(str_detect(location, "tainoura|arikawa")) 

sppfd = bind_rows(sppfd, 
          sppfd %>% filter(str_detect(location, "arikawa")) %>% 
            mutate(location = "arikawagaramo")) %>% 
  mutate(location = recode(location,
                           tainoura = 1, 
                           arikawaamamo = 2,
                           arikawagaramo = 3)) %>%
  mutate(location = factor(location, 
                           levels = c(1,2,3),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Zostera)",
                                     "Arikawa (Sargassum)"))) %>% 
  filter(str_detect(location, "Isoyake|Sargassum"))

dset01 = 
  dset01 %>%   
  mutate(location = recode(location,
                                      tainoura = 1, 
                                      arikawaamamo = 2,
                                      arikawagaramo = 3)) %>%
  mutate(location = factor(location, 
                           levels = c(1,2,3),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Zostera)",
                                     "Arikawa (Sargassum)"))) %>% 
  filter(str_detect(location, "Isoyake|Sargassum"))

sppfd = 
  sppfd %>% 
  unnest() %>% 
  mutate(Date = as.Date(datetime)) %>% 
  group_by(location, Date) %>% 
  mutate(ppfd = ppfd - min(ppfd),
         month = month(Date))

dset01 = 
  dset01 %>% 
  mutate(month = month(Date))



pecurve = function(x, pmax, alpha, rd) {
  pmax * (1 - exp(-alpha/pmax * x)) - rd
}

dset02 = inner_join(dset01,
                    sppfd %>% rename(sppfd = ppfd) %>% 
                      select(location, datetime, sppfd, Date),
                    by = c("location", "datetime", "Date"))

dset02 = dset02 %>% mutate(ampm_group = ifelse(H < 12, "AM", "PM"))

dset02 = 
  dset02 %>% 
  group_by(Date, location, position) %>% 
  nest() %>% 
  spread(position, data) %>% 
  mutate(data = map2(`0m`, `1m`, function(X, Y) {
  if(is.null(X) | is.null(Y)) {
    NULL
  } else {
    rate = X$rate + Y$rate
    tibble(datetime = X$datetime, rate, 
           temperature = X$temperature, 
           ppfd = X$ppfd, 
           sppfd = X$sppfd,
           H = X$H, 
           light_group = X$light_group,
           ampm_group = X$ampm_group)
  }
}))



p1 = dset02 %>% 
  mutate(chk = map_lgl(data, is.null)) %>% 
  filter(!chk) %>%  
  unnest(data) %>% 
  mutate(month = month(datetime)) %>% 
  group_by(location, month) %>% 
  filter(month == 1) %>% 
  ggplot() +
  geom_line(aes(x = H, y = rate, group = Date, color = light_group)) +
  facet_grid(location~month)

  dset02 %>% 
  mutate(chk = map_lgl(data, is.null)) %>% 
  filter(!chk) %>%  
  unnest(data) %>% 
  mutate(month = month(datetime)) %>% 
  group_by(location, month) %>% 
  filter(month == 10) %>% 
  filter(str_detect(location, "Tai")) %>% 
  ggplot() +
  geom_path(aes(x = ppfd, y = rate, group = Date, 
                 color = ampm_group)) +
  facet_wrap("Date")

p3 = 
  dset02 %>% 
  mutate(chk = map_lgl(data, is.null)) %>% 
  filter(!chk) %>%  
  unnest(data) %>% 
  mutate(month = month(datetime)) %>% 
  group_by(location, month) %>% 
  filter(month == 1) %>% 
  ggplot() +
  geom_path(aes(x = ppfd, y = rate, group = Date, 
                color = ampm_group)) +
  facet_grid(location ~ ampm_group)

gridExtra::grid.arrange(p1,p2,p3)

try_pecurve = possibly(
  function(X) {nls(rate ~ try_pecurve(ppfd, pmax, alpha, rd),
      data = X,
      start = list(pmax = 0.5, alpha = 0.001, rd = 0.1))},
  NULL)

dset02 = dset02 %>% mutate(model = map(data, try_pecurve))
dset02



dset02 %>% 
  mutate(chk = map_lgl(data, is.null)) %>% 
  filter(!chk) %>%  
  unnest(data) %>% 
  mutate(month = month(datetime)) %>% 
  group_by(location, month) %>% 
  ggplot() +
  geom_path(aes(x = ppfd, y = rate, group = Date, 
                color = ampm_group)) +
  facet_grid(. ~ location)


smodel1 = stan_model("pemodel_multilevel_saturating.stan")
smodel2 = stan_model("pemodel_multilevel_inhibiting.stan")

# 事前分布の場所（平均）
priormuPMAXmu      = 1
priormuALPHAmu     = log(0.01)
priormuBETAmu     = log(0.01)
priormuRESPmu      = 1

# 事前分布の散らばり（標準偏差）
priormuPMAXsigma   = abs(priormuPMAXmu) * 1
priormuALPHAsigma  = 1.
priormuBETAsigma  =  1.
priormuRESPsigma   = abs(priormuRESPmu) * 1
priorCauchy        = 2.5


arikawa = 
  dset02 %>% 
  filter(str_detect(location, "Arikawa")) %>% 
  mutate(chk = map_lgl(data, is.null)) %>% 
  filter(!chk) %>%  
  unnest(data) %>% 
  mutate(ampm_group = factor(ampm_group)) %>% 
  mutate(month = month(Date))

arikawa = 
  arikawa %>% 
  group_by(Date, month) %>% 
  nest() %>% 
  group_by(month) %>% 
  sample_n(5) %>% 
  unnest() %>% 
  ungroup() %>% 
  filter(str_detect(ampm_group, "AM")) %>% 
  mutate(idx = as.numeric(as.factor(month))) %>% 
  arrange(idx)

pnet = arikawa %>% pull(rate)
ppfd = arikawa %>% pull(ppfd)
idx = arikawa %>% pull(idx)
idx %>% unique()
temperature = arikawa %>% pull(temperature)
S = arikawa %>% group_by(idx) %>% summarise(S = length(rate)) %>% pull(S)
S

max_ppfd = max(ppfd)
K = length(unique(idx))
N = length(pnet)
M = 101

nchains = 4
ncores = nchains
niter = 2000

nctrl = list(adapt_delta = 0.90,
             max_treedepth = 10)

stanout1 =  sampling(smodel1, chains = nchains, cores = ncores,
                     iter = niter,
                     control = nctrl,
                     refresh = 250)

stanout2 =  sampling(smodel2, chains = nchains, cores = ncores,
                     iter = niter,
                     control = nctrl,
                     refresh = 250)

print(stanout1, pars = c("muPMAX", "muALPHA", "muRESP", "sigmaNP"))
print(stanout2, pars = c("muPMAX", "muALPHA", "muBETA", "muRESP", "sigmaNP"))

# save(stanout1, file = "stanout1.rda")




# モデル診断
np = nuts_params(stanout1)
lp = log_posterior(stanout1)
neff = neff_ratio(stanout1)
stanout1.rhat = rhat(stanout1)
stanout1.array = as.array(stanout1)

# "exceeded the maximum tree depth" が残っているとだめ
# "divergent transitions" があるとだめ
check_treedepth(stanout1)
check_divergences(stanout1)

# PARS = c("muPMAX", "muALPHA", "muRESP")
# mcmc_trace(stanout1.array, regex_pars = PARS, np = np)
# 
# PARS = c("muPMAX", "muALPHA", "muRESP")
# mcmc_pairs(stanout1.array, regex_pars = PARS, np = np)
# mcmc_rhat(stanout1.rhat)
# mcmc_neff(stanout1.rhat)


# データと当てはめたモデルの図 -----
get_fitted_data = function(X, Y) {
    pars = parnames(X)
    max_ppfd = Y %>% pull(ppfd) %>% max()
    X %>% tidy(
      pars = grep("yhat", pars, value = TRUE),
      conf.int = TRUE,
      conf.level = 0.8,
      conf.method = "HPDinterval") %>%
      mutate(ppfd = seq(0, max_ppfd, length = length(estimate)))
  }

get_predicted_data = function(X, Y) {
  pars = parnames(X)
  max_ppfd = Y %>% pull(ppfd) %>% max()
  X %>% tidy(
    pars = grep("ypred", pars, value = TRUE),
    conf.int = TRUE,
    conf.level = 0.8,
    conf.method = "HPDinterval") %>%
    mutate(ppfd = seq(0, max_ppfd, length = length(estimate)))
}

fitdata = get_fitted_data(stanout1, dset01)
preddata = get_predicted_data(stanout1, dset01)

fitdata = 
  fitdata %>% 
  mutate(term = str_extract(term, "[0-9]+,[0-9]+")) %>% 
  separate(term, into = c("idx", "n"))

preddata = 
  preddata %>% 
  mutate(term = str_extract(term, "[0-9]+,[0-9]+")) %>% 
  separate(term, into = c("idx", "n")) 

rawdata =
  arikawa %>% 
  select(ppfd, pnet = rate, idx = idx)  %>% 
  mutate(idx = factor(idx))

## モデルに対する信用区間
fitdata %>%
  mutate(idx = factor(idx)) %>% 
  ggplot()+
  geom_point(aes(x = ppfd, y = pnet, group = idx, color = idx), 
             alpha = 0.5, data = rawdata) +
  geom_line(aes(x = ppfd, y = estimate, color = idx)) +
  # geom_ribbon(aes(x = ppfd, ymin = conf.low, ymax = conf.high, fill = idx), alpha = 0.5) +
  facet_wrap("idx")


  ## 予測値に対する信用区間
preddata %>%
  mutate(idx = factor(idx)) %>% 
  ggplot()+
  geom_point(aes(x = ppfd, y = pnet, group = idx, color = idx), alpha = 0.5, data = rawdata) +
  geom_smooth(aes(x = ppfd, y = pnet, group = idx, color = idx), alpha = 0.5, data = rawdata,
              method = "glm", 
              formula = y~x) +
  geom_line(aes(x = ppfd, y = estimate, color = idx)) +
  geom_ribbon(aes(x = ppfd, ymin = conf.low, ymax = conf.high, fill = idx), alpha = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")  +
  facet_wrap("idx", ncol = 2)

tibble(pmax = get_posterior_mean(stanout1, pars = "PMAX")[,"mean-all chains"],
       m = 1:10) %>% 
  ggplot() +
  geom_line(aes(x = m, y = pmax))

