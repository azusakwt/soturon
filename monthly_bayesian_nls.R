#一次生産比較
#河手梓
#2019-01-09


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
library(rstan)
library(bayesplot)
library(brms)
library(broom)

#データの読み込み
df1 = read_csv("Modified_data/kamigoto_production.csv")
light = read_csv("Modified_data/light_calibrate.csv")
df1 = 
  df1 %>% 
  ungroup() %>% 
  mutate(location = recode(location,
                           "Tainoura (Isoyake)" = 1, 
                           "Arikawa (Zostera)" = 2,
                           "Arikawa (Sargassum)" = 3)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2,3),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Zostera)",
                                     "Arikawa (Sargassum)"))) 

df1 = df1 %>% filter(!str_detect(location, "Zostera")) 
df1 = df1 %>% mutate(location = factor(location)) # 外した要因のレベルも外す
df1 %>% pull(location)

df1 = df1 %>% spread(position, value) %>% 
  mutate(value = `1m`+`0m`)
df1
# write_csv(df1,"../soturon_2019/Modified_data/primary_production_0m+1m.csv")

light = 
  light %>% 
  group_by(location, Date) %>% 
  summarise(ppfd = 10*60*sum(ppfd, na.rm=T)/10^6) %>% 
  ungroup() %>% 
  filter(str_detect(location, "tainoura|arikawa")) %>% 
  mutate(location = recode(location,
                           "tainoura" = 1, 
                           "arikawaamamo" = 2,
                           "arikawagaramo" = 3)) %>% 
  mutate(location = factor(location, 
                           levels = c(1,2,3),
                           label = c("Tainoura (Isoyake)",
                                     "Arikawa (Zostera)",
                                     "Arikawa (Sargassum)"))) 

light %>% pull(ppfd) %>% range()

df2 = full_join(df1 %>% filter(str_detect(key, "NEP")) %>% drop_na() %>% 
                  select(Date, location, value),
                light, by = c("Date", "location") ) %>% 
  filter(str_detect(location, "Tainoura|Sarg"))

df2 %>% mutate(month = month(Date)) %>% 
  ggplot() +
  geom_point(aes(x = ppfd, y = value)) +
  facet_grid(month ~ location)



smodel1 = stan_model("pemodel_multilevel_saturating.stan")

fit_stan_model = function (LOCATION, nctrl) {
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
  
  
  tofit = 
    df2  %>% 
    filter(str_detect(location, LOCATION)) %>% 
    mutate(month = month(Date)) %>% 
    mutate(idx = as.numeric(as.factor(month))) %>% 
    arrange(idx) %>% 
    drop_na
  
  
  pnet = tofit %>% pull(value)
  ppfd = tofit %>% pull(ppfd)
  idx  = tofit %>% pull(idx)
  temperature = tofit %>% pull(ppfd)
  S = tofit %>% group_by(idx) %>% summarise(S = length(value)) %>% pull(S)
  
  max_ppfd = max(ppfd)
  K = length(unique(idx))
  N = length(pnet)
  M = 101
  
  nchains = 4
  ncores = nchains
  niter = 5000
  
  stanout1 =  sampling(smodel1, chains = nchains, cores = ncores,
                       iter = niter, control = nctrl,
                       refresh = 250)
  list(stanout1, tofit)
}


nctrl = list(adapt_delta = 0.99, max_treedepth = 11)
arikawa_out = fit_stan_model("Arikawa", nctrl = nctrl)
tainoura_out = fit_stan_model("Tainoura", nctrl = nctrl)

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


compile_fit = function(stanout) {
  
  fitdata = get_fitted_data(stanout[[1]], stanout[[2]])
  preddata = get_predicted_data(stanout[[1]], stanout[[2]])
  
  fitdata = 
    fitdata %>% 
    mutate(term = str_extract(term, "[0-9]+,[0-9]+")) %>% 
    separate(term, into = c("idx", "n")) %>% 
    mutate(idx = factor(idx, labels = month.abb[1:12]))
  
  preddata = 
    preddata %>% 
    mutate(term = str_extract(term, "[0-9]+,[0-9]+")) %>% 
    separate(term, into = c("idx", "n")) %>% 
    mutate(idx = factor(idx, labels = month.abb[1:12]))
  
  rawdata =
    stanout[[2]] %>% 
    select(ppfd, pnet = value, idx = idx)  %>% 
    mutate(idx = factor(idx, labels = month.abb[1:12]))
  tibble(fit = list(fitdata), predict = list(preddata), raw = list(rawdata))
  
}

arikawa = compile_fit(arikawa_out)
tainoura = compile_fit(tainoura_out)

## モデルに対する信用区間

## 予測値に対する信用区間
arikawa %>% unnest(predict) %>%
  ggplot()+
  geom_point(aes(x = ppfd, y = pnet, group = idx, color = idx), alpha = 0.5, 
             data = arikawa %>% unnest(raw)) +
  geom_line(aes(x = ppfd, y = estimate, color = idx)) +
  geom_ribbon(aes(x = ppfd, ymin = conf.low, ymax = conf.high, fill = idx), alpha = 0.5) +
  facet_wrap("idx", ncol = 2)

  ggplot() +
  geom_line(aes(x = m, y = pmax, color = "A"), 
            data = tibble(pmax = get_posterior_mean(arikawa_out[[1]], pars = "PMAX")[,"mean-all chains"],
                                                m = 1:12))+
  geom_line(aes(x = m, y = pmax, color = "T"), 
            data = tibble(pmax = get_posterior_mean(tainoura_out[[1]], pars = "PMAX")[,"mean-all chains"],
                                                m = 1:12))




