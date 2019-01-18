# Non-linear Bayesian Model of P-E Curve

library(magrittr)
library(tidyverse)
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

dset01 = 
  dset01 %>% 
  mutate(month = month(Date))

dset01 %>% 
  group_by(Date, location) %>% 
  filter(light_group) %>% 
  ggplot() +
  geom_point(aes(x = ppfd, y = rate, group = Date), alpha = 0.25) +
  facet_grid(location~month)


pecurve = function(x, pmax, alpha, rd) {
  pmax * (1 - exp(-alpha/pmax * x)) - rd
}
try_pecurve = possibly(pecurve, NULL)


dset01 %>% 
  group_by(Date, location, position) %>% 
  nest() %>% 
  spread(position, data) %>% 
  slice(807) %>% unnest(`0m`) %>% 
  print(n = Inf)
  mutate(data = map2(`0m`, `1m`, function(X, Y) {
    
    rate = X$rate + Y$rate
    tibble(datetime = X$datetime, rate, X$temperature, X$ppfd, X$H, X$light_group)
  }))








smodel1 = stan_model("pemodel_multilevel_saturating.stan")

# 事前分布の場所（平均）
priormuPMAXmu      = 300
priormuALPHAmu     = log(3)
priormuRESPmu      = 5

# 事前分布の散らばり（標準偏差）
priormuPMAXsigma   = abs(priormuPMAXmu) * 50
priormuALPHAsigma  = 1.5
priormuRESPsigma   = abs(priormuRESPmu) * 50
priorCauchy        = 2.5

pnet = dset01 %>% pull(value)
ppfd = dset01 %>% pull(ppfd)
temperature = dset01 %>% pull(temperature)
idx = dset01 %>% pull(idx)
S = dset01 %>% group_by(idx) %>% summarise(S = length(value)) %>% pull(S)

max_ppfd = max(ppfd)
K = length(unique(idx))
N = length(pnet)
M = 101

nchains = 4
ncores = nchains
niter = 2000

nctrl = list(adapt_delta = 0.95,
             max_treedepth = 12)

stanout1 =  sampling(smodel1, chains = nchains, cores = ncores,
                     iter = niter,
                     control = nctrl,
                     refresh = 250)

print(stanout1, pars = c("muPMAX", "muALPHA", "muRESP", "sigmaNP"))
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
  mutate(term = str_extract(term, "[1-2],[0-9]+")) %>% 
  separate(term, into = c("idx", "n")) %>% 
  mutate(idx = factor(as.numeric(idx), label = c("Tainoura (Isoyake)", 
                                                 "Arikawa (Sargassum)")))

preddata = 
  preddata %>% 
  mutate(term = str_extract(term, "[1-2],[0-9]+")) %>% 
  separate(term, into = c("idx", "n")) %>% 
  mutate(idx = factor(as.numeric(idx), label = c("Tainoura (Isoyake)", 
                                                 "Arikawa (Sargassum)")))

rawdata =
  dset01 %>% 
  select(ppfd, pnet = value, idx = idx) %>%
  mutate(idx = factor(idx)) %>% 
  mutate(idx = factor(as.numeric(idx), label = c("Tainoura (Isoyake)", 
                                                 "Arikawa (Sargassum)")))

## モデルに対する信用区間
fitdata %>%
  ggplot()+
  geom_point(aes(x = ppfd, y = pnet, group = idx, color = idx), alpha = 0.5, data = rawdata) +
  geom_line(aes(x = ppfd, y = estimate, color = idx)) +
  geom_ribbon(aes(x = ppfd, ymin = conf.low, ymax = conf.high, fill = idx), alpha = 0.5) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2")  +
  facet_wrap("idx", ncol = 1)


  ## 予測値に対する信用区間
preddata %>%
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


