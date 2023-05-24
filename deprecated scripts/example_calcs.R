
library(sdmTMB)
library(tidyverse)
# m is the model dataframe I read in -- your RDS
m <- read_rds(here::here('model output','dover sole models.rds'))
# first fit the null model, intercept only
fit <- m$model[[1]]
dat <- fit$data
null <- sdmTMB(cpue_kg_km2 ~ 1,
               spatial="off",
               mesh = fit$mesh,
               data = fit$data)
null_dev <- -2 * as.numeric(logLik(null))

log_lik <- unlist(lapply(m$model, logLik))
m$resid_dev <- -2 * log_lik
m$null_dev <- null_dev
m$dev_explained <- 100 * (m$null_dev - m$resid_dev)/m$null_dev

# now for residual weighting. note the residuals here 
# are the super fast PIT version -- we could replace with 
# MCMC but that'd take a little longer

resid_weighted <- m$weight[[1]] * residuals(m$model[[1]]) + 
  m$weight[[2]] * residuals(m$model[[2]]) + 
  m$weight[[3]] * residuals(m$model[[3]]) + 
  m$weight[[4]] * residuals(m$model[[4]])
dat <- dat %>% mutate(resid_weighted=c(resid_weighted))
m$dev_explained

# map?
dat %>% 
  filter(year%in% c(2003:2010)) %>% 
  ggplot(aes(longitude,latitude,col=resid_weighted))+
  geom_point()+
  scale_color_gradient2()
