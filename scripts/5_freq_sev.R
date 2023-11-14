##############################################
#  Claim frequency and severity modeling
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mtpl <- read.table('../data/PC_data.txt',
                   header = TRUE, stringsAsFactors = TRUE) %>% as_tibble() %>% rename_all(tolower) %>% rename(expo = exp)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart)
fit <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
             data = mtpl,
             method = 'poisson',
             control = rpart.control(maxdepth = 3,
                                     cp = 0))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart.plot)
rpart.plot(fit, cex = 1.5)


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------
# sprintf(mtpl %>% 
#           dplyr::filter(bm< 2 & ageph>=56) %>% 
#           dplyr::summarise(claim_freq = sum(nclaims) / sum(expo)), fmt = '%#.8f') 


mtpl %>% 
  dplyr::filter(bm< 2 & ageph>=56) %>% 
  dplyr::summarise(claim_freq = sum(nclaims) / sum(expo)) %>% as.data.frame()


## --------------------------------------------------------------------------------------------------------------------------------------------------
k <- 1

alpha <- 1/k^2

mu <- mtpl %>% with(sum(nclaims)/sum(expo))

beta <- alpha/mu

mtpl %>% 
  dplyr::filter(bm < 2, ageph >= 56) %>% 
  dplyr::summarise(prediction = (alpha + sum(nclaims))/(beta + sum(expo))) %>% as.data.frame()


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
             data = mtpl,
             method = 'poisson',
             control = rpart.control(maxdepth = 3,
                                     cp = 0),
             parms = list(shrink = 10^-5))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
             data = mtpl,
             method = 'poisson',
             control = rpart.control(maxdepth = 3,
                                     cp = 0),
             parms = list(shrink = 10^5))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
mtpl %>% 
  dplyr::filter(bm < 2, ageph >= 56) %>% 
  dplyr::summarise(claim_freq = sum(nclaims)/sum(expo)) %>% as.data.frame()


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------
# Q1
set.seed(9753) # reproducibilty
fit <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
             data = mtpl,
             method = 'poisson',
             control = rpart.control(maxdepth = 20,
                                     minsplit = 2000,
                                     minbucket = 1000,
                                     cp = 0,
                                     xval = 5))

# Q2
plotcp(fit)
# Q3
cpt <- fit$cptable
min_xerr <- which.min(cpt[,"xerror"])
cpt[min_xerr,]
fit_srt <- prune(fit, cp = cpt[min_xerr, 'CP'])
# Q4
rpart.plot(fit_srt, cex = 0.6)
rpart.plot(fit_srt, cex = 0.6, type = 0)
rpart.plot(fit_srt, cex = 0.6, type = 0, extra = 0)
