##############################################
#  From bagging to Random forest
##############################################



## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------


library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
mtpl <- read.table('../data/PC_data.txt',
                   header = TRUE, stringsAsFactors = TRUE) %>% as_tibble() %>% rename_all(tolower) %>% rename(expo = exp)

set.seed(486291) # reproducibility

bsample_1 <- mtpl %>% nrow %>% sample(replace = TRUE)
bsample_2 <- mtpl %>% nrow %>% sample(replace = TRUE)

mtpl_b1 <- mtpl %>% dplyr::slice(bsample_1)
mtpl_b2 <- mtpl %>% dplyr::slice(bsample_2)

library(rpart)
fit_b1 <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
                data = mtpl_b1,
                method = 'poisson',
                control = rpart.control(
                  maxdepth = 3,
                  minsplit = 2000,
                  minbucket = 1000,
                  cp = 0))
fit_b2 <- rpart(formula = cbind(expo,nclaims) ~ ageph + agec + bm + power + coverage + fuel + sex + fleet + use,
                data = mtpl_b2,
                method = 'poisson',
                control = rpart.control(
                  maxdepth = 3,
                  minsplit = 2000,
                  minbucket = 1000,
                  cp = 0))

library(rpart.plot)
rpart.plot(fit_b1, cex = 1.4, extra = 0)
rpart.plot(fit_b2, cex = 1.4, extra = 0)
