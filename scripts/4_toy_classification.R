##############################################
#  Toy example for classification
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(54321) # reproducibility
dfc <- tibble::tibble(
  x1 = rep(seq(0.1,10,by = 0.1), times = 100),
  x2 = rep(seq(0.1,10,by = 0.1), each = 100),
  y = as.factor(
    pmin(1,
         pmax(0,
              round(
                1*(x1+2*x2<8) + 1*(3*x1+x2>30) + 
                  rnorm(10000,sd = 0.5))
         )
    )
  )
)


## --------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(dfc, aes(x = x1, y = x2)) + geom_point(aes(color = y))


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart)
fit <- rpart(formula = y ~ x1 + x2,
             data = dfc,
             method = 'class',
             control = rpart.control(maxdepth = 2))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart.plot)
rpart.plot(fit, digits = 4, cex = 1.5)


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x1 + x2,
             data = dfc,
             method = 'class',
             control = rpart.control(maxdepth = 20,
                                     minsplit = 10,
                                     minbucket = 5,
                                     cp = 0))


## --------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(fit, dfc, type = 'class')


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_class <- function(dt, preds){
  dt %>% mutate(pred = preds) %>% ggplot(aes(x = x1, y = x2)) +
    geom_point(aes(color = pred))
}

plot_pred_class(dt = dfc, preds = predict(fit, dfc, type = 'class'))





## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------
# Q1
set.seed(87654) # reproducibility
fit <- rpart(formula = y ~ x1 + x2,
             data = dfc,
             method = 'class',
             control = rpart.control(maxdepth = 20,
                                     minsplit = 10,
                                     minbucket = 5,
                                     cp = 0,
                                     xval = 5))
# Q2 
plotcp(fit)
# Q3
cpt <- fit$cptable
# Q4
min_xerr <- which.min(cpt[,'xerror'])
se_rule <- min(which(cpt[, 'xerror'] < (cpt[min_xerr, 'xerror'] + cpt[min_xerr, 'xstd'])))

fit_1 <- prune(fit, cp = cpt[min_xerr, 'CP'])
plot_pred_class(dt = dfc, preds = predict(fit_1, dfc, type = 'class'))

fit_2 <- prune(fit, cp = cpt[se_rule, 'CP'])
plot_pred_class(dt = dfc, preds = predict(fit_2, dfc, type = 'class'))
