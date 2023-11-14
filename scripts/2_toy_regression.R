##############################################
#  Toy example for regression
##############################################



## --------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
set.seed(54321) # reproducibility
dfr <- tibble::tibble(
  x = seq(0, 2 * pi, length.out = 500),
  m = 2 * sin(x),
  y = m + rnorm(length(x), sd = 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------
dfr
ggplot(dfr, aes(x = x)) + geom_point(aes(y = y), alpha = 0.3) + geom_line(aes(y = m), colour = 'darkgreen', linewidth = 1.5)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart)
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova', # MSE  
             control = rpart.control(maxdepth = 1))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart.plot)
rpart.plot(fit, digits = 4, cex = 2)


## --------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(fit, dfr)


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg <- function(dt, preds){
  dt %>% mutate(pred = preds) %>% ggplot(aes(x = x)) +
    geom_point(aes(y = y), alpha = 0.3) +
    geom_line(aes(y = m), colour = 'darkgreen', linewidth = 1.5) +
    geom_line(aes(y = pred), colour = 'darkred', linewidth = 1.5) + theme_bw()
}

plot_pred_reg(dt = dfr, preds = predict(fit, dfr))


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova',
             control = rpart.control(maxdepth = 2))
print(fit)


## --------------------------------------------------------------------------------------------------------------------------------------------------
rpart.plot(fit, digits = 4, cex = 1.5)


## --------------------------------------------------------------------------------------------------------------------------------------------------
plot_pred_reg(dt = dfr, preds = predict(fit, dfr))


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------
# Q1
# Subset observations in node 6
obs <- dfr %>% dplyr::filter(x < 0.535141)
# Predict
pred <- mean(obs$y)
pred

# Q2
dev <- sum((obs$y - pred)^2)
dev


## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova',
             control = rpart.control(maxdepth = 20,
                                     minsplit = 10,
                                     minbucket = 5,
                                     cp = 0))

plot_pred_reg(dt = dfr, preds = predict(fit, dfr))
print(fit)

## --------------------------------------------------------------------------------------------------------------------------------------------------
fit <- rpart(formula = y ~ x,
             data = dfr,
             method = 'anova',
             control = rpart.control(maxdepth = 20,
                                     minsplit = 10,
                                     minbucket = 5,
                                     cp = 1))

plot_pred_reg(dt = dfr, preds = predict(fit, dfr))
print(fit)




