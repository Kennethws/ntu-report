#####################################################################
###------------------------------------------------------------------
### Topic : NTU Research Report
### Date  : 2020-06-17
### Author: Chao Wang
###------------------------------------------------------------------
#####################################################################

library(tidyverse)

#--------------------------------------------------------------------
### read data
hmnist28 <- read.csv('data/hmnist_28_28_L.csv', header = T)
hmnist64 <- read.csv('data/hmnist_64_64_L.csv', header = T)

#--------------------------------------------------------------------
### wrangle data

## construct different sets of features

# Lower-order histogram features
mu <- rowMeans(hmnist64)
variance <- rowSds(as.matrix(hmnist64))^2
skew <- apply(hmnist64, 1, skewness)
kurt <- apply(hmnist64, 1, kurtosis)
m5 <- rowSums((hmnist64 - mu)^5) / ncol(hmnist64)

lower.order <- cbind(mu, variance, skew, kurt, m5) %>% 
  as.data.frame() %>% 
  setNames(c('mean', 'variance', 'skewness', 'kurtosis', 
             'fifth central moment'))

# Higher-order histogram features
higher.order <- sapply(2:11, function(i) {
  rowSums((hmnist64 - mu)^i) / ncol(hmnist64)
})

higher.order <- higher.order %>% 
  as.data.frame() %>% 
  setNames(paste0('central moment ', 2:11))

# Local binary patterns (LBP)


#--------------------------------------------------------------------
### save data
save(hmnist28, file = 'rdas/hmnist28.rda')
save(hmnist64, file = 'rdas/hmnist64.rda')
save(lower.order, file = 'rdas/lower-order.rda')
save(higher.order, file = 'rdas/higher-order.rda')
