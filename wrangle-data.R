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

x <- hmnist64[,-4097]
## construct different sets of features

# 1. Lower-order histogram features
mu <- rowMeans(x)
variance <- rowSds(as.matrix(x))^2
skew <- apply(x, 1, skewness)
kurt <- apply(x, 1, kurtosis)
m5 <- rowSums((x - mu)^5) / ncol(x)

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


# 2. Higher-order histogram features
higher.order <- sapply(2:11, function(i) {
  rowSums((x - mu)^i) / ncol(x)
})

higher.order <- higher.order %>% 
  as.data.frame() %>% 
  setNames(paste0('central moment ', 2:11))

# 3. Local binary patterns histogram fourier features (LBP-HF)
# LBP
memory.limit(size = 20000) # file too large, needs more RAM
LBP <- lbp(as.matrix(x), r = 1)

# output will be the size of (m-2) * (n-2) because pixels on the 
# edge cannot serve as centers of the neighbourhood
lbp.uniform <- LBP$lbp.u2 # after considering uniform pattern
lbp.origin <- LBP$lbp.ori # before considering uniform pattern

m <- matrix(as.matrix(x[1,]), 64, 64)
r <- lbp(m, 1)
image(m)
image(r$lbp.u2)
hist(r$lbp.u2)
hist(r$lbp.ori, breaks = 256)
image(rot90c(r$lbp.u2),col = gray((0:58)/58), main="lbp.u2 (r=1, 8 points)", useRaster=TRUE,
      asp=1, axes=FALSE)
# Local binary patterns (LBP)


#--------------------------------------------------------------------
### save data
save(hmnist28, file = 'rdas/hmnist28.rda')
save(hmnist64, file = 'rdas/hmnist64.rda')
save(lower.order, file = 'rdas/lower-order.rda')
save(higher.order, file = 'rdas/higher-order.rda')
save(lbp.origin, file = 'rdas/lbp-origin.rda')
save(lbp.uniform, file = 'rdas/lbp-uniform.rda')
