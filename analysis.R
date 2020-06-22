#####################################################################
###------------------------------------------------------------------
### Topic : NTU Research Report
### Date  : 2020-06-17
### Author: Chao Wang
###------------------------------------------------------------------
#####################################################################

library(tidyverse)
library(broom)
library(dslabs)
library(caret)
library(nnet)
library(matrixStats)
library(e1071)
library(wvtool)
library(matrixStats)
library(e1071)
library(ggrepel)
library(stargazer)
ds_theme_set()
options(digit = 4)

#--------------------------------------------------------------------
### load data
load('rdas/hmnist28.rda')
load('rdas/hmnist64.rda')
load('rdas/lower-order.rda')
load('rdas/higher-order.rda')

#--------------------------------------------------------------------
### analysis

## display and save sample images
si <- hmnist64 %>% 
  mutate(index = 1:5000) %>% 
  group_by(label) %>% 
  summarise(sample = first(index)) %>% 
  .$sample

tumour <- array(NA, 5000)
tumour[si] <- c('simple-stroma', 'tumour-epithelium', 'complex-stroma',
                'immune-cell', 'debris', 'mucosal-gland', 'adipose-tissue',
                'background')

sapply(si, function(i) {
  png(paste0('figs/',tumour[i],'.png'))
  image(matrix(as.matrix(hmnist64[i, -4097]), 64, 64)[,64:1],
        main = tumour[i],
        xaxt = 'n', yaxt = 'n')
  dev.off()
})

png('figs/sample-image-64.png')
image(matrix(as.matrix(hmnist64[630, -4097]), 64, 64)[, 64:1],
      main = '64x64 pixels',
      xaxt = 'n', yaxt = 'n')
dev.off()


## Preprocessing
y <- factor(hmnist64$label)
x <- hmnist64[,-4097] %>% as.matrix()
x.scale <- apply(x, 2, scale)

set.seed(924)
test.index <- createDataPartition(y, times = 1, p = .1, list = F)
test.x <- x.scale[test.index,]
test.y <- y[test.index]

train.x <- x.scale[-test.index,]
train.y <- y[-test.index]
train.data <- cbind(train.x, train.y)

  
# column variance
sds <- colSds(x)

qplot(sds, bins = 256, fill = I('red')) + 
  ggtitle('Column SD of each pixel')
ggsave('figs/column-variance.png')

# all features seem to provide some info, can't remove any
nzv <- nearZeroVar(x.scale)


## Gray-scale machine learning
# PCA
load('rdas/pca.rda')

# memory.limit(size = 20000)
# pca <- prcomp(train.x)
# save(pca, file = 'rdas/pca.rda')

# plot variance
ten <- first(which(summary(pca)$importance[3,] >= 0.9))
one <- first(which(summary(pca)$importance[3,] >= 0.99))

qplot(1:ncol(pixel.scale), summary(pca)$importance[3,], 
      col = I('blue'), size = I(.7)) +
  xlab('number of columns') +
  ylab('variance retained') +
  ggtitle('variance proportion')
ggsave('figs/variance-proportion.png')

# plot PCs
qplot(PC1, PC2, data = as.data.frame(pca$x), 
      col = train.y) +
  theme(legend.title = element_blank()) +
  ggtitle('PC2 vs. PC1 for eight categories')
ggsave('figs/plot-pcs.png')

# PCs boxplot
data.frame(type = train.y, pca$x[, 1:5]) %>% 
  gather(PC, value, -type) %>% 
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot() +
  theme(legend.title = element_blank()) +
  ggtitle('boxplot of 5 PCs')
ggsave('figs/boxplot-5pcs.png')
# only PC1 makes a big difference, which makes sense

# then only plot PC1
data.frame(type = train.y, pca$x[, 1]) %>% 
  gather(PC, value, -type) %>% 
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot() +
  theme(legend.position = 'none') +
  ggtitle('boxplot of PC1')
ggsave('figs/boxplot-1pc.png')
# looks feasible, can be separated

## machine learning
low.scale <- lower.order %>% 
  mutate_all(scale)

high.scale <- higher.order %>% 
  mutate_all(scale)

x <- cbind(low.scale, high.scale)
y <- hmnist64$label %>% as.character()

set.seed(924)
test.index <- createDataPartition(y, times = 1, p = .1, list = F)
test.x <- x[test.index,]
test.y <- y[test.index]

train.x <- x[-test.index,]
train.y <- y[-test.index]
train.data <- cbind(train.x, train.y)

# logistic
logist.fit <- multinom(train.y ~ ., data = train.data)
pred <- predict(logist.fit, test.x)
mean(pred == test.y)
load('rdas/hmnist64.rda')