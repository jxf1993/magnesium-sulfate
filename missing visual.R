rm(list = ls())#清理环境

#载入包
library(tidyverse)
library(mice)
library(VIM)

full_data <- read.csv("C:/Users/71908/Desktop/CS-AKI/finaldata-missing.csv")
colnames(full_data)

summary(full_data)

md.pattern(full_data)


aggr_plot <- aggr(full_data, col=c('navyblue','red'),numbers=TRUE,labels=names(full_data),cex.axis=0.7,gap=2,ylab=c("Histogram of missing data","Pattern"))




