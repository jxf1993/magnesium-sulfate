rm(list = ls())#清理环境

#载入包
library(twang)
library(Matching)
library(tidyverse)
library(survey)

#PS
full_data <- read.csv("C:/Users/71908/Desktop/CS-AKI/finaldata.csv")

colnames(full_data)

# 分类变量
factor <- c("sex","ethnicity","admission_type","surgery",
                "renal_disease","myocardial_infarct","chronic_pulmonary_disease",
                "congestive_heart_failure","liver_disease","hypertension","diabetes",
                "magnesium_level","severe_aki","aki","severe_aki_creat",
                "aki_creat","icu_mort","hos_mort")

# 分类批量设置为因子变量
for(i in factor) {full_data[,i] <- as.factor(full_data[,i])}

#匹配变量选择
fml1<-'exposure ~  age + sex + ethnicity + bmi  + admission_type + 
Fluid_balance + MAP + sofa_max + renal_disease + myocardial_infarct + 
chronic_pulmonary_disease + congestive_heart_failure + liver_disease + 
hypertension + diabetes + bicarbonate + bun + hemoglobin + sodium + 
potassium'

#计算权重，采用gbm方法
ps_ate <- ps(as.formula(fml1),
                  data = full_data,
                  interaction.depth = 2,
                  shrinkage = 0.01,
                  perm.test.iters = 0,
                  estimand = "ATE",
                  verbose = FALSE,
                  stop.method = c("es.mean", "es.max", "ks.mean", "ks.max"),
                  n.trees = 10000,
                  train.fraction = 0.8,
                  cv.folds = 3,
                  n.cores = 8)

pred <- ps_ate$ps$es.mean.ATE
full_data <- full_data %>% mutate(ps = pred)
full_data <- full_data %>% mutate(ps_weight = get.weights(ps_ate, stop.method = "es.mean"))

#ipw
factor <- c("exposure")

# 分类批量设置为因子变量
for(i in factor) {full_data[,i] <- as.factor(full_data[,i])}

primary_ipw <- glm(severe_aki ~ exposure, data = full_data,
                   weights = full_data$ps_weight, family = binomial)
summary(primary_ipw)
exp(cbind(OR = coef(primary_ipw), confint(primary_ipw)))


#double robust
ipw_svydesign <- svydesign(ids = ~ subject_id, weights = ~ ps_weight, data = full_data)


fml <- 'severe_aki ~ exposure + age + sex + ethnicity + bmi  + admission_type + Fluid_balance + MAP + sofa_max + renal_disease + myocardial_infarct + chronic_pulmonary_disease + congestive_heart_failure + liver_disease + hypertension + diabetes + bicarbonate + bun + hemoglobin + sodium + potassium'

logi <- svyglm(as.formula(fml),
               family = quasibinomial,
               design = ipw_svydesign)
summary(logi)
exp(cbind(OR = coef(logi), confint(logi)))


#制作ipw表
library(tableone)

dt <- subset(full_data,select = -c(subject_id,exposure))
vars<-c(colnames(dt))

tabWeight<-svyCreateTableOne(vars=vars,strata = "exposure",data=ipw_svydesign,addOverall = TRUE)
tableone<-print(tabWeight,smd = TRUE,varLabels = TRUE,showAllLevels = TRUE,test = TRUE)
write.csv(tableone,file = "TableIPW.csv")


