rm(list = ls())#清理环境

#载入包
library(tidyverse)

#数据导入
data <- read.csv("C:/Users/71908/Desktop/CS-AKI/finaldata.csv")
colnames(data)
view(data)


# 分类变量
factorCols <- c("exposure","sex","ethnicity","admission_type","surgery",
                "renal_disease","myocardial_infarct","chronic_pulmonary_disease",
                "congestive_heart_failure","liver_disease","hypertension","diabetes",
                "magnesium_level","severe_aki","aki","severe_aki_creat",
                "aki_creat","icu_mort","hos_mort")

# 分类批量设置为因子变量
for(i in factorCols) {data[,i] <- as.factor(data[,i])}


#单因素logi分析
fml <- 'severe_aki ~ exposure'

Unadjusted <- glm(as.formula(fml), data = data, family = binomial, na.action = na.exclude)
summary(Unadjusted)
exp(cbind(OR = coef(Unadjusted), confint(Unadjusted)))

#Model I
fml1 <- 'severe_aki ~ exposure + age + sex + ethnicity + bmi + admission_type'

Model1 <- glm(as.formula(fml1), data = data, family = binomial, na.action = na.exclude)
summary(Model1)
exp(cbind(OR = coef(Model1), confint(Model1)))

#Model II
fml2 <- 'severe_aki ~ exposure + age + sex + ethnicity + bmi + admission_type + bicarbonate + bun + hemoglobin + sodium + potassium'

Model2 <- glm(as.formula(fml2), data = data, family = binomial, na.action = na.exclude)
summary(Model2)
exp(cbind(OR = coef(Model2), confint(Model2)))

#Model III
fml3 <- 'severe_aki ~ exposure + age + sex + ethnicity + bmi + admission_type + Fluid_balance + MAP + bicarbonate + bun + hemoglobin + sodium + potassium'

Model3 <- glm(as.formula(fml3), data = data, family = binomial, na.action = na.exclude)
summary(Model3)
exp(cbind(OR = coef(Model3), confint(Model3)))


#Model IV
fml4 <- 'severe_aki ~ exposure + age + sex + ethnicity + bmi + admission_type + Fluid_balance + MAP + sofa_max + renal_disease + myocardial_infarct + chronic_pulmonary_disease + congestive_heart_failure + liver_disease + hypertension + diabetes + bicarbonate + bun + hemoglobin + sodium + potassium'

Model4 <- glm(as.formula(fml4), data = data, family = binomial, na.action = na.exclude)
summary(Model4)
exp(cbind(OR = coef(Model4), confint(Model4)))







