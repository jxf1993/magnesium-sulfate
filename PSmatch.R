rm(list = ls())#清理环境

#载入包
library(twang)
library(Matching)
library(tidyverse)


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
fml1<-'exposure ~  age + sex + ethnicity + bmi + admission_type + 
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

#匹配子集
set.seed(4958)
ps_matches <- Match(Y = full_data$severe_aki, Tr = full_data$exposure,
                    X = full_data$ps, M = 1, estimand = "ATT", caliper = 0.01,
                    exact = FALSE, replace = FALSE, ties = FALSE)

datamatched <- full_data[unlist(ps_matches[c("index.treated","index.control")]),]

write.csv(datamatched, file="datamatched.csv")



#ps match
rm(list = ls())#清理环境
library(tidyverse)

data <- read.csv("C:/Users/71908/Desktop/CS-AKI/datamatched.csv")

# 分类变量
factorCols <- c("exposure","sex","ethnicity","admission_type","surgery",
                "renal_disease","myocardial_infarct","chronic_pulmonary_disease",
                "congestive_heart_failure","liver_disease","hypertension","diabetes",
                "magnesium_level","severe_aki","aki","severe_aki_creat",
                "aki_creat","icu_mort","hos_mort")

# 分类批量设置为因子变量
for(i in factorCols) {data[,i] <- as.factor(data[,i])}

fml <- 'severe_aki ~ exposure'

psmodel <- glm(as.formula(fml), data = data, family = binomial, na.action = na.exclude)
summary(psmodel)
exp(cbind(OR = coef(psmodel), confint(psmodel)))


