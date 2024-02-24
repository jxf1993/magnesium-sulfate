rm(list = ls())#清理环境

full_data <- read.csv("C:/Users/71908/Desktop/CS-AKI/finaldata.csv")

#载入包
library("plyr")
library(tableone)


# 分类变量
factorCols <- c("exposure","sex","ethnicity","admission_type","surgery",
                "renal_disease","myocardial_infarct","chronic_pulmonary_disease",
                "congestive_heart_failure","liver_disease","hypertension","diabetes",
                "magnesium_level","severe_aki","aki","severe_aki_creat",
                "aki_creat","icu_mort","hos_mort")

# 分类批量设置为因子变量
for(i in factorCols) {full_data[,i] <- as.factor(full_data[,i])}


dt <- subset(full_data,select = -c(subject_id,exposure))
vars<-c(colnames(dt))


tab1<-CreateTableOne(vars=vars,strata = "exposure",data=full_data,addOverall = TRUE)
tableone<-print(tab1,smd = TRUE,varLabels = TRUE,showAllLevels = TRUE,test = TRUE)
write.csv(tableone,file = "Table.csv")
