rm(list = ls())#��������

full_data <- read.csv("C:/Users/71908/Desktop/CS-AKI/finaldata.csv")

#�����
library("plyr")
library(tableone)


# �������
factorCols <- c("exposure","sex","ethnicity","admission_type","surgery",
                "renal_disease","myocardial_infarct","chronic_pulmonary_disease",
                "congestive_heart_failure","liver_disease","hypertension","diabetes",
                "magnesium_level","severe_aki","aki","severe_aki_creat",
                "aki_creat","icu_mort","hos_mort")

# ������������Ϊ���ӱ���
for(i in factorCols) {full_data[,i] <- as.factor(full_data[,i])}


dt <- subset(full_data,select = -c(subject_id,exposure))
vars<-c(colnames(dt))


tab1<-CreateTableOne(vars=vars,strata = "exposure",data=full_data,addOverall = TRUE)
tableone<-print(tab1,smd = TRUE,varLabels = TRUE,showAllLevels = TRUE,test = TRUE)
write.csv(tableone,file = "Table.csv")