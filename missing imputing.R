rm(list = ls())
library("plyr")

data <- read.csv("C:/Users/71908/Desktop/CS-AKI/data/finaldata-missing.csv")
colnames(data)


factorCols <- c("exposure","sex","ethnicity","admission_type","surgery","renal_disease",
                "myocardial_infarct","chronic_pulmonary_disease","congestive_heart_failure",
                "liver_disease","hypertension","diabetes")

for(i in factorCols) {data[,i] <- as.factor(data[,i])}

library(mice)
data <- subset(data,select=-c(subject_id))

# 定义求众数函数，直接复制粘贴即可
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# 定义补充缺失值函数，本样例采用pmm法，可根据情况修改，无需修改则直接复制即可
fix_na <- function(factorCols,data,n) {
  imp=mice(data,m=n,method="pmm")
  datas = list()
  for(i in c(1:n)) datas[[i]] <- complete(imp, action=i)
  colnames <- c()
  numeric_cols <- c()
  result <- 1 : length(datas[[1]][,1])
  allCols <- colnames(datas[[1]])
  for(colname in allCols) {
    temp <- 1 : length(datas[[1]][,1])
    for(data in datas) temp <- cbind(temp,data[[colname]])
    temp <- temp[,-1]
    if(colname %in% factorCols) {
      mode_value <- apply(temp,1,getmode)
      result <- cbind(result,mode_value)
    }else{
      mean_value <- apply(temp,1,mean)
      result <- cbind(result,mean_value)
      numeric_cols <- c(numeric_cols, colname)
    }
    colnames <- c(colnames,colname)
  }
  result <- as.data.frame(result[,-1])
  names(result) <- colnames
  for(i in factorCols) result[[i]] = as.factor(result[[i]])
  for(i in numeric_cols) result[[i]] = as.numeric(result[[i]])
  return (result)
}


data <- fix_na(factorCols,data,5)

View(data)

write.csv(data,'DATA.csv', row.names=F)
