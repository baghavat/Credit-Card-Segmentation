credit_data<-read.csv("credit-card-data.csv")

View(credit_data)
dim(credit_data)


##################################Exploratory Data Analysis############################################

#Col names
colnames(credit_data)

#Structure of the data
str(credit_data)
head(credit_data)

#Summary of the input data
summary(credit_data)

#Hist plot of BALANCE feature
hist(credit_data$BALANCE)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$BALANCE_FREQUENCY)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$PURCHASES)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$ONEOFF_PURCHASES)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$INSTALLMENTS_PURCHASES)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$CASH_ADVANCE)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$PURCHASES_FREQUENCY)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$ONEOFF_PURCHASES_FREQUENCY)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$PURCHASES_INSTALLMENTS_FREQUENCY)

#Hist plot of BALANCE_FREQUENCY feature
hist(credit_data$CASH_ADVANCE_FREQUENCY)


#Missing values
missing_value=data.frame(apply(credit_data,2,function(input){sum(is.na(input))}))
missing_value$features=row.names(credit_data)
names(missing_value)[1]= "Missing_Percentage"
missing_value$Missing_Percentage=(missing_value$Missing_Percentage/nrow(credit_data))
missing_value

# Features CREDIT_LIMIT and MINIMUM_PAYMENTS have Nul values. 
# Missing value imputation with median
credit_data$MINIMUM_PAYMENTS[is.na(credit_data$MINIMUM_PAYMENTS)] <- median(credit_data$MINIMUM_PAYMENTS)
credit_data$CREDIT_LIMIT[which(is.na(credit_data$CREDIT_LIMIT))] <- median(credit_data$CREDIT_LIMIT)

library(imputeMissings)
credit_data<- impute(credit_data,method = "median/mode")
sum(is.na(credit_data$CREDIT_LIMIT))
sum(is.na(credit_data$MINIMUM_PAYMENTS))

library(ggplot2)
# Outlier Analysis
ggplot(data = credit_data, aes(x = "", y = MINIMUM_PAYMENTS)) + 
  geom_boxplot() # No outliers

ggplot(data = credit_data, aes(x = "", y = CREDIT_LIMIT)) + 
  geom_boxplot() # No outliers

#As we can see that most of the features has outliers, if we exclude outlier, loss of data is huge. Instead, let's normalize the data and apply log function.

#credit_data=subset(credit_data,select = c("PURCHASES","PURCHASES_FREQUENCY","CASH_ADVANCE","CASH_ADVANCE_FREQUENCY","CREDIT_LIMIT","BALANCE","MINIMUM_PAYMENTS", 
#                                          "PAYMENTS"))


credit_data<-within(credit_data, rm(CUST_ID))


#KPI
#New Variables creation# 

credit_data$Monthly_Avg_PURCHASES <- credit_data$PURCHASES/credit_data$TENURE
credit_data$Monthly_CASH_ADVANCE <- credit_data$CASH_ADVANCE/credit_data$TENURE
credit_data$LIMIT_USAGE <- credit_data$BALANCE/credit_data$CREDIT_LIMIT
credit_data$MIN_PAYMENTS_RATIO <- credit_data$PAYMENTS/credit_data$MINIMUM_PAYMENTS

#########################################Feature Scaling and Feature Selection#####################################################################
Scaled_data = scale(credit_data)

View(Scaled_data)

Scaled_data<- impute(Scaled_data,method = "median/mode")

data_New<-subset(Scaled_data,select = c('BALANCE_FREQUENCY', 'ONEOFF_PURCHASES', 'ONEOFF_PURCHASES_FREQUENCY',
                                       'CREDIT_LIMIT', 'PAYMENTS', 'MINIMUM_PAYMENTS', 'Monthly_Avg_PURCHASES', 'Monthly_CASH_ADVANCE', 'LIMIT_USAGE',
                                       'MIN_PAYMENTS_RATIO'))

View(data_New)

################################################################## CLUSTERING ######################################################################

# Using KMeans algorithm. Going for 3,4,5 and 6 means ckustering
cluster_three <- kmeans(data_New,3)
cluster_four <- kmeans(data_New,4)
cluster_five <- kmeans(data_New,5)
cluster_six <- kmeans(data_New,6)

clust<-cluster_three$cluster

install.packages("animation")
library(animation)

View(clust)
plot(clust)
rm(c3)
d3<-data_New
c3<-cbind(d3,clust)
View(c3)
plot(c3)

clust<-cluster_four$cluster
d4<-data_New
c4<-cbind(d4,clust)
View(c3)
plot(c4)
