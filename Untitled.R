data <- read.csv("hr_final.csv")
summary(data)
str(data)
table(data$JobSatisfaction)
table(data$JobLevel)
table(data$PerformanceRating)
data$Attrition <- as.factor(data$Attrition)
data$JobLevel <- as.factor(data$JobLevel)
data$JobSatisfaction <- as.factor(data$JobSatisfaction)
data$PerformanceRating <- as.factor(data$PerformanceRating)
#Finding Outliers - Monthly Income
quantile_monthlyincome = quantile(data$MonthlyIncome,0.99)
mean_monthlyincome = mean(data$MonthlyIncome)
std_monthlyincome = sd(data$monthlyincome)
is_outlier = mean_monthlyincome + 3*std_monthlyincome
print(is_outlier)

#Finding Outliers - TotalWorkingYears
quantile_totworkingyrs = quantile(data$TotalWorkingYears,0.99)
mean_totworkingyrs = mean(data$TotalWorkingYears)
std_totworkingyrs = sd(data$TotalWorkingYears)
is_outlier = mean_totworkingyrs + 3*std_totworkingyrs
print(is_outlier)
table(data$TotalWorkingYears>quantile_totworkingyrs)

#Finding Outliers - YearsAtCompany
quantile_yearsatcompany = quantile(data$YearsAtCompany,0.99)
mean_yearsatcompany = mean(data$YearsAtCompany)
std_yearsatcompany = sd(data$YearsAtCompany)
is_outlier = mean_yearsatcompany + 3*std_yearsatcompany
print(is_outlier)
table(data$YearsAtCompany>quantile_yearsatcompany)
data$YearsAtCompany[data$YearsAtCompany>is_outlier] <- quantile_yearsatcompany

#Finding Outliers - YearsWithCurrManager
quantile_yearswithcurrmanager = quantile(data$YearsWithCurrManager,0.99)
mean_yearswithcurrmanager = mean(data$YearsWithCurrManager)
std_yearswithcurrmanager = sd(data$YearsWithCurrManager)
is_outlier = mean_yearswithcurrmanager + 3*std_yearswithcurrmanager
print(is_outlier)
table(data$YearsWithCurrManager>quantile_yearswithcurrmanager)
##data$YearsAtCompany[data$YearsAtCompany>is_outlier] <- quantile_yearsatcompany

#Proportion of Attrition in data
prop.table(table(data$Attrition))
#As there is a 78:22 ratio lets split the dataset into attrtn =0  and attrtn = 1
data_0 = data[(data$Attrition == "0"),]
data_1 = data[(data$Attrition == "1"),]

dt_0 = sort(sample(nrow(data_0), nrow(data_0)*.7))
train_0<-data_0[dt_0,]
test_0<-data_0[-dt_0,]

dt_1 = sort(sample(nrow(data_1), nrow(data_1)*.7))
train_1<-data_1[dt_1,]
test_1<-data_1[-dt_1,]

train <- rbind(train_0,train_1)
test <- rbind(test_0,test_1)

model <- glm(data = train,
             family = binomial(link = "logit"),
             Attrition~.)
summary(model)

model_final <- glm(data = train,
                   family = binomial(link="logit"),
                   Attrition~train$OverTime+train$Gender+train$JobSatisfaction+train$MaritalStatus+train$NumCompaniesWorked+train$TotalWorkingYears+train$YearsWithCurrManager)
summary(model_final)

fitted.results <- predict(model_final,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != train$Attrition)
print(paste('Accuracy',1-misClasificError))

library("caret")
confusionMatrix(train$Attrition,fitted.results,cutoff = 0.5)
