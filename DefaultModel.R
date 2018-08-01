library("dplyr")
default_data = read.csv("dcc.csv")
summary(default_data)
default_data$SEX = as.factor(default_data$SEX)
default_data$EDUCATION = as.factor(default_data$EDUCATION)
default_data$MARRIAGE = as.factor(default_data$MARRIAGE)
default_data$default_payment_next_month = as.factor(default_data$default_payment_next_month)

is_outlier <- function(x) {
  return(x < quantile(x, 0.01) - 1.5 * IQR(x) | x > quantile(x, 0.99) + 1.5 * IQR(x))
}

table(is_outlier(default_data$BILL_AMT1))
replace_negative = quantile(default_data$BILL_AMT1, 0.01) - 1.5 * IQR(default_data$BILL_AMT1)
replace_positive = quantile(default_data$BILL_AMT1, 0.99) + 1.5 * IQR(default_data$BILL_AMT1)
default_data$BILL_AMT1[default_data$BILL_AMT1 <= replace_negative] = replace_negative
default_data$BILL_AMT1[default_data$BILL_AMT1 >= replace_positive] = replace_positive

table(is_outlier(default_data$BILL_AMT2))
replace_negative = quantile(default_data$BILL_AMT2, 0.01) - 1.5 * IQR(default_data$BILL_AMT2)
replace_positive = quantile(default_data$BILL_AMT2, 0.99) + 1.5 * IQR(default_data$BILL_AMT2)
default_data$BILL_AMT2[default_data$BILL_AMT2 <= replace_negative] = replace_negative
default_data$BILL_AMT2[default_data$BILL_AMT2 >= replace_positive] = replace_positive

table(is_outlier(default_data$BILL_AMT3))
replace_negative = quantile(default_data$BILL_AMT3, 0.01) - 1.5 * IQR(default_data$BILL_AMT3)
replace_positive = quantile(default_data$BILL_AMT3, 0.99) + 1.5 * IQR(default_data$BILL_AMT3)
default_data$BILL_AMT3[default_data$BILL_AMT3 <= replace_negative] = replace_negative
default_data$BILL_AMT3[default_data$BILL_AMT3 >= replace_positive] = replace_positive

table(is_outlier(default_data$BILL_AMT4))
replace_negative = quantile(default_data$BILL_AMT4, 0.01) - 1.5 * IQR(default_data$BILL_AMT4)
replace_positive = quantile(default_data$BILL_AMT4, 0.99) + 1.5 * IQR(default_data$BILL_AMT4)
default_data$BILL_AMT4[default_data$BILL_AMT4 <= replace_negative] = replace_negative
default_data$BILL_AMT4[default_data$BILL_AMT4 >= replace_positive] = replace_positive

table(is_outlier(default_data$BILL_AMT5))
replace_negative = quantile(default_data$BILL_AMT5, 0.01) - 1.5 * IQR(default_data$BILL_AMT5)
replace_positive = quantile(default_data$BILL_AMT5, 0.99) + 1.5 * IQR(default_data$BILL_AMT5)
default_data$BILL_AMT5[default_data$BILL_AMT5 <= replace_negative] = replace_negative
default_data$BILL_AMT5[default_data$BILL_AMT5 >= replace_positive] = replace_positive

table(is_outlier(default_data$BILL_AMT6))
replace_negative = quantile(default_data$BILL_AMT6, 0.01) - 1.5 * IQR(default_data$BILL_AMT6)
replace_positive = quantile(default_data$BILL_AMT6, 0.99) + 1.5 * IQR(default_data$BILL_AMT6)
default_data$BILL_AMT6[default_data$BILL_AMT6 <= replace_negative] = replace_negative
default_data$BILL_AMT6[default_data$BILL_AMT6 >= replace_positive] = replace_positive

table(is_outlier(default_data$PAY_AMT1))
replace_negative = quantile(default_data$PAY_AMT1, 0.01) - 1.5 * IQR(default_data$PAY_AMT1)
replace_positive = quantile(default_data$PAY_AMT1, 0.99) + 1.5 * IQR(default_data$PAY_AMT1)
default_data$PAY_AMT1[default_data$PAY_AMT1 <= replace_negative] = replace_negative
default_data$PAY_AMT1[default_data$PAY_AMT1 >= replace_positive] = replace_positive

table(is_outlier(default_data$PAY_AMT2))
replace_negative = quantile(default_data$PAY_AMT2, 0.01) - 1.5 * IQR(default_data$PAY_AMT2)
replace_positive = quantile(default_data$PAY_AMT2, 0.99) + 1.5 * IQR(default_data$PAY_AMT2)
default_data$PAY_AMT2[default_data$PAY_AMT2 <= replace_negative] = replace_negative
default_data$PAY_AMT2[default_data$PAY_AMT2 >= replace_positive] = replace_positive

table(is_outlier(default_data$PAY_AMT3))
replace_negative = quantile(default_data$PAY_AMT3, 0.01) - 1.5 * IQR(default_data$PAY_AMT3)
replace_positive = quantile(default_data$PAY_AMT3, 0.99) + 1.5 * IQR(default_data$PAY_AMT3)
default_data$PAY_AMT3[default_data$PAY_AMT3 <= replace_negative] = replace_negative
default_data$PAY_AMT3[default_data$PAY_AMT3 >= replace_positive] = replace_positive

table(is_outlier(default_data$PAY_AMT4))
replace_negative = quantile(default_data$PAY_AMT4, 0.01) - 1.5 * IQR(default_data$PAY_AMT4)
replace_positive = quantile(default_data$PAY_AMT4, 0.99) + 1.5 * IQR(default_data$PAY_AMT4)
default_data$PAY_AMT4[default_data$PAY_AMT4 <= replace_negative] = replace_negative
default_data$PAY_AMT4[default_data$PAY_AMT4 >= replace_positive] = replace_positive

table(is_outlier(default_data$PAY_AMT5))
replace_negative = quantile(default_data$PAY_AMT5, 0.01) - 1.5 * IQR(default_data$PAY_AMT5)
replace_positive = quantile(default_data$PAY_AMT5, 0.99) + 1.5 * IQR(default_data$PAY_AMT5)
default_data$PAY_AMT5[default_data$PAY_AMT5 <= replace_negative] = replace_negative
default_data$PAY_AMT5[default_data$PAY_AMT5 >= replace_positive] = replace_positive

table(is_outlier(default_data$PAY_AMT6))
replace_negative = quantile(default_data$PAY_AMT6, 0.01) - 1.5 * IQR(default_data$PAY_AMT6)
replace_positive = quantile(default_data$PAY_AMT6, 0.99) + 1.5 * IQR(default_data$PAY_AMT6)
default_data$PAY_AMT6[default_data$PAY_AMT6 <= replace_negative] = replace_negative
default_data$PAY_AMT6[default_data$PAY_AMT6 >= replace_positive] = replace_positive

prop.table(table(default_data$default_payment_next_month))

data_0 = default_data[(default_data$default_payment_next_month == "0"),]
data_1 = default_data[(default_data$default_payment_next_month == "1"),]

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
             default_payment_next_month~.)
summary(model)

model <- glm(data = train,
             family = binomial(link = "logit"),
             default_payment_next_month~LIMIT_BAL + SEX + AGE + PAY_0 + PAY_2 + BILL_AMT2 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5)

fitted.results <- predict(model,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$default_payment_next_month)
print(paste('Accuracy',1-misClasificError))

library("caret")
confusionMatrix(factor(test$default_payment_next_month),factor(fitted.results))
library("InformationValue")
plotROC(actuals = test$default_payment_next_month,
        predictedScores = as.numeric(fitted(model)))
plotROC(test$default_payment_next_month,fitted.results)
