webdata = read.csv("Web_Analytics_Data.csv")
summary(webdata)
str(webdata)
unique(webdata$Pages_Per_Session)
# Detecting Outlier
quantile_avgsession = quantile(webdata$Avg_Session_Duration,0.99)
mean_avgsession = mean(webdata$Avg_Session_Duration)
std_avgsession = sd(webdata$Avg_Session_Duration)
is_outlier = mean_avgsession + 3*std_avgsession
print(is_outlier)
table(webdata$Avg_Session_Duration>quantile_avgsession)
webdata$Avg_Session_Duration[webdata$Avg_Session_Duration>quantile_avgsession] = quantile_avgsession
prop.table(table(webdata$Order))
## cosine similarity ==== multi collinearity ==== gini impurity ==== entropy  === test on retail