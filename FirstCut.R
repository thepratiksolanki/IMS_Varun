library("arules")
library("arulesViz")
data = read.csv("Retail_Data.csv")
data = data[,-1]
data <- as(data,"transactions")
temp <- as(data,"data.frame")
itemFrequencyPlot(data,topN=10)
rules <- apriori(data,parameter = list(supp=0.1,conf = 0.3,minlen=2,maxlen=3))
inspect(rules[1:5])
rules1 <- sort(rules,by="supp",descending = T)
inspect(rules1[1:5])
write.csv(data.frame(inspect(rules1)),file="rules.csv")
# Driver -> fix rhs / Driven -> fix lhs
new_rules <- apriori(data, parameter = list(supp=0.08,conf=0.34,minlen=2),
                     appearance = list(lhs="Prod1=A",default="rhs"))
inspect(new_rules)
new_rules1 <- apriori(data, parameter = list(supp=0.08,conf=0.32,minlen=2),
                     appearance = list(rhs="Prod2=E",default="lhs"))
new_rules1 <- sort(new_rules1,by="supp",descending=T)
inspect(new_rules1)
new_rules2 <- apriori(data, parameter = list(supp=0.1,conf=0.3,minlen=2),
                      appearance = list(rhs=c("Prod3=H","Prod2=E","Prod2=G"),default="lhs"))
new_rules2 <- sort(new_rules2,by="supp",descending=T)
write.csv(data.frame(inspect(new_rules2)),file="new_rules2.csv")
