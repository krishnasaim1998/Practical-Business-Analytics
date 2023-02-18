data <- read.csv('Telecom_customer_churn.csv')

library(ggplot2)

   

print(ggplot(data, aes(x = churn)) +
    geom_bar(aes(fill = as.factor(churn))) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.2, colour = "black") +
  scale_fill_manual(values = c("red","black")))

print(ggplot(data, aes(income, fill=as.factor(churn))) + geom_density() + scale_x_log10() + 
  scale_fill_manual(values = c("red","black")))

print(ggplot(data, aes(totrev, fill=as.factor(churn))) + geom_density() + scale_x_log10() +
  scale_fill_manual(values = c("red","black")))

print(ggplot(data, aes(eqpdays, fill=as.factor(churn))) + geom_density() + scale_x_log10() + 
  scale_fill_manual(values = c("red","black")))

print(ggplot(data, aes(months, fill=as.factor(churn))) + geom_density() + scale_x_log10() + 
  scale_fill_manual(values = c("red","black")))

print(ggplot(data, aes(mou_Mean, fill=as.factor(churn))) + geom_density() +
    scale_fill_manual(values = c("red","black")))

print(ggplot(data, aes(change_mou, fill=as.factor(churn))) + geom_density()+
  scale_fill_manual(values = c("red","black")))

print(ggplot(data, aes(x = area, fill = as.factor(churn))) +
  geom_bar(aes(fill = as.factor(churn)), position = 'dodge') +
  scale_fill_manual(values = c("red","black")) + coord_flip())

ggplot(data, aes(x = refurb_new, fill = as.factor(churn))) +
  geom_bar(aes(fill = as.factor(churn)), position = 'dodge') +
  scale_fill_manual(values = c("red","black")) 

  

