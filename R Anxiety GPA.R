setwd("E:\\Stats\\Homework Due 922")
x<-read.csv(file="anxietyGPA.csv", header=TRUE)
#problem S13
summary(x)
plot(x)
cor(x$Anxiety,x$GPA)
cor(x$Anxiety,x$GPA)^2
hist(x$Anxiety)

#problem S16 attempt at regression model
model1<-lm(x$Anxiety~x$GPA)
summary(model1)
plot(model1)

abline(model1)

pnorm(1)
(100-31)/15
pnorm(-4.6)
