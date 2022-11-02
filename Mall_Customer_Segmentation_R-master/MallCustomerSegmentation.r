#Mall Customer Segmentation

#packages
library(dplyr)  #Purpose --> Count
library(plotrix) # Purpose --> Pie chart graph
library(readxl) #Purpose --> Read Xlxs
library(qcc)  
library(qicharts)

mall.data <- read.csv("H:/Mall_Customers.csv")
print(mall.data)

#Load the data:
str(mall.data)

#visualize data
data.table::data.table(mall.data) #head()

#quick summary of the data
summary(mall.data)

#Some Statistical value of features
#1.sd
sd(mall.data$Age)
sd(mall.data$Annual.Income..k..)
sd(mall.data$Spending.Score..1.100.)

#2.IQR
IQR(mall.data$Age)
IQR(mall.data$Annual.Income..k..)
IQR(mall.data$Spending.Score..1.100.)

#Plot the graph
gender = table(mall.data$Gender)
barplot(gender, main="Using Barplot to display Gender Comparision", ylab="Number", xlab ="Gender", col = rainbow(2),legend=rownames(gender))

#Female is in higher trends than the male

#Pie Chart
pct = round((gender/sum(gender))*100)
lbs = paste(c("Female","Male")," ",pct,"%",sep=" ")
pie(gender, labels = lbs, main="Pie Chart Depicting Ratio of Female and Male")

#Age - Histogram
hist(mall.data$Age,
     col = "red",
     xlab = "Age Class",
     ylab = "Frequency",
     labels = TRUE)

#Boxplot - Age
boxplot(mall.data$Age,
        col=rainbow(1),
        main="Boxplot for Descriptive Analysis of Age",
        labels= TRUE)

#Annual Income
hist(mall.data$Annual.Income..k..,
     col= "red",
     xlab = "income Class",
     ylab = "Frequency",
     labels = TRUE)

plot(density(mall.data$Annual.Income..k..),
     main="Density plot for Annual Income",
     xlab = "Income",
     ylab = "Density")
polygon(density(mall.data$Annual.Income..k..),col = "Black")

#oneway
one.way.mall = aov(mall.data$Annual.Income..k.. ~ mall.data$Group , data = mall.data )
summary(one.way.mall)

#twoway
two.way.mal = aov(mall.data$Group ~ mall.data$Annual.Income..k..+mall.data$Spending.Score..1.100. , data = mall.data)
summary(two.way.mal)

#boxplot
boxplot(mall.data$Annual.Income..k.. ~ mall.data$Age,data = mall.data, main="AGE VS ANNUAL INCOME", xlab="AGE",ylab = "ANNUAL INCOME" , col = rainbow(7))

#p charts
qcc(mall.data$Spending.Score..1.100., type = 'p', sizes = 1000 )
qcc(mall.data$Annual.Income..k.., type = 'p', sizes = 1000)
#qcc(mall.data, type = c("p"),sizes = 1000)
#qcc(mall.data, type = c("p"),sizes = 1000)

#c charts
qcc(mall.data$Annual.Income..k.., type = 'c', sizes = 1000)
qcc(mall.data$Spending.Score..1.100., type = 'c', sizes = 1000)

#Linear Regression
x <- c(mall.data$Gender)
y <- c(mall.data$Spending.Score..1.100.)
relation <- lm(y~x)
print(relation)
print(summary(relation))

z <- c(mall.data$Age)
y <- c(mall.data$Spending.Score..1.100.)
relation1 <- lm(z~y)
print(relation1)

print(summary(relation1))

#cor()
cor(mall.data$Annual.Income..k..,mall.data$Spending.Score..1.100.)
print("Cor - value is +ve, Hence Annual income and spending income are dependent.")

#Non-Parametric  Kruskal
kruskal <- kruskal.test(mall.data$Annual.Income..k.. ~ mall.data$Spending.Score..1.100.)
print(kruskal)

#Time Series Analysis - Annual Income
mall.data$Annual.Income..k..
annualincome <- ts(mall.data$Annual.Income..k..)
annualincome
plot.ts(annualincome)

#Time Series Analysis - Spending Score
mall.data$Spending.Score..1.100.
spendingscore <- ts(mall.data$Spending.Score..1.100.)
spendingscore
plot.ts(spendingscore)

#cumsum - Annual Income 
frame=data.frame(mall.data$Annual.Income..k..)
frame$cum =cumsum(mall.data$Annual.Income..k..)
plot(frame$cum, type='l', xlab='Days', ylab='Annual Income',col="blue")

#cumsum - Spending Score
frame=data.frame(mall.data$Spending.Score..1.100.)
frame$cum =cumsum(mall.data$Spending.Score..1.100.)
plot(frame$cum, type='l', xlab='Days', ylab='Spending Score',col="blue")