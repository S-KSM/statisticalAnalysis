
library(xlsx)
library(dplyr)
library(ggplot2)
library(GGally)

mydata = read.csv("Heat_Tolerance.csv",sep = ",",colClasses = c("factor","character","character","numeric",
"numeric","numeric",
"numeric","factor",
"numeric","factor","factor"),na.strings = "NA")

mydata <- tbl_df(mydata)

A <- mydata[mydata$Sample == "A",]
B <- mydata[mydata$Sample == "B",]
C <- mydata[mydata$Sample == "C",]
D <- mydata[mydata$Sample == "D",]
E <- mydata[mydata$Sample == "E",]

A <- A[c(1,7,8,9,10,11)]
B <- B[c(1,7,8,9,10,11)]
C <- C[c(1,7,8,9,10,11)]
D <- D[c(1,7,8,9,10,11)]
E <- E[c(1,7,8,9,10,11)]

A25 = A[A$Temperature==25,]
B25 = B[B$Temperature==25,]
C25 = C[C$Temperature==25,]
D25 = D[D$Temperature==25,]
E25 = E[E$Temperature==25,]

A30 = A[A$Temperature==30,]
B30 = B[B$Temperature==30,]
C30 = C[C$Temperature==30,]
D30 = D[D$Temperature==30,]
E30 = E[E$Temperature==30,]

A35 = A[A$Temperature==35,]
B35 = B[B$Temperature==35,]
C35 = C[C$Temperature==35,]
D35 = D[D$Temperature==35,]
E35 = E[E$Temperature==35,]


par(mfrow = c(2,3))

boxplot(A$Survival~A$Temperature,xlab = "Temp",ylab="Survival",main= "A")
boxplot(B$Survival~B$Temperature,xlab = "Temp",ylab="Survival",main= "B")
boxplot(C$Survival~C$Temperature,xlab = "Temp",ylab="Survival",main= "C")
boxplot(D$Survival~D$Temperature,xlab = "Temp",ylab="Survival",main= "D")
boxplot(E$Survival~E$Temperature,xlab = "Temp",ylab="Survival",main= "E")

boxplot(E$Survival~E$Temperature,xlab = "Temp",ylab="Survival",main= "E")

plot(A$Survival~A$NumberofDays,ylab= "Survival", xlab = "Days",main="A",col = A$Temperature)
abline(glm(A25$Survival~A25$NumberofDays),col = "black",lwd =2)
abline(glm(A30$Survival~A30$NumberofDays),col = "red",lwd =2)
abline(glm(A35$Survival~A35$NumberofDays),col = "green",lwd =2)

plot(B$Survival~B$NumberofDays,ylab= "Survival", xlab = "Days",main="B",col = B$Temperature)
abline(glm(B25$Survival~B25$NumberofDays),col = "black",lwd =2)
abline(glm(B30$Survival~B30$NumberofDays),col = "red",lwd =2)
abline(glm(B35$Survival~B35$NumberofDays),col = "green",lwd =2)

plot(C$Survival~C$NumberofDays,ylab= "Survival", xlab = "Days",main="C",col = C$Temperature)
abline(glm(C25$Survival~C25$NumberofDays),col = "black",lwd =2)
abline(glm(C30$Survival~C30$NumberofDays),col = "red",lwd =2)
abline(glm(C35$Survival~C35$NumberofDays),col = "green",lwd =2)

plot(D$Survival~D$NumberofDays,ylab= "Survival", xlab = "Days",main="D",col = D$Temperature)
abline(glm(D25$Survival~D25$NumberofDays),col = "black",lwd =2)
abline(glm(D30$Survival~D30$NumberofDays),col = "red",lwd =2)
abline(glm(D35$Survival~D35$NumberofDays),col = "green",lwd =2)

plot(E$Survival~E$NumberofDays,ylab= "Survival", xlab = "Days",main="E",col = E$Temperature)
abline(glm(E25$Survival~E25$NumberofDays),col = "black",lwd =2)
abline(glm(E30$Survival~E30$NumberofDays),col = "red",lwd =2)
abline(glm(E35$Survival~E35$NumberofDays),col = "green",lwd =2)


t.test(A30$Survival,B30$Survival)
t.test(A30$Survival,C30$Survival)
t.test(A30$Survival,D30$Survival)
t.test(A30$Survival,E30$Survival)
