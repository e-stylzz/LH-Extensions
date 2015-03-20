#Author:  Eric Barb
#Description:  Working with Extensions and their probability


#Get basics in order
setwd("~/GitHub/LH-Extensions")
extensions <- read.csv("~/GitHub/LH-Extensions/src/extmain.csv")


#Bring in Libraries
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)

#Create Train and Test Sets
extensions$group <- runif(dim(extensions)[1])
test <- subset(extensions, extensions$group <= 0.1)
train <- subset(extensions, extensions$group > 0.1)
