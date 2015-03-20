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
library(ggplot2)

#Create Train and Test Sets
extensions$group <- runif(dim(extensions)[1])
test <- subset(extensions, extensions$group <= 0.1)
train <- subset(extensions, extensions$group > 0.1)



# Feature Engineering #

#Convert Extended to a factor
train$Extended <- as.factor(train$Extended)

# Change Date Fields to Dates 
train$StartDate <- as.Date(train$StartDate, format = "%m/%d/%Y")
train$EndDate <- as.Date(train$EndDate, format = "%m/%d/%Y")
train$ExtStartDate <- as.Date(train$ExtStartDate, format = "%m/%d/%Y")
train$ExtEndDate <- as.Date(train$ExtEndDate, format = "%m/%d/%Y")

# Create a new variables to show the DURATION in days. Includes all calendar days
train$OrigDuration <- difftime(train$EndDate, train$StartDate, units = "days")
train$ExtDuration <- difftime(train$ExtEndDate, train$ExtStartDate, units = "days")

#Convert EmpType to character and manipulate it.  Change different subcontractor types to be subs then convert back
train$EmpType <- as.character(train$EmpType)
train$EmpType[train$EmpType == "Subcontractor: Partner Sourced"] <- "Subcontractor"
train$EmpType[train$EmpType == "Subcontractor: Corporate"] <- "Subcontractor"
train$EmpType[train$EmpType == "Subcontractor: Independent/1099"] <- "Subcontractor"
train$EmpType[train$EmpType == "Subcontractor: GLS"] <- "Subcontractor"
train$EmpType <- as.factor(train$EmpType)

#Create a grouping based on Rates and then apply that to a new field called RateGroup
breaks <- c(50, 100, 125, 130, 135, 140, 145, 150, 175, 200)
RateGroups <- cut(train$Rate, breaks= breaks)
train$RateGroup <- RateGroups



##### Plots to evaluate relationships #####

# side by side bar chart
ggplot(train) + geom_bar(aes(x= RateGroup, fill= Extended),position = "dodge")

# 100% fill bar chart
ggplot(train) + geom_bar(aes(x= RateGroup, fill= Extended),position = "fill")


ggplot(train, aes(x= RateGroup, y=ExtNum)) +
  geom_point() +
  stat_smooth(method = "lm")
#coord_flip()


#   facet_wrap(~EmpType + Practice ) +
#   geom_histogram(binwidth = 5) +
#   xlab("Rate") +
#   ylab("Total Count")
#   theme(axis.text.x = element_text(angle = 45, vjust = 1.0))



# Modeling #

fit <- rpart(Extended ~ RateGroup + ServiceOffering + OrigDuration + EmpType,
             data = train, method = "class")

fancyRpartPlot(fit)

