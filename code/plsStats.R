###################
#PLS DATA ANALYSIS#
###################


library(lmSupport)
rm(list=ls())   #clear all objects from workspace

filepath = "path_directory"
#Reading data matrix
d <- read.csv(file="path_directory", header=TRUE, sep=",")
d <- subset(d, select = -c(X) ) # -- Removing the variable
id = 1:length(d$totalScore)
d$id=id #adding id variable

#Describe variables
varDescribe(d)

#Histogram for stable block
varPlot(d$LearningRateStable, VarName = 'Learning rate stable block', AddPoints = 'Rug', Detail = 3)
#Histogram for volatile block
varPlot(d$LearningRateVolatile, VarName = 'Learning rate volatile block', AddPoints = 'Rug', Detail = 3)

#####################################################
#Correlation learning rate volatile and stable block#
#####################################################
m1 = lm(d$LearningRateStable~d$LearningRateVolatile, data = d)
modelSummary(m1)
modelCaseAnalysis(m1,Type = "RESIDUALS" )

#Correlation plot
plot(d$LearningRateStable, d$LearningRateVolatile , main="Corr Stable Block and Volatile Block", 
     xlab="Learning Rate Stable Block ", ylab="Learning Rate Volatile Block", pch=19)
abline(lm(d$LearningRateVolatile~d$LearningRateStable), col="red") # regression line (y~x) 

#############################################
#Correlation LSI scores and total task score#
#############################################
m2 = lm(d$totalScore~d$LsiScores, data = d)
modelSummary(m2)
modelEffectSizes(m2)
#Checking for outliers
modelCaseAnalysis(m2, Type = "RESIDUALS")

#Correlation plot (with outlier)
plot(d$LsiScores, d$totalScore , main="Corr Total Score & LSI Scores", 
     xlab="LSI Scores ", ylab="Total Task Score", pch=19)

abline(lm(d$totalScore~d$LsiScores), col="red") # regression line (y~x) 

####filter out outlier####
d1 <- subset(d, id!= 8)

#re-run analysis without outlier
m21 = lm(d1$totalScore~d1$LsiScores, data = d1)
modelSummary(m21)
modelEffectSizes(m21)

#Correlation plot (without outlier)
plot(d1$LsiScores, d1$totalScore , main="Corr Total Score & LSI Scores", 
     xlab="LSI Scores ", ylab="Total Task Score", pch=19)
abline(lm(d1$totalScore~d1$LsiScores), col="red") # regression line (y~x) 


####################################################
#Correlation LSI scores and change in learning rate#
####################################################

##Change in learning rate = Learning rate in Volatile Block - Learning Rate in Stable block
##Index flexibility in adapting the learning rate from one block to the other

m3 = lm(d$LearningRateChange~d$LsiScores, data = d)
modelSummary(m3)
modelEffectSizes(m3)
#Checking for outliers
modelCaseAnalysis(m3, Type = "RESIDUALS")

#Correlation plot with outlier
plot(d$LsiScores, d$LearningRateChange , main="Corr Change in LR and LSI", 
     xlab="LSI Scores ", ylab="Change in Learning Rate", pch=19)

abline(lm(d$LearningRateChange~d$LsiScores), col="red") # regression line (y~x) 

####filter out outlier###
d2 <- subset(d, id!= 6)

#re-reun model without outlier
m31 = lm(d2$LearningRateChange~d2$LsiScores, data = d2)
modelSummary(m31)
modelEffectSizes(m31)

#Correlation plot (without outlier)
plot(d2$LsiScores, d2$LearningRateChange , main="Corr Change in LR and LSI", 
     xlab="LSI Scores ", ylab="Change in Learning Rate", pch=19)

abline(lm(d2$LearningRateChange~d2$LsiScores), col="red") # regression line (y~x) 

##########################################################
#Correlation LSI scores and learning rate in stable block#
##########################################################

m4 = lm(d$LearningRateStable~d$LsiScores, data = d)
modelSummary(m4)
modelEffectSizes(m4)
#Checking for outliers
modelCaseAnalysis(m4, Type = "RESIDUALS")

#Correlation plot (with outlier)
plot(d$LsiScores, d$LearningRateStable , main="Corr LR stable block & LSI Scores", 
     xlab="LSI Scores ", ylab="Learning Rate Stable Block", pch=19)

abline(lm(d$LearningRateStable~d$LsiScores), col="red") # regression line (y~x) 

#filter outlier 
d3 <- subset(d, id!= 6)

#re-reun model without outlier
m41 = lm(d3$LearningRateStable~d3$LsiScores, data = d3)
modelSummary(m41)
modelEffectSizes(m41)

#Correlation plot(without outlier)
plot(d3$LsiScores, d3$LearningRateStable , main="Corr LR stable block & LSI Scores", 
     xlab="LSI Scores ", ylab="Learning Rate Stable Block", pch=19)

abline(lm(d$LearningRateStable~d$LsiScores), col="red") # regression line (y~x) 

############################################################
#Correlation LSI scores and learning rate in volatile block#
###########################################################

m5 = lm(d$LearningRateVolatile~d$LsiScores, data = d)
modelSummary(m5)
modelEffectSizes(m5)
#Checking for outliers
modelout <- modelCaseAnalysis(m5, Type = "RESIDUALS")

#Correlation plot (with outlier)
plot(d$LsiScores, d$LearningRateVolatile , main="Corr LR stable block & LSI Scores", 
     xlab="LSI Scores ", ylab="Learning Rate Volatile Block", pch=19)

abline(lm(d$LearningRateVolatile~d$LsiScores), col="red") # regression line (y~x) 

#filter outlier 
d4 <- subset(d, id!= 1)

#re-reun model without outlier
m51 = lm(d4$LearningRateVolatile~d4$LsiScores, data = d4)
modelSummary(m51)
modelEffectSizes(m51)

#Correlation plot(without outlier)
plot(d4$LsiScores, d4$LearningRateVolatile , main="Corr LR Volatile Block & LSI Scores", 
     xlab="LSI Scores ", ylab="Learning Rate Volatile Block", pch=19)
abline(lm(d$LearningRateVolatile~d$LsiScores), col="red") # regression line (y~x) 




