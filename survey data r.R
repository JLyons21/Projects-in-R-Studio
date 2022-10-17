#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("readr")
library(readr)
attributes(CompleteResponses)
attributes(SurveyIncomplete)
summary(CompleteResponses)
summary(SurveyIncomplete)
str(CompleteResponses)
str(SurveyIncomplete)
names(CompleteResponses)
names((SurveyIncomplete))
hist(CompleteResponses$age)
hist(SurveyIncomplete$age)
hist(CompleteResponses$brand)
#Acer 0 Sony 1
#install.packages("tidyverse")
library(dplyr)
head(CompleteResponses)
head(SurveyIncomplete)
region <- SurveyIncomplete$zipcode
region
#install.packages("DataExplorer")
library(DataExplorer)
plot_bar(CompleteResponses)
plot_qq(CompleteResponses)
plot_density(CompleteResponses)
plot_density(SurveyIncomplete)
plot_prcomp(CompleteResponses)
plot_prcomp(SurveyIncomplete)
plot_correlation(CompleteResponses)
#salary has the highest correction to Brand preference .21
str(CompleteResponses)
str(SurveyIncomplete)
#install.packages("GGally")
library(GGally)
CompleteResponses$brand<-factor(CompleteResponses$brand)
CompleteResponses %>%
  select("salary", "age","zipcode") %>%
  ggpairs(mapping = aes(color = CompleteResponses$brand, alpha = .5))
SurveyIncomplete$brand<-factor(SurveyIncomplete$brand)
#install.packages("SmartEDA")
#install.packages("ggplot2")
#install.packages("lattice")
library(caret)
plot_correlation(CompleteResponses)
CompleteResponses$car<-factor(CompleteResponses$car)
CompleteResponses$elevel<-factor(CompleteResponses$elevel)
SurveyIncomplete$car<-factor(SurveyIncomplete$car)
SurveyIncomplete$elevel<-factor(SurveyIncomplete$elevel)
plot_correlation(CompleteResponses)
CompleteResponses %>%
  select("salary", "age","zipcode") %>%
  ggpairs(mapping = aes(color = CompleteResponses$brand, alpha = .5))
save.image()
set.seed(421)
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
gbmFit1 <- train(brand~., data = training, method = "gbm", trControl=fitControl, verbose = FALSE)
gbmFit1
predict(gbmFit1, newdata = head(testing))
save.image()

set.seed(421)
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
rfFit1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
rfFit1
rfFit2 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)
rfFit2
rfFit3 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneLength = 21)
rfFit3
rfGrid <- expand.grid(mtry=c(1,2,3))
#3 -  acc .783  kappa .499
rfFitm1 <- train(brand~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid)
rfFitm1
rfGrid <- expand.grid(mtry=c(4,5,6))
#6 - acc .914  kappa .818
rfGrid <- expand.grid(mtry=c(7,8,9))
#9 - acc .917 kappa .828
rfGrid <- expand.grid(mtry=c(10,11,12))
#10 - acc -919 kappa .824
rfGrid <- expand.grid(mtry=c(13,14,15))
#14 - acc .9166 kappa .823
#best overall was 9
#install.packages("C50")
#install.packages("inum")
library(C50)
set.seed(421)
inTraining <- createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
training <- CompleteResponses[inTraining,]
testing <- CompleteResponses[-inTraining,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
C50Fit1 <- train(brand~., data = training, method = "C5.0", trControl=fitControl, verbose = FALSE)
C50Fit1
# trials =10 model = tree and winnow = true
#important factors - Salary, Age, Car9, Car 14, Car 11 Education level 1 and 3
varImp(C50Fit1)
summary(gbmFit1)
plot(varImp(gbmFit1))
print(gbmFit1)
varImp(rfFitm1)
plot(varImp(rfFitm1))
plot(varImp(C50Fit1))
#salary, age, credit, zipcode
plot(gbmFit1)
plot(C50Fit1)
plot(rfFitm1)
#compare model and accuracy
resample_results <- resamples(list(RandomForest = rfFitm1, C5.0 = C50Fit1, GBM = gbmFit1))
resample_results
resample_results$values
summary(resample_results)
bwplot(resample_results)
diff_results <- diff(resample_results)
summary(diff_results, metric = "accuracy")
compare_models(C50Fit1, rfFitm1)
testpred1 <-predict(rfFitm1, testing)
postResample(testpred1, testing$brand)
confusionMatrix(testpred1, testing$brand)
#accuracy 93% kappa .8574
finalpred1 <- predict(rfFitm1, SurveyIncomplete)
summary(finalpred1)
#0 1889 and 1 3111
summary(CompleteResponses$brand)
#0 3744  1 6154
postResample(finalpred1, SurveyIncomplete$brand)
#accuracy is going to be terrible since that data is not correct
#accuracy .388  kappa .0115
summary(SurveyIncomplete$brand)
# 0 4937   1 63
#adding final predictions into the Survey Incomplete data set
SurveyIncomplete['brand'] <- finalpred1
#SurveyIncomplete = subset(SurveyIncomplete, select = -finalbrand)
#SurveyIncomplete['brand'] <- SurveyIncomplete$finalbrand
summary(CompleteResponses)
summary(SurveyIncomplete)
#merge both datasets 
mergedfinal <- rbind(CompleteResponses, SurveyIncomplete)
summary(mergedfinal)
#install.packages("writexl")
library(writexl)
write_xlsx(SurveyIncomplete,'C:\\Users\\bling\\Desktop\\Jens Class\\surveyincomplete.xlsx')
write_xlsx(mergedfinal,'C:\\Users\\bling\\Desktop\\Jens Class\\merged.xlsx')
comboprediction <- predict(rfFitm1, mergedfinal)
confusionMatrix(comboprediction, mergedfinal$brand)
 library(ggplot2)
qplot(mergedfinal$brand)
qplot(comboprediction)
mergedfinal['preds'] <- comboprediction
summary(mergedfinal)
save.image()
plot_bar(mergedfinal)
save.image()
boxplot(mergedfinal$preds, mergedfinal$brand)
summary(mergedfinal)
qplot(mergedfinal$brand)
qplot(mergedfinal$preds)
plot(mergedfinal$brand, col = 1) # Plot 1st histogram using a transparent color
plot(mergedfinal$preds, col = 2, add = TRUE) # Add 2nd histogram using different color
