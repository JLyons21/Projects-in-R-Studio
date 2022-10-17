library(readr)
library(corrplot)
library(caret)
library(ggplot2)
library(lattice)
library(dplyr)
#install.packages("corrplot")
#need to dummify data
DumData <- dummyVars(" ~ .", data = existingproductattributes2017)
readyData <- data.frame(predict(DumData, newdata = existingproductattributes2017))
summary(readyData)
summary(existingproductattributes2017)
str(readyData)
readyData$attributeWithMissingData <- NULL
corrData <- cor(readyData)
corrData
corrplot(corrData)
readyData <- select(readyData, -BestSellersRank)
readyData <- select(readyData, -ProfitMargin)
readyData <- select(readyData, -ProductNum)
#PC, Laptops, Netbooks and Smartphones
#laptop - highest corr was price .296 (low positive)
#netbook - highest corr was Recommend Product -.363 (low negative)
#pc - highest corr was price .547 (medium positive)
#smartphones - highest corr was Product Height - -.196 (low negative)
#volume highest corr was 5 star 1.0 (perfect positive) followed by 4 stars (stars corr went down with each less star) and pos service
set.seed(422)
trainSize<-round(nrow(readyData)*0.7) 
testSize<-nrow(readyData)-trainSize
trainSize
testSize
training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,] 
vol.model<-lm(Volume~ x5StarReviews, trainSet)
vol.model1<-lm(Volume~ Price, trainSet)
vol.model2<-lm(Volume~ Price+PositiveServiceReview+Recommendproduct, trainSet)
summary(vol.model)
#model is overfit - since both features have a perfect correlation, it was an unsuccessful model
summary(vol.model1)
summary(vol.model2)
plot(vol.model1)
save.image()
set.seed(422)
inTraining1 <- createDataPartition(readyData$Volume, p = .75, list = FALSE)
training1 <- readyData[inTraining1,]
testing1 <- readyData[-inTraining1,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
rfFit1 <- train(Volume~., data = training1, method = "rf", trControl=fitControl, tuneLength = 1)
rfFit1
# RMSE 724.3416 / r2 - 92.62% MAE - 351.8918 mtry was 8
inTraining2 <- createDataPartition(readyData$Volume, p = .7, list = FALSE)
training2 <- readyData[inTraining2,]
testing2 <- readyData[-inTraining2,]
fitControl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
gbmFit1 <- train(Volume~., data = training2, method = "gbm", trControl=fitControl, verbose = FALSE)
inTraining3 <- createDataPartition(readyData$Volume, p = .7, list = FALSE)
training3 <- readyData[inTraining3,]
testing3 <- readyData[-inTraining3,]
fitControl3 <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
svmFit1 <- train(Volume~., data = training3, method = "svmLinear2", trControl=fitControl, verbose = FALSE)
svmFit1
#svm - rmse - 258.5443 r2 - 93.37 mae 165.4101
gbmFit1
#gmb - n trees 50 - interaction depth 3 - rmse 1005.962 r2 76.58 mae 636.4232
predict(svmFit1, newdata = head(testing3))
predict(gbmFit1, newdata = head(testing2))
predict(rfFit1, newdata = head(testing1))
summary(gbmFit1)
plot(varImp(gbmFit1))
print(gbmFit1)
save.image()
summary(svmFit1)
plot(varImp(svmFit1))
plot(varImp(rfFit1))
save.image()
resample_results <- resamples(list(RandomForest = rfFit1, SVM = svmFit1, GBM = gbmFit1))
resample_results
resample_results$values
summary(resample_results)
bwplot(resample_results)
diff_results <- diff(resample_results)
summary(diff_results, metric = "accuracy")
compare_models(svmFit1, rfFit1)
testpred1 <-predict(gbmFit1, testing2)
summary(testpred1)
testpred2 <-predict(svmFit1, testing3)
summary(testpred2)
testpred3 <- predict(rfFit1, testing1)
summary(testpred3)
compare_models(svmFit1, gbmFit1)
compare_models(rfFit1, gbmFit1)
DumData2 <- dummyVars(" ~ .", data = newproductattributes2017)
NewData2 <- data.frame(predict(DumData2, newdata = newproductattributes2017))
NewData2 <- select(NewData2, -BestSellersRank)
NewData2 <- select(NewData2, -ProfitMargin)
NewData2 <- select(NewData2, -ProductNum)
summary(NewData2)
str(NewData2)
NewData2$attributeWithMissingData <- NULL
summary(NewData2)
finalpred1 <- predict(rfFit1, NewData2)
summary(finalpred1)
NewData2 ['Volume'] <- finalpred1
summary(NewData2$Volume)
summary(NewData2)
plot(NewData2$Volume)
#install.packages("writexl")
#library(writexl)
write_xlsx(NewData2,'C:\\Users\\bling\\Desktop\\Jens Class\\Newproductsvolume.xlsx')
#library(ggplot2)
NewData2$Volume <- round(NewData2$Volume, digits = 0)
summary(NewData2)
write_xlsx(NewData2,'C:\\Users\\bling\\Desktop\\Jens Class\\Newproductsvolume2.xlsx')
#sums for Pc - 650 Laptop - 341 netbook - 1831 smartphone - 1692
save.image()
