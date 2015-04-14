library(plyr)
library(randomForest)

# data contains parameters related to how physicians react to different types of marketing techniques (ie digital, email, phone call, etc). 
data_test <- data2

# predict the dependent variable values from the other variables in the data_testset

k = 5 # Folds

# sample from 1 to k, nrow times (the number of observations in the data_test)
data_test$id <- sample(1:k, nrow(data_test), replace = TRUE)
list <- 1:k

# prediction and testset data_test frames that we add to with each iteration over
# the folds

prediction <- data.frame()
testsetCopy <- data.frame()

#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k){
  # remove rows with id i from data_testframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(data_test, id %in% list[-i])
  testset <- subset(data_test, id %in% c(i))
  
  # run a random forest model
  mymodel <- randomForest(rep_live~speciality+location+hcp_value+p2p+digital_pull+rep_remote+digital_push+direct_mail, data = trainingset, ntree = 100)
  #mymodel <- rpart(rep_live~speciality+location+hcp_value+p2p+digital_pull+rep_remote+digital_push+direct_mail, data = trainingset, method = "class", cp = 2.7785e-04)
  #mymodel <- boosting(rep_live~speciality+location+hcp_value+p2p+digital_pull+rep_remote+digital_push+direct_mail, data = trainingset)
  #mymodel <- naiveBayes(rep_live~speciality+location+hcp_value+p2p+digital_pull+rep_remote+digital_push+direct_mail, data = trainingset)
  
  # remove response
  temp <- as.data.frame(predict(mymodel, testset[c(-11)]))
  #temp <- as.data.frame(predict(mymodel, testset[c(-11)], type = "class"))
  #temp.boosting <- predict.boosting(mymodel, testset)
  #temp <- as.data.frame(temp.boosting$class)
  #temp <- as.data.frame(predict(mymodel, testset[c(-11)]))
  
  # append this iteration's predictions to the end of the prediction data_test frame
  prediction <- rbind(prediction, temp)
  
  
  # append this iteration's test set to the test set copy data frame
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[c(11)]))
  
  progress.bar$step()
}

# add predictions and actual dependent variables values
result <- cbind(prediction, testsetCopy[,1])
names(result) <- c("Predicted", "Actual")
result$Difference <- result$Actual == result$Predicted

# As an example use % correct classification 
table(result$Difference)
# % Accuracy
sum(result$Difference)/length(result$Difference)
