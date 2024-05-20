#Load libraries
library(readxl)
library(caret)
library(klaR)
library(MLeval)
library(caTools)
#Load raw data
pe_ae <- read_excel("~/Downloads/Embolism.xlsx")
#Remove unnecessary data
pe_ae<-pe_ae[-1,]
pe_ae<-pe_ae[,-1]
#Convert type to numeric for each variable
pe_ae <- data.frame(sapply(pe_ae, as.numeric))
pe_ae$outcome.PE <- as.factor(pe_ae$outcome.PE)
#Only include complete cases
pe_ae <- pe_ae[complete.cases(pe_ae), ]
#Dropped following variables because all the values were the same
pe_ae <-  subset(pe_ae, 
                 select = -c(Contraceptives, Fracturesanesthsurg,Hemoptysis,number.of.ED.Visits.Previous.Year))
levels(pe_ae$outcome.PE) <- c('No.PE', 'PE')
#Set seed for reproducibility
set.seed(1)
#80/20 train/test split
split <- sample.split(pe_ae$outcome.PE, SplitRatio = 0.8)
train_data <- pe_ae[split, ]
test_data <- pe_ae[!split, ]

#Naive Bayes
ctrlspecs <- trainControl(method="cv", 
                          number=10, 
                          savePredictions="all",
                          classProbs=TRUE)

nb_model <- train(outcome.PE ~ ., 
                  data=train_data, 
                  method="nb", 
                  trControl=ctrlspecs)


t_vals <- seq(0.01,.45,by=.001)
# Evaluate on train data to obtain threshold
predictions_nbt <- predict(nb_model, type="prob")
my_df <- data.frame(Column1 = numeric(), Column2 = numeric(),Column3=numeric())
for (val in t_vals)
{
  predicted_classes <- ifelse(predictions_nbt[,2] > val, 'PE', 'No.PE')
  cm<-confusionMatrix(data=as.factor(predicted_classes), 
                      reference=as.factor(train_data$outcome.PE), positive="PE")
  my_df <- rbind(my_df, c(cm$byClass[1], cm$byClass[2],val))
  
}
names(my_df) <- c("Sensitivity", "Specificity","Threshold")
my_df[order(my_df$Sensitivity,decreasing=TRUE), ]
#Threshold of 0.019 chosen

# Make predictions on the test data
predictions_nb <- predict(nb_model, newdata = test_data, type="prob")

# Convert probabilities to class labels
predicted_classes <- ifelse(predictions_nb[,2] >.019, 'PE', 'No.PE')

# Create a confusion matrix
confusionMatrix(as.factor(predicted_classes), as.factor(test_data$outcome.PE),positive="PE")

x <- evalm(nb_model)

#Try Again with Downsampling
idx.dat <- c(which(train_data$outcome.PE == 'PE'),
             sample(which(train_data$outcome.PE == 'No.PE'), 109))
train_data2 <- train_data[idx.dat,]

nb_model2 <- train(outcome.PE ~ ., 
                  data=train_data2, 
                  method="nb", 
                  trControl=ctrlspecs)


t_vals <- seq(0.05,.45,by=.001)
# Predict on train data
predictions_t <- predict(nb_model2, type="prob")
my_df <- data.frame(Column1 = numeric(), Column2 = numeric(),Column3=numeric())
for (val in t_vals)
{
  predicted_classes <- ifelse(predictions_t[,2] > val, 'PE', 'No.PE')
  cm<-confusionMatrix(data=as.factor(predicted_classes), 
                      reference=as.factor(train_data2$outcome.PE), positive="PE")
  my_df <- rbind(my_df, c(cm$byClass[1], cm$byClass[2],val))
  
}
names(my_df) <- c("Sensitivity", "Specificity","Threshold")
my_df[order(my_df$Sensitivity,decreasing=TRUE), ]

# Make predictions on the test data
predictions <- predict(nb_model2, newdata = test_data, type="prob")

# Convert probabilities to class labels
predicted_classes <- ifelse(predictions[,2] > .056, 'PE', 'No.PE')

# Create a confusion matrix
confusionMatrix(as.factor(predicted_classes), as.factor(test_data$outcome.PE),positive="PE")