#Load Data
library(readxl)
library(caret)
library(MASS)
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

full_model <- stepAIC(glm(train_data$outcome.PE ~ ., data = train_data, family = binomial),direction = "backward")

#Logistic Regression
ctrlspecs <- trainControl(method="cv", 
                          number=10, 
                          savePredictions="all",
                          classProbs=TRUE)

lr_model <- train(outcome.PE ~ PE.History + stroke.or.TIA + htn + BMI + HR + Age.at.Encounter, 
                data=train_data, 
                method="glm", 
                family=binomial, 
                trControl=ctrlspecs)


t_vals <- seq(0.1,.45,by=.001)
# Evaluate on train data to obtain threshold
predictions_lrt <- predict(lr_model, type="prob")
my_df <- data.frame(Column1 = numeric(), Column2 = numeric(),Column3=numeric())
for (val in t_vals)
{
  predicted_classes <- ifelse(predictions_lrt[,2] > val, 'PE', 'No.PE')
  cm<-confusionMatrix(data=as.factor(predicted_classes), 
                      reference=as.factor(train_data$outcome.PE), positive="PE")
  my_df <- rbind(my_df, c(cm$byClass[1], cm$byClass[2],val))
  
}
names(my_df) <- c("Sensitivity", "Specificity","Threshold")
my_df[order(my_df$Sensitivity,decreasing=TRUE), ]
#Threshold of 0.13 chosen

# Make predictions on the test data
predictions_lr <- predict(lr_model, newdata = test_data, type="prob")

# Convert probabilities to class labels
predicted_classes <- ifelse(predictions_lr[,2] > 0.13, 'PE', 'No.PE')

# Create a confusion matrix
confusionMatrix(as.factor(predicted_classes), as.factor(test_data$outcome.PE),positive="PE")

x <- evalm(lr_model)

#Try Again with Downsampling
idx.dat <- c(which(train_data$outcome.PE == 'PE'),
             sample(which(train_data$outcome.PE == 'No.PE'), 109))
train_data2 <- train_data[idx.dat,]

lr_model2 <- train(outcome.PE ~ PE.History + stroke.or.TIA + htn + BMI + HR + Age.at.Encounter, 
                  data=train_data2, 
                  method="glm", 
                  family=binomial, 
                  trControl=ctrlspecs)


t_vals <- seq(0.1,.45,by=.001)
# Predict on train data
predictions_t <- predict(lr_model2, type="prob")
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
predictions <- predict(lr_model2, newdata = test_data, type="prob")

# Convert probabilities to class labels
predicted_classes <- ifelse(predictions[,2] > 0.37, 'PE', 'No.PE')

# Create a confusion matrix
confusionMatrix(as.factor(predicted_classes), as.factor(test_data$outcome.PE),positive="PE")




