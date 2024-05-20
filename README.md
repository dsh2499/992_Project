# 992_Project

Title 'Models to Predict Pulmonary Embolism for Patients with Asthma Exacerbation'

Background: Pulmonary embolism's symptoms are non-specific and make it hard to diagnose as it may be confused with other lung-related ailments.
Using data from patients with asthma exacerbation, evaulate four models (logistic, random forest, support vector machines, and naive bayes)
to assess the viability of using models to predict pulmonary embolism given baseline demographical and clinical characteristics.

Files:

992_LR.R - Logistic Regression Model

992_NB.R - Naive Bayes Model

992_RF.R - Random Forest Model

992_SVM.R - Support Vector Machine Model

Each model predicted pulmonary embolism using a threshold determined by performance on training data. A confusion matrix was also made to get the specificity and senestivity of each model.

992_AUC_ROC.R
Using the output from the files above, constructed AUCROC curve for each model. Since the output wasn't saved out, need to run those four files above so that this file can be ran.
May update it to save output from the four files that there is no need for unnecessary code running.

Full results and report are in: 



