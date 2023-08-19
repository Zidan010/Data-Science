SHL_Dataset <- read.csv("F:/Data Science/sleep health lifestyle dataset/Sleep_health_and_lifestyle_dataset.csv",header = TRUE,sep = ",")
names(SHL_Dataset)
Attribute <- names(SHL_Dataset) 
DataType <- c(typeof(SHL_Dataset$Person.ID), typeof(SHL_Dataset$Gender), typeof(SHL_Dataset$Age),
               typeof(SHL_Dataset$Occupation), typeof(SHL_Dataset$Sleep.Duration), typeof(SHL_Dataset$Quality.of.Sleep), 
               typeof(SHL_Dataset$Physical.Activity.Level), typeof(SHL_Dataset$Stress.Level),
               typeof(SHL_Dataset$BMI.Category), typeof(SHL_Dataset$Blood.Pressure), typeof(SHL_Dataset$Heart.Rate),
               typeof(SHL_Dataset$Daily.Steps), typeof(SHL_Dataset$Sleep.Disorder)) 
data.frame(Attribute, DataType)
View(SHL_Dataset)
SHL_Dataset_mod <- read.csv("F:/Data Science/sleep health lifestyle dataset/Sleep_health_and_lifestyle_dataset.csv",header = TRUE,sep = ",")


library(dplyr)
SHL_Dataset_mod$Gender <- ifelse(SHL_Dataset_mod$Gender == "Male", 1, 0)
Gender_Actual_Value_freq <- table(SHL_Dataset$Gender)
View(Gender_Actual_Value_freq)
Gender_Changed_Value_freq <- table(SHL_Dataset_mod$Gender)
View(Gender_Changed_Value_freq)
View(SHL_Dataset_mod)


Occupation_actual_freq <- table(SHL_Dataset_mod$Occupation)
View(Occupation_actual_freq)
SHL_Dataset_mod <- SHL_Dataset_mod %>%
  mutate_at(vars(Occupation), as.factor) %>%
  mutate(across(Occupation, as.integer))
Occupation_Changed_Value_freq <- table(SHL_Dataset_mod$Occupation)
View(Occupation_Changed_Value_freq)
View(SHL_Dataset_mod)
prac_datatset_8 <- SHL_Dataset


BMI_actual_freq <- table(SHL_Dataset_mod$BMI.Category)
View(BMI_actual_freq)
SHL_Dataset_mod <- SHL_Dataset_mod %>%
  mutate_at(vars(BMI.Category), as.factor) %>%
  mutate(across(BMI.Category, as.integer))
BMI_Changed_Value_freq <- table(SHL_Dataset_mod$BMI.Category)
View(BMI_Changed_Value_freq)
View(SHL_Dataset_mod)



Blood_Pressure_freq <- table(SHL_Dataset_mod$Blood.Pressure)
View(Blood_Pressure_freq)

blood_pressure_component <- strsplit(SHL_Dataset_mod$Blood.Pressure, "/")
systolic <- as.numeric(sapply(blood_pressure_component, "[[", 1))
diastolic <- as.numeric(sapply(blood_pressure_component, "[[", 2))


SHL_Dataset_mod$Blood.Pressure <- ifelse(systolic < 120 & diastolic < 80, "Normal",
                                       ifelse(systolic >= 160 | diastolic >= 100, "Stage 2 Hypertension",
                                              ifelse(systolic >= 140 | diastolic >= 90, "Stage 1 Hypertension", "Pre-hypertension")))

SHL_Dataset_mod$Blood.Pressure <- factor(SHL_Dataset_mod$Blood.Pressure, levels = c("Normal", "Pre-hypertension", "Stage 1 Hypertension", "Stage 2 Hypertension"), ordered = TRUE)
SHL_Dataset_mod$Blood.Pressure <- as.numeric(SHL_Dataset_mod$Blood.Pressure) - 1


prac_datatset_lstst <- SHL_Dataset
blood_pressure_componentlstst <- strsplit(prac_datatset_lstst$Blood.Pressure, "/")
systolicss <- as.numeric(sapply(blood_pressure_componentlstst, "[[", 1))
diastolicss <- as.numeric(sapply(blood_pressure_componentlstst, "[[", 2))


prac_datatset_lstst$Blood.Pressure <- ifelse(systolicss < 120 & diastolicss < 80, "Normal",
                                         ifelse(systolicss >= 160 | diastolicss >= 100, "Stage 2 Hypertension",
                                                ifelse(systolicss >= 140 | diastolicss >= 90, "Stage 1 Hypertension", "Pre-hypertension")))


prac_datatset_lst$Blood.Pressure <- factor(prac_datatset_lst$Blood.Pressure, levels = c("Normal", "Pre-hypertension", "Stage 1 Hypertension", "Stage 2 Hypertension"), ordered = TRUE)
prac_datatset_lst$Blood.Pressure <- as.numeric(prac_datatset_lst$Blood.Pressure) - 1



Blood_Pressure_mid_chngd_freq <- table(prac_datatset_lstst$Blood.Pressure)
View(Blood_Pressure_mid_chngd_freq)



SD_act_freq <- table(SHL_Dataset$Sleep.Disorder)
View(SD_act_freq)

SHL_Dataset_mod <- SHL_Dataset_mod %>%
  mutate_at(vars(Sleep.Disorder), as.factor) %>%
  mutate(across(Sleep.Disorder, as.integer))

SD_chng_freq <- table(SHL_Dataset_mod$Sleep.Disorder)
View(SD_chng_freq)
Sleep_disorder_changed_freq <- table(prac_datatset_8$Sleep.Disorder)
View(Sleep_disorder_changed_freq)



colSums(is.na(SHL_Dataset_mod))


normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
num_of_column<- c("Age","Occupation","Sleep.Duration","Quality.of.Sleep","Physical.Activity.Level",
               "Stress.Level","BMI.Category","Blood.Pressure","Heart.Rate","Daily.Steps")

SHL_Dataset_mod[num_of_column] <- lapply(SHL_Dataset_mod[num_of_column], normalize)



correlation_matrix <- cor(data())

correlation_matrix <- cor(prac_datatset_8[c("Person.ID","Gender","Age","Occupation","Sleep.Duration","Quality.of.Sleep","Physical.Activity.Level",
                                            "Stress.Level","BMI.Category","Blood.Pressure","Heart.Rate","Daily.Steps")], prac_datatset_8$Sleep.Disorder)



correlation_matrix_main <- cor(SHL_Dataset_mod[c("Person.ID","Gender","Age","Occupation","Sleep.Duration","Quality.of.Sleep","Physical.Activity.Level",
                                            "Stress.Level","BMI.Category","Blood.Pressure","Heart.Rate","Daily.Steps")], SHL_Dataset_mod$Sleep.Disorder)

print(correlation_matrix_main)


install.packages("corrplot")
library(corrplot)
correlation_matrix_main <- cor(SHL_Dataset_mod)

corrplot(correlation_matrix_main, method = "color", type = "upper")
View(correlation_matrix_main)



install.packages("class")
library(class)
set.seed(7)  
sample_indicesM <- sample(nrow(SHL_Dataset_mod), nrow(prac_datatset_8) * 0.7) 
train_dataM <- SHL_Dataset_mod[sample_indicesM, ]
test_dataM <- SHL_Dataset_mod[-sample_indicesM, ]

featuresFM <- c("Gender", "Age", "Occupation", "Sleep.Duration", "Quality.of.Sleep",
                "Physical.Activity.Level","Stress.Level","BMI.Category", "Blood.Pressure", "Heart.Rate", "Daily.Steps")


train_featuresM <- train_dataM[featuresFM]
train_labelsM <- train_dataM$Sleep.Disorder
test_featuresM <- test_dataM[featuresFM]
test_labelsM <- test_dataM$Sleep.Disorder

k <- 5  
predicted_labelsM <- knn(train_featuresM, test_featuresM, train_labelsM, k)
View(predicted_labelsM)

accuracyM <- sum(predicted_labelsM == test_labelsM) / length(test_labelsM)
cat("kNN Classifier Accuracy for Train Test Approach:", accuracyM * 100, "%\n")
View(accuracyM)

conf_matrix_TT <- table(predicted_labels, test_labels)
print(conf_matrix_TT)
View(conf_matrix_TT)

TT_recall_f <- numeric(nrow(conf_matrix_TT))
TT_precision_f <- numeric(nrow(conf_matrix_TT))

for (i in 1:nrow(conf_matrix_TT)) {
  TT_recall_f[i] <- conf_matrix_TT[i, i] / sum(conf_matrix_TT[i, ])
  TT_precision_f[i] <- conf_matrix_TT[i, i] / sum(conf_matrix_TT[, i])
}

cat("\nRecall for each class in Train Test Approach:",TT_recall_f)

cat("\nPrecision for each class:",TT_precision_f)


library(ggplot2)

conf_matrixM <- as.data.frame(as.table(conf_matrix_TT))
names(conf_matrixM) <- c("Predicted_Labels", "Actual_Labels", "Frequency")

ggplot(conf_matrixM, aes(x = Predicted_Labels, y = Actual_Labels, fill = Frequency)) +
  geom_tile() +
  labs(title = "Confusion Matrix - Train-Test Approach",
       x = "Predicted Labels", y = "Actual Labels") +
  scale_fill_gradient(low = "white", high = "green")


library(tidyr)

class_metrics_df <- data.frame(Class = unique(test_labelsM),
                               Recall = TT_recall_f,
                               Precision = TT_precision_f)

class_metrics_df_long <- tidyr::gather(class_metrics_df, Metric, Value, Recall:Precision)


ggplot(class_metrics_df_long, aes(x = Class, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recall and Precision Values for Each Class - Train-Test Approach",
       x = "Class", y = "Value") +
  scale_fill_manual(values = c("Recall" = "blue", "Precision" = "green")) +
  theme_minimal()







install.packages("class")
library(class)

num_foldsff <- 10

accuracy_valuesff <- numeric(num_foldsff)
recall_valuesffzz <- matrix(0, nrow = num_foldsff, ncol = 3)  
precision_valuesffzz <- matrix(0, nrow = num_foldsff, ncol = 3) 


fold_featuresff <- c("Gender", "Age", "Occupation", "Sleep.Duration", "Quality.of.Sleep",
                     "Physical.Activity.Level","Stress.Level","BMI.Category", "Blood.Pressure", "Heart.Rate", "Daily.Steps")


for (fold in 1:num_foldsff) {
  set.seed(2)  
  fold_indicesff <- sample(nrow(SHL_Dataset_mod), nrow(SHL_Dataset_mod) * 0.1)  
  test_foldff <- SHL_Dataset_mod[fold_indicesff, ]
  train_foldff <- SHL_Dataset_mod[-fold_indicesff, ]
  
 
  train_fold_featuresff <- train_foldff[fold_featuresff]
  train_fold_labelsff <- train_foldff$Sleep.Disorder
  test_fold_featuresff <- test_foldff[fold_featuresff]
  test_fold_labelsff <- test_foldff$Sleep.Disorder
  
  k <- 5  
  fold_predicted_labelsff <- knn(train_fold_featuresff, test_fold_featuresff, train_fold_labelsff, k)
  View(fold_predicted_labelsff)
 
  accuracy_valuesff[fold] <- sum(fold_predicted_labelsff == test_fold_labelsff) / length(test_fold_labelsff)
  View(accuracy_valuesff)
  
  fold_conf_matrixff <- table(fold_predicted_labelsff, test_fold_labelsff)
  cat("Confusion Matrix -for 10 Fold :\n")
  print(fold_conf_matrixff)
  View(fold_conf_matrixff)
  
  
  ffrecall <- numeric(nrow(fold_conf_matrixff))
  ffprecision <- numeric(nrow(fold_conf_matrixff))
  
  for (i in 1:nrow(fold_conf_matrixff)) {
    ffrecall[i] <- fold_conf_matrixff[i, i] / sum(fold_conf_matrixff[i, ])
    ffprecision[i] <- fold_conf_matrixff[i, i] / sum(fold_conf_matrixff[, i])
  }
  
  recall_valuesffzz[fold, ] <- ffrecall
  precision_valuesffzz[fold, ] <- ffprecision
  
}

mean_accuracyff <- mean(accuracy_valuesff)
mean_recall_valuesffzz <- colMeans(recall_valuesffzz)
mean_precision_valuesffzz <- colMeans(precision_valuesffzz)

cat("Mean Accuracy with 10-Fold Cross-Validation:", mean_accuracyff * 100, "%\n")
print(mean_precision_valuesffzz * 10)
print(mean_recall_valuesffzz * 10)

ggplot(as.data.frame(as.table(fold_conf_matrixff)),
       aes(x = test_fold_labelsff, y = fold_predicted_labelsff, fill = Freq)) +
  geom_tile() +
  labs(title = "Confusion Matrix - 10-Fold Cross-Validation",
       x = "Predicted Labels", y = "Actual Labels") +
  scale_fill_gradient(low = "white", high = "green") +
  theme_minimal()


class_metrics_dfzz <- data.frame(Class = c("Class 1", "Class 2", "Class 3"),
                                Mean_Recall = mean_recall_valuesffzz*10,
                                Mean_Precision = mean_precision_valuesffzz*10)

# Reshape the data for plotting
class_metrics_dfzz_long <- tidyr::gather(class_metrics_dfzz, Metric, Value, Mean_Recall:Mean_Precision)

# Creating bar plots for mean recall and precision values for each class
ggplot(class_metrics_dfzz_long, aes(x = Class, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Recall and Precision Values for Each Class - 10-Fold Cross-Validation",
       x = "Class", y = "Value") +
  scale_fill_manual(values = c("Mean_Recall" = "blue", "Mean_Precision" = "green")) +
  theme_minimal()







