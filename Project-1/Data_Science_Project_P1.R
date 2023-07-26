

# Converting Excel to CSV -----------------------------------------------------------------------------
install.packages("readxl")
install.packages("writexl")
library(readxl)
library(writexl)
excel_file <- read_excel("F:/Data Science/Titanic - Modified.xlsx")
write.csv(excel_file, "F:/Data Science/Titanic - Modified.csv")
Dataset_Base <- read.csv("F:/Data Science/Titanic - Modified.csv",header = TRUE,sep = ",")
Dataset_Base
View(Dataset_Base)


# Data Exploration ------------------------------------------------------------------------------------

# column names of data_set ----------------------------------------------------------------------------
Dataset_Prac <- read.csv("F:/Data Science/Titanic - Modified.csv",header = TRUE,sep = ",")
names(Dataset_Prac)

#Attribute types of dataset----------------------------------------------------------------------------
attributes <- names(Dataset_Base) 
dataTypes <- c(typeof(Dataset_Base$X), typeof(Dataset_Base$gender), typeof(Dataset_Base$age),
              typeof(Dataset_Base$sibsp), typeof(Dataset_Base$parch), typeof(Dataset_Base$fare), 
              typeof(Dataset_Base$embarked), typeof(Dataset_Base$class), typeof(Dataset_Base$who),
              typeof(Dataset_Base$alone), typeof(Dataset_Base$survived)) 
data.frame(attributes, dataTypes)


#Annotating one column name at a time of Data_set -----------------------------------------------------
names(Dataset_Prac)[4]<-"No of siblings"
View(Dataset_Prac)

# Annotate all column names at once of data set--------------------------------------------------------
colnames(Dataset_Prac) <- c("X","gender", "age", "No of siblings", 
                            "no of parents/children aboard", "pass fare", "port embarkation", 
                            "ticket class", "who(man/women/child)", "pass was alone or not?", 
                            "survived or not")
View(Dataset_Prac)

# Annotate values -------------------------------------------------------------------------------------
Dataset_Prac$gender<- factor(Dataset_Prac$gender,levels=c(0,1),labels=c("male","female"))
View(Dataset_Prac)

#Structure summery of data set-------------------------------------------------------------------------
str(Dataset_Prac)

#Descriptive Statistics Using summary function---------------------------------------------------------
summary(Dataset_Prac)

#Multiple column Standard deviation--------------------------------------------------------------------
library(dplyr)
dataset_sd <- Dataset_Prac %>% summarise_if(is.numeric,sd)
View(dataset_sd)

#Row wise standard deviation---------------------------------------------------------------------------
library(matrixStats)
library(dplyr)
Dataset_Prac$SD_of_3_4=rowSds(as.matrix(Dataset_Prac[,c(3,4)]))
Dataset_Prac$SD_of_4_5=rowSds(as.matrix(Dataset_Prac[,c(4,5)]))
View(Dataset_Prac)

#Taking random N rows and viewing a single column and its SD-------------------------------------------
random_sample <- sample_n(Dataset_Prac,10)
random_fare_sd <- random_sample$`pass fare`
sd(random_fare_sd)
View(random_sample)

View(Dataset_Prac)

#Counting Null values in each column-------------------------------------------------------------------
colSums(is.na(Dataset_Prac))

# Outlier detection with missing values----------------------------------------------------------------
#Creating frequency table for categorical values-------------------------------------------------------
# Create a frequency table for the "gender" variable
gender_freq <- table(Dataset_Prac$gender)
gender_freq

# Create a frequency table for the "who(man/women/child)" variable-------------------------------------
who_freq <- table(Dataset_Prac$`who(man/women/child)`)
who_freq

# Create a frequency table for the "pass was alone or not?" variable-----------------------------------
alone_freq <- table(Dataset_Prac$`pass was alone or not?`)
alone_freq

# Create a frequency table for the "port embarkation" variable-----------------------------------------
embarked_port_freq <- table(Dataset_Prac$`port embarkation`)
embarked_port_freq

# Create a frequency table for the "ticket class" variable---------------------------------------------
ticket_class_freq <- table(Dataset_Prac$`ticket class`)
ticket_class_freq

View(gender_freq)
View(who_freq)
View(embarked_port_freq)
View(ticket_class_freq)
View(alone_freq)

#visualization using box plot , scatter plot, histogram to check outliers-------------------------------
#outlier may appear as points far away from majority----------------------------------------------------

# Box plot
age_plot <- boxplot(Dataset_Prac$age)
View(age_plot)

# Scatter plot
plot(Dataset_Prac$age)
plot(Dataset_Prac$`pass fare`) 

# Histogram
hist(Dataset_Prac$age)
hist(Dataset_Prac$`No of siblings`)
hist(Dataset_Prac$`no of parents/children aboard`)
hist(Dataset_Prac$`pass fare`)
hist(Dataset_Prac$`survived or not`)


#Handling outlier in "age"------------------------------------------------------------------------------
#removing the rows with age more than 100
age_outlier_dataset<-Dataset_Prac
age_outlier_dataset <- subset(age_outlier_dataset, age <= 100)
plot(age_outlier_dataset$age)
boxplot(age_outlier_dataset$age)

#Replacing more than 100 values with the mean value of age
age_replace <- Dataset_Prac
age_replace$age[age_replace$age > 100] <- mean(age_replace$age)
View(age_replace)
plot(age_replace$age)

#Handling outlier in "pass was alone or not"------------------------------------------------------------
library(dplyr)
# Convert inconsistent values to "FALSE" and "TRUE"
pass_alone_outlier <- age_replace %>% mutate(`pass was alone or not?` = ifelse(`pass was alone or not?` == "FALL", FALSE,
                                                                               ifelse(`pass was alone or not?` == "FALSE", FALSE,
                                            ifelse(`pass was alone or not?` == "TRUE", TRUE, `pass was alone or not?`))))
View(pass_alone_outlier)
pass_alone_outlier_freq <- table(pass_alone_outlier$`pass was alone or not?`)
pass_alone_outlier_freq


#Handling missing values---------------------------------------------------------------------------------
#For age variable
age_missing_1 <- Dataset_Prac
library(dplyr)
# Replace missing values in 'age' with the mean----------------------------------------------------------
age_missing_1$age <- ifelse(is.na(age_missing_1$age), mean(age_missing_1$age, na.rm = TRUE), age_missing_1$age)
View(age_missing_1)
colSums(is.na(age_missing_1))

gender_missing_1 <- Dataset_Prac
library(dplyr)
which(is.na(Dataset_Prac$gender))
# Replace missing values in 'gender' with the max freq value---------------------------------------------
mode_gender <- names(which.max(table(gender_missing_1$gender)))
gender_missing_1$gender[is.na(gender_missing_1$gender)] <- mode_gender
View(gender_missing_1)
colSums(is.na(gender_missing_1))

# handle missing values in by deleting the missing row---------------------------------------------------
missing_class_remove_1 <- Dataset_Prac
missing_class_remove_out<-na.omit(missing_class_remove_1)
View(missing_class_remove_out)
colSums(is.na(missing_class_remove_out))

# Replace missing values in 'port embarkation' with the mode---------------------------------------------
missing_embark_1 <- Dataset_Prac
mode_port_embarkation <- names(which.max(table(missing_embark_1$`port embarkation`)))
missing_embark_1$`port embarkation`[is.na(missing_embark_1$`port embarkation`)] <- mode_port_embarkation
View(missing_embark_1)
which(is.na(missing_embark_1$`port embarkation`))


#In main dataset, Handling missing values using most freq/max,mean values--------------------------------

#for age
Dataset_Prac$age <- ifelse(is.na(Dataset_Prac$age), mean(Dataset_Prac$age, na.rm = TRUE), Dataset_Prac$age)
View(Dataset_Prac)

#for gender
Dataset_Prac$gender[is.na(Dataset_Prac$gender)] <- mode_gender

#for ticket class
tic_class <- names(which.max(table(Dataset_Prac$`ticket class`)))
Dataset_Prac$`ticket class`[is.na(Dataset_Prac$`ticket class`)] <- tic_class

#for port embarkation
Dataset_Prac$`port embarkation`[is.na(Dataset_Prac$`port embarkation`)] <- mode_port_embarkation

#Deleting attribute with missing value
Dataset_Prac <- subset(Dataset_Prac, select = -SD_of_3_4)
Dataset_Prac <- subset(Dataset_Prac, select = -SD_of_4_5)

#Creating new Standard deviation for column 3,4
Dataset_Prac$sd_of_3n4=rowSds(as.matrix(Dataset_Prac[,c(3,4)]))
Dataset_Prac$sd_of_4n5=rowSds(as.matrix(Dataset_Prac[,c(4,5)]))

colSums(is.na(Dataset_Prac))


#In main Dataset, Handling previously found outlier without missing values------------------------------

#for age
#replacing more than 100 years values with age mean value
Dataset_Prac$age[Dataset_Prac$age > 100] <- mean(Dataset_Prac$age)
plot(Dataset_Prac$age)

#for pass was alone or not
# Convert inconsistent values to "FALSE" and "TRUE"
Dataset_Prac <- Dataset_Prac %>% mutate(`pass was alone or not?` = ifelse(`pass was alone or not?` == "FALL", FALSE,
                                                                               ifelse(`pass was alone or not?` == "FALSE", FALSE,
                                                                                      ifelse(`pass was alone or not?` == "TRUE", TRUE, `pass was alone or not?`))))
Dataset_Prac_freq <- table(Dataset_Prac$`pass was alone or not?`)
Dataset_Prac_freq

#Getting rid of noisy values-----------------------------------------------------------------------------
#Round the values in 'pass fare' to the nearest whole number
Dataset_Prac$`pass fare` <- round(Dataset_Prac$`pass fare`)

#Round the values in 'sd_of_3n4' and "sd_of_4_5" up to two decimal place number
Dataset_Prac$sd_of_3n4 <- round(Dataset_Prac$sd_of_3n4, 2)
Dataset_Prac$sd_of_4n5 <- round(Dataset_Prac$sd_of_4n5, 2)
Dataset_Prac$age <- round(Dataset_Prac$age, 2)
View(Dataset_Prac)

#specific column
Dataset_Prac$`pass fare`
Dataset_Prac$sd_of_3n4
Dataset_Prac$sd_of_4n5
selected_variable <- Dataset_Prac[c("age","pass fare","sd_of_3n4", "sd_of_4n5")]
selected_variable


library(dplyr)
# Calculating the standard deviation of numeric columns
main_sd <- Dataset_Prac %>% summarise_if(is.numeric, sd)
main_sd_values <- unlist(main_sd)
# Plotting histogram of standard deviation values
barplot(main_sd_values, main = "Histogram of Standard Deviations", xlab = "Columns", ylab = "Standard Deviation")



# Separating numeric and categorical columns to find mean ,mode and median of all columns--------
numericcols <- Dataset_Prac[, sapply(Dataset_Prac, is.numeric)]
categoricalcols <- Dataset_Prac[, !sapply(Dataset_Prac, is.numeric)]

# Calculating mean
meanvalues_numeric <- colMeans(numericcols)
meanvalues_categorical <- sapply(categoricalcols, function(x) length(unique(x)) / length(x))

# Calculating mode 
modevalues_categorical <- sapply(categoricalcols, function(x) {
  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]})

modevalues_numeric <- sapply(numericcols, function(x) {
  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]})

# Calculating median
medianvalues_numeric <- sapply(numericcols, median, na.rm = TRUE)

# Combining values for all columns
fmeanvalues <- c(meanvalues_categorical, meanvalues_numeric)
fmodevalues <- c(modevalues_categorical, modevalues_numeric)
fmodevalues <- as.numeric(fmodevalues)

# Plot a histogram of the mode and median values
barplot(fmeanvalues, main = "Histogram of Mean", xlab = "Columns", ylab = "Value")
barplot(fmodevalues, main = "Histogram of Mode", xlab = "Columns", ylab = "Value")
barplot(medianvalues_numeric, main = "Histogram of Median", xlab = "Columns", ylab = "Value")


