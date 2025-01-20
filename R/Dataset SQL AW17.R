library(readxl)
library(readxl)

DataSet_SQL_Massive_Data_Analysis <- read_excel("C:/Users/manus/Desktop/Massive Data Analysis/AssignmentsDataSet SQL Massive Data Analysis.xlsx")

TotalSales <- read_excel("C:/Users/manus/Desktop/Massive Data Analysis/Assignments/DataSet SQL Massive Data Analysis.xlsx", 
                         sheet = "ST Total Sales")
BikePurchase <- read_excel("C:/Users/manus/Desktop/Massive Data Analysis/Assignments/DataSet SQL Massive Data Analysis.xlsx", 
                           sheet = "Discrete Var Bike Purchase")

ClientSpending <- read_excel("C:/Users/manus/Desktop/Massive Data Analysis/Assignments/DataSet SQL Massive Data Analysis.xlsx", 
                             sheet = "Continuous Var Client Spending")

Orders <- read_excel("C:/Users/manus/Desktop/Massive Data Analysis/Assignments/DataSet SQL Massive Data Analysis.xlsx", 
                     sheet = "Data Without Labels (Unsupervised)")


# QUESTION 1:
# What type of variables does my dataset contain?
# Perform a descriptive analysis of the variables in the dataset
# (descriptive statistics, correlation)

str(BikePurchase)
str(ClientSpending)
str(Orders)
str(TotalSales)
summary(BikePurchase)
summary(ClientSpending)
summary(Orders)
summary(TotalSales)
sum(is.na(BikePurchase))
sum(is.na(ClientSpending))
sum(is.na(Orders))
sum(is.na(TotalSales))

# The dataset consists of 4 sheets with data.
# The "BikePurchase" sheet contains variables related to bike purchases and client characteristics, 
# a mix of numeric, date, and character variables, with 19 variables and 18,484 observations. 
# There are no missing values in this sheet.
# The "ClientSpending" sheet contains variables related to client spending, 
# also a mix of numeric, date, and character variables, with 18 variables and 1,848 observations, and no missing values.
# In the "Orders" and "TotalSales" sheets, we find variables with incorrect formats, 
# so we will transform them first.

Orders$Size <- as.numeric(Orders$Size)
Orders$Weight <- as.numeric(Orders$Weight)

TotalSales$Sales...3 <- as.numeric(TotalSales$Sales...3)
TotalSales$Sales...4 <- as.numeric(TotalSales$Sales...4)
TotalSales$Sales...5 <- as.numeric(TotalSales$Sales...5)

# Verify that the values have been converted to numeric
str(Orders)
str(TotalSales)

# Check for missing values in these sheets
sum(is.na(Orders))
sum(is.na(TotalSales))

# The "Orders" sheet contains variables related to orders and their characteristics, 
# with numeric and character variables, 16 variables, and 121,317 observations, 
# with 136,461 missing values in the "Size" and "Weight" variables.
# The "TotalSales" sheet contains variables related to total sales, 
# with date and numeric formats, 5 variables, and 1,124 observations, 
# with a total of 231 missing values.

# QUESTION 2:
# Could you estimate a model to classify whether an individual will make a purchase or not? 
# Use a logistic regression model and a decision tree.
# (use the "Discrete Var Bike Purchase" sheet)

# Split the dataset
library(caTools)

split <- sample.split(BikePurchase$BikePurchase, SplitRatio = 0.8)
View(split)

training <- subset(BikePurchase, split == TRUE)
validation <- subset(BikePurchase, split == FALSE)

# Check if the split is balanced
prop.table(table(BikePurchase$BikePurchase))
prop.table(table(training$BikePurchase))
prop.table(table(validation$BikePurchase))

# Confirming this, we create models using the training data
# Divide the ages into groups

training$AgeGroup <- cut(training$Age, 
                         breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                         labels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                         right = FALSE)

validation$AgeGroup <- cut(validation$Age, 
                           breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                           labels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                           right = FALSE)

logit_model_training <- glm(BikePurchase ~ Country + 
                              as.factor(AgeGroup) +
                              as.factor(MaritalStatus) +
                              as.factor(YearlyIncome) +
                              as.factor(Gender) +
                              as.factor(TotalChildren)+
                              as.factor(Education)+
                              as.factor(Occupation)+
                              as.factor(NumberCarsOwned),
                            data = training,
                            family = "binomial")
summary(logit_model_training)

# Make predictions
prediction_training <- predict(logit_model_training, type = "response")

# Encode the predictions
prediction_training_encoded <- ifelse(prediction_training > 0.5, 1, 0)

length(training$BikePurchase)
length(prediction_training_encoded)

sum(is.na(training))
training <- na.omit(training)
# Omitted missing values as the number of observations was not the same.

# Make predictions
prediction_validation <- predict(logit_model_training, newdata = validation, type = "response")

# Encode predictions
prediction_validation_encoded <- ifelse(prediction_validation > 0.5, 1, 0)

########## DECISION TREE ###########
library(rpart)
library(rpart.plot)
decision_tree_model <- rpart(BikePurchase ~ Country + 
                               as.factor(AgeGroup) +
                               as.factor(MaritalStatus) +
                               as.factor(YearlyIncome) +
                               as.factor(Gender) +
                               as.factor(TotalChildren)+
                               as.factor(Education)+
                               as.factor(Occupation)+
                               as.factor(NumberCarsOwned),
                             data = training,
                             method = "class")

rpart.plot(decision_tree_model)
summary(decision_tree_model)

# Make predictions
tree_prediction_training <- predict(decision_tree_model, type = "class")

## Now with the validation data
# Make predictions
tree_prediction_validation <- predict(decision_tree_model, newdata = validation, type = "class")

# QUESTION 3:
# How good are the models? 
# What are the most important variables when it comes to predicting a client's purchase? 
# (use the "Discrete Var Bike Purchase" sheet)

# Calculate confusion matrix
library(caret)
confusionMatrix(as.factor(training$BikePurchase), as.factor(prediction_training_encoded))

# Calculate confusion matrix
confusionMatrix(as.factor(validation$BikePurchase), as.factor(prediction_validation_encoded))

####### Logistic Model Results #####
# The accuracy of the logistic model is 65.11%, with 64.79% accuracy for non-purchasing clients 
# and 65.44% for purchasing clients. 
# The model's accuracy is low.

# Calculate confusion matrix
confusionMatrix(as.factor(training$BikePurchase), as.factor(tree_prediction_training))

# Calculate confusion matrix
confusionMatrix(as.factor(validation$BikePurchase), as.factor(tree_prediction_validation))

#### Decision Tree Results #####
# The model's accuracy is 64.56%, very similar to the logistic model, 
# with 70.59% for non-purchasing clients and 58.38% for purchasing clients.
# Accuracy remains low.

summary(logit_model_training)

summary(decision_tree_model)

## For the logistic regression model, the variables with the highest importance include different categories of 
# Country, AgeGroup, YearlyIncome, TotalChildren, Education, Occupation, and NumberCarsOwned. These variables show statistical significance and substantial coefficients.

# In the decision tree model, the most important variables are NumberCarsOwned, AgeGroup, TotalChildren, Country,
# Education, Occupation, and YearlyIncome. The importance of these variables stems from their ability to reduce impurity in the tree nodes.

# Both models agree on the importance of several variables, although the way their importance is evaluated differs between the methods.


# QUESTION 4:
# With the complete dataset, applying unsupervised learning techniques,
# would you be able to define different customer typologies? 
# You can use the variables you consider appropriate
# (use the tab Var Discreta Adq Bicicleta)


# RESPONSE 4
# In unsupervised learning, the data has no labels and
# models are used to find internal or latent relationships.
# There are no dependent variables; all variables are independent,
# and it is used for clustering or grouping on one side,
# or for dimensionality reduction or data simplification.

# Clustering: grouping observations that have a high degree of similarity.

AdqBicicleta <- read_excel("C:/Users/Manus/Desktop/Massive Data Analysis/Assignments/DataSet SQL Massive Data Analysis.xlsx", 
                           sheet = "Var Discreta Adq Bicicleta")

# Analyze the structure again
str(AdqBicicleta)
summary(AdqBicicleta)


# Visual inspection is always interesting,
# but it's not possible because there are non-numeric variables.


# Use the k-means algorithm, which only works with numeric data,
# so I will create a dataset with only numeric data (remove non-numeric columns):

AdqBicicletanumerico1 <- AdqBicicleta[,-(4:5)]
AdqBicicletanumerico2 <- AdqBicicletanumerico1[,-4]
AdqBicicletanumerico3 <- AdqBicicletanumerico2[,-(5:7)]
AdqBicicletanumerico4 <- AdqBicicletanumerico3[,-(6:8)]
AdqBicicletanumerico5 <- AdqBicicletanumerico4[,-(7:8)]

str(AdqBicicletanumerico5)
# Now I have the dataset with all numeric variables.


# Now let's use the k-means algorithm, which only works with numeric data.
# This algorithm is used for clustering; let's divide the dataset into 2 clusters.

kmeans(AdqBicicletanumerico5,2)
kmeans(AdqBicicletanumerico5,4)
kmeans(AdqBicicletanumerico5,5)
kmeans(AdqBicicletanumerico5,10)
# K-means is sensitive to outliers, so they must be removed.
# To make sense of the clustering, as the number of clusters increases, the metric improves.
# So we choose the division with the highest metric: 
# We select 10 clusters with a metric of 83.7%.

# However, it is necessary to find "something" that indicates the optimal number of clusters.
# There are libraries to find the optimal number of clusters.
# One such library is factoextra.
install.packages("factoextra")
library(factoextra)

# This library allows us to identify the optimal number of clusters.
fviz_nbclust(AdqBicicletanumerico5, kmeans)
# When we apply k-means, the metric displayed is the Within Cluster Sum of Squares (WSS).
fviz_nbclust(AdqBicicletanumerico5, kmeans, method = "wss")
# The resulting graph allows us to identify the optimal number of clusters using the elbow method.

# There are other libraries to optimize the number of clusters, such as NbClust.
install.packages("NbClust")
library(NbClust)
NbClust(AdqBicicletanumerico5, min.nc = 2, max.nc = 15, method = "kmeans")

# QUESTION 5:
# Predict total sales 
# (use the tab ST Ventas Totales, the first column) 
# for the next 2 months.


# Import the dataset: DataSet SQL Analisis Masivo de Datos.xlsx and sheet ST Ventas Totales #
library(readxl)
DataSet_SQL_Analisis_Masivo_de_Datos <- read_excel("C:/Users/Manus/Desktop/Massive Data Analysis/Assignments/DataSet SQL Massive Data Analysis.xlsx", 
                                                   sheet = "ST Ventas Totales ")
# View the table
View(DataSet_SQL_Analisis_Masivo_de_Datos)

# Create a copy of the data as always:
datos <- DataSet_SQL_Analisis_Masivo_de_Datos

# Get familiar with the structure of the data:
str(datos)
summary(datos)

sum(is.na(datos$Sales...2)) # Are there empty cells in column 2? = NO
sum(is.na(datos$Sales...3))  # Are there empty cells in column 3? = NO (R error)

# For R, as long as there is "something" in the cell (in our case NULL), it considers it not empty.
# Since columns 3, 4, and 5 are numeric, we could give them numeric format:
datos$Sales...3 <- as.numeric(datos$Sales...3)
sum(is.na(datos$Sales...3))

# We can also give numeric format to columns 4 and 5 (NULL becomes NA).
datos[,4:5] <- sapply(datos[,4:5], as.numeric)
summary(datos)

# As always, visual inspection is a good tool:
plot(datos$Sales...2)
# Remember that for time series we must format it or 
# if we want to plot them
plot.ts(datos$Sales...2)

# Visual analysis shows that there is a seasonality pattern.
# Starting from row 1093, there is a downward step.
# This could be either because it's natural or due to data extraction errors.
# We opt for the second option and decide to remove the data from row 1093 onwards.
# Keep only the data up to row 1093.
datos1 <- datos[1:1093,]
plot.ts(datos1$Sales...2)

# When we have "dirty data/strange patterns," one option is to group by dates.
# Grouping by dates can help "clean the series."
# To clean, we will use the lubridate library.
install.packages("lubridate")
library(lubridate)

# This library allows grouping by dates (weeks, months, quarters, years).
# How does it work?
# First, we create columns for weeks, months, quarters, and years,
# although we only need the months.
datos1$semana <- floor_date(datos1$OrderDate, "week")
datos1$mes <- floor_date(datos1$OrderDate, "month")
datos1$trimestre <- floor_date(datos1$OrderDate, "quarter")
datos1$año <- floor_date(datos1$OrderDate, "year")

# Now we can group (group by) by weeks, months, etc.
# For this, we call the dplyr library.

install.packages("dplyr")
library(dplyr)
# Calculate monthly sales

ventasmensuales <- datos1 %>%
  group_by(mes) %>%
  summarise(ventasmes = sum(Sales...2))

plot.ts(ventasmensuales$ventasmes)

# If we want to make a prediction and the series is "noisy,"
# we can sacrifice accuracy by changing the frequency and working with the trend component.
# We can work with monthly data (in our case ventasmensuales).
plot.ts(ventasmensuales$ventasmes)
# Observing the series, it is evident that even with monthly frequency, the series is quite noisy.
# One option is to smooth the series by working with the trend component (smooth the series).
ventasmensuales <- ts(ventasmensuales$ventasmes, start = c(2011, 5), frequency = 12)
plot(ventasmensuales)
# In topic 4, to extract the trend component, we use the decompose command.
decompose(ventasmensuales)
plot(decompose(ventasmensuales))
# The problem with the decompose command is that extracting the trend (trend) loses observations on both sides.
# There are other libraries that allow us to avoid losing observations,
# such as the mfilter library.

install.packages("mFilter")
library(mFilter)
# This library works as follows:
modelotendencia <- hpfilter(ventasmensuales, freq = 2)
# Now we extract the trend (without losing observations).
tendenciaventasmensuales <- modelotendencia$trend
plot(ventasmensuales)
lines(tendenciaventasmensuales, col = "red") # Smooth the series.
# Let's compare this trend with the one from decompose.
modelodecompose <- decompose(ventasmensuales)
# Extract trend from decompose.
tendenciaventasmensuales_decompose <- modelodecompose$trend
plot(ventasmensuales)
lines(tendenciaventasmensuales_decompose, col = "blue")

















