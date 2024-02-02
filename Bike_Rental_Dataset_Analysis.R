#install.packages("readxl")
library(readxl)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)


# load libraries and dataset

datab <- read_excel("1657875746_day.xlsx")
datab

# attribte datatype conversion
datab$dteday <-format(as.Date(datab$dteday, format="%d/%m/%Y"),"%Y")
datab$dteday

is.na(datab)  # missing values analysis


ggplot(data, aes(x = datab$mnth, y = datab$cnt)) +
  geom_line() +
  geom_point() +
  labs(title = "Year-wise Target Variable",
       x = "Year",
       y = "Target Variable") +
  theme_bw()
#-----------------------------------------------------------------------------



# Yearly Distribution
ggplot(data, aes(x = datab$dteday, y = datab$cnt)) +
  geom_line() +  
  geom_point() +
  labs(title = "Yearly Distribution",
       x = "Year",
       y = "Total No. of Bikes") +
  theme_bw()

#-----------------------------------------------------------------------------
# Boxplot for outlier detection
ggplot(data, aes(x = "", y = datab$cnt)) +
  geom_boxplot() +
  labs(title = "Box Plot of Variable",
       x = "",
       y = "Variable") 


#-------------------------------------------------------------------------------

# Correlation Calculation

data <- datab[-c(1,2)]
data


corr_mat <- round(cor(data),2)  
head(corr_mat)

dist <- as.dist((1-corr_mat)/2)

# hierarchical clustering the dist matrix
hc <- hclust(dist)
corr_mat <-corr_mat[hc$order, hc$order]

melted_corr_mat <- melt(corr_mat)
melted_corr_mat


ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) + 
geom_tile()

#----------------------------------------------------------------------




# Load the required libraries
library(randomForest)



target_variable <- "cnt"
predictors <- setdiff(names(data), target_variable)

# Split the data into training and testing sets
set.seed(123)  # Set a seed for fixing randomness
train_index <- sample(1:nrow(data), 0.75 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Random Forest model
rf_model <- randomForest(formula = as.formula(paste(target_variable, "~", paste(predictors, collapse = "+"))),
                         data = train_data,
                         ntree = 700)

# Predictions on the testing set
predictions <- predict(rf_model, newdata = test_data)

# Model performance
mean_squared_error <- mean((test_data[, target_variable] - predictions)^2)
mean_squared_error
print(paste("Mean Squared Error:", mean_squared_error))

r_squared <- 1 - (sum((test_data[, target_variable] - predictions)^2) / 
                    sum((test_data[, target_variable] - mean(test_data[, target_variable]))^2))
print(paste("R-squared:", r_squared))


