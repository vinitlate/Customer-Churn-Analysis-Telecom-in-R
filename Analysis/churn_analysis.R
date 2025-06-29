
# Churn Analysis in Telecom Dataset
# ---------------------------------
# Authors: Vinit Late & Sahil Bora
# Project: ST635 - Final Project
# Description: This R script performs data preprocessing, EDA, regression modeling,
# PCA, and clustering analysis to study customer churn.

# -------------------------------
# 📦 Load Required Libraries
# -------------------------------
library(ggplot2)
library(dplyr)
library(car)
library(caret)
library(pROC)

# -------------------------------
# 📂 Load and Prepare Data
# -------------------------------
data <- Customer_Data  # Replace with your actual data loading if needed

# Check structure and missing values
str(data)
colSums(is.na(data))

# Fix specific categorical responses
columns_to_fix <- c("OnlineSecurity", "OnlineBackup", "DeviceProtection", 
                    "TechSupport", "StreamingTV", "StreamingMovies")
data[columns_to_fix] <- lapply(data[columns_to_fix], function(x) {
  factor(ifelse(x == "No internet service", "No", as.character(x)))
})

# Convert categorical columns to factors
categorical_columns <- c("gender", "SeniorCitizen", "Partner", "Dependents", 
                         "PhoneService", "MultipleLines", "InternetService", 
                         "Contract", "PaperlessBilling", "PaymentMethod", "Churn")
data[categorical_columns] <- lapply(data[categorical_columns], as.factor)

# Drop rows with missing TotalCharges
data <- data[!is.na(data$TotalCharges), ]

# Drop invalid factors with fewer than 2 levels
invalid_factors <- sapply(data[, sapply(data, is.factor)], function(x) length(levels(x)) < 2)
data <- data[, !invalid_factors]

# -------------------------------
# 📊 Exploratory Data Analysis
# -------------------------------
# (Add your ggplot2 visualizations here, e.g., churn dist, tenure, etc.)

# -------------------------------
# 📈 Multiple Linear Regression
# -------------------------------
monthly_model <- lm(MonthlyCharges ~ tenure + InternetService + Contract + SeniorCitizen, data = data)
summary(monthly_model)
vif(monthly_model)
plot(monthly_model$fitted.values, resid(monthly_model), main = "Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")
qqPlot(monthly_model$residuals)

# -------------------------------
# 🤖 Logistic Regression
# -------------------------------
data$Churn <- as.factor(data$Churn)
data$Contract <- relevel(data$Contract, ref = "Month-to-month")
data$InternetService <- relevel(data$InternetService, ref = "DSL")

set.seed(123)
split <- createDataPartition(data$Churn, p = 0.7, list = FALSE)
train <- data[split, ]
test <- data[-split, ]

logit_model <- glm(Churn ~ tenure + MonthlyCharges + InternetService + Contract + SeniorCitizen + Dependents,
                   data = train, family = binomial)
summary(logit_model)
vif(logit_model)

test$pred_prob <- predict(logit_model, newdata = test, type = "response")
test$pred_class <- ifelse(test$pred_prob > 0.5, "Yes", "No")
confusionMatrix(as.factor(test$pred_class), test$Churn)

roc_obj <- roc(test$Churn, test$pred_prob)
plot(roc_obj, main = "ROC Curve")
cat("AUC:", auc(roc_obj), "\n")
print(exp(coef(logit_model)))  # Odds ratios
crPlots(logit_model)

# -------------------------------
# 🧬 Principal Component Analysis
# -------------------------------
num_data <- data[, sapply(data, is.numeric)]
scaled <- scale(num_data)
pca <- prcomp(scaled, center = TRUE, scale. = TRUE)
summary(pca)

pr.var <- pca$sdev^2
pve <- pr.var / sum(pr.var)
cum_pve <- cumsum(pve)

par(mfrow = c(1, 2))
plot(pve, type = "b", main = "Scree Plot", xlab = "PC", ylab = "Variance Explained")
plot(cum_pve, type = "b", main = "Cumulative Variance", xlab = "PC", ylab = "Cumulative Explained")

pc_df <- data.frame(pca$x[, 1:2], Churn = data$Churn)
ggplot(pc_df, aes(x = PC1, y = PC2, color = Churn)) +
  geom_point(alpha = 0.6) +
  labs(title = "PC1 vs PC2 by Churn")

# -------------------------------
# 🎯 K-Means Clustering
# -------------------------------
kdat <- num_data
outliers <- unique(unlist(lapply(apply(kdat, 2, sort, decreasing = TRUE, index.return = TRUE), function(x) head(x$ix, 5))))
kdat <- kdat[-outliers, ]

set.seed(123)
k3 <- kmeans(kdat, centers = 3, nstart = 20)
kdat$Cluster <- as.factor(k3$cluster)

# Add cluster names
kdat$ClusterName <- ifelse(kdat$Cluster == 1, "High Spenders",
                      ifelse(kdat$Cluster == 2, "Discount Seekers", "New Customers"))

# Link back to churn
kdat$RowID <- as.numeric(rownames(kdat))
data$RowID <- as.numeric(rownames(data))
kdat <- merge(kdat, data[, c("RowID", "Churn")], by = "RowID")

# Churn rate by cluster
churn_summary <- aggregate(as.numeric(kdat$Churn == "Yes"), by = list(Cluster = kdat$Cluster), mean)
colnames(churn_summary)[2] <- "ChurnPercentage"
churn_summary$ChurnPercentage <- churn_summary$ChurnPercentage * 100
churn_summary$ClusterName <- ifelse(churn_summary$Cluster == 1, "High Spenders",
                              ifelse(churn_summary$Cluster == 2, "Discount Seekers", "New Customers"))
print(churn_summary)

# Cluster churn plot
ggplot(churn_summary, aes(x = ClusterName, y = ChurnPercentage, fill = ClusterName)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  labs(title = "Churn Percentage by Cluster", x = "Customer Cluster", y = "Churn Percentage (%)") +
  theme_minimal()
