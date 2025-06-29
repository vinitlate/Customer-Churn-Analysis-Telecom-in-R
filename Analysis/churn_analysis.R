
# Churn Analysis in Telecom Dataset
# ---------------------------------
# Authors: Vinit Late & Sahil Bora
# Project: Intermediate Statistical Modelling for Business Final Project
# Description: This R script performs data preprocessing, EDA, regression modeling, PCA, and clustering analysis to study customer churn.

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
data <- Customer_Data

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
# Churn Distribution with Custom Colors
ggplot(data, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1)), vjust = -0.5) +
  labs(title = "Churn Distribution", x = "Churn Status", y = "Count") +
  scale_fill_manual(values = c("No" = "#F8766D", "Yes" = "#00BFC4")) +
  theme_minimal()

# Tenure Distribution with Custom Colors
ggplot(data, aes(x = tenure)) +
  geom_histogram(binwidth = 5, fill = "#00bfc4", color = "#f8766d") +
  labs(title = "Distribution of Tenure", x = "Tenure (Months)", y = "Frequency") +
  theme_minimal()


# Bar Chart: Internet Service and Churn by Gender
ggplot(data, aes(x = InternetService, fill = Churn)) +
  geom_bar(position = "dodge") +
  facet_wrap(~gender) +
  labs(title = "Internet Service and Churn by Gender",
       x = "Internet Service",
       y = "Count") +
  theme_minimal()

# Bar Chart: Dependents Distribution by Churn
ggplot(data, aes(x = Dependents, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Dependents Distribution by Churn",
       x = "Dependents",
       y = "Count") +
  theme_minimal()

# Bar Chart: Churn Distribution by Partner Status
ggplot(data, aes(x = Partner, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Churn Distribution by Partner Status",
       x = "Partner Status",
       y = "Count") +
  theme_minimal()

# Bar Chart: Senior Citizen Churn
ggplot(data, aes(x = SeniorCitizen, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Senior Citizen Churn",
       x = "Senior Citizen Status",
       y = "Count") +
  theme_minimal()

# Bar Chart: Online Security and Churn
ggplot(data, aes(x = OnlineSecurity, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Online Security and Churn",
       x = "Online Security",
       y = "Count") +
  theme_minimal()

# Bar Chart: Paperless Billing and Churn
ggplot(data, aes(x = PaperlessBilling, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Paperless Billing and Churn",
       x = "Paperless Billing",
       y = "Count") +
  theme_minimal()

# Bar Chart: Tech Support and Churn
ggplot(data, aes(x = TechSupport, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Tech Support and Churn",
       x = "Tech Support Availability",
       y = "Count") +
  theme_minimal()

# Bar Chart: Phone Service and Churn
ggplot(data, aes(x = PhoneService, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Phone Service and Churn",
       x = "Phone Service",
       y = "Count") +
  theme_minimal()

# Density Plot: Monthly Charges by Churn
ggplot(data, aes(x = MonthlyCharges, fill = Churn)) +
  geom_density(alpha = 0.6) +
  labs(title = "Monthly Charges by Churn",
       x = "Monthly Charges",
       y = "Density") +
  theme_minimal()

# Density Plot: Tenure by Churn
ggplot(data, aes(x = tenure, fill = Churn)) +
  geom_density(alpha = 0.6) +
  labs(title = "Tenure by Churn",
       x = "Tenure",
       y = "Density") +
  theme_minimal()

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
