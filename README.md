# 📊 Customer Churn Analysis in Telecom

This project investigates **customer churn** in a telecom dataset using a mix of statistical modeling and unsupervised learning techniques. It was developed as a final project for **ST635 - Applied Statistical Methods** at Bentley University by MSBA candidates **Vinit Late** and **Sahil Bora**.

---

## 🎯 Objectives

- Identify key drivers that influence **customer churn**
- Predict churn using **logistic regression**
- Understand revenue behavior using **multiple linear regression**
- Reduce data complexity with **Principal Component Analysis (PCA)**
- Segment customers with **K-Means Clustering** to inform retention strategy

---

## 🧠 Key Findings

- 📉 **Month-to-month contracts** and **Fiber Optic internet** are strong churn indicators  
- ⏳ Longer **tenure** reduces churn probability  
- 💰 **Internet service type** is the most significant predictor of Monthly Charges  
- 🔍 PCA captured **95% of variance** with just two components  
- 👥 K-Means revealed three actionable clusters:  
  - **High Spenders** – long tenure, high revenue, but highest churn  
  - **Discount Seekers** – stable customers with low charges  
  - **New Customers** – early-stage users with moderate churn  

---

## 📁 Project Structure
```
Customer-Churn-Analysis-Telecom-in-R/
├──Data/
│ └── Customer_Data.xlxs
├──Analysis/
│ └── churn_analysis.R
├──gold/
│ ├── Gold_Layer_Daily_Sales.py
│ ├── Gold_Layer_Daily_Sales_By_Category.py
│ ├── Gold_Layer_Customer_Summary.py
│ ├── Gold_Layer_Product_Performance.py
│ ├── Gold_Layer_Country_Sales.py
│ ├── Gold_Layer_Sales_Calendar.py
│ ├── Gold_Layer_Customers.py
│ ├── Gold_Layer_Products.py
│ └── Gold_Layer_Orders.py
├── README.md
```
---

## 🧰 Techniques Used

- **Multiple Linear Regression** (to model Monthly Charges)
- **Logistic Regression** (to predict churn)
- **PCA** (for dimensionality reduction)
- **K-Means Clustering** (for customer segmentation)
- **ROC Curve / AUC**, **VIF**, **Partial Residual Plots** for diagnostics

---

## 🛠️ Libraries

- `ggplot2`  
- `dplyr`  
- `car`  
- `caret`  
- `pROC`  
- `factoextra`  

---

## 📌 How to Run

1. Clone the repo  
2. Place your dataset in the `/data` folder (e.g., `customer_data.csv`)  
3. Open `scripts/churn_analysis.R` in RStudio  
4. Run the script step by step for full analysis and visualizations  

---

## 💡 Business Implications

This project doesn't just explain *who* is likely to churn—it shows **why**, and helps businesses act on it. From loyalty programs for new customers to pricing strategy for high spenders, the insights drive targeted, cost-effective retention efforts.

---

## 👥 Authors

- **Vinit Late** | MSBA 2025  
- **Sahil Bora** | MSBA 2025

