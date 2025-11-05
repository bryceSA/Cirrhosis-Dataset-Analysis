# 🩺 Cirrhosis Stage Classification Using Tree-Based Methods

**Authors:** Bryce Anderson, Zoe Cruz  
**Date:** June 2024  
**Course:** Math 447 – Statistical Learning

---

## 🧠 Abstract

Cirrhosis is a chronic liver disease that significantly impacts liver function and can lead to liver failure.  
This study leverages data from the **Mayo Clinic trial on primary biliary cirrhosis (PBC)** conducted between 1974 and 1984.  

The goal is to develop a **classification model** to predict the stage of cirrhosis based on **16 predictor variables** — both categorical and quantitative. Using **Random Forest** and **Gradient Boosting** methods, we assessed classification performance and identified the most significant predictors.

- The **Random Forest** model achieved an overall accuracy of **61.45%**.  
- The **Gradient Boosting** model suffered from overfitting, achieving **50.06%** accuracy.  
- **Albumin** and **hepatomegaly** were the most influential predictors, while **sex** was found to be less important.

These results underscore the **complexity of predicting cirrhosis stages** and the need for larger, more diverse datasets to improve model reliability.

---

## 🩸 1. Introduction

Cirrhosis occurs when healthy liver cells are replaced by scar tissue (*fibrosis*), which prevents the liver from functioning properly and can lead to failure.  
We analyzed data from the **Mayo Clinic PBC trial** (1974–1984), covering patients with cirrhosis stages **1 through 4**.

### Objective
To develop a classification model that:
1. Accurately predicts cirrhosis stage based on 16 predictors.  
2. Identifies which variables are most influential.

This is an important classification problem — by identifying strong predictors, clinicians could potentially reduce unnecessary testing and improve early diagnosis.

### Predictor Variables
- **6 categorical variables** (e.g., hepatomegaly, edema, drug treatment)  
- **10 quantitative variables** (e.g., bilirubin, albumin, age, cholesterol)

We applied **Random Forest** and **Gradient Boosting** classification methods implemented in **R**.

---

## 🌳 2. Methods

### 2.1 Decision Tree

We began with a **Decision Tree** using the `rpart` library in R.  
The complexity parameter (`cp`) was set to **0.02** to prune the tree for better interpretability.

From the initial model, we observed **high variability** — suggesting this would be a difficult classification problem due to overlapping class boundaries.

![Figure 1: Decision Tree with cp = 0.02](figures/figure1_tree.png)

---

### 2.2 Random Forest

We next built a **Random Forest (RF)** model using **70% of the data for training** and **30% for validation**.  
Varying the train-test split between 50–80% had little effect on accuracy.

**Model parameters:**
- `mtry = 4` (≈ √p, where p = number of predictors)
- Out-of-bag (OOB) error rate: **55.44%**
- Tuned model OOB error (via `tuneRF`): **50.36%**
- Validation accuracy: **61.45%**

| Metric | Value |
|:--|:--|
| OOB Error | 55.44% |
| Tuned OOB Error | 50.36% |
| Validation Accuracy | 61.45% |

The RF model struggled particularly with **Stage 1** classifications (error rate ≈ 0.92), likely due to a **small sample size** (n = 21).


#### Variable Importance
We examined both **Mean Decrease in Gini** and **Mean Decrease in Accuracy**.  
Higher values indicate greater importance — removing these variables would degrade model performance.


Key takeaways:
- **Albumin** and **hepatomegaly** were highly influential.
- **Edema** and **Drug (treatment vs placebo)** were relatively unimportant and could be removed.

---

### 2.3 Gradient Boosting

We implemented a **Gradient Boosting** model to reduce bias and variance.  
However, it suffered from substantial **overfitting**, even after:
- Reducing interaction depth  
- Adjusting learning rate  
- Changing number of trees and folds  

**Best accuracy:** **50.06%**, ~10% lower than Random Forest.

Interestingly, **hepatomegaly** ranked low in importance under the boosting model.

---

## 🧩 3. Conclusion

Our models struggled to produce high classification accuracy for cirrhosis stage prediction.  
However, we gained valuable insight into key predictors of advanced cirrhosis.

**Findings:**
- **Sex** had little impact on cirrhosis stage.  
- **Hepatomegaly** and **albumin** were consistently strong indicators.  
- Small sample size and high variability limited predictive performance.  

Future work could explore:
- Larger or balanced datasets  
- Dimensionality reduction (feature selection)  
- Comparison of treatment vs placebo effects  

---

## 📄 Full Report

You can view or download the complete paper here:  
➡️ [**Project Document (Full Paper)**](447_Project.pdf)
