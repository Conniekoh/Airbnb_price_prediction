# Airbnb Price Prediction

![](https://cdn.pixabay.com/photo/2018/05/14/12/18/airbnb-3399753_960_720.jpg)

___
# Description
This project was completed by Connie Koh in partial fulfillment of APANPS5200: Applied Analytics Frameworksand Methods I, Spring 2021. This project is a listing of over 41,739 Airbnb rentals in New York City. The goal of this project is to predict the price for a rental using 97 variables on the property, host, and past reviews.
___
# Goal
To predict price of an Airbnb rental. Generate a prediction for each id in scoringData.csv.
___
# Metric
Evaluated based on RMSE (root mean squared error) score. Lower the RMSE score, better the model.
___
# Steps
Evaluated based on RMSE (root mean squared error) score. Lower the RMSE score, better the model.
1. Install library packages 
2. Read a dataset of 41,739 observations and 97 variables
3. Data Wrangling. Improved data quality by changing data structure and transforming variables, including missing data imputation, factor’s levels redefinition, training and testing dataset separation
4. Select significant variables using Exploratory Data Analysis (EDA), feature selection including lasso and Principal Components Regression (PCA)
5. Trained supervised machine learning models, including Linear Regression, Logistic Regression, Random Forest, Ranger, tuneRanger, and XGBoost; tuneRanger had the lowest RMSE score of 288.63

:file_folder: [See my module](https://github.com/Conniekoh/Airbnb_price_prediction/blob/main/codility/conniekoh_kaggle_comp_submssion_final.v2.r)
___
## Before Release
- [x] Finish my changes
- [x] this is a complete item
- [ ] this is an incomplete item
