# COVID-19 Death Rate Prediction
#### Author: Kelly Li and Yana Xu
## What We Did
We made models to predict COVID-19 death rates in U.S. states. This project was for our data science class. We wanted to see which model works best so health departments could use it.

## Our Main Question
Which machine learning model is best at predicting COVID-19 death rates at the state level?

## Data We Used
- Population numbers from U.S. Census Bureau
- COVID-19 data (cases, deaths, etc.) from CDC
- Region info from GitHub

## What's in Our Dataset
Our file `final_data.csv` has these columns:
- `cases100k`: COVID cases per 100,000 people
- `deaths100k`: COVID deaths per 100,000 people (what we're trying to predict)
- `hosp100k`: Hospital admissions per 100,000 people
- `ICU_hosp`: ICU hospitalizations
- `vacc_pct`: Percent of people fully vaccinated
- `boost_pct`: Percent of people with boosters
- `total_boost`: Total booster shots given

> [!NOTE]
> When vaccination data was missing, we just put 0.

## How We Cleaned the Data
1. We **standardized** everything to rates per 100,000 people
2. We **filled in** missing vaccination data with 0
3. We **got rid of** variables that didn't vary or were too similar to others
4. We **grouped** data two ways:
   - **Monthly:** averaged each month
   - **Weekly:** averaged each week

## Models We Tried
We tested four different models to predict COVID deaths:

1. **Linear Regression (LM)**: The basic straight-line model
2. **K-Nearest Neighbors (KNN)**: Finds similar periods and copies their death rates
3. **Random Forest (RF)**: Combines lots of decision trees 
4. **XGBoost (XGB)**: A more complicated boosting model

We wanted to find which model was most accurate but also consistent.

## What We Found

### Model Performance

1. **Monthly data** worked better than weekly data for all models
2. **KNN model** was the most accurate:
   - Monthly KNN: RMSE = 0.57, R² = 0.87
   - Weekly KNN: RMSE = 0.89, R² = 0.73
3. **Random Forest** was almost as good:
   - Monthly RF: RMSE = 0.63, R² = 0.84
   - Weekly RF: RMSE = 0.90, R² = 0.72
4. **Linear Regression** and **XGBoost** didn't work as well:
   - Monthly LM: RMSE = 0.70, R² = 0.81
   - Weekly LM: RMSE = 0.96, R² = 0.68
   - Monthly XGB: RMSE = 0.73, R² = 0.79
   - Weekly XGB: RMSE = 1.05, R² = 0.62

## Why This Matters

### Why Monthly Data Worked Better
Monthly data probably worked better because:
- It smooths out weird reporting (like holiday backlogs)
- It has less noise
- It shows the real trends instead of just data collection issues

> [!IMPORTANT]
> Even though monthly forecasts make you wait longer, they're more reliable for planning.

### Why KNN Worked Best
KNN probably worked well because:
- Different states had COVID waves at different times
- So each state has "neighbor" states that already went through similar situations
- KNN just finds those similar situations and uses their death rates

### How This Could Be Used
- Health departments don't need fancy data science teams to use KNN
- It's easy to explain where the predictions come from
- Simple models with monthly data can predict COVID deaths pretty well

## Problems With Our Study

> [!WARNING]
> Keep these issues in mind:

1. We only used one year of training data
2. We didn't account for the delay between cases and deaths
3. We used 0 for missing vaccination data
4. We only looked at reported COVID deaths, not excess deaths

## What We'd Do Next Time
- Use more years of data
- Add time delays between variables
- Try models that can use data from multiple states
- Add movement data and variant info
- Look at excess deaths too

## How To Run Our Code

```r
# Load packages
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)

# Get data
covid_data <- read.csv("final_data.csv")

# Run models
source("covid_prediction_models.R")

# Make charts
source("visualization_scripts.R")
```

## Packages You Need
- R programming language
- These R packages:
  - `tidyverse` - For data cleaning and charts
  - `caret` - For running machine learning
  - `randomForest` - For Random Forest model
  - `xgboost` - For XGBoost model
  - `stats` - For linear regression and KNN

## License
MIT License
