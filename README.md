# Kaggle- Bosch Production Line Performance Competition
These are the scripts I used to create my submissions for the Kaggle [Bosch Production Line Performance Competition](https://www.kaggle.com/c/bosch-production-line-performance).  

This model finished 41st out of 1373 teams.

Note: I assembled these file several weeks after the end of the competition.  I did my best to clean everything up but some errors may still be present!

## Data
The goal of the competition was to predict which parts would be failures on the production based upon anonymized numerical, categorical and date data supplied by Bosch.  The data is publicly available [here](https://www.kaggle.com/c/bosch-production-line-performance/data).

The data is quite unbalanced with failures being a relatively rare event on the production line.  Therefore, models generated for the competition were evaluated using the [Matthews Correlation Coefficient]([Matthews Correlation Coefficient]) (MCC).


## Dependancies
The model was generated using R and Rstudio.  The following R packages were used:

* dplyr
* data.table
* xgboost
* Matrix
* caret
* rBayesianOptimization

## Content

The files contained this repo and their purpose in the generating the model are summarized below:

### Data Cleaning
  **1.compress_dates.R**- Bosch supplied individual dates for every categorical and numeric feature of every part.  However, the dates for these features are identical for each station a part passed through in production.  The exception to this is station 24 and 25 which have an entry and exit date for each part.  Therefore the date features for each part can be reduced to a single feature for each station except 24 and 25 which each have two features. By removing these duplicate dates ~1100 features can be reduced to 53.  This made a huge difference in memory usage and the speed of data manipulation!

  **2.dedupe_categoricals.R**- A number of the categorical features supplied by Bosch are duplicates.  This script searches through the categorical features and saves a list of the names of non-duplicate features. This was another huge gain in memory usage and speed!

  **3.process_categoricals.R**- This processes the non-duplicated categorical features.  The category codes are encoded as integers which is required for building the model using XGBoost.

  **4.process_numeric.R**- Processes the numerical features. Set NA values equal to zero and add an offset of +2 to true numeric values so that models can differentiate between the two.

### Feature Engineering

  **5.generate_leak_feature.R**- One of the biggest predictors for a part being a failure is if it entered the production line near another part that failed.  The training and test data are randomly split from data collected over a period of time. A part in the training data may be known to have failed.  We can than create features to label any parts in the test data that entered the production line near a known failure as being a higher risk of failure.  This was identified as a possible "data leak" in the competition.  See [here for more info](https://www.kaggle.com/mmueller/bosch-production-line-performance/road-2-0-4). This script extracts these leak features.

  **6.additional_features.R**- Features were generated related to the time the part was in production.  Namely how long the part spent in each of 3 separate production lines, total time in production and the derivative of the cumulative sum of part failures when the part entered production.

  **7.additional_features2.R**- Further features, largely based off comparing the features of each part to the features of parts before and after it in the production line.  These features are in the spirit of the leak by identifying if the part was on the line near a failed part.

### Model Building/Prediction

  **8.feature_importance_bayop.R**- The model was built using [XGBoost](https://github.com/dmlc/xgboost).  In order to try to find the best parameter tuning I tried using a Bayesian Optimizer.  Using the optimizer ended up being quite slow and didn't end up offering too much of a model improvement.

  **9.generate_model.R**- Created XGBoost predictive model using parameters from the Bayesian Optimizer.  The model is trained on 90% of the training data and cross validated on the remaining 10%.  The cross validation is especially important to determine the best threshold probability above which a part is predicted as a failure.  The threshold value is important in determining the final MCC score of the model.

  **10.prediction.R**- Apply the XGBoost model and threshold found during cross validation to the test data to generate the submission CSV file for Kaggle.
