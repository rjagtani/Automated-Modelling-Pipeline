# Automated-Modelling-Pipeline - v1

Collection of modules built to automate standard data mining tasks such as Data Profiling, Data Visualization, Data Imputation, Model Building (using CARET) and Model Evaluation. These modules can be used independently or in the aforementioned sequence to reduce turnaround time for data science projects. This is an ongoing effort and has ample scope for improvement/additions. The pipeline bit (which will connect these modules) is still WIP and will be released soon. Looking forward to any suggestions/contributions. 

Improvements/Additions Planned : 

1. Data Visualisation : Second Version to include ability to do dplyr-style manipulations to the variables before a plot (as one may not want to plot the variables as is). The function will then pick up the resultant columns and their datatypes and generate suitable plots.
2. Data Profiling : Based on user feedback, v2.0 may include an xlsx with numerical,categorical and datetime variables in separate tabs instead of csvs. This will reduce the hassle of dealing with 3 files - one for each datatype
3. Model Evaluation : v2 to include model evaluation metrics for 'multiclass classification' and 'regression' models. Presently only available for binary classification
4. Imputation Method : v2 to include supervised imputation methods such as RandomForest and KNN. Also, there are many bugs in the module that need to be fixed due to a high number of dependencies and lack of user testing. 
5. Caret Model Scripts : These are just wrappers around caret which make Cross Validation and Model Tuning easy and saves the user from reinventing the wheel each time for a new project. Current options include Random Forest, GBM and GLMNet. Other commonly used methods (such as XGBoost,SVM will be added in v2)

