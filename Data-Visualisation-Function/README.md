# Data-Visualisation-Function - dataviz
A function which takes the dataset as an argument and generates publication quality univariate (for each variable) and bivariate plots (for each variable pair : Discrete-Discrete,Continuous-Continuous,Discrete-Continuos,Time-Continuous) using ggplot2. Provides option to generate plots for user selected variables as well.
Dependencies : ggplot2,reshape2

Types of Univariate Plots : 

1. Continuous : Histogram, Density plot & Boxplot
2. Discrete : Barplot

Types of Bivariate Plots : 
1. Continuous-Continuous : Scatter plot with trend line, Correlation matrix & heatmap
2. Discrete-Continuous : Boxplot, Violin Plot
3. Discrete-Discrete : Jitter Plot
4. Time-Continuous : Line plot,Scatter plot

Arguments : 

1. df - Dataframe containing variables
2. exclude_cols - Character vector of column names to exclude;optional
3. datetime_cols - Character vector of columns to be considered as Date variables;optional
4. datetime_format - format if date variables are present;optional
5. corr - whether correlation matrix is required for continuous-continuos variables; default is TRUE
6. selected_plots - whether user will provide selection of variables for which plots will be generated; if true a dialog box will open asking user to enter a CSV with variables in the first column as X axis and variables in the second column as y axis. Default is FALSE. Note : This feature is useful when the dataframe has a large number of variables. The function, by default will produce plots for each variable pair. However, this can be time consuming and futile as the user may not require each variable pair to visualized. To overcome this problem, this feature lets the user select variables/relationhips for which the function will automatically generate suitable plots. A demo csv has been provided in the repository named 'required_plots.csv' (user needs to follow the same format and keep 1st column as x axis and 2nd column as y axis. The file should not have headers) In case the relationship of a particular variable needs to be visualized with all the other variables, a '.' can be used in the 2nd column (y axis) of the csv.
7. required_plots - selected variables as dataframe- same as previous feature;to be used if user wants to provide selection of variables as a dataframe;optional
8. univariate - Whether univariate plots are required; default is TRUE

Demo Function Call : 
df1=read.csv('superstores.csv')
dataviz(df=df1,exclude_cols=c('Country','Order.ID','Customer.ID','Customer.Name','Product.ID','Product.Name','Row.ID','Postal.Code'),datetime_format = '%m/%d/%Y',corr=T,required_plots = NULL,univariate = T,selected_plots=F)

Demo Output provided in repository;Directory : Plots


