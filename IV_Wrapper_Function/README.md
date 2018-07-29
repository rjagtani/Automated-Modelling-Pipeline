# IV_Wrapper_Function
Wrapper Function for Generating IVs in binary classification problems

Returns IVs for independent variables (variables other than target variable and unique key in the given dataframe) using the function smbinning. Takes care of all data pre and post processing.

List of arguments : 
1. df - Dataframe object
2. vars - character vector of column names for which IV needs to be generated
3. unique_key - Will not be altered in any way and will be preserved till the final stage
4. target_var  - name of the 'y' / target variable - 
5. maxcat  - same as maxcat in the smbinning function 


The function returns the following objects : 
1. Numerical Variables (as identified and converted by the function - a subset of 'vars' argument)
2. Categorical Variables (as identified and converted by the function - a subset of 'vars' argument)
3. IV Table - Cleaned IV table with feautres sorted in decreasing order of IV value - i.e the variable with the highest classification power comes first.
4. Dataset - The dataset used for generating IVs  - for future reference



