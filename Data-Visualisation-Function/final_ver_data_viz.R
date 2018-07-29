############################################# Loading Required Packages ############################################

library(ggplot2)
library(reshape2)

############################################# Univariate plots ####################################################

univariate_histogram=function(data,x,dir=getwd())
{
  a=ggplot(data,aes(get(x))) 
  a + geom_histogram(bins=20,color='black',fill='dodgerblue4') + xlab(x) + ylab('Frequency') + theme_minimal()
  ggsave(paste0(dir,'/Plots/Univariate/Histograms/',x,'.jpeg'))
}

univariate_density=function(data,x,dir=getwd())
{
  a=ggplot(data,aes(get(x))) 
  a + geom_density(color='black',fill='dodgerblue4') + xlab(x) + ylab('Density') + theme_minimal()
  ggsave(paste0(dir,'/Plots/Univariate/Density/',x,'.jpeg'))
}

univariate_boxplot=function(data,x,dir=getwd())
{
  a=ggplot(data,aes(y=get(x),x="")) 
  a + geom_boxplot(fill='dodgerblue4',color='black') + xlab('') + ylab(x) + theme_minimal()
  ggsave(paste0(dir,'/Plots/Univariate/Boxplots/',x,'.jpeg'))
}  

univariate_barplot=function(data,x,dir=getwd())
{
  a=ggplot(data,aes(get(x))) 
  a + geom_bar(color='black',fill='dodgerblue4') + xlab(x) + ylab('Frequency') + theme_minimal()
  ggsave(paste0(dir,'/Plots/Univariate/Barplots/',x,'.jpeg')) 
}

################################################ Bivariate plots ##############################################



bivariate_scatter=function(data,x,y,dir=getwd()) {
  b=ggplot(data,aes(get(x),get(y)))
  b + geom_smooth(method='lm') + geom_point(color='dodgerblue4') + xlab(x) + ylab(y) + theme_minimal()
  ggsave(paste0(dir,'/Plots/Bivariate/Continuous-Continuous/Scatter/',x,'/',x,'_',y,'.jpeg'))
}

bivariate_jitter=function(data,x,y,dir=getwd()) {
  b=ggplot(data,aes(get(x),get(y))) 
  b + geom_jitter(color='dodgerblue4') + xlab(x) + ylab(y) + theme_minimal()
  ggsave(paste0(dir,'/Plots/Bivariate/Discrete-Discrete/',x,'/',x,'_',y,'.jpeg'))  
}

bivariate_boxplot=function(data,x,y,dir=getwd()) {
  b=ggplot(data,aes(x=get(x),y=get(y)))
  b + geom_boxplot(fill='dodgerblue4',color='black') + xlab(x) + ylab(y) + theme_minimal()
  ggsave(paste0(dir,'/Plots/Bivariate/Discrete-Continuous/Boxplot/',x,'/',x,'_',y,'.jpeg'))
}


bivariate_violin=function(data,x,y,dir=getwd()) {
  b=ggplot(data,aes(x=get(x),y=get(y)))
  b + geom_violin(fill='dodgerblue4',color='black') + xlab(x) + ylab(y) + theme_minimal()
  ggsave(paste0(dir,'/Plots/Bivariate/Discrete-Continuous/Violin_Plot/',x,'/',x,'_',y,'.jpeg'))
}

timeseries_line=function(data,x,y,date_range,dir=getwd())
{
  ggplot(data,aes(get(x),get(y))) + scale_x_date(date_breaks = paste0(date_range,' days')) + geom_line(color='dodgerblue4') + xlab(x) + ylab(y) + theme_minimal()
  ggsave(paste0(dir,'/Plots/Bivariate/Time Series/Line/',x,'/',x,'_',y,'.jpeg'))
}

timeseries_scatter=function(data,x,y,date_range,dir=getwd())
{
  ggplot(data,aes(get(x),get(y))) + scale_x_date(date_breaks = paste0(date_range,' days')) + geom_point(color='dodgerblue4') + xlab(x) + ylab(y) + theme_minimal()
  ggsave(paste0(dir,'/Plots/Bivariate/Time Series/Scatter/',x,'/',x,'_',y,'.jpeg'))
}


correlation_heatmap=function(data,numerical_columns,dir=getwd())
{
  num_data=data[,numerical_columns]
  cormat=round(cor(num_data,use='pairwise.complete.obs'),2)
  melted_cormat <- melt(cormat)
  head(melted_cormat)
  ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") + xlab('Variable 2') + ylab('Variable 1') + theme_minimal()
  write.csv(cormat,paste0(dir,'/Plots/Bivariate/Continuous-Continuous/Correlation/corr_matrix.csv'),row.names = T)
  ggsave(paste0(dir,'/Plots/Bivariate/Continuous-Continuous/Correlation/corr_matrix.jpeg'))
}


scatter_all=function(data,numerical_columns,current_column,directory=getwd())
{
  other_num_cols=numerical_columns[!numerical_columns %in% current_column]
  if(length(other_num_cols)>0)
  {
    dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous/Scatter/',current_column))  
    for(j in 1:length(other_num_cols))
    {
      bivariate_scatter(data=data,x=current_column,y=other_num_cols[j],dir=directory)
    }
  }
}


violin_all=function(data,numerical_columns,current_column,directory=getwd())
{
  if(length(numerical_columns)>0)
  {
    dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Violin_Plot/',current_column))
    for(k in 1:length(numerical_columns))
    {
      bivariate_violin(data=data,x=current_column,y=numerical_columns[k],dir=directory)
    }
  }
}

boxplot_all=function(data,numerical_columns,current_column,directory=getwd())
{
  if(length(numerical_columns)>0)
  {
    dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Boxplot/',current_column))
    for(k in 1:length(numerical_columns))
    {
      bivariate_boxplot(data=data,x=current_column,y=numerical_columns[k],dir=directory)
    }
  }
}

timeseries_line_all=function(data,numerical_columns,current_column,dr=range_date_1,directory=getwd())
{
  dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Line/',current_column))
  for(j in 1:length(numerical_columns))
  {
    timeseries_line(data=data,x=current_column,y=numerical_columns[j],date_range=dr,dir=directory)
  }
}

timeseries_scatter_all=function(data,numerical_columns,current_column,dr=range_date_1,directory=getwd())
{
  dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Scatter/',current_column))
  for(j in 1:length(numerical_columns))
  {
    timeseries_scatter(data=data,x=current_column,y=numerical_columns[j],date_range=dr,dir=directory)
  }
}

jitter_all=function(data,categorical_columns,current_column,directory=getwd())
{
  other_cat_cols=categorical_columns[!categorical_columns %in% current_column]
  if(length(other_cat_cols)>0)
  { 
    dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Discrete/',current_column))
    for(j in 1:length(other_cat_cols))
    { 
      bivariate_jitter(data=data,x=current_column,y=other_cat_cols[j],dir=directory)
    }
  }
}

##################################### Main Function Body ###########################################################################

dataviz=function(df,exclude_cols=NULL,numerical_cols=NULL,categorical_cols=NULL,datetime_cols=NULL,datetime_format=NULL,max.cat=5,directory=getwd(),corr=TRUE,required_plots=NULL,selected_plots=F,univariate=TRUE)
{
  #df=df1,exclude_cols=NULL,numerical_cols=NULL,categorical_cols=NULL,datetime_cols=NULL,datetime_format=NULL,max.cat=5,directory=getwd(),corr=T,required_plots=NULL
  if(selected_plots==T)
  {  
     print('Choose file with selected variables')
     required_plots=read.csv(file.choose(),header=F)
  } 
  df=as.data.frame(df)
  if(length(numerical_cols)>0)
  {
    df[,numerical_cols]=lapply(as.data.frame(df[,numerical_cols]),function(x) as.numeric(as.character(x)))
  }
  if(length(categorical_cols)>0)
  {
    df[,categorical_cols]=lapply(as.data.frame(df[,categorical_cols]),as.character)
  }
  if(length(datetime_cols)>0 & !is.null(datetime_format))
  {
    df[,datetime_cols]=lapply(as.df.frame(df[,datetime_cols]),function(x) as.Date(as.character(x),format=datetime_format))
  }
  if(!is.null(datetime_format))
  {
    
    check_date_col=lapply(df,function(x) try(as.Date(x[!is.na(x)],format=datetime_format),silent = T))
    date_cols=names(unlist(lapply(check_date_col,function(x) if(class(x)!='try-error'){if(sum(!is.na(x))>0) {1}})))
    date_cols=date_cols[!date_cols %in% datetime_cols]
    df[,date_cols]=lapply(as.data.frame(df[,date_cols]),function(x) as.Date(as.character(x),format=datetime_format))
  } 
  
  #### Scatter plots 
  df[,exclude_cols]=NULL
  length_unique=function(x) length(unique(x[!is.na(x)]))
  num_cols=names(df)[which(unlist(lapply(df,class)) %in% c('numeric','integer'))]
  cat_cols=names(df)[which(unlist(lapply(df,class)) %in% c('factor','character'))]
  date_cols=names(df)[which(unlist(lapply(df,class))=='Date')]
  
  ### univariate - numeric 
  dir.create(paste0(directory,'/Plots'))
  if(univariate)
  {
    dir.create(paste0(directory,'/Plots/Univariate'))
    dir.create(paste0(directory,'/Plots/Univariate/Boxplots'))
    dir.create(paste0(directory,'/Plots/Univariate/Histograms'))
    dir.create(paste0(directory,'/Plots/Univariate/Density'))
    dir.create(paste0(directory,'/Plots/Univariate/Barplots'))
  }
  dir.create(paste0(directory,'/Plots/Bivariate'))
  dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous'))
  dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous/Scatter'))
  dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Discrete'))
  dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous'))
  dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Boxplot'))
  dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Violin_Plot'))
  dir.create(paste0(directory,'/Plots/Bivariate/Time Series'))
  dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Scatter'))
  dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Line'))
  if(length(num_cols)>0)
  { 
    
    ### Correlation matrix
    if(corr){
      dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous/Correlation'))
      correlation_heatmap(data=df,numerical_columns=num_cols,dir=directory)
    }
    if(univariate)
    { 
      for(i in 1:length(num_cols))
      {
        univariate_histogram(data=df,x=num_cols[i],dir=directory)
        univariate_boxplot(data=df,x=num_cols[i],dir=directory)
        univariate_density(data=df,x=num_cols[i],dir=directory)
      }
    }
  }
  if(length(cat_cols)>0)
  { 
    df[,cat_cols]=lapply(df[,cat_cols],as.character)
    for(i in 1:length(cat_cols))
    { 
      max_cat=min(max.cat,length_unique(df[,cat_cols[i]]))
      if(length_unique(df[,cat_cols[i]])>=max.cat)
      {
        freq_dist=as.data.frame(table(df[,cat_cols[i]]))
        freq_dist = freq_dist[order(-freq_dist$Freq),]
        top_n_cat=as.character(freq_dist$Var1[1:(max_cat-1)])
        df[,cat_cols[i]]=ifelse(df[,cat_cols[i]] %in% top_n_cat,df[,cat_cols[i]],'Others')
      }
      df[,cat_cols[i]]=as.factor(df[,cat_cols[i]])
      if(univariate){
        univariate_barplot(data=df,x=cat_cols[i],dir=directory) 
      }
    }
  }
  if(is.null(required_plots))
  {
    
    if(length(num_cols)>0)
    { 
      
      for(i in 1:length(num_cols))
      {
        scatter_all(data=df,numerical_columns=num_cols,current_column=num_cols[i],directory = directory)
      }
    }
    
    if(length(cat_cols)>0)
    { 
      
      for(i in 1:length(cat_cols))  
      {
        jitter_all(data=df,categorical_columns=cat_cols,current_column=cat_cols[i],directory = directory)
        boxplot_all(data=df,numerical_columns=num_cols,current_column=cat_cols[i],directory = directory)
        violin_all(data=df,numerical_columns=num_cols,current_column=cat_cols[i],directory = directory)
      } 
    }
    
    if(length(date_cols)>0)
    { 
      
      for(i in 1:length(date_cols))
      {
        range_date=difftime(max(df[,date_cols[i]],na.rm = T),min(df[,date_cols[i]],na.rm=T),units='days')
        range_date_1=as.character(floor(range_date/5))
        timeseries_line_all(data=df,numerical_columns=num_cols,current_column=date_cols[i],directory = directory,dr=range_date_1)
        timeseries_scatter_all(data=df,numerical_columns=num_cols,current_column=date_cols[i],directory = directory,dr=range_date_1)
      }
    }
  }
  else {
    required_plots[,1:2]=lapply(required_plots[,1:2],as.character)
    required_plots=required_plots[!required_plots[,1] %in% exclude_cols,]
    required_plots=required_plots[!required_plots[,2] %in% exclude_cols,]
    for(i in 1:nrow(required_plots))
    {
      if(required_plots[i,1] %in% num_cols & required_plots[i,2] %in% num_cols)
      {
        dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous/Scatter/',required_plots[i,1]))
        bivariate_scatter(data=df,x=required_plots[i,1],y=required_plots[i,2],dir=directory)
        
      } else if(required_plots[i,1] %in% cat_cols & required_plots[i,2] %in% num_cols) {
        
        dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Boxplot/',required_plots[i,1]))
        dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Violin_Plot/',required_plots[i,1]))
        bivariate_boxplot(data=df,x=required_plots[i,1],y=required_plots[i,2],dir=directory)
        bivariate_violin(data=df,x=required_plots[i,1],y=required_plots[i,2],dir=directory)
        
      } else if(required_plots[i,1] %in% cat_cols & required_plots[i,2] %in% cat_cols) {
        
        dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Discrete/',required_plots[i,1]))
        bivariate_jitter(data=df,x=required_plots[i,1],y=required_plots[i,2],dir=directory)
        
      } else if(required_plots[i,1] %in% date_cols & required_plots[i,2] %in% num_cols){
        
        dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Scatter/',required_plots[i,1]))
        dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Line/',required_plots[i,1]))
        range_date=difftime(max(df[,required_plots[i,1]],na.rm = T),min(df[,required_plots[i,1]],na.rm=T),units='days')
        range_date_1=as.character(floor(range_date/5))
        timeseries_line(data=df,x=required_plots[i,1],y=required_plots[i,2],date_range=range_date_1,dir=directory)
        timeseries_scatter(data=df,x=required_plots[i,1],y=required_plots[i,2],date_range=range_date_1,dir=directory)
        
      } else if(required_plots[i,1] %in% num_cols & required_plots[i,2]=='.'){
        
        scatter_all(data=df,numerical_columns=num_cols,current_column=required_plots[i,1],directory = directory)
        
      } else if(required_plots[i,1] %in% cat_cols & required_plots[i,2]=='.'){
        
        jitter_all(data=df,categorical_columns=cat_cols,current_column=required_plots[i,1],directory = directory)
        boxplot_all(data=df,numerical_columns=num_cols,current_column=required_plots[i,1],directory = directory)
        violin_all(data=df,numerical_columns=num_cols,current_column=required_plots[i,1],directory = directory)
        
      } else if(required_plots[i,1] %in% date_cols & required_plots[i,2]=='.'){
        
        range_date=difftime(max(df[,required_plots[i,1]],na.rm = T),min(df[,required_plots[i,1]],na.rm=T),units='days')
        range_date_1=as.character(floor(range_date/5))
        timeseries_line_all(data=df,numerical_columns=num_cols,current_column=required_plots[i,1],directory = directory,dr=range_date_1)
        timeseries_scatter_all(data=df,numerical_columns=num_cols,current_column=required_plots[i,1],directory = directory,dr=range_date_1)
      }
      else {
        print(paste0('No plots available for class ',class(df[,required_plots[i,1]]),' as X axis and class ',class(df[,required_plots[i,2]]),' as y axis'))
      }
    }
    
  }
}


################################################ Function Call ############################################################

setwd('C:\\Users\\Rohit Jagtani\\Documents\\GitHub\\Data-Visualisation-Function')
df1=read.csv('superstores.csv')
dataviz(df=df1,exclude_cols=c('Country','Order.ID','Customer.ID','Customer.Name','Product.ID','Product.Name','Row.ID','Postal.Code'),datetime_format = '%m/%d/%Y',corr=T,required_plots = NULL,univariate = T,selected_plots=T)

################################################################################################################################




