######  Change in Logic Imputation Function 

#### Impute function 

detach(package:dplyr,unload=T)
detach(package:RPostgreSQL,unload=T)
library(plyr)
library(dplyr)
library(data.table)
library(smbinning)


treatment=function(df=NULL,unique_key=NULL,num_cutoff=0.15,cat_cutoff=0.15)
{ 
  # unique_key="name"
  # num_cutoff=0.15
  # cat_cutoff=0.15
  # df=sr_2017
  k=as.data.frame(smbinning.eda(df))
  k$eda.Field=as.character(k$eda.Field)
  fill_vars=k[k$eda.Miss==0,"eda.Field"]
  #cat_vars=as.character(k[k$eda.Type=="Other" | k$eda.Type=="Factor","eda.Field"])
  #cat_vars=cat_vars[cat_vars!=unique_key]
  #df[,cat_vars]=lapply(df[,cat_vars],as.factor)
  k$treatment=ifelse((k$eda.Type=="Other" | k$eda.Type=="Factor") & k$edapct.Miss<=cat_cutoff,"Mode",NA)
  k$treatment=ifelse((k$eda.Type=="Other" | k$eda.Type=="Factor") & k$edapct.Miss>cat_cutoff & k$eda.Unique<=20,"WOE",k$treatment)
  k$treatment=ifelse((k$eda.Type=="Other" | k$eda.Type=="Factor") & k$edapct.Miss>cat_cutoff & k$eda.Unique>20,"Bucket_WOE",k$treatment)
  k$treatment=ifelse(k$eda.Type=="Num/Int" & k$edapct.Miss<=num_cutoff,"Median",k$treatment)
  k$treatment=ifelse(k$eda.Type=="Num/Int" & k$edapct.Miss>num_cutoff & k$eda.Unique>50,"WBM",k$treatment)
  k$treatment=ifelse(k$eda.Type=="Num/Int" & k$edapct.Miss>num_cutoff & k$eda.Unique>20 & k$eda.Unique<=50,"Bucket_WOE",k$treatment)
  k$treatment=ifelse(k$eda.Type=="Num/Int" & k$edapct.Miss>num_cutoff & k$eda.Unique<=20,"WOE",k$treatment)
  k$treatment=ifelse(k$eda.Field %in% fill_vars,"No Missings",k$treatment)
  k$treatment=ifelse(k$eda.Field %in% unique_key,"Unique Key",k$treatment)
  write.csv(k,"Treatments.csv",row.names=F)
  print("Treamtent file Written to Working Directory.Make changes to the treatments if any.Save & close the file")
  x=readline("Do you want to continue? (y/n) : ")
  if(x=='n')
  {
    stop("Process Stopped")
  }
  else{
    k=read.csv("Treatments.csv")
  }
  return(k)
}

mean_impute=function(df=NULL,vars=NULL)
{    
  temp_df=data.frame()
  for(i in 1:length(vars))
  {
    mean_imputation=mean(df[,vars[i]],na.rm=T)
    df[,vars[i]][is.na(df[,vars[i]])]=mean_imputation
    print(paste0("Column ",vars[i]," : Mean Imputed"))
    temp_df[i,"eda.Field"]=vars[i]
    temp_df[i,"imputed_value"]=as.character(mean_imputation)
  }
  return(list(df,temp_df))
}

median_impute=function(df=NULL,vars=NULL)
{   
  temp_df=data.frame()
  for(i in 1:length(vars))
  {
    median_imputation=median(df[,vars[i]],na.rm=T)
    df[,vars[i]][is.na(df[,vars[i]])]=median_imputation
    print(paste0("Column ",vars[i],": Median Imputed"))
    temp_df[i,"eda.Field"]=vars[i]
    temp_df[i,"imputed_value"]=as.character(median_imputation)
  }
  return(list(df,temp_df))
}

zero_impute=function(df=NULL,vars=NULL)
{   
  temp_df=data.frame()
  for(i in 1:length(vars))
  {
    
    df[,vars[i]][is.na(df[,vars[i]])]=0
    print(paste0("Column ",vars[i]," : Zero Imputed"))
    temp_df[i,"eda.Field"]=vars[i]
    temp_df[i,"imputed_value"]='0'
  }
  return(list(df,temp_df))
}


factor_impute=function(df=NULL,vars=NULL)
{   
  temp_df=data.frame()
  for(i in 1:length(vars))
  {
    df[,vars[i]]=as.character(df[,vars[i]])
    df[,vars[i]]=ifelse(is.na(df[,vars[i]]),'Missing',df[,vars[i]])
    df[,vars[i]]=as.factor(df[,vars[i]])
    temp_df[i,"eda.Field"]=vars[i]
  }
  temp_df$imputed_value='Missing'
  return(list(df,temp_df))
}

min_impute=function(df=NULL,vars=NULL)
{   
  temp_df=data.frame()
  for(i in 1:length(vars))
  {
    x=df[,vars[i]] 
    x=ifelse(df[,vars[i]]==-Inf,min(x[!is.infinite(x)],na.rm=TRUE),x)
    df[,vars[i]] = x
    temp_df[i,"eda.Field"]=vars[i]
    temp_df[i,"imputed_value"]=as.character(min(x[!is.infinite(x)],na.rm=TRUE))
  } 
  return(list(df,temp_df))
}

max_impute=function(df=NULL,vars=NULL)
{
  for(i in 1:length(vars))
  {
    x=df[,vars[i]] 
    x=ifelse(df[,vars[i]]==Inf,max(x[!is.infinite(x)],na.rm=TRUE),x)
    df[,vars[i]] = x
    temp_df[i,"eda.Field"]=vars[i]
    temp_df[i,"imputed_value"] = as.character(max(x[!is.infinite(x)],na.rm=TRUE))
  }
  return(list(df,temp_df))
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv=uniqv[!is.na(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_impute=function(df=NULL,vars=NULL)
{   
  temp_df=data.frame()
  for(i in 1:length(vars))
  {
    mode_imputation=getmode(df[,vars[i]])
    df[,vars[i]][is.na(df[,vars[i]])]=mode_imputation
    print(paste0("Column ",vars[i]," : Mode Imputed"))
    temp_df[i,"eda.Field"]=vars[i]
    temp_df[i,"imputed_value"]= as.character(mode_imputation)
  }
  return(list(df,temp_df))
}


iv = function(df=NULL,vars=NULL,unique_key=NULL,target_var=NULL,maxcat=20)
{
   
  #df=df;unique_key=unique_key;target_var=target_var;maxcat=20;vars=iv_vars
  # unique_key="name"
  # target_var="y_flag"
  df=df[,c(unique_key,target_var,vars)]
  df[,target_var]=as.integer(as.character(df[,target_var]))
  if(sum(is.na(df[,target_var]))!=0)
  {
    print("Error : Missing Y values")
  }  
  
  summ <- as.data.frame(smbinning.eda(df, rounding = 3, pbar = 1))
  factor_vars=as.character(summ[summ$eda.Type=="Factor","eda.Field"])
  df[,factor_vars]=lapply(df[,factor_vars],as.character)
  df[is.na(df)] <- -99999
  dat=data.table(df)
  # duplicating the data for further use
  mod_data <- copy(dat)
  dim(mod_data)
  colnames(mod_data)=gsub(".","",colnames(mod_data),fixed=TRUE)
  # BASED ON UNIQUE VALUES, CATEGORIZE VARIABLES AS CONTINUOUS OR CATEGORICAL (FACTOR)
  col_names <- names(mod_data)
  #col_names=col_names[-c(1,2)]
  col_names
  con_vars <- c()
  cat_vars <- c()
  misd_vars <- c()
  
  for(i in seq_along(col_names))
  {
    # checks the number of unique value and coerces the variable accrodingly
    # number 10 is taken as for N>10 smbinning does not allow categorical variables
    v1 <- uniqueN(mod_data[, get(col_names[i])])
    
    if(v1 >20 && !(col_names[i] %in% c(target_var,unique_key)))
    {
      con_vars <- c(con_vars, col_names[i])
    }
    else if(v1 <= 20 && !(col_names[i] %in% c(target_var,unique_key)))
    {
      cat_vars <- c(cat_vars, col_names[i])
    }
  }
  
  con_vars
  cat_vars
  
  for(i in seq_along(con_vars))
  {
    print(paste0('Variable converted to numeric for WoE calculation : ',con_vars[i]))
    mod_data[,con_vars[i]]<-as.numeric(mod_data[,get(con_vars[i])])
  }
  
  
  for(i in seq_along(cat_vars))
  {
    print(paste0('Variable converted to factor for WOE calculation : ',cat_vars[i]))
    mod_data[,cat_vars[i]]<-as.factor(mod_data[,get(cat_vars[i])])
  }
  
  
  mod_data[is.na(mod_data)] <- -99999  
  #summ<-smbinning.eda(mod_data, rounding = 3, pbar = 1)$eda   
  iv_chars <- data.frame("CharName" = character(0), "IV" = numeric(0), stringsAsFactors=FALSE)
  blank_row <- c("NA", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "NA")
  #rm(iv_table)
  
  
  for(i in 1:length(cat_vars))
  {
    
    
    fac_bin = try(smbinning.factor(df = mod_data, y = target_var, x = cat_vars[i],maxcat = 20), TRUE)
    
    if(class(fac_bin)=="character")
    {
      print(paste0('IV not generated for categorical variable : ',cat_vars[i],' due to error : ',fac_bin))
      misd_vars <- rbind(misd_vars,c(cat_vars[i],fac_bin))
    }
    else
    {
      print(paste0('IV generated for categorical variable : ',cat_vars[i]))
      iv_chars[nrow(iv_chars)+1,] <- c(cat_vars[i], fac_bin$iv) 
      df_temp <- data.frame(fac_bin$ivtable)
      df_temp$CharName <- cat_vars[i]
      
      if(!exists("iv_table")){
        iv_table <- copy(df_temp)
        iv_table <- rbind(iv_table, blank_row)
      } else{
        iv_table <- rbind(iv_table, df_temp, blank_row)
      }
      
      
    }
  }
  
  # BASED ON VARIABLE CATEGORIZATION, APPLY BINNING FOR CONTINUOUS VARIABLES AND CALCULATE IV
  
  for(i in seq_along(con_vars))
  {
    if((is.numeric(mod_data[[con_vars[i]]]) | is.integer(mod_data[[con_vars[i]]])))
    {
      
      temp_df <- mod_data[get(con_vars[i]) !=0 & get(con_vars[i]) !=-99999, .(get(con_vars[i]),get(target_var))]
      x.Pct20.Breaks=as.vector(quantile(temp_df[,1,with=F], probs=seq(0,1,0.2), na.rm=TRUE))
      Cuts.x.Pct20=x.Pct20.Breaks[1:(length(x.Pct20.Breaks))]
      cut_len <- length(Cuts.x.Pct20)
      for( j in 0:(cut_len-1))
      {
        Cuts.x.Pct20[cut_len-j+1]=Cuts.x.Pct20[cut_len-j]
      }
      Cuts.x.Pct20[1]=-99999 
      Cuts.x.Pct20[2]=0
      fac_bin=try(smbinning.custom(df=mod_data,y=target_var,x=con_vars[i],cuts=Cuts.x.Pct20))
      
      if(class(fac_bin)=="character")
      { 
        print(paste0('IV not generated for numerical variable : ',con_vars[i],' due to error : ',fac_bin))
        misd_vars <- rbind(misd_vars,c(con_vars[i],fac_bin))
      }
      else
      {
        df_temp <- data.frame(fac_bin$ivtable)
        df_temp$CharName <- con_vars[i]
        
        iv_chars[nrow(iv_chars)+1,] <- c(con_vars[i], fac_bin$iv)
        
        if(!exists("iv_table")){
          iv_table <- copy(df_temp)
          iv_table <- rbind(iv_table, blank_row)
        } else{
          iv_table <- rbind(iv_table, df_temp, blank_row)
        }
        print(paste0('IV generated for numerical variable : ',con_vars[i]))
      }
      
    }
    else
    {
      misd_vars <- rbind(misd_vars,c(con_vars[i],"not numeric / integer"))
    }
  }
  
  # this block converts the iv table to desired format and writes it to a csv
  
  iv_table_temp <- iv_table
  iv_table_temp <- subset(iv_table_temp,Cutpoint!="NA")
  iv_table_temp$CharBinNum <- ave(iv_table_temp$Cutpoint, iv_table_temp$CharName, FUN = seq_along)
  iv_list<-subset(iv_table,Cutpoint=="Total")
  iv_list<-arrange(iv_list,desc(IV))
  iv_list$IV_Char <- iv_list$IV
  iv_list<-subset(iv_list, select=c("IV_Char","CharName"))
  iv_table_temp2 <- merge(x=iv_table_temp, y=iv_list, by= "CharName", all.x=T)
  iv_table_sorted <- iv_table_temp2[order(-as.numeric(iv_table_temp2$IV_Char), iv_table_temp2$CharName, as.numeric(iv_table_temp2$CharBinNum)),]
  #write.csv(iv_table_sorted,paste0(getwd(),"/iv_table.csv"),row.names=FALSE)
  #print("IV table written to working directory")
  ############# Cleaning IV table
  
  iv_table_sorted1=iv_table_sorted[iv_table_sorted$CntRec!=0,]
  iv_table_sorted1=iv_table_sorted1[iv_table_sorted1$Cutpoint!='Total',]
  iv_table_sorted1$IV=as.numeric(iv_table_sorted1$IV)
  iv_table_sorted1$WoE=as.numeric(iv_table_sorted1$WoE)
  ##### subsetting on relevant_columns
  # iv_unclean_cols=unique(as.character(iv_table_sorted1[iv_table_sorted1$IV==Inf,"CharName"]))
  # iv_table_sorted_2=iv_table_sorted1[iv_table_sorted1$CharName %in% iv_unclean_cols,]
  #write.csv(iv_table_sorted1,'/tcad2/client10/model2_ds/IV/iv_table_selected.csv',row.names = FALSE)
  
  ##### substituting min WOE instead of -Inf
  
  iv_fields=unique(as.character(iv_table_sorted1[is.infinite(iv_table_sorted1$WoE),"CharName"]))
  iv_table_sorted2=iv_table_sorted1[iv_table_sorted1$CharName %in% iv_fields,]
  #iv_table_sorted2=iv_table_sorted2[iv_table_sorted2$CharName!="opportunity_owner_s_region__c_la",]
  #iv_table_sorted2=iv_table_sorted2[iv_table_sorted2$CntRec!=0,]
  #iv_table_sorted2=iv_table_sorted2[iv_table_sorted2$Cutpoint!='Total',]
  iv_lookup=iv_table_sorted2[!is.infinite(iv_table_sorted2$WoE),]
  iv_lookup1 = iv_lookup %>% group_by(CharName) %>% summarise(min_woe=min(WoE),max_woe=max(WoE))
  iv_table_sorted2=merge(iv_table_sorted2,iv_lookup1,by="CharName",all.x=TRUE)
  #sum(is.na(iv_table_sorted2$min_woe))
  iv_table_sorted2$WoE=ifelse(iv_table_sorted2$WoE==-Inf,iv_table_sorted2$min_woe,iv_table_sorted2$WoE)
  iv_table_sorted2$WoE=ifelse(iv_table_sorted2$WoE==Inf,iv_table_sorted2$max_woe,iv_table_sorted2$WoE)
  #write.csv(iv_table_sorted2,'/tcad2/client10/model2_ds_1608/IV/iv_table_cleaned.csv',row.names = FALSE)
  
  ##### combining already clean IVs with clean IVs
  
  iv_table_correct=iv_table_sorted1[!iv_table_sorted1$CharName %in% iv_fields,]
  iv_table_sorted2$min_woe=NULL
  iv_table_sorted2$max_woe=NULL
  final_iv_table=rbind(iv_table_correct,iv_table_sorted2)
  #iv_table_correct=iv_table_correct[iv_table_correct$CntRec!=0,]
  #iv_table_correct=iv_table_correct[iv_table_correct$Cutpoint!='Total',]
  #write.csv(final_iv_table,paste0(getwd(),"/iv_table_cleaned.csv"),row.names =FALSE)
  #print("Cleaned IV Table written to working directory")
  return(list(con_vars,cat_vars,final_iv_table,mod_data))
}

iv_pre_process = function(df=NULL,vars=NULL,unique_key=NULL,con_vars=iv_list[[1]],cat_vars=iv_list[[2]])
{
  # df=mds
  # unique_key="name"
  # target_var="y_flag"
  con_vars=vars[vars %in% con_vars]
  cat_vars=vars[vars %in% cat_vars]
  df=df[,c(unique_key,vars)]
  summ <- as.data.frame(smbinning.eda(df, rounding = 3, pbar = 1))
  factor_vars=as.character(summ[summ$eda.Type=="Factor","eda.Field"])
  df[,factor_vars]=lapply(df[,factor_vars],as.character)
  df[is.na(df)] <- -99999
  dat=data.table(df)
  # duplicating the data for further use
  mod_data <- copy(dat)
  dim(mod_data)
  colnames(mod_data)=gsub(".","",colnames(mod_data),fixed=TRUE)
  # BASED ON UNIQUE VALUES, CATEGORIZE VARIABLES AS CONTINUOUS OR CATEGORICAL (FACTOR)

  
  for(i in seq_along(con_vars))
  {
    mod_data[,con_vars[i]]<-as.numeric(mod_data[,get(con_vars[i])])
  }
  
  
  for(i in seq_along(cat_vars))
  {
    mod_data[,cat_vars[i]]<-as.factor(mod_data[,get(cat_vars[i])])
  }
  
  
  mod_data[is.na(mod_data)] <- -99999  
  mod_data=as.data.frame(mod_data)
  return(mod_data)
}

#df=temp;vars=treatment_vars
woe_impute=function(df=NULL,vars=NULL,cat_vars=iv_list[[2]],con_vars=iv_list[[1]],iv_table=iv_list[[3]],df1=iv_list[[4]])
{  
  # vars=c("opp_won_last_9","opportunities_won","opp_last_9","std")
  # df1=iv_list[[4]]
  # cat_vars=iv_list[[2]]
  #  con_vars=iv_list[[1]]
  # iv_table=iv_list[[3]
  df1=as.data.frame(df1)
  a=subset(iv_table,CharName %in% vars)
  num_vars =  vars[vars %in% con_vars]
  cat_vars =  vars[vars %in% cat_vars] 
  a=iv_table[,c("CharName","Cutpoint","CntRec","WoE")]
  temp_df1=data.frame()
  temp_df2=data.frame()
  if(length(num_vars)!=0)
  {
  a=iv_table[iv_table$CharName %in% num_vars,]
  a$cp=gsub("<=","",a$Cutpoint)
  a$cp=as.numeric(a$cp)
  a = a %>% group_by(CharName) %>% mutate(lag_cp=lag(cp),row_num=row_number()) %>% group_by(CharName) %>% mutate(max_row_num=max(row_num)) %>% mutate(diff=max_row_num-row_num)
  a=as.data.frame(a)
  a[,"lag_cp"]=ifelse(is.na(a[,"lag_cp"]),-100000,a[,"lag_cp"])
  for(i in 1:nrow(a))
  {
    if(a$row_num[i]==1)
    {  
      a[i,"bin_statement"]=paste0("df1$",a$CharName[i],"_WOE"," = ","ifelse(df1$",a$CharName[i],">","as.numeric(",a$lag_cp[i],")"," & ","df1$",a$CharName[i],"<=","as.numeric(",a$cp[i],")",",",as.character(a$WoE[i]),",",NA,")")
    }
    else {  
      if(a$diff[i]!=0)
      {  
        a[i,"bin_statement"]=paste0("df1$",a$CharName[i],"_WOE"," = ","ifelse(df1$",a$CharName[i],">","as.numeric(",a$lag_cp[i],")"," & ","df1$",a$CharName[i],"<=","as.numeric(",a$cp[i],")",",",as.character(a$WoE[i]),",","df1$",a$CharName[i],"_WOE",")")
      } else {
        a[i,"bin_statement"]=paste0("df1$",a$CharName[i],"_WOE"," = ","ifelse(df1$",a$CharName[i],">","as.numeric(",a$lag_cp[i],")",",",as.character(a$WoE[i]),",","df1$",a$CharName[i],"_WOE",")")
      }  }
    print(a$bin_statement[i])
    eval(parse(text=a$bin_statement[i]))
  }
  temp_df1=as.data.frame(subset(a,a$Cutpoint=="<= -99999",select=c("CharName","WoE")))
  colnames(temp_df1)=c("eda.Field","imputed_value")
  }
  
  ####### Cat Vars 
  
  if(length(cat_vars)!=0)
  {
  a2=iv_table[iv_table$CharName %in% cat_vars,]
  a2=a2 %>% group_by(CharName) %>% mutate(row_num=row_number())
  
  for(i in 1:nrow(a2))
  {
    if(a2$row_num[i]==1)
    {  
      a2[i,"bin_statement"]=paste0("df1$",a2$CharName[i],"_WOE"," = ","ifelse(df1$",a2$CharName[i]," =",a2$Cutpoint[i],",",as.character(a2$WoE[i]),",",NA,")")
    }
    else 
    {  
      a2[i,"bin_statement"]=paste0("df1$",a2$CharName[i],"_WOE"," = ","ifelse(df1$",a2$CharName[i]," =",a2$Cutpoint[i],",",as.character(a2$WoE[i]),",","df1$",a2$CharName[i],"_WOE",")")
    }   
    
    print(a2$bin_statement[i])
    eval(parse(text=a2$bin_statement[i]))
  }
  temp_df2=as.data.frame(subset(a2,a2$Cutpoint=="= '-99999'",select=c("CharName","WoE")))
  colnames(temp_df2)=c("eda.Field","imputed_value")
  }
  
  if(nrow(temp_df1)>0)
  {
  temp_df=rbind(temp_df1,temp_df2)
  }
  else {
  temp_df=temp_df2
  }
  temp_df$imputed_value=as.character(temp_df$imputed_value)
  woe_cols=df1[,colnames(df1)[which(grepl(colnames(df1),pattern="_WOE"))]]
  df=cbind(df,woe_cols)
  return(list(df,temp_df))
}


wbm_impute=function(df=NULL,vars=NULL,new_iv_clean=iv_list[[3]])
{
  iv_table_num=new_iv_clean[new_iv_clean$CharName %in% vars,]
  iv_table_num_na=iv_table_num[iv_table_num$Cutpoint=='<= -99999',]
  
  ### Merging NA s with non Nas
  
  iv_table_num=merge(iv_table_num,iv_table_num_na[,c("CharName","WoE")],by="CharName",all.x=TRUE,suffixes=c("","_na"))
  iv_table_num$WoE=as.numeric(iv_table_num$WoE)
  iv_table_num$WoE_na=as.numeric(iv_table_num$WoE_na)
  
  ####### Taking absolute difference of two ivs
  
  iv_table_num$min_diff=abs(iv_table_num$WoE-iv_table_num$WoE_na)
  
  #### Removing -99999 where difference should be zero
  
  iv_table_num1=iv_table_num[iv_table_num$Cutpoint!='<= -99999',]
  iv_table_num1=arrange(iv_table_num1,CharName)
  iv_table_num1=iv_table_num1 %>%
    group_by(CharName) %>%
    mutate(lvar = lag(Cutpoint),min_distance = min(min_diff))
  iv_table_num1=as.data.frame(iv_table_num1)
  
  #### Selecting bucket where difference between distance and min distance is zero 
  
  iv_table_num1$diff_min_dist=iv_table_num1$min_diff-iv_table_num1$min_dist
  iv_table_num1_na=subset(iv_table_num1,iv_table_num1$diff_min_dist==0)
  
  ##### Cleaning bucket values
  
  rel_bins=iv_table_num1_na[,c("CharName","lvar","Cutpoint")]
  rel_bins$lvar=gsub("<=","",rel_bins$lvar)
  rel_bins$Cutpoint=gsub("<=","",rel_bins$Cutpoint)
  rel_bins$lvar=as.numeric(rel_bins$lvar)
  rel_bins$Cutpoint=as.numeric(rel_bins$Cutpoint)
  rel_bins$lvar[is.na(rel_bins$lvar)]=-99999
  
  temp_df=data.frame()
  ####### Looping over all columns to get WBM imputed values 
  
  for(i in 1:length(rel_bins$CharName))
  { 
    temp_col=rel_bins$CharName[i]
    k=df[,temp_col]
    woe_based_mean=mean(k[k>=rel_bins[i,"lvar"] & k<=rel_bins[i,"Cutpoint"]],na.rm=TRUE)
    k=ifelse(is.na(k),woe_based_mean,k)
    df[,temp_col]=k
    #print(i)
    temp_df[i,"eda.Field"]=temp_col
    temp_df[i,"imputed_value"]=as.character(woe_based_mean)
  }
  
  
  return(list(df,temp_df))
}


final_call=function(df=NULL,df_test=NULL,target_var=NULL,unique_key=NULL,maxcat=20)
{ 
  treatment_df=treatment(df=df,unique_key=unique_key)
  iv_vars=as.character(treatment_df[treatment_df$treatment=='WBM' | treatment_df$treatment=='WOE',"eda.Field"])
  unique_treatments=unique(as.character(treatment_df$treatment))
  unique_treatments=unique_treatments[!unique_treatments %in% c('No Missings','Unique Key')]
  if(sum(unique_treatments %in% c('WOE','WBM'))>0)
  {
    iv_list=iv(df=df,unique_key=unique_key,target_var=target_var,maxcat=20,vars=iv_vars)
    iv_list <<- iv_list
  }
  temp=df
  treatments_done=data.frame()
  for(i in 1:length(unique_treatments))
  { 
    treatment_vars=as.character(treatment_df[treatment_df$treatment==unique_treatments[i],"eda.Field"])
    df1=get(paste0(tolower(unique_treatments[i]),"_impute"))(df=temp,vars=treatment_vars)
    temp=df1[[1]]
    imp_done=df1[[2]]
    imp_done$imputed_value=as.character(imp_done$imputed_value)
    treatments_done=as.data.frame(rbind(treatments_done,imp_done))
    print(paste0(unique_treatments[i]," imputation done"))
  }
  
  treatments_done=merge(treatments_done,treatment_df[,c("eda.Field","treatment")],by="eda.Field")
  treatments_done$treatment=as.character(treatments_done$treatment)
  
  if(!is.null(df_test))
  {
    woe_names=as.character(treatments_done[treatments_done$treatment=="WOE","eda.Field"])
    woe_names=woe_names[!is.na(woe_names)]
    if(length(woe_names)>0)
    {
    test_woe_pre=iv_pre_process(df=df_test,vars=woe_names,unique_key = unique_key)
    test_woe_list=woe_impute(df=df_test,vars=woe_names,cat_vars=iv_list[[2]],con_vars=iv_list[[1]],iv_table=iv_list[[3]],df1=test_woe_pre)
    df_test=test_woe_list[[1]]
    }
    other_treatments=subset(treatments_done,treatment!="WOE") 
    for(i in 1:nrow(other_treatments))
    {
      field=other_treatments[i,"eda.Field"] 
      value_to_impute=other_treatments[i,"imputed_value"]
      field_class=class(df_test[,field])  
      df_test[,field][is.na(df_test[,field])]=get(paste0("as.",field_class))(value_to_impute)
      print(paste0("Test dataframe  Column ",field," imputed"))
    }
  return(list(df1[[1]],treatments_done,df_test))
  }
  return(list(df1[[1]],treatments_done))
}

################### FUNCTION TESTING

mds=read.csv("all_vars_2015_m2.csv")
#mds_summ=as.data.frame(smbinning.eda(mds))
mds$y_flag=NULL
y_flag=as.integer(runif(9348,0,2))
mds=cbind(mds,y_flag)
table(mds$y_flag)
mds_train=mds[1:7000,]
mds_test=mds[7001:9348,]

#####  Function_Call

l1=final_call(df=mds_train,df_test=mds_test,unique_key="name",target_var="y_flag")

####   Extracting Elements

full_ds=l1[[1]]
done_imp=l1[[2]]
full_test=l1[[3]]


#############################################################################################################


df=mds_train;df_test=mds_test;unique_key="name";target_var="y_flag"

kk=as.data.frame(smbinning.eda(full_ds))


aq=airquality
aq_train=aq[1:99,]
aq_test=aq[100:153,]
mds=read.csv("/tcad2/client10/rjagtani10_restored_data/new_data_scoring/model_ds_null.csv")
mds_summ=as.data.frame(smbinning.eda(mds))
y_flag=as.integer(runif(2415,0,2))
mds=cbind(mds,y_flag)
l1=final_call(df=mds,df_test=NULL,unique_key="name",target_var="y_flag",maxcat = 20)




mds_train=mds[1:2000,]
mds_test=mds[2001:2415,]

a=read.csv("model_ds_null.csv",stringsAsFactors = F)
iv_list=iv(df = mds,unique_key="name",target_var ="y_flag",maxcat = 20,vars=colnames(mds))
ds_imp=woe_imp(vars = c("opp_won_last_9","opportunities_won","opp_last_9","std"),df = iv_list[[4]],cat_vars = iv_list[[2]],con_vars = iv_list[[1]],iv_table = iv_list[[3]])
list=ds_imp[[2]]    
