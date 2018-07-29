setwd('/home/rjagtani03')
detach(package:dplyr,unload=T)
detach(package:RPostgreSQL,unload=T)
library(plyr)
library(dplyr)
library(data.table)
library(smbinning)

#### Reading File and cleaning 
# 
# email=fread('email_resp_y.csv',sep='|')
# email=as.data.frame(email)
# write.csv(email,'email_resp_y_v1.csv',row.names = F)
# colnames(email)=gsub(colnames(email),pattern = 'ro_email_attr_croma4.',replacement = '')
# email=fread('email_resp_y_v1.csv',sep=',')
# 
# 
# sms=read.table('sms_resp_y.csv',sep='\t',header=T,quote="",fill=T)
# sms=as.data.frame(sms)
# colnames(sms)=gsub(colnames(sms),pattern = 'ro_sms_attr_croma4.',replacement = '')
# write.csv(sms,'sms_resp_y_v1.csv',row.names = F)
# sms=fread('sms_resp_y_v1.csv',sep=',')

################

email=fread('email_resp_y.csv',sep=',')
email=as.data.frame(email)
#email_summ=as.data.frame(smbinning.eda(email))
#write.csv(email_summ,'data_summ_email.csv',row.names = F)
email_summ=read.csv('data_summ_email.csv',stringsAsFactors = F)
remove_cols=as.character(email_summ$eda.Field[email_summ$Action=='Remove'])
integer_cols=as.character(email_summ$eda.Field[email_summ$Action=='integer'])
factor_cols=as.character(email_summ$eda.Field[email_summ$Action=='factor'])
numeric_cols=as.character(email_summ$eda.Field[email_summ$Action=='numeric'])
#View(email_summ[,c('eda.Field','eda.Miss','eda.Unique','Action')])

###############

iv = function(df=NULL,vars=NULL,unique_key=NULL,target_var=NULL,maxcat=20,numeric_cutoff=10)
{
  #df=email;unique_key='tuid1';target_var='y_flag';maxcat=20;vars=c(factor_cols,numeric_cols);numeric_cutoff=10
  # df=df1 
  # vars=c('P')
  # 
  # unique_key="name"
  #  target_var="y_flag"
  df=as.data.frame(df)
  df=df[,c(unique_key,target_var,vars)]
  df[,target_var]=as.integer(as.character(df[,target_var]))
  if(sum(is.na(df[,target_var]))!=0)
  {
    print("Error : Missing Y values")
  }  
  
  summ <- as.data.frame(smbinning.eda(df, rounding = 3, pbar = 1))
  factor_vars=as.character(summ[summ$eda.Type=="Factor","eda.Field"])
  df[,factor_vars]=lapply(as.data.frame(df[,factor_vars]),as.character)
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
    
    if(v1 > numeric_cutoff && !(col_names[i] %in% c(target_var,unique_key)))
    {
      con_vars <- c(con_vars, col_names[i])
    }
    else if(v1 <= numeric_cutoff && !(col_names[i] %in% c(target_var,unique_key)))
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
  
  mod_data=as.data.frame(mod_data)
  for(i in 1:length(cat_vars))
  {
  
    mod_data1=mod_data[,c(target_var,unique_key,cat_vars[i])]
    fac_bin = try(smbinning.factor(df = mod_data1, y = target_var, x = cat_vars[i],maxcat = maxcat), TRUE)
    
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
  
  for(i in 1:length(con_vars))
  {
    mod_data1=mod_data[,c(target_var,unique_key,con_vars[i])]
    mod_data1=as.data.table(mod_data1)
    if((is.numeric(mod_data1[[con_vars[i]]]) | is.integer(mod_data1[[con_vars[i]]])))
    {
      
      temp_df <- mod_data1[get(con_vars[i]) !=0 & get(con_vars[i]) !=-99999, .(get(con_vars[i]),get(target_var))]
      x.Pct20.Breaks=as.vector(quantile(temp_df[,1,with=F], probs=seq(0,1,0.2), na.rm=TRUE))
      Cuts.x.Pct20=x.Pct20.Breaks[1:(length(x.Pct20.Breaks))]
      cut_len <- length(Cuts.x.Pct20)
      for( j in 0:(cut_len-1))
      {
        Cuts.x.Pct20[cut_len-j+1]=Cuts.x.Pct20[cut_len-j]
      }
      Cuts.x.Pct20[1]=-99999 
      Cuts.x.Pct20[2]=0
      fac_bin=try(smbinning.custom(df=mod_data1,y=target_var,x=con_vars[i],cuts=Cuts.x.Pct20))
      
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
  return(list(con_vars,cat_vars,iv_table_sorted,final_iv_table,mod_data))
}


iv_list=iv(df = email,unique_key = 'tuid1',target_var = 'y_flag',vars=c(factor_cols,numeric_cols),numeric_cutoff = 10)
save(iv_list,file='iv_email.Rda')
write.csv(iv_list[[3]],'iv_table.csv',row.names = F)
cv=iv_list[[2]]
which(cv=='id_auto_ins')
