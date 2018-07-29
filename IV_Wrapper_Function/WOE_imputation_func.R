#### Function call - IV

iv_list=iv(df = df1,target_var = 'Survived',vars=colnames(df1)[3:10])


#### WOE Imputation Function

woe_impute=function(df=NULL,cat_vars=iv_list[[2]],con_vars=iv_list[[1]],iv_table=iv_list[[4]],df1=iv_list[[5]])
{  
  # vars=c("opp_won_last_9","opportunities_won","opp_last_9","std")
  # df1=iv_list[[4]]
  # cat_vars=iv_list[[2]]
  #  con_vars=iv_list[[1]]
  # iv_table=iv_list[[3]
  df1=as.data.frame(df1)
  vars=as.character(unique(iv_table$CharName))
  num_vars=vars[vars %in% con_vars]
  cat_vars=vars[vars %in% cat_vars]
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

############# WOE Imputation function call


a1=woe_impute(df=df1)
a1_impute=a1[[1]]
