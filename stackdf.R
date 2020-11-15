# Sample Data
###########################################################
n <- 100

df1 <- data.frame(
  var1 = sample(c("yes", "no"), size = n, replace = TRUE),
  var2 = rnorm(n = n, mean = 100, sd = 10),
  var3 = factor(sample(c("yes", "no"), size = n, replace = TRUE)),
  var4 = sample(1:5, size = n, replace = TRUE)
  )


df2 <- data.frame(
  var1 = sample(c("yes", "no"), size = n, replace = TRUE),
  var2 = rnorm(n = n, mean = 100, sd = 10),
  var4 = sample(1:100, size = n, replace = TRUE)
  )

df3 <- data.frame(
  var1 = sample(c("yes", "no", "don't know"), size = n, replace = TRUE),
  var2 = rnorm(n = n, mean = 100, sd = 10),
  var3 = factor(sample(c("yes", "no"), size = n, replace = TRUE)),
  var4 = sample(1:5, size = n, replace = TRUE)
)


# Stacking-Function
###########################################################
library(plyr)
library(tidyverse)

stackdf <- function(...){
  list_of_dfs <- list(...)
  variable_overview <- data.frame(df_no = NA, 
                                  varname = NA, 
                                  class = NA, 
                                  type = NA,
                                  discrete = NA,
                                  fct_levels = NA
                                  )
  df_no <- 1
  
  for (df in list_of_dfs){
    for(var in colnames(df)){
      
      # Check if answer is a factor and count factor levels
      if(is.factor(df[[var]])){
        fct_levels = length(levels(df[[var]]))
      } else{
        fct_levels = NA
      }
      
      # Check if answer is numeric and if it is continuous or discrete
      if(is.numeric(df[[var]])){
        if(length(unique(df[[var]])) > 6){
          discrete = 0
        } else{
          discrete = 1
        }
      } else{
        discrete = NA
      }

      # Add a new entry with type, class etc. for all variables the dfs
      variable_overview <- variable_overview %>% add_row(
              df_no = df_no,
              varname = var,
              class = class(df[[var]]),
              type = typeof(df[[var]]),
              discrete = discrete,
              fct_levels = fct_levels
             )
    }
    
    df_no <- df_no + 1
  }
  
  # Sort by variable and id
  variable_overview <- variable_overview %>% arrange(varname, df_no)

  # Renaming variables if types don't match
  stacked_dfs <- plyr::rbind.fill(df1, df2)
    for (var in unique(variable_overview$varname)) {
    print(var)
  }
  
  print(variable_overview)
}


stackdf(df1, df2, df3)
