# Sample Data
###########################################################
n <- 10

#df1 <- data.frame(
#  var1 = factor(sample(c("yes", "no", "maybe"), size = n, replace = TRUE)),
#  var2 = rnorm(n = n, mean = 100, sd = 10),
#  var3 = factor(sample(c("yes", "no"), size = n, replace = TRUE)),
#  var4 = sample(1:5, size = n, replace = TRUE)
#  )

#df2 <- data.frame(
#  var1 = factor(sample(c("yes", "no"), size = n, replace = TRUE)),
#  var2 = rnorm(n = n, mean = 100, sd = 10),
#  var4 = sample(1:100, size = n, replace = TRUE)
#  )

#df3 <- data.frame(
#  var1 = factor(sample(c("yes", "no"), size = n, replace = TRUE)),
#  var2 = rnorm(n = n, mean = 100, sd = 10),
#  var3 = factor(sample(c("yes", "no"), size = n, replace = TRUE)),
#  var4 = sample(1:5, size = n, replace = TRUE)
#)

df1 <- data.frame(var1 = sample(c("a"), size = n, replace = TRUE),
                  var2 = factor(sample(c("a", "b"), size = n, replace = TRUE)))
df2 <- data.frame(var1 = sample(c("a", "b"), size = n, replace = TRUE),
                  var2 = factor(sample(c("a", "b"), size = n, replace = TRUE)),
                  var3 = "test")
df3 <- data.frame(var1 = sample(c("a", "b", "c"), size = n, replace = TRUE),
                  var2 = factor(sample(c("a", "b"), size = n, replace = TRUE)))

# Stacking-Function
###########################################################
library(plyr)
library(tidyverse)

stackdf <- function(..., .showMessages = FALSE){
  
  list_of_dfs <- list(...)
  variable_overview <- data.frame(df_no = NA, 
                                  varname = NA, 
                                  class = NA, 
                                  type = NA,
                                  discrete = NA,
                                  fct_levels = NA,
                                  reason = "",
                                  rename_to = ""
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
      
      # Check if answer is a character variable and count levels
      if(is.character(df[[var]])){
        fct_levels = length(unique(df[[var]]))
      } else{
        fct_levels = NA
      }
      
      # Check if answer is numeric and if it is continuous or discrete
      if(is.numeric(df[[var]]) | is.character(df[[var]])){
        if(length(unique(df[[var]])) > 6){
          discrete = 0
        } else{
          discrete = 1
        }
      } else{
        discrete = 0
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
  variable_overview <- variable_overview %>% 
    arrange(varname, df_no) %>%
    filter(!is.na(df_no))
  
  # Compare variables between different waves
  ############################################
  
  variable_overview <- variable_overview %>% 
    # Begründungen für Variablen-Umbenennungen
    mutate(reason ="",
          # Faktoren, aber unterschiedliche Anzahl Levels
          reason = ifelse(
                          lag(varname, 1) == lag(varname, 0) &     
                          lag(class, 0) == 'factor' & 
                          lag(fct_levels, 1) != lag(fct_levels, 0),
                          "number of levels changed (factor)",
                          reason
                         ),
          # Character, aber unterschiedliche Anzahl Levels
          reason = ifelse(
            lag(varname, 1) == lag(varname, 0) &     
              lag(class, 0) == 'character' & 
              lag(fct_levels, 1) != lag(fct_levels, 0),
            "number of levels changed (character)",
            reason
          ),
            # Zahlen, aber nicht beide konkret/stetig
            reason = ifelse(
                            lag(varname, 1) == lag(varname, 0) &     
                            lag(class, 0) %in% c('numeric', 'integer') & 
                            lag(discrete, 1) != lag(discrete, 0),
                            "discrete/continuous changed",
                            reason
                           ),
            reason = ifelse(
                            is.na(reason), "", reason
                           ),
          
            # neue Variablennamen erstellen
            # Falls es bei der aktuellen Variable eine Begründung gibt, Name anpassen
            rename_to = ifelse(
                              lag(varname, 1) == lag(varname, 0) &
                              lag(reason, 0) != "", 
                              paste0(lag(varname, 0), "__W", lag(df_no, 0)), ""
                              ),
            rename_to = ifelse(
                              lag(varname, 1) == lag(varname, 0) &
                              lag(reason, 1) != "" &
                              lag(reason, 0) == "", 
                              lag(rename_to, 1), lag(rename_to, 0)
                              )
              )

  # Tabelle aller Variablennamen und -umbenennungen ausgeben
  if(.showMessages == TRUE){
    print(variable_overview)
  }
  
  new_varnames <- variable_overview %>% filter(rename_to != '') %>%
    select(df_no, varname, rename_to) %>%
    mutate(df_no = paste0("df", df_no))
  
  # Variablen in den Datensätzen umbenennen
  for(i in seq_len(nrow(new_varnames))) {
    tmp1 <- get(new_varnames$df_no[i])
    i1 <- match(new_varnames$varname[i], names(tmp1))
    names(tmp1)[i1] <- new_varnames$rename_to[i]
    assign(new_varnames$df_no[i], tmp1)
  }
  
  list_of_dfs2 <- list()
  for (i in 1:length(list(...))){
    list_of_dfs2[[i]] <- get(paste0("df", i))
  }
  
  # Alle Dataframes zusammenfügen
  stacked_dfs <- plyr::rbind.fill(list_of_dfs2)
  
  # Spalten alphabetisch sortieren
  stacked_dfs <- stacked_dfs %>% 
    select(sort(names(.)))

}

stackdf(df1, df2, .showMessages = TRUE)