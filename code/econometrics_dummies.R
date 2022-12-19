

#clear memory and setup
rm(list=ls())
options(scipen=999)

#packages
library(tidyverse)
library(fixest)
library(data.table)
library(broom)
library(modelsummary)
library(msm)
library(ggpubr)
library(reshape2)
library(kableExtra)

#load data

volumes <- read.csv('../temporary/volumes_opt_industry.csv')

volumes <- volumes[,-1] #Drops 'X' which is just the index attached by Python

#Create years and bins
years <- seq(1510,1890, by=1)
bins <- seq(1610, 1890, by = 20)

volumes <- volumes[volumes$Year_rounded >= (min(bins) - 10),]
#Merge volume years to closest bin
a = data.table(Value=volumes$Year_rounded) #Extract years
a[,merge:=Value] #Give data.table something to merge on
b = data.table(Value = bins)
b[,merge:=Value]
setkeyv(a, c('merge')) #Sort for quicker merge
setkeyv(b, c('merge'))
rounded = b[a, roll='nearest'] #Merge to nearest
rounded <- distinct(rounded) #Get distinct values for easier merge to 'volumes'
# volumes <- data.table(volumes)
volumes <- merge(volumes, rounded, by.x = "Year_rounded", by.y = "merge")
#Remove unnecessary column
volumes <- volumes %>%
  subset(select = -c(i.Value)) %>%
  rename(bin = Value)


#drop obs before 1610 bin
volumes <- volumes %>%
  filter(bin >= 1610)

#Create dummies
volumes <- as_tibble(volumes)
volumes <- volumes %>%
  mutate(science_dummy = ifelse(volumes$Science == pmax(volumes$Science, volumes$Religion, volumes$Political.Economy), 1, 0)) %>%
  mutate(religion_dummy = ifelse(volumes$Religion == pmax(volumes$Science, volumes$Religion, volumes$Political.Economy), 1, 0)) %>%
  mutate(political.economy_dummy = ifelse(volumes$Political.Economy == pmax(volumes$Science, volumes$Religion, volumes$Political.Economy), 1, 0)) 


#model
mod <- feols(optimism_percentile ~ science_dummy + political.economy_dummy + science_dummy*Religion + science_dummy*Political.Economy + religion_dummy*Political.Economy + religion_dummy*Science + political.economy_dummy*Religion + political.economy_dummy*Science + i(bin, science_dummy, 1610) + i(bin, political.economy_dummy, 1610) + i(bin, science_dummy*Religion, 1610) + i(bin, science_dummy*Political.Economy, 1610) + i(bin, religion_dummy*Political.Economy, 1610) + i(bin, religion_dummy*Science, 1610) + i(bin, political.economy_dummy*Religion, 1610) + i(bin, political.economy_dummy*Science, 1610) + i(bin, ref = 1610) - Religion - Political.Economy - Science, data = volumes)
             
             
etable(mod)


#Get results

estimates <- tibble::rownames_to_column(mod$coeftable, "coefficient")

#Clean to make grouping by year and variable easier

#parse variable names into FEs and dep vars
estimates <- estimates %>%
  mutate(coefficient = str_remove(coefficient, "bin::")) %>%
  mutate(coefficient = str_replace(coefficient, '^([^0-9:]*):([^0-9:]*)$', '\\1 * \\2')) %>% #Format interactions correctly
  mutate(split = strsplit(coefficient, ':')) %>%
  mutate(year = sapply(split, function(x) x[1]),
         variable = ifelse(sapply(split, length) == 2, sapply(split, function(x) x[2]), sapply(split, function(x) x[1]))) %>% #assign years and variables to correct columns
  select(-split)

#Fix some special cases
estimates <- estimates %>%
  mutate(variable = ifelse(is.na(as.numeric(variable)), variable, "(Intercept)")) %>%
  mutate(year = ifelse(is.na(as.numeric(year)), 'Reference', year))

#Reshape coefficients, std. errors, and pvalues into dfs

transform_estimates <- function(df, stat){
  transformed <- dcast(df, variable ~ year, value.var = stat)
  
  transformed <- transformed %>%
    relocate("Reference", .after = "variable") %>%
    arrange(str_length(variable), variable) %>%
    arrange(variable != "(Intercept)")
  
  return(transformed)
}
coefs <- transform_estimates(estimates, "Estimate")
std_errs <- transform_estimates(estimates, "Std. Error")
pvalue <- transform_estimates(estimates, "Pr(>|t|)")

#Get output compatible w/ modelsummary

models <- list()
for (i in 2:ncol(coefs)){
  model <- list()
  class(model) <- "custom"
  tidy.custom <- function(x, ...) {
    data.frame(
      term = coefs$variable,
      estimate = coefs[[i]],
      std.error = std_errs[[i]],
      p.value = pvalue[[i]]
    )
  }
  
  #10 because that is the first model in the second half of results. No way to generalize, is what it is given how we are presenting results. Change to 2 for a generalized results table.
  if (i == 10) {
    glance.custom <- function(x, ...) {
      data.frame(
        "nobs" = mod$nobs,
        "r.squared" = glance(mod)$r.squared
        # "adj.r.squared" = glance(mod)$adj.r.squared
      )
    }
  } else{
    glance.custom <- function(x, ...) {
      data.frame(
      )
    }
  }
  models[[colnames(coefs)[[i]]]] <- modelsummary(model, output = "modelsummary_list")
}


#Modelsummary table

cm <- c('science_dummy' = '$\\mathds{1}(\\text{Science})$', 'religion_dummy' = '$\\mathds{1}(\\text{Religion})$', 'political.economy_dummy' = '$\\mathds{1}(\\text{PolitEcon})$', 'science_dummy * Religion' = '$\\mathds{1}(\\text{Science}) \\times \\text{Religion}$', 'science_dummy * Political.Economy' = '$\\mathds{1}(\\text{Science}) \\times \\text{PolitEcon}$', 'religion_dummy * Political.Economy' = '$\\mathds{1}(\\text{Religion}) \\times \\text{PolitEcon}$','religion_dummy * Science' = '$\\mathds{1}(\\text{Religion}) \\times \\text{Science}$', 'political.economy_dummy * Religion' = '$\\mathds{1}(\\text{PolitEcon}) \\times \\text{Religion}$', 'political.economy_dummy * Science' = '$\\mathds{1}(\\text{PolitEcon}) \\times \\text{Science}$') #Coefficient map

modelsummary(models[1:8],
             stars = TRUE,
             coef_rename = cm,
             escape = FALSE,
             threeparttable = TRUE,
             title = "Dependent Variable: Optimism Percentile",
             output = "../output/mod_dummies_1.tex")

modelsummary(models[9:15],
             stars = TRUE,
             coef_rename = cm,
             escape = FALSE,
             threeparttable = TRUE,
             notes = "Volumes are placed into 20 year ((+/-) 10 year) bins. Columns represent interactions between bin fixed effects and the variables of interest (rows). Observations prior to 1600 are dropped. Independent variables are indicators that take on a value of one for the highest weighted category for each volume. Standard errors in parenthesis.",
             output = "../output/mod_dummies_2.tex")

modelsummary(models,
             stars = TRUE,
             coef_rename = cm,
             escape = FALSE,
             threeparttable = TRUE,
             notes = "Volumes are placed into 20 year ((+/-) 10 year) bins. Columns represent interactions between bin fixed effects and the variables of interest (rows). Observations prior to 1600 are dropped. Independent variables are indicators that take on a value of one for the highest weighted category for each volume. Standard errors in parenthesis.",
             output = "../output/mod_dummies_combined.tex")




