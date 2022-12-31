
#Clear memory and setup
rm(list=ls())
options(scipen=999)


#Load Packages

library(tidyverse)
library(fixest)
library(data.table)
library(broom)
library(modelsummary)
library(msm)
library(ggpubr)
library(reshape2)
library(kableExtra)




#Load Data
volumes <- read.csv('../temporary/volumes_opt_industry.csv')


#Clean up
volumes <- volumes[,-1] #Drops 'X' which is just the index attached by Python


#Create years and bins
years <- seq(1510,1890, by=1)
bins <- seq(1510, 1890, by = 20)


#Assign volumes to bins
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
volumes_1 <- volumes %>%
  filter(bin >= 1610)





#Regressions


mod <- feols(optimism_percentile ~ Science + Political.Economy + Science*Political.Economy + Science*Religion + Religion*Political.Economy + i(bin, Science, 1610) + i(bin, Political.Economy, 1610) + i(bin, Science*Religion, 1610) + i(bin, Science*Political.Economy, 1610) + i(bin, Political.Economy*Religion, 1610) + i(bin, ref = 1610) - Religion, data = volumes_1)




#model with bin FE entering alone
# mod_no_interactions <- feols(optimism_percentile ~ Science + Political.Economy + Science*Political.Economy + Science*Religion + Religion*Political.Economy + i(bin, ref = 1610) - Religion, data = volumes_1)

# 
# 
# etable(mod_no_interactions)

#Group results by year and variable
estimates <- tibble::rownames_to_column(mod$coeftable, "coefficient")

#add deltamethod variables

estimates <- estimates %>%
  mutate(ref = paste0("x", 1:length(estimates$coefficient)))

#parse variable names into FEs and dep vars


estimates <- estimates %>%
  mutate(coefficient = str_remove(coefficient, "bin::")) %>%
  mutate(coefficient = str_replace(coefficient, '^([^0-9:]*):([^0-9:]*)$', '\\1 * \\2')) %>% #Format interactions correctly
  mutate(split = strsplit(coefficient, ':')) %>%
  mutate(year = sapply(split, function(x) x[1]),
         variable = ifelse(sapply(split, length) == 2, sapply(split, function(x) x[2]), sapply(split, function(x) x[1]))) %>% #assign years and variables to correct columns
  select(-split)

estimates <- estimates %>%
  mutate(variable = ifelse(is.na(as.numeric(variable)), variable, "(Intercept)")) %>%
  mutate(year = ifelse(is.na(as.numeric(year)), 'Reference', year))



#Reshape coefficients and std errors into dfs

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
refs <- transform_estimates(estimates, "ref")



#Get output into models to be compatible with modelsummary
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
  
  #10 because that is the first model in the second of the split results table. Need to change to 2 if going for one continuous table
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



#r Main results table
rename <- c("Political.Economy" = "PolitEcon", "industry_percentile" = "Industry",
            "Science * Religion" = "$\\text{Science} \\times \\text{Religion}$", "Science:industry_percentile" = "$\\text{Science} \\times \\text{Industry}$", "Religion:industry_percentile" = "$\\text{Religion} \\times \\text{Industry}$", "Science * Political.Economy" = "$\\text{Science} \\times \\text{PolitEcon}$", "Political.Economy * Religion" = "$\\text{Religion} \\times \\text{PolitEcon}$", "Science:Religion:industry_percentile" = "$\\text{Science} \\times \\text{Religion} \\times \\text{Industry}$", "Science:industry_percentile:Political.Economy" = "$\\text{Science} \\times \\text{PolitEcon} \\times \\text{Industry}$", "Religion:industry_percentile:Political.Economy" = "$\\text{Religion} \\times \\text{PolitEcon} \\times \\text{Industry}$")

modelsummary(models, output = "markdown", stars = TRUE)

modelsummary(models[1:8],
             stars = TRUE,
             coef_rename = rename,
             title = "Dependent Variable: Optimism Percentile",
             escape = FALSE,
             threeparttable=TRUE,
             output="../output/optimism_1.tex"
)

#split into two to fit onto page

modelsummary(models[9:15],
             stars = TRUE,
             coef_rename = rename,
             title = "Dependent Variable: Optimism Percentile",
             escape = FALSE,
             threeparttable = TRUE,
             notes = "Volumes are placed into 20 year ((+/-) 10 year) bins. Columns represent interactions between bin fixed effects and the variables of interest (rows). Observations prior to 1600 are dropped. Standard errors in parenthesis.",
             output="../output/optimism_2.tex"
)

modelsummary(models,
             stars = TRUE,
             coef_rename = rename,
             title = "Dependent Variable: Optimism Percentile",
             escape = FALSE,
             threeparttable = TRUE,
             notes = "Volumes are placed into 20 year ((+/-) 10 year) bins. Columns represent interactions between bin fixed effects and the variables of interest (rows). Observations prior to 1600 are dropped. Standard errors in parenthesis.",
             output="../output/optimism_combined.tex"
)


#Marginal effects

#transpose for easier computing
coefs_t <- data.table::transpose(coefs, make.names = "variable", keep.names = "bin")
std_errs_t <- data.table::transpose(std_errs, make.names = "variable", keep.names = "bin")
refs_t <- data.table::transpose(refs, make.names = "variable", keep.names = "bin")

#initialize marginal effects dataframe
marginal <- as_tibble(coefs_t$bin)

marginal <- marginal %>%
  mutate(s100 = coefs_t['Science'])

formula <- sprintf("~ %s", refs[2,2])
  
deltamethod(as.formula(formula), coef(mod), vcov(mod))



