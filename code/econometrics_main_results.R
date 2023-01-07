
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
library(margins)




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

volumes_1$bin <- as.factor(volumes_1$bin)

bins <- as_factor(seq(1610, 1890, by = 20))

model <- lm(optimism_percentile ~ Religion * Science * bin + Religion * Political.Economy * bin + Science * Political.Economy * bin - Religion - Religion * bin + Political.Economy + bin, volumes_1)

summary(model)

#function for getting marginal effects
get_marginal_science <- function(model, s, r, p){
  tmp <- model %>%
    margins(
      variables = "Science",
      at = list(Science = s, Religion = r, Political.Economy = p, bin = bins)
    ) %>%
    summary()
  
  return(tmp)
}

s100_m <- get_marginal_science(model = model, s = 1, r = 0, p = 0)
s50r50_m <- get_marginal_science(model = model, s = 0.5, r = 0.5, p = 0)
s50p50_m <- get_marginal_science(model = model, s = 0.5, r = 0, p = 0.5)
thirds_m <- get_marginal_science(model = model, s = 1/3, r = 1/3, p = 1/3)

s100_m$label <- "100% Science"
s50r50_m$label <- "50% Science 50% Religion"
s50p50_m$label <- "50% Science 50% Political Economy"
thirds_m$label <- "1/3 Each"

s100_m$bin <- bins
s50r50_m$bin <- bins
s50p50_m$bin <- bins
thirds_m$bin <- bins

marg <- rbind(s100_m, s50r50_m, s50p50_m, thirds_m)

marginal_fig <- ggplot(marg, aes(x = bin, y = AME, group = label)) +
  geom_line(aes(color = label, linetype = label)) +
  geom_ribbon(aes(y = AME, ymin = lower, ymax = upper, fill = label), alpha = 0.2) +
  labs(title = "Marginal Effects", x = "Year", y = "Value") +
  theme(legend.position = "none")

show(marginal_fig)

ggsave("../output/marginal_effects.png", width = 5.5)

#Predicted Values

pred <- function(lm, sci, rel, pol){
  data <- data.frame(Science = sci, Political.Economy = pol, Religion = rel, bin = bins)
  prediction <- predict(lm, newdata = data, interval = "confidence", se.fit =TRUE)
  fit <- data.frame(prediction$fit)
  return(fit)
}



s100_p <- pred(lm = model, sci = 1, rel = 0, pol = 0)
s50r50_p <- pred(lm = model, sci = 0.5, rel = 0.5, pol = 0)
s50p50_p <- pred(lm = model, sci = 0.5, rel = 0, pol = 0.5)
thirds_p <- pred(lm = model, sci = 1/3, rel = 1/3, pol = 1/3)

s100_p$label <- "100% Science"
s50r50_p$label <- "50% Science 50% Religion"
s50p50_p$label <- "50% Science 50% Political Economy"
thirds_p$label <- "1/3 Each"

s100_p$bin <- bins
s50r50_p$bin <- bins
s50p50_p$bin <- bins
thirds_p$bin <- bins

pred <- rbind(s100_p, s50r50_p, s50p50_p, thirds_p)

predicted_fig <- ggplot(pred, aes(x = bin, y = fit, group = label)) +
  geom_line(aes(color = label, linetype = label)) +
  geom_ribbon(aes(y = fit, ymin = lwr, ymax = upr, fill = label), alpha = 0.2) +
  labs(title = "Predicted Values", x = "Year", y = "Value")

show(predicted_fig)

ggsave("../output/predicted_values.png", width = 8)




