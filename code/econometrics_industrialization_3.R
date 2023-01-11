

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
library(margins)

#load data

volumes <- read.csv('../temporary/volumes_opt_industry_2_3.csv')

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

volumes <- volumes %>%
  rename(c("industry_percentile" = "industry_3_percentile"))

#model
mod <- feols(optimism_percentile ~ Science +
               Political.Economy +
               industry_percentile +
               Science*Political.Economy +
               Science*Religion +
               Religion*Political.Economy +
               Science*industry_percentile +
               Political.Economy*industry_percentile +
               # Religion*industry_percentile +
               Science*Political.Economy*industry_percentile +
               Science*Religion*industry_percentile +
               Religion*Political.Economy*industry_percentile +
               i(bin, Science, 1610) +
               i(bin, Political.Economy, 1610) +
               i(bin, industry_percentile, 1610) +
               i(bin, Science*Religion, 1610) +
               i(bin, Science*Political.Economy, 1610) +
               i(bin, Political.Economy*Religion, 1610) +
               i(bin, Science*industry_percentile, 1610) +
               i(bin, Political.Economy*industry_percentile, 1610) +
               # i(bin, Religion*industry_percentile, 1610) +
               i(bin, Science*Political.Economy*industry_percentile, 1610) +
               i(bin, Science*Religion*industry_percentile, 1610) +
               i(bin, Religion*Political.Economy*industry_percentile, 1610) +
               i(bin, ref = 1610) - Religion - Religion*industry_percentile + industry_percentile,
             data = volumes)

# mod <- feols(optimism_percentile ~
#         i(bin, Science*Religion*industry_percentile, 1610) +
#         i(bin, Science*Political.Economy*industry_percentile, 1610) +
#         i(bin, Religion*Political.Economy*industry_percentile, 1610), data = volumes)

etable(mod)

#Get results

estimates <- tibble::rownames_to_column(mod$coeftable, "coefficient")

#Clean to make grouping by year and variable easier

#parse variable names into FEs and dep vars
estimates <- estimates %>%
  mutate(coefficient = str_remove(coefficient, "bin::")) %>%
  mutate(coefficient = str_replace(coefficient, '^([^0-9:]*):([^0-9:]*)$', '\\1 * \\2')) %>% #Format interactions correctly
  mutate(coefficient = str_replace(coefficient, '^([^0-9:]*):([^0-9:]*):([^0-9:]*)$', '\\1 * \\2 * \\3')) %>%
  mutate(split = strsplit(coefficient, ':')) %>%
  mutate(year = sapply(split, function(x) x[1]),
         variable = ifelse(sapply(split, length) == 2, sapply(split, function(x) x[2]), sapply(split, function(x) x[1]))) %>% #assign years and variables to correct columns
  select(-split)

#Fix some special cases
estimates <- estimates %>%
  mutate(variable = ifelse(is.na(as.numeric(variable)), variable, "(Intercept)")) %>%
  mutate(year = ifelse(is.na(as.numeric(year)), 'Reference', year)) %>%
  mutate(variable = ifelse(variable == 'Political.Economy * industry_percentile * Religion', 'Religion * Political.Economy * industry_percentile', variable)) %>%
  mutate(variable = ifelse(variable == 'Science * industry_percentile * Religion', 'Science * Religion * industry_percentile', variable))


#Reshape coefficients, std. errors, and pvalues into dfs

transform_estimates <- function(df, stat){
  transformed <- dcast(df, variable ~ year, value.var = stat)
  
  transformed <- transformed %>%
    relocate("Reference", .after = "variable") %>%
    arrange(str_length(variable), variable) %>%
    arrange(variable != "(Intercept)" & variable != "industry_percentile")
  
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

cm <- c("Political.Economy" = "PolitEcon",
        "industry_percentile" = "Industry",
        "Science * Religion" = "$\\text{Science} \\times \\text{Religion}$",
        "Science * industry_percentile" = "$\\text{Science} \\times \\text{Industry}$",
        "Religion:industry_percentile" = "$\\text{Religion} \\times \\text{Industry}$",
        "Science * Political.Economy" = "$\\text{Science} \\times \\text{PolitEcon}$",
        "Political.Economy * Religion" = "$\\text{Religion} \\times \\text{PolitEcon}$",
        "Science:Religion:industry_percentile" = "$\\text{Science} \\times \\text{Religion} \\times \\text{Industry}$",
        "Science:industry_percentile:Political.Economy" = "$\\text{Science} \\times \\text{PolitEcon} \\times \\text{Industry}$",
        "Religion:industry_percentile:Political.Economy" = "$\\text{Religion} \\times \\text{PolitEcon} \\times \\text{Industry}$")


modelsummary(models[1:8],
             stars = TRUE,
             coef_rename = cm,
             escape = FALSE,
             threeparttable = TRUE,
             title = "Dependent Variable: Optimism Percentile",
             output = "../output/industry_3/mod_industry_1.tex")

modelsummary(models[9:15],
             stars = TRUE,
             coef_rename = cm,
             escape = FALSE,
             threeparttable = TRUE,
             notes = "Volumes are placed into 20 year ((+/-) 10 year) bins. Columns represent interactions between bin fixed effects and the variables of interest (rows). Observations prior to 1600 are dropped. Independent variables are indicators that take on a value of one for the highest weighted category for each volume. Standard errors in parenthesis.",
             output = "../output/industry_3/mod_industry_3.tex")

modelsummary(models,
             stars = TRUE,
             coef_rename = cm,
             escape = FALSE,
             threeparttable = TRUE,
             notes = "Volumes are placed into 20 year ((+/-) 10 year) bins. Columns represent interactions between bin fixed effects and the variables of interest (rows). Observations prior to 1600 are dropped. Independent variables are indicators that take on a value of one for the highest weighted category for each volume. Standard errors in parenthesis.",
             output = "../output/industry_3/mod_industry_combined.tex")


modelsummary(models, stars = TRUE)

volumes$bin <- as.factor(volumes$bin)

bins <- as.factor(seq(1610, 1890, by = 20))


model <- lm(optimism_percentile ~ Religion * Science * industry_percentile * bin + Religion * Political.Economy * industry_percentile * bin + Science * Political.Economy * industry_percentile * bin - Religion * industry_percentile * bin + bin + bin * industry_percentile, volumes)

summary(mod)

summary(model)

#function for getting marginal effects
get_marginal_science <- function(model, s, r, p, ind){
  tmp <- model %>%
    margins(
      variables = "Science",
      at = list(Science = s, Religion = r, Political.Economy = p, industry_percentile = ind, bin = bins)
    ) %>%
    summary()
  
  return(tmp)
}

s100_0 <- get_marginal_science(model = model, s = 1, r = 0, p = 0, ind = 0)
s50r50_0 <- get_marginal_science(model = model, s = 0.5, r = 0.5, p = 0, ind = 0)
s50p50_0 <- get_marginal_science(model = model, s = 0.5, r = 0, p = 0.5, ind = 0)
thirds_0 <- get_marginal_science(model = model, s = 1/3, r = 1/3, p = 1/3, ind = 0)


s100_1 <- get_marginal_science(model = model, s = 1, r = 0, p = 0, ind = 1)
s50r50_1 <- get_marginal_science(model = model, s = 0.5, r = 0.5, p = 0, ind = 1)
s50p50_1 <- get_marginal_science(model = model, s = 0.5, r = 0, p = 0.5, ind = 1)
thirds_1 <- get_marginal_science(model = model, s = 1/3, r = 1/3, p = 1/3, ind = 1)

s100_0$label <- "100% Science"
s50r50_0$label <- "50% Science 50% Religion"
s50p50_0$label <- "50% Science 50% Political Economy"
thirds_0$label <- "1/3 Each"

s100_1$label <- "100% Science"
s50r50_1$label <- "50% Science 50% Religion"
s50p50_1$label <- "50% Science 50% Political Economy"
thirds_1$label <- "1/3 Each"


s100_0$bin <- bins
s50r50_0$bin <- bins
s50p50_0$bin <- bins
thirds_0$bin <- bins

s100_1$bin <- bins
s50r50_1$bin <- bins
s50p50_1$bin <- bins
thirds_1$bin <- bins

marginal_0 <- rbind(s100_0, s50r50_0, s50p50_0, thirds_0)

marginal_1 <- rbind(s100_1, s50r50_1, s50p50_1, thirds_1)


# export/import figs, takes a while to run the marginal effects

write.csv(marginal_0, "../temporary/industry_3/marginal_0.csv")
write.csv(marginal_1, "../temporary/industry_3/marginal_1.csv")

marginal_0 <- read.csv("../temporary/industry_3/marginal_0.csv")
marginal_1 <- read.csv("../temporary/industry_3/marginal_1.csv")

marginal_fig_0 <- ggplot(marginal_0, aes(x = bin, y = AME, group = label)) +
  geom_line(aes(color = label, linetype = label)) +
  geom_ribbon(aes(y = AME, ymin = lower, ymax = upper, fill = label), alpha = 0.2) +
  labs(title = "Marginal Effects (Ind = 0)", x = "Year", y = "Value") +
  theme(legend.position = "none")

show(marginal_fig_0)

marginal_fig_1 <- ggplot(marginal_1, aes(x = bin, y = AME, group = label)) +
  geom_line(aes(color = label, linetype = label)) +
  geom_ribbon(aes(y = AME, ymin = lower, ymax = upper, fill = label), alpha = 0.2) +
  labs(title = "Marginal Effects (Ind = 1)", x = "Year", y = "Value") +
  theme(legend.position = "none")

show(marginal_fig_1)
