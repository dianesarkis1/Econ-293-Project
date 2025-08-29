library(haven)
setwd("")
data <- read_dta("Replication_Dataset.dta")

Y <- data$vote_lega_euro
W <- data$dummy_euro_4

covariates_original <- c("age", "eco_mode", "vote_lega_municipal", "EDU1", "EDU2", "EDU3",
                         "profile_gross_personal_eu", "everyweek", "female", "gov_firms_responsibility",
                         "green_policies_positive", "climate_neutrality", "km_1k_to_5k", "km_5k_to_10k",
                         "km_10k_to_20k", "km_20k_to_30k", "km_less_1k", "km_more_30k", "pay_eco_friendly", 
                         "recycled_materials", "taxes_eco_friendly", "use_month", "use_week", "use_year",
                         "water_bottle")
covariates <- c("age", "factor(eco_mode)", "vote_lega_municipal", "EDU1", "EDU2", "EDU3",
                "factor(profile_gross_personal_eu)", "everyweek", "female", "gov_firms_responsibility",
                "green_policies_positive", "climate_neutrality", "km_1k_to_5k", "km_5k_to_10k",
                "km_10k_to_20k", "km_20k_to_30k", "km_less_1k", "km_more_30k", "pay_eco_friendly", 
                "factor(recycled_materials)", "taxes_eco_friendly", "use_month", "use_week", "use_year",
                "water_bottle")

attr(data$profile_gross_personal_eu, which = "class") <- NULL

XX <- model.matrix(formula(paste0("~", paste0(covariates, collapse="+"))), data=data, drop.intercept=TRUE)
XX <- XX[, -1]

library(devtools) 
install_github("xnie/rlearner", force=TRUE)
library(rlearner)

remove.packages("vctrs")
install.packages("vctrs")
library(vctrs)

rlasso_fit = rlasso(XX, W, Y)
rlasso_est = predict(rlasso_fit, XX)



