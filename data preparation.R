library(haven)
#setwd("")
data <- read_dta("Replication_Dataset.dta")

Y <- data$vote_lega_euro
W <- data$diesel_euro4_ass

covariates <- c("age", "factor(eco_mode)", "vote_lega_municipal", "EDU1", "EDU2", "EDU3",
                "INC1", "INC2", "INC3", "INC4", "INC5", "INC6", "INC7", "INC8", "INC9", "INC10", "INC11", "INC12",
                "INC13", "INC14", "INC15", "INC16","everyweek", "female", "gov_firms_responsibility",
                "green_policies_positive", "climate_neutrality", "km_1k_to_5k", "km_5k_to_10k",
                "km_10k_to_20k", "km_20k_to_30k", "km_less_1k", "km_more_30k", "pay_eco_friendly", 
                "factor(recycled_materials)", "taxes_eco_friendly", "use_month", "use_week", "use_year",
                "water_bottle")

X <- model.matrix(formula(paste0("~", paste0(covariates, collapse="+"))), data=data)
X <- X[, -1]


#Note that INC15 is a dummy if income question does not apply, and INC16 is prefer not to respond

# train-test split
# Separate treatment and control groups
treatment_data <- data[data$diesel_euro4_ass == 1, ]
control_data <- data[data$diesel_euro4_ass == 0, ]

# Set seed for reproducibility
set.seed(42)

# Split treatment group into train and test datasets
treatment_train <- treatment_data[sample(nrow(treatment_data), floor(0.7 * nrow(treatment_data))), ]
treatment_test <- treatment_data[!(rownames(treatment_data) %in% rownames(treatment_train)), ]

# Split control group into train and test datasets
control_train <- control_data[sample(nrow(control_data), floor(0.7 * nrow(control_data))), ]
control_test <- control_data[!(rownames(control_data) %in% rownames(control_train)), ]

# Merge train datasets of treatment and control groups
train_data <- rbind(treatment_train, control_train)

# Merge test datasets of treatment and control groups
test_data <- rbind(treatment_test, control_test)

Y_train <- train_data$vote_lega_euro
W_train <- train_data$diesel_euro4_ass
X_train <- model.matrix(formula(paste0("~", paste0(covariates, collapse="+"))), data=train_data)
X_train <- X_train[, -1]

Y_test <- test_data$vote_lega_euro
W_test <- test_data$diesel_euro4_ass
X_test <- model.matrix(formula(paste0("~", paste0(covariates, collapse="+"))), data=test_data)
X_test <- X_test[, -1]
