#Meant to be run after data preparation.R

#Install Packages, uncomment first 2 lines
  #install.packages("devtools")  # if you don't have this installed yet.
  #devtools::install_github('susanathey/causalTree')

# Section 0: Import Data, Clean ----------------------------------------------------------

library(lmtest)
library(causalTree)
library(plm)
library(ggplot2)

#Fix covariate list since looping over it, add dummy variables:
covariates <- c("age", "`factor(eco_mode)2`", "`factor(eco_mode)3`", "`factor(eco_mode)4`", "`factor(eco_mode)5`", "vote_lega_municipal", "EDU1", "EDU2", "EDU3",
                "INC1", "INC2", "INC3", "INC4", "INC5", "INC6", "INC7", "INC8", "INC9", "INC10", "INC11", "INC12",
                "INC13", "INC14", "INC15", "INC16","everyweek", "female", "gov_firms_responsibility",
                "green_policies_positive", "climate_neutrality", "km_1k_to_5k", "km_5k_to_10k",
                "km_10k_to_20k", "km_20k_to_30k", "km_less_1k", "km_more_30k", "pay_eco_friendly", 
                "`factor(recycled_materials)2`", "`factor(recycled_materials)3`", "`factor(recycled_materials)4`", "`factor(recycled_materials)5`", "taxes_eco_friendly", "use_month", "use_week", "use_year",
                "water_bottle")
extra_covariates_factor <- c("`factor(eco_mode)2`", "`factor(eco_mode)3`", "`factor(eco_mode)4`", "`factor(eco_mode)5`",
                             "`factor(recycled_materials)2`", "`factor(recycled_materials)3`", "`factor(recycled_materials)4`", "`factor(recycled_materials)5`")

train_data = as.data.frame(cbind(X_train,W_train,Y_train))
test_data = as.data.frame(cbind(X_test,W_test,Y_test))


fmla <- paste("Y_train", "~", paste(covariates, collapse = " + "))
head(fmla)


# 2 subset split of training into split and estimates for creating causal tree and then checking it:
indices = split(seq(nrow(train_data)), sort(seq(nrow(train_data)) %% 2))
names(indices) = c('split', 'est')
  # one to max heterogeneity, one to estimate, and one to test validity of.

#split into 50/50 split and est from training, both with certain number of control and treated samples
new_treatment_data <- train_data[train_data$W_train == 1, ]
new_control_data <- train_data[train_data$W_train== 0, ]

treatment_split <- new_treatment_data[sample(nrow(new_treatment_data), floor(0.5 * nrow(new_treatment_data))), ]
treatment_est <- new_treatment_data[!(rownames(new_treatment_data) %in% rownames(treatment_split)), ]
control_split <- new_control_data[sample(nrow(new_control_data), floor(0.5 * nrow(new_control_data))), ]
control_est <- new_control_data[!(rownames(new_control_data) %in% rownames(control_split)), ]

# Merge train datasets of treatment and control groups
split_data <- rbind(treatment_split, control_split)

# Merge test datasets of treatment and control groups
est_data <- rbind(treatment_est, control_est)


# Section 1 Fit Causal Tree, Heatmap of Covariate Values per leaf and split -------------------------------------------

# Fit tree
ct.unpruned <- honest.causalTree(
  formula=fmla,            # Define the model
  data=split_data,
  treatment=split_data$W_train,  
  est_data=est_data,
  est_treatment=est_data$W_train,
  minsize=1,                 # Min. number of treatment and control cases in each leaf
  HonestSampleSize=nrow(est_data), #  Num obs used in estimation after splitting
  # We recommend not changing the parameters below
  split.Rule="CT",            # Define the splitting option
  cv.option="TOT",            # Cross validation options
  cp=0,                       # Complexity parameter
  split.Honest=TRUE,          # Use honesty when splitting
  cv.Honest=TRUE              # Use honesty when performing cross-validation
)

ct.unpruned

ct.cptable <- as.data.frame(ct.unpruned$cptable)


cp.selected <- which.min(ct.cptable$xerror)
cp.optimal <- ct.cptable[cp.selected, "CP"]

# Prune the tree at optimal complexity parameter.
ct.pruned <- prune(tree=ct.unpruned, cp=cp.optimal)

# Predict point estimates (on estimation sample)
tau.hat.est <- predict(ct.pruned, newdata=est_data)

# Create a factor column 'leaf' indicating leaf assignment in the estimation set
num.leaves <- length(unique(tau.hat.est))
leaf <- factor(tau.hat.est, levels=sort(unique(tau.hat.est)), labels = seq(num.leaves))

#Doing the same as above for our unpruned tree, then unpruned tau.hat.est
  # un.tau.hat.est = predict(ct.unpruned, newdata=data[indices$est,])
  # num.leaves2 = length(unique(un.tau.hat.est))
  # leaf2 = factor(un.tau.hat.est, levels=sort(unique(un.tau.hat.est)), labels = seq(num.leaves2))


rpart.plot(
  x=ct.pruned,      # Pruned tree do ct.pruned, else ct.unpruned
  type=3,             # Draw separate split labels for the left and right directions
  fallen=TRUE,        # Position the leaf nodes at the bottom of the graph
  leaf.round=1,       # Rounding of the corners of the leaf node boxes
  extra=100,          # Display the percentage of observations in the node
  branch=.1,          # Shape of the branch lines
  box.palette="GnPu") # Palette for coloring the node



#Table of covariate values per leaf:

# this works : print(sd(as.data.frame(lapply(data[,2], as.numeric))[,1]))

df <- mapply(function(covariate) {
  # Looping over covariate names
  # Compute average covariate value per leaf (with correct standard errors)
  fmla <- formula(paste0(covariate, "~ 0 + leaf"))
  ols <- lm(fmla, data=transform(est_data, leaf=leaf))
  ols.res <- coeftest(ols, vcov=vcovHC(ols, "HC2"))
  
  # Retrieve results
  avg <- as.numeric(ols.res[,1])
  print(as.numeric(ols.res[,1]))
  stderr <- as.numeric(ols.res[,2])
  print(stderr)

  # Tally up results
  # Change num.leaves to num.leaves2 for UNPRUNED TREE
  data.frame(covariate, avg, stderr, leaf=paste0("Leaf", seq(num.leaves)),
             # Used for coloring
             scaling=pnorm((avg - mean(avg))/sd(avg)),
             # We will order based on how much variation is 'explain' by the averages
             # relative to the total variation of the covariate in the data
              variation=sd(avg) / sd(train_data[,covariate]),
               #sd(as.data.frame(lapply(data[,covariate], as.numeric))[,1]),
             # String to print in each cell in heatmap below
             labels=paste0(signif(avg, 3), "\n", "(", signif(stderr, 3), ")"))
}, covariates[!(covariates %in% extra_covariates_factor)], SIMPLIFY = FALSE)

df <- do.call(rbind, df)

# a small optional trick to ensure heatmap will be in decreasing order of 'variation'
df$covariate <- reorder(df$covariate, order(df$variation))

# plot heatmap
heatmap_HTE = ggplot(df) +
  aes(leaf, covariate) +
  geom_tile(aes(fill = scaling)) + 
  geom_text(aes(label = labels)) +
  scale_fill_gradient(low = "#E1BE6A", high = "#40B0A6") +
  ggtitle(paste0("Average covariate values within each leaf")) +
  theme_minimal() + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=11))

ggsave("test_plot.png", heatmap_HTE, height = 20)



# Section 3 -- Causal Forest, then Detection? of Heterogeneity, then RATE curve


  #3.3 - Rate Curve: y-axis: test causal forest predictions, x-axis is trained causal forest predictions





# Section 4 -- Best Linear Projections