library(haven)
library(boot)
library(marginaleffects)

#setwd("")
df <- read_dta("Replication_Dataset.dta")


# f <- vote_lega_euro ~ dummy_euro_4 + age + I(age * age) + factor(eco_mode) + EDU1 + EDU2 + EDU3 + EDU4 +
#   climate_neutrality + dummy_buy + dummy_donation + dummy_genitori_click + dummy_newsletter +
#   dummy_podcast + dummy_social + dummy_watch_video + dummy_zeroco2_click + factor(profile_gross_personal_eu) +
#   everyweek + female + gov_firms_responsibility + green_policies_positive + km_1k_to_5k + km_5k_to_10k + 
#   km_10k_to_20k + km_20k_to_30k + km_less_1k + km_more_30k + pay_eco_friendly + factor(recycled_materials) +
#   factor(short_shower) + taxes_eco_friendly + ten_km + use_day + use_month + use_week + use_year +
#   water_bottle + work_everyweek + workcar_day + workcar_month + workcar_week + workcar_year
# 
# fit <- glm(f, data = df)
# 
# avg_comparisons(fit, variables = list(dummy_euro_4 = 0:1))


f2 <- vote_lega_euro ~ diesel_euro4_ass + age + factor(eco_mode) + vote_lega_municipal + EDU1 + EDU2 + EDU3 +
  factor(profile_gross_personal_eu) + everyweek + female +
  gov_firms_responsibility + green_policies_positive + climate_neutrality +
  km_1k_to_5k + km_5k_to_10k + km_10k_to_20k + km_20k_to_30k +
  km_less_1k + km_more_30k + pay_eco_friendly +
  factor(recycled_materials) + taxes_eco_friendly +
  use_month + use_week + use_year + water_bottle + dummy_buy + dummy_donation + dummy_genitori_click + dummy_newsletter +
  dummy_podcast + dummy_social + dummy_watch_video + dummy_zeroco2_click

fit2 <- glm(f2, data = df, family=binomial)

avg_comparisons(fit2, variables = list(diesel_euro4_ass = 0:1))

