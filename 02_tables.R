
#Table 1

# Remove rows with missing dietary weights
nhanes_sub <- nhanes_sub[!is.na(nhanes_sub$WTDRD1), ]

# Define survey design
nhanes_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~ WTDRD1,
  nest = TRUE,
  data = nhanes_sub
)


vars <- c("age_category", "gender","marital_status", "income_category",
          "bmi_category","vigorous_excercise" ,"sleep_disorder",
          "prescribed_meds", "depression_cat")
nhanes_sub <- nhanes_sub %>%
  left_join(n_paq %>% select(SEQN, vigorous_excercise), by = "SEQN")

table1_unweighted <- CreateTableOne(vars = vars, data = nhanes_sub)
table1_df <- as.data.frame(print(table1_unweighted, showAllLevels = TRUE))
# write.xlsx(table1_df, file = "Table1_Descriptives.xlsx", overwrite = TRUE)

# -----------------------------
# Table 2: Crude Odds Ratios
# -----------------------------
predictors <- c("fiber_grams", "gender", "age_category",
                "marital_status", "prescribed_meds", "sleep_disorder",
                "vigorous_excercise", "bmi_overweight", "income_category")

crude_list <- list()

for (var in predictors) {
  
  formula <- as.formula(paste("depression_cat ~", var))
  fit <- svyolr(formula, design = nhanes_design)
  
  coef_pred <- coef(fit)
  ci <- confint(fit)
  se <- sqrt(diag(vcov(fit)))
  z <- coef_pred / se
  pval <- 2 * (1 - pnorm(abs(z)))
  
  keep <- !grepl("\\|", names(coef_pred))
  
  df <- data.frame(
    Variable = names(coef_pred)[keep],
    OR = exp(coef_pred[keep]),
    Lower95CI = exp(ci[keep, 1]),
    Upper95CI = exp(ci[keep, 2]),
    p_value = pval[keep]
  )
  
  crude_list[[var]] <- df
}

crude_results <- bind_rows(crude_list) %>%
  mutate(Model = "Crude")

# -----------------------------
# Table 2: Adjusted Odds Ratios
# -----------------------------
nhanes_sub <- nhanes_sub %>%
  mutate(
    depression_cat = factor(
      depression_cat,
      levels = c("None/minimal", "Mild", "Moderate", "Moderately severe", "Severe"),
      ordered = TRUE
    )
  )

nhanes_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTDRD1,
  nest = TRUE,
  data = nhanes_sub
)

fit_svy_ordinal <- svyolr(
  depression_cat ~ fiber_grams + age_category + gender + marital_status + prescribed_meds +
    sleep_disorder + vigorous_excercise + bmi_overweight + income_category,
  design = nhanes_design
)

coef <- coef(fit_svy_ordinal)
se <- summary(fit_svy_ordinal)$coefficients[, "Std. Error"]
p_value <- 2 * (1 - pnorm(abs(coef / se)))

adjusted_results <- data.frame(
  Variable = names(coef),
  OR = exp(coef),
  Lower95CI = exp(coef - 1.96 * se),
  Upper95CI = exp(coef + 1.96 * se),
  p_value = p_value,
  Model = "Adjusted"
)

# -----------------------------
# Combine and export both tables
# -----------------------------

# Add both tables as separate sheets
# addWorksheet(wb, "Crude_ORs")
# writeData(wb, "Crude_ORs", crude_results)
# 
# addWorksheet(wb, "Adjusted_ORs")
# writeData(wb, "Adjusted_ORs", adjusted_results)
# 
# # Save to Excel file
# saveWorkbook(wb, "Table2_Crude_Adjusted.xlsx", overwrite = TRUE)


# # Create survey design object
# nhanes_design <- svydesign(
#   id = ~SDMVPSU,
#   strata = ~SDMVSTRA,
#   weights = ~WTDRD1,
#   nest = TRUE,
#   data = nhanes_sub
# )

# -----------------------------
# Table 3: Interaction terms
# -----------------------------

# Define survey design
nhanes_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTDRD1,
  nest = TRUE,
  data = nhanes_sub
)

#Test for interaction with gender
fit_gender_interaction <- svyolr(
  depression_cat ~ fiber_grams * gender + RIDAGEYR + prescribed_meds +
    sleep_disorder + vigorous_excercise + bmi_overweight + income_category,
  design = nhanes_design
)

summary(fit_gender_interaction)


#Test for interaction with income
fit_income_interaction <- svyolr(
  depression_cat ~ fiber_grams * income_category + RIDAGEYR + gender +
    prescribed_meds + sleep_disorder + vigorous_excercise + bmi_overweight,
  design = nhanes_design
)

summary(fit_income_interaction)

#Test for interaction with sleep
fit_sleep_interaction <- svyolr(
  depression_cat ~ fiber_grams * sleep_disorder  + RIDAGEYR + gender +
    prescribed_meds + income_category + vigorous_excercise +bmi_overweight,
  design = nhanes_design
)

summary(fit_sleep_interaction)

#Test for interaction with vigorous excercise
fit_sport_interaction <- svyolr(
  depression_cat ~ fiber_grams * vigorous_excercise  + RIDAGEYR + gender +
    prescribed_meds + income_category + sleep_disorder +bmi_overweight,
  design = nhanes_design
)

summary(fit_sport_interaction)

#Get the OR and p-values
# Function to get OR, 95% CI, and p-value
get_OR_interaction_p <- function(model) {
  coefs <- coef(model)
  ci <- confint(model)
  tval <- coefs / sqrt(diag(vcov(model)))  # t = estimate / SE
  pval <- 2 * (1 - pnorm(abs(tval)))      # two-sided p-value
  
  data.frame(
    Variable = names(coefs),
    OR = exp(coefs),
    Lower95CI = exp(ci[,1]),
    Upper95CI = exp(ci[,2]),
    p_value = pval
  )
}

#Gender interaction
get_OR_interaction_p(fit_gender_interaction)

#Income interaction
get_OR_interaction_p(fit_income_interaction)

#Sleep interaction
get_OR_interaction_p(fit_sleep_interaction)

#Sport interaction
get_OR_interaction_p(fit_sport_interaction)


#Extract and save it in excel
# Function to extract only interaction terms (those with ":")
get_interactions_only <- function(model, label) {
  df <- get_OR_interaction_p(model)
  df <- df[grepl(":", df$Variable), ]  # keep only interaction terms
  df$Model <- label
  df[, c("Model", "Variable", "OR", "Lower95CI", "Upper95CI", "p_value")]
}

# Apply to each model
gender_int <- get_interactions_only(fit_gender_interaction, "Fiber × Gender")
income_int <- get_interactions_only(fit_income_interaction, "Fiber × Income")
sleep_int  <- get_interactions_only(fit_sleep_interaction, "Fiber × Sleep disorder")
sport_int  <- get_interactions_only(fit_sport_interaction, "Fiber × Vigorous exercise")

# Combine all into one data frame
table3_results <- bind_rows(gender_int, income_int, sleep_int, sport_int)

# Print results rounded to 3 decimals
print(table3_results, digits = 3)

# Export to Excel
# write.xlsx(table3_results, file = "Table3_Interaction_ORs.xlsx", overwrite = TRUE)

# -----------------------------
# Table 4: Subgroup Analysis
# -----------------------------

#create a male model
fit_svy_male <- svyolr(
  depression_cat ~ fiber_grams + age_category + marital_status +
    prescribed_meds + sleep_disorder + vigorous_excercise + bmi_overweight + income_category,
  design = subset(nhanes_design, gender == "Male")
)

#create a female model
fit_svy_female <- svyolr(
  depression_cat ~ fiber_grams + age_category + marital_status +
    prescribed_meds + sleep_disorder + vigorous_excercise + bmi_overweight + income_category,
  design = subset(nhanes_design, gender == "Female")
)

summary(fit_svy_male)
summary(fit_svy_female)


# Function to extract OR, CI, and p-values
get_OR_p <- function(fit) {
  coef_est <- coef(fit)
  se <- sqrt(diag(vcov(fit)))          # Standard errors
  z_val <- coef_est / se               # Wald z-values
  p_val <- 2 * (1 - pnorm(abs(z_val))) # Two-sided p-values
  ci <- confint(fit)
  
  data.frame(
    Variable = names(coef_est),
    OR = exp(coef_est),
    Lower95CI = exp(ci[,1]),
    Upper95CI = exp(ci[,2]),
    p_value = p_val
  )
}

# Male subgroup
results_male <- get_OR_p(fit_svy_male)
results_male

# Female subgroup
results_female <- get_OR_p(fit_svy_female)
results_female

# write.xlsx(results_male, file = "Table4_male.xlsx", overwrite = TRUE)
# write.xlsx(results_female, file = "Table4_female.xlsx", overwrite = TRUE)



#Table 5 Sensitivity Analysis



# # Binary outcome: Depression (≥10)
nhanes_sub <- nhanes_sub[!is.na(nhanes_sub$WTDRD1), ]

nhanes_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTDRD1,
  nest = TRUE,
  data = nhanes_sub
)

# Fit modified Poisson model
poisson_model <- svyglm(
  depressed_binary ~ fiber_grams + age_category + gender + marital_status + 
    prescribed_meds + sleep_disorder + vigorous_excercise + 
    bmi_overweight + income_category,
  design = nhanes_design,
  family = quasipoisson(link = "log")
)

# Extract coefficients, CIs, p-values
summary(poisson_model)

# Get PRs and 95% CIs
coef_est <- coef(poisson_model)
se <- sqrt(diag(vcov(poisson_model)))
z <- coef_est / se
p_val <- 2 * (1 - pnorm(abs(z)))
ci_lower <- exp(coef_est - 1.96 * se)
ci_upper <- exp(coef_est + 1.96 * se)

results_poisson <- data.frame(
  Variable = names(coef_est),
  PR = exp(coef_est),
  Lower95CI = ci_lower,
  Upper95CI = ci_upper,
  p_value = p_val
)

# View fiber effect
subset(results_poisson)
write.xlsx(results_poisson, file = "Table5_sensitivity.xlsx", overwrite = TRUE)

