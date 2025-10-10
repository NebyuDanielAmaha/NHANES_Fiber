library(haven)
library(dplyr)
library(svyVGAM)
library(survey)
library(stringr)
library(ggplot2)
library(tableone)
library(openxlsx)

#PART I RECODING VARIABLES
n_demo<- read_xpt("DEMO_L.xpt")
n_demo <- read_xpt("DEMO_L.xpt", col_select = c("SEQN", "RIAGENDR", "RIDAGEYR",
                                                "DMDMARTZ", "SDMVPSU", "SDMVSTRA","DMDHREDZ","DMDEDUC2"))

#gender
n_demo <- n_demo %>%
  mutate(
    gender = case_when(
      RIAGENDR == 1 ~ 1,  # Male
      RIAGENDR == 2 ~ 0,  # Female
      TRUE ~ NA_real_     # Missing values
    )
  )

#age category

n_demo <- n_demo %>%
  mutate(
    age_category = case_when(
      RIDAGEYR >= 20 & RIDAGEYR <= 34 ~ "20-34",
      RIDAGEYR >= 35 & RIDAGEYR <= 49 ~ "35-49",
      RIDAGEYR >= 50 & RIDAGEYR <= 65 ~ "50-65",
      RIDAGEYR > 65 ~ "65+",
      TRUE ~ NA_character_
    )
  )

#marital status
n_demo <- n_demo %>%
  mutate(
    marital_status = case_when(
      DMDMARTZ == 1 ~ "Married/living with partner",
      DMDMARTZ == 2 ~ "Widowed/Divorced/Separated",
      DMDMARTZ == 3 ~ "Never married",
      TRUE ~ NA_character_
    )
  )

# Prescription medications
n_medicine <- read_xpt("RXQ_RX_L.xpt", col_select = c("SEQN", "RXQ033"))
n_medicine <- n_medicine %>%
  mutate(
    prescribed_meds = case_when(
      RXQ033 == 1 ~ 1,        # YES
      RXQ033 == 2 ~ 0,       # NO
      TRUE ~ NA_real_        # Missing or invalid
    )
  )

# Income
n_income <- read_xpt("INQ_L.xpt", col_select = c("SEQN", "INDFMMPC"))
#recode
n_income <- n_income %>%
  mutate(
    income_category = case_when(
      INDFMMPC == 1 ~ "Low income",
      INDFMMPC == 2 ~ "Middle income",
      INDFMMPC == 3 ~ "High income",
      TRUE ~ NA_character_ # Refused (7), Don't know (9), Missing (.) become NA
    )
  )

#EXPOSURE VARIABLES
n_fiber_day1 <- read_xpt("DR1TOT_L.xpt", col_select = c("SEQN", "DR1TFIBE","WTDRD1"))
n_fiber_day2 <- read_xpt("DR2TOT_L.xpt", col_select = c("SEQN", "DR2TFIBE","WTDR2D"))


n_fiber <- n_fiber_day1 %>%
  left_join(n_fiber_day2, by = "SEQN") %>%
  mutate(
    fiber_grams = rowMeans(select(., DR1TFIBE, DR2TFIBE), na.rm = TRUE)  # average of both days
  )

#In Quartiles
quartile_cutoffs <- quantile(n_fiber$fiber_grams, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

#fiber quartiles
n_fiber <- n_fiber %>%
  mutate(
    fiber_quartile = case_when(
      fiber_grams <= quartile_cutoffs[1] ~ "Q1",
      fiber_grams > quartile_cutoffs[1] & fiber_grams <= quartile_cutoffs[2] ~ "Q2",
      fiber_grams > quartile_cutoffs[2] & fiber_grams <= quartile_cutoffs[3] ~ "Q3",
      fiber_grams > quartile_cutoffs[3] ~ "Q4",
      TRUE ~ NA_character_ # Handles missing fiber_grams values
    )
  )

# Body measures
n_bmi <- read_xpt("BMX_L.xpt",col_select = c("SEQN", "BMXBMI"))
#recode
n_bmi <- n_bmi %>%
  mutate(
    bmi_category = case_when(
      BMXBMI < 18.5 ~ "Underweight",
      BMXBMI >= 18.5 & BMXBMI < 25 ~ "Normal weight",
      BMXBMI >= 25 & BMXBMI < 30 ~ "Overweight",
      BMXBMI >= 30 ~ "Obese",
      TRUE ~ NA_character_
    )
  )

#binary overweight or not
n_bmi <- n_bmi %>%
  mutate(
    bmi_overweight = case_when(
      BMXBMI  < 25 ~ "0",
      BMXBMI >= 25 ~ "1",
      TRUE ~ NA_character_
    )
  )

# Vigorous physical activity
n_paq <- read_xpt("PAQ_L.xpt", col_select = c("SEQN", "PAD810Q", "PAD810U", "PAD820"))

n_paq <- n_paq %>%
  mutate(
    vigorous_active = case_when(
      # Zero frequency means no activity
      PAD810Q == 0 ~ 0,
      
      # Valid frequency + valid unit means active
      PAD810Q > 0 & PAD810U %in% c("D", "W", "M", "Y") ~ 1,
      
      # Refused / Don't know / Missing
      PAD810Q %in% c(7777, 9999) | is.na(PAD810Q) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

#Vigorous excercise WHO Guidelines
n_paq <- n_paq %>%
  mutate(
    vigorous_excercise = case_when(
      PAD810U == "W" & PAD810Q >= 3 & PAD820 >= 20 ~ 1,      # ≥3 times/week and ≥20 min/session
      PAD810U == "D" & PAD820 >= 20 ~ 1,                      # Daily vigorous ≥20 min
      PAD810Q == 0 ~ 0,                                       # No vigorous activity
      PAD810Q %in% c(7777, 9999) | is.na(PAD810Q) ~ NA_real_, # Missing or invalid
      TRUE ~ 0                                                # Other valid responses but below threshold
    ),
    vigorous_excercise = factor(
      vigorous_excercise,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

nhanes_sub <- nhanes_sub %>%
  left_join(n_paq %>% select(SEQN, vigorous_excercise), by = "SEQN")


# GENDER
nhanes_data <- nhanes_data %>%
  mutate(
    gender = case_when(
      RIAGENDR == 1 ~ "Male",
      RIAGENDR == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    gender = factor(gender, levels = c("Male", "Female"))
  )

#SLEEP DISORDER (based on hours slept)
n_sleep <- read_xpt("SLQ_L.xpt", col_select = c("SEQN", "SLD012"))

n_sleep <- n_sleep %>%
  mutate(
    sleep_disorder = case_when(
      SLD012 < 7 ~ "Short sleep (<7 hrs)",     # At risk
      SLD012 >= 7 ~ "Adequate sleep (≥7 hrs)", # No risk
      TRUE ~ NA_character_
    ),
    sleep_disorder = factor(sleep_disorder,
                            levels = c("Adequate sleep (≥7 hrs)", "Short sleep (<7 hrs)"))
  )

#PRESCRIBED MEDICATION USE
n_medicine <- read_xpt("RXQ_RX_L.xpt", col_select = c("SEQN", "RXQ033"))
n_medicine <- n_medicine %>%
  mutate(
    prescribed_meds = case_when(
      RXQ033 == 1 ~ "Yes",
      RXQ033 == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    prescribed_meds = factor(prescribed_meds, levels = c("No", "Yes"))
  )

#MARITAL STATUS
n_demo <- n_demo %>%
  mutate(
    marital_status = case_when(
      DMDMARTZ == 1 ~ "Married/living with partner",
      DMDMARTZ == 2 ~ "Widowed/Divorced/Separated",
      DMDMARTZ == 3 ~ "Never married",
      TRUE ~ NA_character_
    ),
    marital_status = factor(
      marital_status,
      levels = c("Married/living with partner",
                 "Widowed/Divorced/Separated",
                 "Never married")
    )
  )

#OUTCOME VARIABLE
n_phq <- read_xpt("DPQ_L.xpt", col_select = c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050", "DPQ060", "DPQ070", "DPQ080", "DPQ090"))
#sum it
n_phq <- n_phq %>%
  mutate(
    phq9_total_score = rowSums(dplyr::select(., starts_with("DPQ0")), na.rm = FALSE)
  )
#create a binary variable
n_phq <- n_phq %>%
  mutate(
    depressed_binary = case_when(
      phq9_total_score >= 10 ~ 1,  # 10 or more = 1 (Depressed)
      phq9_total_score < 10 ~ 0,   # Less than 10 = 0 (Not Depressed)
      TRUE ~ NA_real_              # Missing scores remain NA
    )
  )


#MERGE ALL THE DATA 

# Start with n_demo
nhanes_data <- n_demo %>%
  left_join(n_phq, by = "SEQN") %>%
  left_join(n_fiber, by = "SEQN") %>%
  left_join(n_crp, by = "SEQN") %>%
  left_join(n_paq, by = "SEQN") %>%
  left_join(n_smoking, by = "SEQN") %>%
  left_join(n_income, by = "SEQN") %>%
  left_join(n_sleep, by = "SEQN") %>%
  left_join(n_alq, by = "SEQN") %>%
  left_join(n_bmi, by = "SEQN") %>%
  left_join(n_medicine, by = "SEQN") %>% 
  # Filter for relevant population (e.g., adults aged 18+)
  filter(RIDAGEYR >= 20)


#OUTCOME VARIABLE CATEGORICAL

#Depression as ordinal
nhanes_sub <- nhanes_sub %>%
  mutate(
    depression_cat = case_when(
      phq9_total_score <= 4 ~ "None/minimal",
      phq9_total_score <= 9 ~ "Mild",
      phq9_total_score <= 14 ~ "Moderate",
      phq9_total_score <= 19 ~ "Moderately severe",
      phq9_total_score >= 20 ~ "Severe",
      TRUE ~ NA_character_
    ),
    depression_cat = factor(
      depression_cat,
      levels = c("None/minimal", "Mild", "Moderate", "Moderately severe", "Severe")
    )
  )

