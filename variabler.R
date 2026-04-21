library(tidyverse)
source("data_cleaning.R")

# Istället för att göra prognoser på alla åldrar som finns representerade 
# i datan så skapar vi relevanta åldergrupper
insurance_clean <- insurance_clean %>%
  mutate(age_group = case_when(
    age < 30 ~ "under 30",
    age >= 30 & age < 40 ~ "30-40",
    age >= 40 & age < 55 ~ "40-55",
    TRUE ~ "55+"
  ))

insurance_clean %>% count(age_group)

# Barn eller inte. Lättare att titta på barn eller inte barn, 
# snarare än antalet barn
insurance_clean %>% count(children)

insurance_clean <- insurance_clean %>%
  mutate(has_children = ifelse(children > 0, "yes", "no"))

#Historiska olyckor och utbetalningar
insurance_clean %>% count(prior_accidents)
insurance_clean %>% count(prior_claims)

# Ett viktat riskindex, där historiska utbetalningar väger dubbelt så mycket 
#som olyckshistoriken
insurance_clean %>%
  mutate(history_risk = prior_accidents * 1 + prior_claims * 2) %>%
  count(history_risk)

# Utifrån detta skapar vi fyra kategorier
insurance_clean <- insurance_clean %>%
  mutate(history_risk = case_when(
    prior_accidents * 1 + prior_claims * 2 == 0 ~ "none",
    prior_accidents * 1 + prior_claims * 2 <= 2 ~ "low",
    prior_accidents * 1 + prior_claims * 2 <= 4 ~ "medium",
    TRUE ~ "high"
  ))

insurance_clean %>% count(history_risk)

#BMI
summary(insurance_clean$bmi)

# Skapar grupper utifrån Folkhälsomyndighetens 
# grupperingar https://www.folkhalsomyndigheten.se/vara-amnesomraden/overvikt-och-obesitas/overvikt-och-obesitas-i-befolkningen/
insurance_clean <- insurance_clean %>%
  mutate(bmi_category = case_when(
    bmi < 18.5 ~ "underweight",
    bmi < 25 ~ "normal",
    bmi < 30 ~ "overweight",
    TRUE ~ "obese"
  ))








