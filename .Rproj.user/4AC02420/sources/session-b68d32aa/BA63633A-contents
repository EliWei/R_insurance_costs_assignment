library(tidyverse)
source("understanding_data.R")

#Börjar med att städa de kategoriksa värdena 
insurance <- insurance %>%
  mutate(
    region = str_to_lower(region),
    smoker = str_to_lower(smoker),
    plan_type = str_to_lower(plan_type)
  )

insurance %>% count(region)
insurance %>% count(smoker)
insurance %>% count(plan_type)

# Konvertera age, children, prior_accidents, prior_claims and annual_checkups till heltal
insurance <- insurance %>%
  mutate(across(c(age, children, prior_accidents, prior_claims, annual_checkups), as.integer))

glimpse(insurance)

#Hantera saknade värden
# excercise_level: 22 saknade värden
# bmi: 28 saknade värden
# annual_checkups: 20 saknade värden

# För saknade värden i BMI tar vi mean för kvinnor och män respektive
insurance_clean <- insurance %>%
  group_by(sex) %>%
  mutate(bmi = ifelse(is.na(bmi), mean(bmi, na.rm = TRUE), bmi)) %>%
  ungroup()

# För saknade värden i annual_checkups så tar vi medianen (det är ju ett heltal)
# för kvinnor respektive män
insurance_clean <- insurance_clean %>%
  group_by(sex) %>%
  mutate(annual_checkups = ifelse(is.na(annual_checkups), 
                                  median(annual_checkups, na.rm = TRUE), 
                                  annual_checkups)) %>%
  ungroup()

# Excercise_level är kategorisk. Här skulle vi kunna hitta de vanligaste
# nivåerna och använda dem för att fylla i saknade värden
# Men iställer väljer vi att ta bort de 22 raderna med saknade värden
  insurance_clean <- insurance_clean %>%
  filter(!is.na(exercise_level))

# Kontrollera att inga saknade värden finns kvar
colSums(is.na(insurance_clean))


#
#
#





