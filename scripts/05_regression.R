library(tidyverse)
library(here)
source(here("scripts", "04_analys.R"))

# Multipel modell regression
# Vi tittar på smoker, chronic_condition, age, prior_accidents

model_1 <- lm(charges ~ smoker + chronic_condition + age + prior_accidents, 
             data = insurance_clean)
summary(model_1)

# Modellen (model 1) förklarar 68% av variationerna i kostnader. 
# De fyra variablerna vi tittade på är alla relevanta med p-värden på <0.05
# Kostnaden för rökare är i genomsnitt 7,482 kr högre än för en icke-rökare
# För de med kroniska sjukdomar ligger kostnaden 4,088 kr högre än för de utan sjukdomshistorik
# De med tidigare olyckor kostar 1,267 kr mer än de utan tidigare olyckor
# Och för varje år, ökar en persons kostnad med 72 kr. 

model_2 <- lm(charges ~ smoker + chronic_condition + age + prior_accidents +
                exercise_level + sex + annual_checkups, 
              data = insurance_clean)
summary(model_2)

# Modell 2 tar även med träning, kön och hälsokontroller
# Denna modell förklarar 70% av variationerna i kostander. 
# Men kön och hälsokontroller verkar ha liten eller ingen påverkan  (litet satistiskt stöd).

model_3 <- lm(charges ~ smoker + chronic_condition + age + prior_accidents +
                exercise_level, 
              data = insurance_clean)
summary(model_3)

# Modell 3 förklarar också 70% av variationerna i kostnader
# men med färre variabler

