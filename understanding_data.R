# Du arbetar som analytiker på ett försäkringsbolag. Företaget har historiska data över 
#kunder och kostnader, men vill bättre förstå om prissättningen verkar följa rimliga 
#mönster och vilka faktorer som driver kostnader mest. De vill också undersöka om en 
#regressionsmodell kan användas som stöd vid framtida prissättning av liknande kunder.

library(tidyverse)

# Läs in data
insurance <- read_csv("insurance_costs.csv")

# Visa datasetets storlek
dim(insurance)
glimpse(insurance)

# Datasettet har 1100 rader och 14 kolumner
# 7 kategoriska, 7 doubles

# Hitta saknade värden
colSums(is.na(insurance))

# excercise_level: 22 saknade värden
# bmi: 28 saknade värden
# annual_checkups: 20 saknade värden

#Innan jag tar beslut kring hur dessa saknade värdena ska hanteras
# så vill jag ta reda på lite mer om dem

# Skapa en indikator för saknade värden
insurance %>%
  filter(is.na(bmi)) %>%
  count(sex, age) %>%
  print(n = Inf)

insurance %>%
  filter(is.na(bmi)) %>%
  count(sex)

insurance %>% count(sex)
# Så 18 saknade bmi värden för kvinnor och 10 för män
# jämfört med fördelningen på hela datasetet med 
# 551 females och 549 males

insurance %>%
  filter(is.na(exercise_level)) %>%
  count(sex, age) %>%
  print(n = Inf)

insurance %>%
  filter(is.na(annual_checkups)) %>%
  count(sex, age) %>%
  print(n = Inf)

insurance %>%
  filter(is.na(bmi)) %>%
  count(annual_checkups)

# Kan inte se något mönster kring saknade värden för 
# träning eller för hälsokontroller

# Kolla unika värden ikategoriska variabler
insurance %>% count(region)
insurance %>% count(sex)
insurance %>% count(smoker)
insurance %>% count(chronic_condition)
insurance %>% count(exercise_level)
insurance %>% count(plan_type)

# region behöver städas - inkonsekvent stora/små bokstäver
# smoker behöver städas - inkonsekvent stora/små bokstäver
# plan_type behöver städas - inkonsekvent stora/små bokstäver

# kan vara bra att konvertera age, children, prior_accidents, prior_claims and annual_checkups
# till heltal

