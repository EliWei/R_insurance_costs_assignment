library(tidyverse)
library(here)
source(here("scripts", "03_variabler.R"))

# Undersöka:
# hur kostnaderna är fördelade
# vilka variabler som verkar intressanta att undersöka vidare
# om det finns tydliga skillnader mellan grupper

# Fördelning av försäkringskostnader
# Vi ser att det finns en snedfördelning åt höger med outliers
# vilket drar upp medelvärdet (mean)
p1 <- ggplot(insurance_clean, aes(x = charges)) +
  geom_histogram(bins = 50) +
  labs(title = "Fördelning av försäkringskostnader",
       x = "Kostnad",
       y = "Antal")

ggsave(here("output", "figures", "01_fördelning_kostnader.png"), plot = p1, width = 8, height = 5, dpi = 300)

summary(insurance_clean$charges)

# Rökning och kostnader
# Gruppen rökare har högre kostnader än icke-rökare.
# Mediankostnaden är dubbelt så hög för rökare ön för icke-rökare
p2 <- ggplot(insurance_clean, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() +
  labs(title = "Försäkringskostnader per rökarstatus",
       x = "Rökare",
       y = "Kostnad")

insurance_clean %>%
  group_by(smoker) %>%
  summarise(
    median_charges = median(charges),
    mean_charges = mean(charges)
  )

ggsave(here("output", "figures", "02_rokning.png"), plot = p2, width = 8, height = 5, dpi = 300)

# Kronisk sjukdom
# Gruppen kroniskt sjuka har högre kostnader än de utan kronisk sjukdom
# Mediankostnaden är ca 50% högre för kroniskt sjuka ön för andra
p3 <- ggplot(insurance_clean, aes(x = chronic_condition, y = charges, fill = chronic_condition)) +
  geom_boxplot() +
  labs(title = "Försäkringskostnader per kronisk sjukdom",
       x = "Kronisk sjukdom",
       y = "Kostnad")

insurance_clean %>%
  group_by(chronic_condition) %>%
  summarise(
    median_charges = median(charges),
    mean_charges = mean(charges)
  )

ggsave(here("output", "figures", "03_kronisk_sjukdom.png"), plot = p3, width = 8, height = 5, dpi = 300)


#BMI
#BMI-kategori verkar ha en svagare koppling till kostnader än rökning och kronisk sjukdom.
#Värt att notera built in bias för BMI beräkning (see diskussion i rapporten)
p4 <- ggplot(insurance_clean, aes(x = bmi_category, y = charges, fill = bmi_category)) +
  geom_boxplot() +
  labs(title = "Försäkringskostnader per BMI-kategori",
       x = "BMI-kategori",
       y = "Kostnad")

insurance_clean %>%
  group_by(bmi_category) %>%
  summarise(
    median_charges = median(charges),
    mean_charges = mean(charges)
  )

ggsave(here("output", "figures", "04_bmi.png"), plot = p4, width = 8, height = 5, dpi = 300)

#Age
#Man kan tydligt see att kostnaderna äker med åldern
#tydligast blir det när vi tittar på åldern som en kontinuerlig variabel
#Kontinuerligt
p5 <- ggplot(insurance_clean, aes(x = age, y = charges)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  labs(title = "Försäkringskostnader per ålder",
       x = "Ålder",
       y = "Kostnad")
ggsave(here("output", "figures", "05_alder_kontinuerligt.png"), plot = p5, width = 8, height = 5, dpi = 300)

#åldergruppering
insurance_clean <- insurance_clean %>%
  mutate(age_group = factor(age_group, 
                            levels = c("under 30", "30-40", "40-55", "55+")))

p6 <- ggplot(insurance_clean, aes(x = age_group, y = charges, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Försäkringskostnader per åldersgrupp",
       x = "Åldersgrupp",
       y = "Kostnad")

ggsave(here("output", "figures", "06_aldersgrupp.png"), plot = p6, width = 8, height = 5, dpi = 300)

#Riskindex
# Här finns det ju en korrelation men vi måste vara medvetna om att 
# history_risk är byggd på prior_claims, som antagligen är kopplat till charges
p7 <- ggplot(insurance_clean, aes(x = history_risk, y = charges, fill = history_risk)) +
  geom_boxplot() +
  labs(title = "Försäkringskostnader per riskindex",
       x = "Riskindex",
       y = "Kostnad")
ggsave(here("output", "figures", "07_riskindex.png"), plot = p7, width = 8, height = 5, dpi = 300)

# Tidigare rapporterade olyckor
# Här finns det en tydlig korrelation. Men vi måste vara medveten om att
# även om varje olycka inte är ett försökringsärende så finns det ett 
# samband mellan olyckor och försäkringsutbetalningar
p8 <- ggplot(insurance_clean, aes(x = factor(prior_accidents), y = charges, fill = factor(prior_accidents))) +
  geom_boxplot() +
  labs(title = "Försäkringskostnader per antal tidigare olyckor",
       x = "Antal tidigare olyckor",
       y = "Kostnad")

ggsave(here("output", "figures", "08_olyckor.png"), plot = p8, width = 8, height = 5, dpi = 300)

# Barn
# Något högre kostnad associaerat med att ha barn
p9 <- ggplot(insurance_clean, aes(x = has_children, y = charges, fill = has_children)) +
  geom_boxplot() +
  labs(title = "Försäkringskostnader per barnstatus",
       x = "Har barn",
       y = "Kostnad")

insurance_clean %>%
  group_by(has_children) %>%
  summarise(
    median_charges = median(charges),
    mean_charges = mean(charges)
  )

ggsave(here("output", "figures", "09_barn.png"), plot = p9, width = 8, height = 5, dpi = 300)

# Sammanfattning
# Utifrån min analys känns det intressant att gå vidare med:
# Rökning
# Kroniskt sjuka
# Ålder
# Tidigare olyckor






