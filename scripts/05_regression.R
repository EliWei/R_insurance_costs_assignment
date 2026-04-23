library(tidyverse)
library(here)
source(here("scripts", "04_analys.R"))

# Vi tittar på smoker, chronic_condition, age, prior_accidents

model_1 <- lm(charges ~ smoker + chronic_condition + age + prior_accidents, 
             data = insurance_clean)
summary(model1)

# Modellen (model1) förklarar 68% av variationerna i kostnader. 
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


# Jämför modeller
model_comparison <- tibble(
  model = c(
    "Model 1: rökning + kronisk + ålder + olyckor",
    "Model 2: + träning + kön + hälsokontroller",
    "Model 3: + träning (utan kön och hälsokontroller)"
  ),
  r_squared = c(
    summary(model_1)$r.squared,
    summary(model_2)$r.squared,
    summary(model_3)$r.squared
  ),
  adjusted_r_squared = c(
    summary(model_1)$adj.r.squared,
    summary(model_2)$adj.r.squared,
    summary(model_3)$adj.r.squared
  ),
  residual_se = c(
    summary(model_1)$sigma,
    summary(model_2)$sigma,
    summary(model_3)$sigma
  )
)

model_comparison

# Plottar resultat av modell 3
model_3_diagnostics <- insurance_clean %>%
  mutate(
    fitted_value = fitted(model_3),
    residual = resid(model_3)
  )

# Residualer mot predikterade värden
ggplot(model_3_diagnostics, aes(x = fitted_value, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residualer mot predikterade värden",
    x = "Predikterat värde",
    y = "Residual"
  ) +
  theme_minimal()

# Faktiskt vs predikterat
ggplot(model_3_diagnostics, aes(x = fitted_value, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Faktisk kostnad mot predikterad kostnad",
    x = "Predikterat värde",
    y = "Faktisk kostnad"
  ) +
  theme_minimal()

# QQ-plot
qqnorm(resid(model_3))
qqline(resid(model_3), col = "red")

# Faktiskt vs predikterat
ggplot(model_3_diagnostics, aes(x = fitted_value, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Faktisk kostnad mot predikterad kostnad",
    x = "Predikterat värde",
    y = "Faktisk kostnad"
  ) +
  theme_minimal()

# QQ-plot
qqnorm(resid(model_3))
qqline(resid(model_3), col = "red")

head(model_3_diagnostics)

# Prediktioner med model_3
new_persons <- tibble(
  smoker =            c("yes", "no",  "no",  "yes"),
  chronic_condition = c("yes", "no",  "yes", "no"),
  age =               c(45,    45,    45,    45),
  prior_accidents =   c(1,     0,     0,     0),
  exercise_level =    c("low", "high","low", "high")
)

predict(model_3, newdata = new_persons)

# Person 1 (rökare, kronisk sjukdom, 45 år, 1 olycka, låg träning): 21 093 kr
# Person 2 (icke-rökare, ingen kronisk sjukdom, 45 år, 0 olyckor, hög träning): 6 645 kr
# Person 3 (icke-rökare, kronisk sjukdom, 45 år, 0 olyckor, låg träning): 12 543 kr
# Person 4 (rökare, ingen kronisk sjukdom, 45 år, 0 olyckor, hög träning): 13 933 kr