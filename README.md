# Försäkringskostnader – R-analys

## Projektbeskrivning
Analys av försäkringskostnader för att identifiera kostnadsdrivande faktorer 
och utvärdera om en regressionsmodell kan användas som stöd vid prissättning.

## Projektstruktur
R_individuell/
├── scripts/
│   ├── 01_understanding_data.R
│   ├── 02_data_cleaning.R
│   ├── 03_variabler.R
│   ├── 04_analys.R
│   ├── 05_regression.R
│   └── 06_modelljämförelse.R
├── output/
│   ├── figures/
├── rapport/
│   └── rapport_E_Weise.pdf
├── insurance_costs.csv
├── Run_analysis.R
├── R_individuell.Rproj
└── README.md

## Paket som används
- tidyverse
- here

Installera med:
  install.packages(c("tidyverse", "here"))

## Hur man kör analysen
1. Öppna projektet via .Rproj-filen
2. Kör run_analysis.R för att köra alla skript i ordning
3. Output sparas automatiskt i output/figures