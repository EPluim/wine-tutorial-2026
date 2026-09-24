library(readxl)
library(rstudioapi)
setwd('C:/Users/locha/OneDrive - Maastricht University/Documents/GitHub/wine-tutorial-2026/data')
wine <- read.csv('wine.csv')
wine <- wine[, -1]
write.csv(wine, file = 'wine_cleaned.csv')
