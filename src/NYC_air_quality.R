library(ggplot2)
library(dplyr)
library(shiny)

df = read.csv('Air_Quality.csv')

head(df)

summary(df)

trimmed = df[, c(-7, -12)]
trimmed = trimmed[trimmed$Measure != 'million miles',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total SO2 Emissions',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total NOx Emissions',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total PM2.5 Emissions',]

head(trimmed)

summary(trimmed)

unique(trimmed$Name)