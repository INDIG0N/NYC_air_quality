library(ggplot2)
library(dplyr)
library(shiny)

df = read.csv('Air_Quality.csv')

head(df)

summary(df)

trimmed = df[, c(-7, -12)]

head(trimmed)

summary(trimmed)

miles = trimmed[trimmed$Measure == 'million miles',]
head(miles)