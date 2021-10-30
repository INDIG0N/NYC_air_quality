library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)

# FUNCTIONS
##########
piv_wide_on_name = function(your.frame) {
  df = pivot_wider(your.frame, names_from = Name, values_from = Data.Value)
  return(df)
}
##########


df = read.csv('../data/Air_Quality.csv')

df['Time.Period'] = as.character(df$Time.Period)
df = filter(df, grepl('Annual Average', df$Time.Period))

trimmed = df[, c(-2, -7, -12)]
trimmed = trimmed[trimmed$Measure != 'million miles',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total SO2 Emissions',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total NOx Emissions',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total PM2.5 Emissions',]

particle_rows = unique(trimmed$Name)[c(1, 5, 8, 9, 11)]
O3_rows = unique(trimmed$Name)[c(2, 10, 12, 13)]
SO2_rows = unique(trimmed$Name)[3]
NO2_rows = unique(trimmed$Name)[4]
Benzene_rows = unique(trimmed$Name)[6]
Formaldehyde_rows = unique(trimmed$Name)[7]

particle_df = filter(trimmed, Name %in% particle_rows)
O3_df = filter(trimmed, Name %in% O3_rows)
SO2_df = filter(trimmed, Name == SO2_rows)
NO2_df = filter(trimmed, Name == NO2_rows)
Benzene_df = filter(trimmed, Name == Benzene_rows)
Formaldehyde_df = filter(trimmed, Name == Formaldehyde_rows)

summary(particle_df)