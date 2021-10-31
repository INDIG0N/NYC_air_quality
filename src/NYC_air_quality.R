library(ggplot2)
library(dplyr)
library(shiny)
library(tidyr)

# FUNCTIONS
##########
piv_wide_on_name = function(your.frame) {
  # takes in a dataframe with a name and values column and pivots wide on name
  # The column "values" will be used for the new values
  # Mostly made to make code much more readable
  df = pivot_wider(your.frame, names_from = Name, values_from = Data.Value)
  return(df)
}

select_dates = function(your.frame, char.pattern, valid_dates = F) {
  # Takes a dataframe and a character vector containing 1 regex
  # filters the dataframe by looking for the pattern in the Time.Period column
  # casts time period to numeric by subbing the pattern for an empty string, 
  # and then orders the frame on time period
  # marking valid dates as true will skip the filtering and replacing phase
  
  if (!valid_dates) {
    df = filter(your.frame, grepl(char.pattern, your.frame$Time.Period))
    df$Time.Period = sub(char.pattern, '', df$Time.Period)
  }
  df$Time.Period = as.numeric(df$Time.Period)
  df = df[order(df$Time.Period),]
  return(df)
}
##########

df = read.csv('../data/Air_Quality.csv')

# Making a trimmed dataframe of only the columns and rows I'll use
trimmed = df[, c(-2, -6, -7, -10, -12)]
trimmed = trimmed[trimmed$Measure != 'million miles',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total SO2 Emissions',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total NOx Emissions',]
trimmed = trimmed[trimmed$Name != 'Boiler Emissions- Total PM2.5 Emissions',]

# converting time period to character. We'll be altering this column in later dataframes, I think
trimmed['Time.Period'] = as.character(trimmed$Time.Period)

# isolate vectors of row numbers to slice the trimmed dataframe
particle_rows = unique(trimmed$Name)[c(1, 5, 8, 9, 11)]
O3_rows = unique(trimmed$Name)[c(2, 10, 12, 13)]
SO2_rows = unique(trimmed$Name)[3]
NO2_rows = unique(trimmed$Name)[4]
Benzene_rows = unique(trimmed$Name)[6]
Formaldehyde_rows = unique(trimmed$Name)[7]

# make some dataframes for differnt types of pullutants
particle_df = filter(trimmed, Name %in% particle_rows)
O3_df = filter(trimmed, Name %in% O3_rows)
SO2_df = filter(trimmed, Name == SO2_rows)
NO2_df = filter(trimmed, Name == NO2_rows)
Benzene_df = filter(trimmed, Name == Benzene_rows)
Formaldehyde_df = filter(trimmed, Name == Formaldehyde_rows)

# pivot the dataframes for ease of use
particle_piv = piv_wide_on_name(particle_df)
O3_piv = piv_wide_on_name(O3_df)
SO2_piv = piv_wide_on_name(SO2_df)
NO2_piv = piv_wide_on_name(NO2_df)
Benzene_piv = piv_wide_on_name(Benzene_df)
Formaldehyde_piv = piv_wide_on_name(Formaldehyde_df)

# looks like Benzene_piv only has data for a couple years,
# and there isn't any data on death rates or hospitalizations
# with that little information, I don't think it makes sense to include it in my analysis,
# but I'll leave the references to it in to dcument the process.

# Looks like Formaldehyde is in the same situation, out of the analysis it goes.

# now to pslit the dataframes into seperate ones for each variable
# This will let us eliminate null values while keeping all other data intact
# also I couldn't think of a different way to do this
particle_measures = particle_piv[, 1:6]
particle_measures = filter(particle_measures, !is.na(particle_measures$`Fine Particulate Matter (PM2.5)`))

particle_deaths = particle_piv[, c(1, 2, 3, 4, 5, 7)]
particle_deaths = filter(particle_deaths, !is.na(particle_deaths$`PM2.5-Attributable Deaths`))

particle_emergencies = particle_piv[, c(1, 2, 3, 4, 5, 8)]
particle_emergencies = filter(particle_emergencies, !is.na(particle_emergencies))

# IMPORTANT NOTE TO SELF!!!
# REGERSSION DOESN'T WORK HERE
# THE DATA JUST ISN'T FORMATTED IN A WAY THAT ALLOWS FOR IT
# FOCUS ON CHANGE OVER TIME IN DIFFERNT AREAS

summary(particle_deaths)
head(particle_deaths)
unique(particle_deaths$Time.Period)