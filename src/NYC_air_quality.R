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

# now we'll cut down each dataframe based on the dates we want to examine
# we'll also be converting the dates to integers for easier plotting
# we may as well sort them based on the date while we're at it

# particle_piv has annual averages, so we'll take those
particle_piv = select_dates(particle_piv, 'Annual Average ')

# O3_piv has measurments recorded in the summer of each year,
# that's probably as close as we can get to a yearly average
O3_piv = select_dates(O3_piv, '^Summer ')

# SO2_piv is a bit weirder, the observations go from the start of winter in one year,
# until the beginning of winter in the next. It makes tracking things annualy tricky,
# but since the vast majority of the observation is in the second year of each time period,
# I feel comfortable tracking it all as data for the latter time period,
# now we just need to get the last 2 digits of the time period and add 2000
SO2_piv = select_dates(SO2_piv, 'Winter .....')
SO2_piv$Time.Period = SO2_piv$Time.Period + 2000

# NO2_piv has annual averages, so we can jsut use those
NO2_piv = select_dates(NO2_piv, 'Annual Average ')

# looks like Benzene_piv only has data for a couple years,
# and there isn't any data on death rates or hospitalizations
# with that little information, I don;t think it makes sense to include it in my analysis,
# but I'll leave the references to it in to dcument the process.

# Looks like Formaldehyde is in the same situation, out of the analysis it goes.

summary(particle_piv)
head(particle_piv)
filter(particle_piv, !anyNA(particle_piv))