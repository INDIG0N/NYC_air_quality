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

make_graphs = function(your.frame, new.file.path, x.label, y.label, time.factor = F) {
  for (place in unique(your.frame$Geo.Place.Name)) {
    file.name = paste(place, '.png', sep = '')
    df = your.frame[your.frame$Geo.Place.Name == place, ]
    if (time.factor) {
      ggplot()
      barplot(df[[6]], names.arg = df$Time.Period, xlab = x.label, ylab = y.label, main = as.character(place))
      ggsave(filename = file.name, path = new.file.path)
    } else {
      ggplot()
      plot(x = df$Time.Period, y = df[[6]], xlab = x.label, ylab = y.label, main = place)
      ggsave(filename = file.name, path = new.file.path)
    }
  }
}
##########

if (interactive()) {
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
  # AND there isn't any data on death rates or hospitalizations
  # just one of those would be acceptable,
  # but with that little information, I don't think it makes sense to include it in my analysis.
  # I'll leave the references to it in to dcument the process.
  
  # Looks like Formaldehyde is in the same situation, out of the analysis it goes.
  
  # now to split the dataframes into seperate ones for each variable we want to look at
  # This will let us eliminate null values we made with the pivot while keeping all other data intact.
  # Also, to be condid, I couldn't think of an easier way to do this
  # We'll also select the time periods we want to use, preferring annual averages
  # Some time periods will be cast to factors for use in bar graphs
  
  # PARTICLE_PIV
  particle_measures = particle_piv[, 1:6]
  particle_measures = filter(particle_measures, !is.na(particle_measures[, 6]))
  particle_measures = select_dates(particle_measures, 'Annual Average ')
  
  particle_deaths = particle_piv[, c(1, 2, 3, 4, 5, 7)]
  particle_deaths = filter(particle_deaths, !is.na(particle_deaths[, 6]))
  particle_deaths$Time.Period = as.factor(particle_deaths$Time.Period)
  
  particle_emergencies = particle_piv[, c(1, 2, 3, 4, 5, 8)]
  particle_emergencies = filter(particle_emergencies, !is.na(particle_emergencies[, 6]))
  particle_emergencies$Time.Period = as.factor(particle_emergencies$Time.Period)
  
  particle_emergencies_kids = particle_emergencies[particle_emergencies[['Measure']] == 'Estimated Annual Rate- Children 0 to 17 Yrs Old',]
  particle_emergencies_adults = particle_emergencies[particle_emergencies[['Measure']] == 'Estimated Annual Rate- 18 Yrs and Older',]
  
  # O3_PIV
  O3_measures = O3_piv[, 1:6]
  O3_measures = filter(O3_measures, !is.na(O3_measures[, 6]))
  O3_measures = select_dates(O3_measures, '^Summer ')
  
  O3_deaths = O3_piv[c(1, 2, 3, 4, 5, 7)]
  O3_deaths = filter(O3_deaths, !is.na(O3_deaths[, 6]))
  O3_deaths$Time.Period = as.factor(O3_deaths$Time.Period)
  
  O3_emergencies = O3_piv[c(1, 2, 3, 4, 5, 8)]
  O3_emergencies = filter(O3_emergencies, !is.na(O3_emergencies[, 6]))
  O3_emergencies$Time.Period = as.factor(O3_emergencies$Time.Period)
  
  O3_emergencies_kids = O3_emergencies[O3_emergencies[['Measure']] != 'Estimated Annual Rate- 18 Yrs and Older',]
  O3_emergencies_adults = O3_emergencies[O3_emergencies[['Measure']] == 'Estimated Annual Rate- 18 Yrs and Older',]
  
  # SO2_PIV
  # this one doesn't have any death or hospitalization rates
  # it looks like observations often start in inter of one year,
  # and continue until winter of the next year, so it;ll do for an annual average
  SO2_measures = filter(SO2_piv, !is.na(SO2_piv[, 6]))
  SO2_measures = select_dates(SO2_measures, 'Winter ....-')
  SO2_measures$Time.Period = SO2_measures$Time.Period + 2000
  
  # NO2_PIV
  # NO2_piv doesn't have any deaths or hospitalizations either,
  # at least it doesn't have any nans to filter out.
  NO2_measures = select_dates(NO2_piv, 'Annual Average ')
  
  # now we can make all of the plots
  make_graphs(particle_measures, './../images/particle/measures/', 'Year', 'Fine Particles Mean mcg / m^3')
  make_graphs(particle_deaths, './../images/particle/deaths/', 'Time Period', 'Deaths per 100K (30 & older)', time.factor = T)
  make_graphs(particle_emergencies_kids, './../images/particle/emergencies/kids/', 'Time Period', 'Emergency Room Visits per 100K (Age 0-17)', time.factor = T)
  make_graphs(particle_emergencies_adults, './../images/particle/emergencies/adults/', 'Time Period', 'Emergency Room Visits per 100K (Age 18 & older)', time.factor = T)
  
  make_graphs(O3_measures, './../images/O3/measures/', 'Year', 'O3 ppb (parts per billion)')
  make_graphs(O3_deaths, './../images/O3/deaths/', 'Time Period', 'Deaths per 100K', time.factor = T)
  make_graphs(O3_emergencies_kids, './../images/O3/emergencies/kids/', 'Time Period', 'Emergency Room Visits per 100K (Age 0-17)', time.factor = T)
  make_graphs(O3_emergencies_adults, './../images/O3/emergencies/adults/', 'Time Period', 'Emergency Room Visits per 100K (Age 18 & older)', time.factor = T)
  
  make_graphs(SO2_measures, './../images/SO2/measures/', 'Year', 'SO2 ppb (parts per billion)')
  
  make_graphs(NO2_measures, './../images/NO2/measures/', 'Year', 'NO2 ppb (parts per billion)')
  
  summary(NO2_measures)
}