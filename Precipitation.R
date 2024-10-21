# Set working directory
#setwd("./Data")

# Load the required libraries

library(MASS)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(fitdistrplus)
library(extRemes)

options(scipen = 999) # options(scipen = 0)

# List of station names
stations <- c("MMS01", "MMS31", "OMS11", "SMS31") # )#

# Data frame to store summary statistics
DescStats_table <- data.frame(
  Statistic = c("Mean", "Standard deviation", "Skewness", "Kurtosis", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum"),
  MMS01 = numeric(9),
  MMS31 = numeric(9),
  OMS11 = numeric(9),
  SMS31 = numeric(9)
)

# Data frame to store modelling results
Model_table <- data.frame(
  Statistic = c("Shape Parameter Estimate", 
                "Shape Parameter SE", 
                "Rate Parameter Estimate", 
                "Rate Parameter SE", 
                "AIC"),
  MMS01 = numeric(5),
  MMS31 = numeric(5),
  OMS11 = numeric(5),
  SMS31 = numeric(5)
)

# Data frame to store AIC values from various probability distributions
AIC_table <- data.frame(
  Statistic = c("Gamma AIC", 
                "Weibill AIC", 
                "Normal AIC",
                "GEV AIC",
                "Gumbel AIC"
                ),
  MMS01 = numeric(5),
  MMS31 = numeric(5),
  OMS11 = numeric(5),
  SMS31 = numeric(5)
)

# Functions to handle the plotting of rainfall data

createTimeSeriesPlot <- function(station_data, station) {
  plot(
    station_data$DATE, 
    station_data$Rain, 
    type = "l", 
    xlab = "Year", 
    ylab = "Precipitation (mm)", 
    main = paste("Trend in Daily Rainfall (mm) at station", station),
    xaxt = "n",  # Disable the default x-axis initially
    font.lab = 2  # Bold x and y axis titles
  )
  
  # # Add custom x-axis with labels for each year
  # years <- seq(from = min(station_data$DATE), 
  #              to = max(station_data$DATE), 
  #              by = "year")
  # 
  # axis(1, at = years, labels = format(years, "%Y"), las = 2)  # Rotate labels if needed

  # # Add the custom x-axis with more frequent labels (e.g., yearly)
  axis(1, at = seq(from = min(station_data$DATE),
                   to = max(station_data$DATE),
                   by = "year"),
       format(seq(from = min(station_data$DATE),
                  to = max(station_data$DATE),
                  by = "year"), "%Y"))
}

createAnnualMaximumPlot <- function(annmax, station) {

  plot(
    annmax$Year, 
    annmax$AnnualMaximum, 
    type = "l",
    xlab = "Year",
    ylab = "Maximum annual precipitation (mm)",
    main = paste("Maximum annual Rainfall (mm) at station", station),
    xaxt = "n",  # Disable the default x-axis initially
    font.lab = 2,  # Bold x and y axis titles
    col = "darkblue",
    lwd = 1.5,
    )

  axis(1, at = seq(from = min(annmax$Year),
                   to = max(annmax$Year)))
}

# Loop through each station
for (station in stations) {
  
  # Construct the filename based on the station name
  file_name <- paste0(station, ".csv")
  
  # Read the CSV file
  station_data <- read.csv(file_name, sep = ";", header = TRUE, dec = ",")
  
  # Format the DATE column
  station_data$DATE <- as.Date(station_data$DATE, format = '%d/%m/%Y')
  
  # Clean up the data 
  station_data <- station_data[,-1]
  names(station_data)[2] <- 'Rain'
 
  # Add three additional columns to the data frame
  station_data <- station_data %>% 
    mutate(DATE = ymd(DATE)) %>% 
    mutate_at(vars(DATE), list(year = year, month = month, day = day))

  # Group annual data and find the maximum rainfall for each year
  annmax <- aggregate(station_data$Rain, by = list(station_data$year), max, na.rm=TRUE)
  names(annmax) <- c("Year", "AnnualMaximum")
  
  #print(annmax)
  annmax_file_name <- paste0(station, "_AnnualMaximum.xlsx")
  write.xlsx(annmax, file = annmax_file_name, rowNames = FALSE)
  
  # Filter data to include only non-zero rainfall values
  non_zero_rain <- station_data$Rain[station_data$Rain > 0]

  #print(hist(non_zero_rain))

  # Step 1: Fit various probability distributions to the non-zero rainfall data using MLE
  fitGamma <- fitdist(non_zero_rain, distr = "gamma", method = "mle")
  fitWeibull <- fitdist(non_zero_rain, distr = "weibull", method = "mle")
  fitNormal <- fitdist(non_zero_rain, distr = "norm", method = "mle")
  fitGEV <- fevd(non_zero_rain, type = "GEV", method = "MLE")
  fitGumbel <- fevd(non_zero_rain, type = "Gumbel", method = "MLE")
  #fitExponential <- fevd(non_zero_rain, type = "Exponential", method = "MLE")

  # print(fitGamma)
  # print(summary(fitGamma))
  # print(fitWeibull)
  # print(fitNormal)
  #Assign the summary of an fevd object to a variable
  summaryGEV <- summary(fitGEV, silent = TRUE)
  summaryGumbel <- summary(fitGumbel, silent = TRUE)

  plot(fitGamma, demp = TRUE)
  # plot(fitGamma, histo = FALSE, demp = TRUE)
  # cdfcomp(fitGamma, addlegend=FALSE)
  # denscomp(fitGamma, addlegend=FALSE)
  # ppcomp(fitGamma, addlegend=FALSE)
  # qqcomp(fitGamma, addlegend=FALSE)

  # # Display the fitted parameters and AIC value
  # print(paste("Shape Parameter Estimate:", fitGamma$estimate['shape']))
  # print(paste("Shape Parameter SE:", fitGamma$sd['shape']))
  # print(paste("Rate Parameter Estimate:", fitGamma$estimate['rate']))
  # print(paste("Rate Parameter SE:", fitGamma$sd['rate']))
  # print(paste("AIC for Gamma fit:", AIC(fitGamma)))
  
  
  
  
  

  # Store relevant model results into the table
  Model_table[, station] <- c(
    fitGamma$estimate['shape'],
    fitGamma$sd['shape'],
    fitGamma$estimate['rate'],
    fitGamma$sd['rate'],
    AIC(fitGamma)
  )
  
  AIC_table[, station] <- c(
    AIC(fitGamma),
    AIC(fitWeibull),
    AIC(fitNormal),
    summaryGEV$AIC,
    summaryGumbel$AIC
  )

  # Extract statistics using psych::describe
  stats <- psych::describe(station_data$Rain)
  stats$Q1 <- summary(station_data$Rain)[[2]]
  stats$Q3 <- summary(station_data$Rain)[[5]]
    
  # Store relevant summary statistics into the table
  DescStats_table[, station] <- c(
    stats$mean,
    stats$sd,
    stats$skew,
    stats$kurtosis,
    stats$min,
    stats$Q1,
    stats$median,
    stats$Q3,
    stats$max
  )

  # First, display the plot in the RStudio console
  createTimeSeriesPlot(station_data, station)
  #createAnnualMaximumPlot(annmax, station)
  
  # Save the plot as a PNG file 
  png_filename <- paste0(station, "_Rainfall_Plot.png")
  png(file = png_filename, width = 1500, height = 1000, res = 300)  # Full resolution PNG (e.g., 300 DPI)
  
  # Call the same plotting function to save the plot to the PNG
  createTimeSeriesPlot(station_data, station)
  
  # Close the PNG device (save the plot)
  dev.off()
  
  # Save the cleaned data as an Excel file using openxlsx
  output_file_name <- paste0(station, "_Clean.xlsx")
  write.xlsx(station_data, file = output_file_name, rowNames = FALSE)
}

# View summary statistics and model results for all stations
print(DescStats_table)
print(Model_table)
print(AIC_table)

# Write the table to an Excel file
 write.xlsx(DescStats_table, file = "Rainfall_Descriptive_Statistics.xlsx", rowNames = FALSE)
 write.xlsx(Model_table, file = "Model_table.xlsx", rowNames = FALSE)
 write.xlsx(AIC_table, file = "AIC_table.xlsx", rowNames = FALSE)
 