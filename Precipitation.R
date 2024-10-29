# Set working directory
#setwd("./Data")

#Read the dataset
input.path <- "C:\\Users\\ashok\\Desktop\\Collaboration with Prof Manos\\Project_ModellingRainfall\\Publication data\\Data"
setwd(input.path)

# Load the required libraries

library(MASS)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(fitdistrplus)
library(extRemes)
library(evd)

options(scipen = 999) # options(scipen = 0)

# List of station names
stations <- c("MMS01", "MMS31", "OMS11", "SMS31") #)# 

# Data frame to store summary statistics
DescStats_table <- data.frame(
  Statistic = c("Mean", "Standard deviation", "Skewness", "Kurtosis", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum"),
  MMS01 = numeric(9),
  MMS31 = numeric(9),
  OMS11 = numeric(9),
  SMS31 = numeric(9)
)

# GEV Distribution: parameter estimates
GEV_table <- data.frame(
  Estimate = c("Location Parameter", 
                "Scale Parameter", 
                "Shape Parameter"),
  MMS01 = numeric(3),
  MMS31 = numeric(3),
  OMS11 = numeric(3),
  SMS31 = numeric(3)
)

# Gamma Distribution: parameter estimates
Gamma_table <- data.frame(
  Estimate = c("Shape Parameter", 
                   "Shape Parameter SE", 
                   "Rate Parameter", 
                   "Rate Parameter SE"),
  MMS01 = numeric(4),
  MMS31 = numeric(4),
  OMS11 = numeric(4),
  SMS31 = numeric(4)
)

# Data frame to store AIC values from various probability distributions
AIC_table <- data.frame(
  Distribution = c("Gamma", 
                "Weibull", 
                "Normal",
                "GEV",
                "Gumbel"
  ),
  MMS01 = numeric(5),
  MMS31 = numeric(5),
  OMS11 = numeric(5),
  SMS31 = numeric(5)
)

# Functions to plot rainfall data

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
  
  ## Assign the summary of an fevd object to a variable
  summaryGEV <- summary(fitGEV, silent = TRUE)
  #print(summaryGEV$par)
  
  ## Standard Error Estimates for the GEV distribution
  # loc_se <- summaryGEV$se.theta[["location"]]
  # scale_se <- summaryGEV$se.theta[["scale"]]
  # shape_se <- summaryGEV$se.theta[["shape"]]
  # 
  # print(c(loc_se, scale_se, shape_se))
  
  summaryGumbel <- summary(fitGumbel, silent = TRUE)
  
  # plot(fitGEV)
  # plot(fitGEV, "trace")
  # return.level(fitGEV)
  # return.level(fitGEV, do.ci=TRUE)
  # ci(fitGEV, return.period=c(2,20,100))

  # plot(fitGamma, demp = TRUE)
  # plot(fitGamma, histo = FALSE, demp = TRUE)
  # cdfcomp(fitGamma, addlegend=FALSE)
  # denscomp(fitGamma, addlegend=FALSE)
  # ppcomp(fitGamma, addlegend=FALSE)
  # qqcomp(fitGamma, addlegend=FALSE)

  # constraints <- list(
  #   mean = mean(non_zero_rain),  # mean of the rainfall data
  #   var = var(non_zero_rain)     # variance of the rainfall data
  # )
  # 
  # # Fit a Maximum Entropy model
  # maxent_model <- maxent(constraints, non_zero_rain)
  
  # # Output empirical probabilities
  # print(maxent_model$probabilities)
  
  # Store relevant model results into the table
  
  GEV_table[, station] <- round(c(
    summaryGEV$par[[1]],
    summaryGEV$par[[2]],
    summaryGEV$par[[3]]
  ), 4)
  
  Gamma_table[, station] <- round(c(
    fitGamma$estimate['shape'],
    fitGamma$sd['shape'],
    fitGamma$estimate['rate'],
    fitGamma$sd['rate']
  ), 4)
  
  AIC_table[, station] <- round(c(
    AIC(fitGamma),
    AIC(fitWeibull),
    AIC(fitNormal),
    summaryGEV$AIC,
    summaryGumbel$AIC
  ), 1)
  
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
  png(file = png_filename, width = 1600, height = 1200, res = 300)  # Full resolution PNG (e.g., 300 DPI)

  # Adjust margins to minimize whitespace
  par(mar = c(4, 4, 2, 0.5))  # Change numbers as needed (bottom, left, top, right)
  
  # Call the same plotting function to save the plot to the PNG
  createTimeSeriesPlot(station_data, station)
  
  # Close the PNG device (save the plot)
  dev.off()
  
  # Save the plot as a EPS file
  eps_filename <- paste0(station, "_Rainfall_Plot.eps")
  postscript(file = eps_filename, width = 7, height = 5, paper = "special", onefile = FALSE, horizontal = FALSE)

  # Adjust margins to minimize whitespace
  par(mar = c(4, 4, 2, 0.5))  # Change numbers as needed (bottom, left, top, right)
  
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
print(GEV_table)
print(Gamma_table)
print(AIC_table)

# Write the table to an Excel file
write.xlsx(DescStats_table, file = "Rainfall_Descriptive_Statistics.xlsx", rowNames = FALSE)
write.xlsx(GEV_table, file = "GEV_table.xlsx", rowNames = FALSE)
write.xlsx(Gamma_table, file = "Gamma_table.xlsx", rowNames = FALSE)
write.xlsx(AIC_table, file = "AIC_table.xlsx", rowNames = FALSE)
