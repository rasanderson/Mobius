# Get water quality data for Tweed
# The water_quality (plus Q) data are from Anna Griffin. Should be able to use
# these, but need reformatting for Mobius format. Data are available for
# 'River Tweed @ Norham Gauge' which Lower Tweed (reach 23) and for 'TWEED
# ABOVE GALA WATER FOOT' which roughly corresponds to reach 13 in Jarvie.
# Ideally would use reach 12, but this is probably fine for Upper Tweed.

# Read in the Excel file rather tha CSV as otherwise dates get corrupted
library(readxl)
library(ggplot2)
library(zoo)
library(lubridate)
library(dplyr)
library(tidyr)

rm(list = ls())

raw_wq <- read_xlsx("tweed_persist/2022 Chemistry Data.xlsx",
                    sheet = "Sheet2")
raw_wq$`Date Taken` <- as.Date(raw_wq$`Date Taken`)
raw_upper <- raw_wq[raw_wq$Description == "TWEED ABOVE GALA WATER FOOT",]
raw_lower <- raw_wq[raw_wq$Description == "River Tweed @ Norham Gauge",]
raw_upper_nitrate    <- raw_upper[raw_upper$Determinand == "Nitrate (as N) (mg/L)",]
raw_lower_nitrate    <- raw_lower[raw_lower$Determinand == "Nitrate (as N) (mg/L)",]
raw_upper_tot_phosph <- raw_upper[raw_upper$Determinand == "Total Phosphorus (as P) (mg/L)",]
raw_lower_tot_phosph <- raw_lower[raw_lower$Determinand == "Total Phosphorus (as P) (mg/L)",]
upper_nitrate    <- raw_upper_nitrate[, c("Date Taken", "Determinand", "Result")]
lower_nitrate    <- raw_lower_nitrate[, c("Date Taken", "Determinand", "Result")]
upper_tot_phosph <- raw_upper_tot_phosph[, c("Date Taken", "Determinand", "Result")]
lower_tot_phosph <- raw_lower_tot_phosph[, c("Date Taken", "Determinand", "Result")]
colnames(upper_nitrate)    <- c("samp_date", "determinand", "result")
colnames(lower_nitrate)    <- c("samp_date", "determinand", "result")
colnames(upper_tot_phosph) <- c("samp_date", "determinand", "result")
colnames(lower_tot_phosph) <- c("samp_date", "determinand", "result")
rm(list = c("raw_wq", "raw_upper", "raw_lower",
            "raw_upper_nitrate", "raw_lower_nitrate",
            "raw_upper_tot_phosph", "raw_lower_tot_phosph"))

# Check original data all dates
plot_data <- upper_nitrate
plot_data$moving_avg <- rollmean(plot_data$result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = samp_date, y = result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")
plot_data <- lower_nitrate
plot_data$moving_avg <- rollmean(plot_data$result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = samp_date, y = result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")
plot_data <- upper_tot_phosph
plot_data$moving_avg <- rollmean(plot_data$result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = samp_date, y = result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")
plot_data <- lower_tot_phosph
plot_data$moving_avg <- rollmean(plot_data$result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = samp_date, y = result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")

# Both upper and lower Tweed are from 1993, but there is a gap in upper Tweed
# records between November 2001 and December 2004 inclusive. To allow comparison
# of both N and P we'll run initial models on Jan 2005 to Dec 2019 data.
upper_nitrate <- upper_nitrate[upper_nitrate$samp_date >= as.Date("2005-01-01"),]
lower_nitrate <- lower_nitrate[lower_nitrate$samp_date >= as.Date("2005-01-01"),]
upper_tot_phosph <- upper_tot_phosph[upper_tot_phosph$samp_date >= as.Date("2005-01-01"),]
lower_tot_phosph <- lower_tot_phosph[lower_tot_phosph$samp_date >= as.Date("2005-01-01"),]

# Re-check plots: all look OK (there are 4 missing values on moving_avg)
plot_data <- upper_nitrate
plot_data$moving_avg <- rollmean(plot_data$result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = samp_date, y = result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")
plot_data <- lower_nitrate
plot_data$moving_avg <- rollmean(plot_data$result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = samp_date, y = result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")
plot_data <- upper_tot_phosph
plot_data$moving_avg <- rollmean(plot_data$result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = samp_date, y = result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")
plot_data <- lower_tot_phosph
plot_data$moving_avg <- rollmean(plot_data$result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = samp_date, y = result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")
rm("plot_data")

# Though now complete data, there are roughly half the number of records for
# lower compared to upper Tweed. Calculate the mean sampling date gap for
# nitrate (identical for total phosphorus)
nitrate_gap <- lower_nitrate %>%
  arrange(samp_date) %>%
  mutate(diff_days = as.numeric(difftime(samp_date, lag(samp_date), units = "days")))
mean(nitrate_gap$diff_days, na.rm = TRUE)
nitrate_gap <- upper_nitrate %>%
  arrange(samp_date) %>%
  mutate(diff_days = as.numeric(difftime(samp_date, lag(samp_date), units = "days")))
mean(nitrate_gap$diff_days, na.rm = TRUE)
rm("nitrate_gap")
tot_phosph_gap <- lower_tot_phosph %>%
  arrange(samp_date) %>%
  mutate(diff_days = as.numeric(difftime(samp_date, lag(samp_date), units = "days")))
mean(tot_phosph_gap$diff_days, na.rm = TRUE)
tot_phosph_gap <- upper_tot_phosph %>%
  arrange(samp_date) %>%
  mutate(diff_days = as.numeric(difftime(samp_date, lag(samp_date), units = "days")))
mean(tot_phosph_gap$diff_days, na.rm = TRUE)
rm("tot_phosph_gap")
# Nitrate 28.96 day gap for lower Tweed, 15.38 day gap for upper Tweed
# Total Phosphorus 31.15 gap lower Tweed, 15.53 day gap for upper Tweed 

# Use interpolation to set both upper and lower to every day
# Using just monthly model might be too course given hydrology data is daily
# Create a regular sequence of dates at daily intervals
start_date <- as.Date("2005-01-01") # Year-month-day
end_date   <- as.Date("2019-12-31")
regular_dates <- seq.Date(from = start_date, to = end_date, by = "1 days")

# Merge the original data with the regular dates
lower_nitrate_1d <- lower_nitrate %>%
  arrange(samp_date) %>%
  complete(samp_date = regular_dates)
# Interpolate the result values to the new regular dates
lower_nitrate_1d$result <- na.approx(lower_nitrate_1d$result,
                                     x = lower_nitrate_1d$samp_date,
                                     na.rm = FALSE)
lower_nitrate_1d <- lower_nitrate_1d[is.na(lower_nitrate_1d$result) != TRUE, ]
ggplot() +
  geom_point(data = lower_nitrate_1d, aes(x = samp_date, y = result, colour = "Interpol")) +
  geom_point(data = lower_nitrate, aes(x = samp_date, y = result, colour = "Raw"))
# Merge the original data with the regular dates
upper_nitrate_1d <- upper_nitrate %>%
  arrange(samp_date) %>%
  complete(samp_date = regular_dates)
# Interpolate the result values to the new regular dates
upper_nitrate_1d$result <- na.approx(upper_nitrate_1d$result,
                                      x = upper_nitrate_1d$samp_date,
                                      na.rm = FALSE)
upper_nitrate_1d <- upper_nitrate_1d[is.na(upper_nitrate_1d$result) != TRUE, ]
ggplot() +
  geom_point(data = upper_nitrate_1d, aes(x = samp_date, y = result, colour = "Interpol")) +
  geom_point(data = upper_nitrate, aes(x = samp_date, y = result, colour = "Raw"))
# Same for total phosphorus
# Lower
lower_tot_phosph_1d <- lower_tot_phosph %>%
  arrange(samp_date) %>%
  complete(samp_date = regular_dates)
# Interpolate the result values to the new regular dates
lower_tot_phosph_1d$result <- na.approx(lower_tot_phosph_1d$result,
                                      x = lower_tot_phosph_1d$samp_date,
                                      na.rm = FALSE)
lower_tot_phosph_1d <- lower_tot_phosph_1d[is.na(lower_tot_phosph_1d$result) != TRUE, ]
ggplot() +
  geom_point(data = lower_tot_phosph_1d, aes(x = samp_date, y = result, colour = "Interpol")) +
  geom_point(data = lower_tot_phosph, aes(x = samp_date, y = result, colour = "Raw"))
# Upper
upper_tot_phosph_1d <- upper_tot_phosph %>%
  arrange(samp_date) %>%
  complete(samp_date = regular_dates)
# Interpolate the result values to the new regular dates
upper_tot_phosph_1d$result <- na.approx(upper_tot_phosph_1d$result,
                                         x = upper_tot_phosph_1d$samp_date,
                                         na.rm = FALSE)
upper_tot_phosph_1d <- upper_tot_phosph_1d[is.na(upper_tot_phosph_1d$result) != TRUE, ]
ggplot() +
  geom_point(data = upper_tot_phosph_1d, aes(x = samp_date, y = result, colour = "Interpol")) +
  geom_point(data = upper_tot_phosph, aes(x = samp_date, y = result, colour = "Raw"))

# Met data
# Note, to download using wget use, for example
# wget -v -O ~/tmp/2005_tas.nc https://catalogue.ceh.ac.uk/datastore/eidchub/835a50df-e74f-4bfb-b593-804fd61d5eab/tas/chess-met_tas_gb_1km_daily_20050101-20050131.nc --user=roy.sanderson@newcastle.ac.uk --password= --auth-no-challenge
