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
upper_tot_phosph <- raw_upper_nitrate[, c("Date Taken", "Determinand", "Result")]
lower_tot_phosph <- raw_lower_nitrate[, c("Date Taken", "Determinand", "Result")]
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

# As far as I can tell Mobius needs the same timesteps for hydrology as for
# water quality. We have roughly (!) monthly data for water quality, but daily
# data for hydrology (Q).
# For speed, interpolate to daily data and see if that works first.
library(lubridate)
raw_wq$samp_date_yday <- yday(raw_wq$samp_date)
sinday <- sin(2 * pi * raw_wq$samp_date_yday/365.25)
cosday <- cos(2 * pi * raw_wq$samp_date_yday/365.25)
n_lm <- lm(log1p(tot_nitrogen) ~ sinday + cosday + samp_date, data = raw_wq)
summary(n_lm)
raw_wq$n_fit <- expm1(fitted(n_lm))
ggplot(raw_wq, aes(x = samp_date, y = tot_nitrogen)) +
  geom_point() +
  geom_line(aes(y = n_fit))
# Hmm. Too crude. May have to use monthly data.