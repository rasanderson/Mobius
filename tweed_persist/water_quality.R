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

raw_wq <- read_xlsx("tweed_persist/2022 Chemistry Data.xlsx",
                    sheet = "Sheet2")
raw_upper <- raw_wq[raw_wq$Description == "TWEED ABOVE GALA WATER FOOT",]
raw_lower <- raw_wq[raw_wq$Description == "River Tweed @ Norham Gauge",]
raw_upper_TP <- raw_upper[raw_upper$Determinand == "Total Phosphorus (as P) (mg/L)",]
raw_upper_Nitrate <- raw_upper[raw_upper$Determinand == "Nitrate (as N) (mg/L)",]
#raw_upper_sediment <- raw_upper[raw_upper$Determinand == "Total solids (dissolved + suspended) (105\u00B0C) (mg/L)", ] # \u00B0 is degrees symbol
raw_lower_TP <- raw_lower[raw_lower$Determinand == "Total Phosphorus (as P) (mg/L)",]
raw_lower_Nitrate <- raw_lower[raw_lower$Determinand == "Nitrate (as N) (mg/L)",]
raw_upper_Nitrate <- raw_upper_Nitrate[, c("Date Taken", "Result")]

colnames(raw_wq_TN) <- c("samp_date", "tot_nitrogen")
colnames(raw_wq_TP) <- c("samp_date", "tot_phosphorus")

raw_wq <- merge(raw_wq_TN, raw_wq_TP, by = "samp_date")
# Quite a lot of missing values before 1995
raw_wq$samp_date <- as.Date(raw_wq$samp_date)
raw_wq <- raw_wq[raw_wq$samp_date >= as.Date("1995-01-01"),]

plot_data <- raw_upper_Nitrate
plot_data$moving_avg <- rollmean(plot_data$Result, k = 5, fill = NA, align = "right")
ggplot(plot_data, aes(x = `Date Taken`, y = Result)) +
  geom_point() +
  geom_line(aes(y = moving_avg), colour = "blue")
ggplot(raw_wq, aes(x = samp_date, y = tot_phosphorus)) +
  geom_point() +
  geom_line()


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