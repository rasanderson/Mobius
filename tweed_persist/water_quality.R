# Get water quality data for Tweed
# Ideally would have it for near Boleside or Galafoot, so could split into
# upper and lower. Unfortunately it is just Till, Whiteadder, Eye, Norham
# and Tweed Harbour. Use Norham initially
# 
library(readxl)
library(ggplot2)

raw_wq_TN <- read_xlsx("~/ExpanDrive/OneDrive Business/Wader - EU LIFE/SEPA Nutrient data 1996-2018/Tweed Nitrate.xlsx",)
raw_wq_TP <- read_xlsx("~/ExpanDrive/OneDrive Business/Wader - EU LIFE/SEPA Nutrient data 1996-2018/Tweed TP.xlsx")
raw_wq_TN <- raw_wq_TN[, c("Sample date...17", "Adjusted result...18")] # Some garbage in column headers
raw_wq_TP <- raw_wq_TP[, c("Sample date", "Adjusted result")]
colnames(raw_wq_TN) <- c("samp_date", "tot_nitrogen")
colnames(raw_wq_TP) <- c("samp_date", "tot_phosphorus")

raw_wq <- merge(raw_wq_TN, raw_wq_TP, by = "samp_date")
# Quite a lot of missing values before 1995
raw_wq$samp_date <- as.Date(raw_wq$samp_date)
raw_wq <- raw_wq[raw_wq$samp_date >= as.Date("1995-01-01"),]

ggplot(raw_wq, aes(x = samp_date, y = tot_nitrogen)) +
  geom_point() +
  geom_line()
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