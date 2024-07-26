# Basic R interface to PERSiST using external CSV data file
# Note that this does not properly account for water flowing from Upper to Lower
# Tweed, but it is a start.

library('Rcpp')
sourceCpp('mobius_r.cpp')

# Keep next few lines as needed for some C++ calls.
none0 <- vector(mode='character', length=0) # It would be nice to use NULL or NA
                                           # instead, but that does not
                                           # interface well with C++ it seems
none1 <- vector(mode='character', length=1)

# Upper Tweed to Reach 12 Boleside
# Read from standard CSV
upper_tweed <- read.csv("temp_rain_Q_upper.csv", header=TRUE) 
upper_tweed$date <- as.Date(upper_tweed$date, format="%Y-%m-%d")
calib <- upper_tweed[upper_tweed$date <= as.Date("1997-12-31"),]

catchment_area <- 1800

cubic_metres_per_day <- upper_tweed$Q * 86400
flow_mm_per_day <- cubic_metres_per_day / catchment_area

mobius_setup_from_parameter_file_and_input_series('BasinObs_upper_testparameters.dat',
                                                 InputDataStartDate = as.character(calib$date[1]),
                                                 AirTemperature = calib$temperature,
                                                 Precipitation = calib$precipitation)

mobius_set_parameter_double('Terrestrial catchment area', 'UpperTweed', catchment_area)
mobius_set_parameter_uint('Timesteps', none0, length(calib$date))
mobius_set_parameter_double('Reach length', 'UpperTweed', 80000)


# Run the model
mobius_run_model()

# This is the predicted flow. Q is observed
pred_flow <- mobius_get_result_series('Reach flow (daily mean)', c('UpperTweed'))
plot(calib$Q, type='l', col='blue')
lines(pred_flow, type='l', col='red')

# Evapotraspiration
evap <- mobius_get_result_series('Evapotranspiration X3', c('All'))
plot(evap, type='l', col='blue')

# Lower Tweed to Reach 23 Noreham
lower_tweed <- read.csv("temp_rain_Q_lower.csv", header=TRUE) 
lower_tweed$date <- as.Date(lower_tweed$date, format="%Y-%m-%d")
calib <- lower_tweed[lower_tweed$date <= as.Date("1997-12-31"),]

catchment_area <- 7000

cubic_metres_per_day <- upper_tweed$Q * 86400
flow_mm_per_day <- cubic_metres_per_day / catchment_area

mobius_setup_from_parameter_file_and_input_series('BasinObs_lower_testparameters.dat',
                                                  InputDataStartDate = as.character(calib$date[1]),
                                                  AirTemperature = calib$temperature,
                                                  Precipitation = calib$precipitation)

mobius_set_parameter_double('Terrestrial catchment area', 'LowerTweed', catchment_area)
mobius_set_parameter_uint('Timesteps', none0, length(calib$date))
mobius_set_parameter_double('Reach length', 'LowerTweed', 80000)


# Run the model
mobius_run_model()

# This is the predicted flow. Q is observed
pred_flow <- mobius_get_result_series('Reach flow (daily mean)', c('LowerTweed'))
plot(calib$Q, type='l', col='blue')
lines(pred_flow, type='l', col='red')

# Evapotraspiration
evap <- mobius_get_result_series('Evapotranspiration X3', c('All'))
plot(evap, type='l', col='blue')
