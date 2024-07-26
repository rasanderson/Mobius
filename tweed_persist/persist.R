# Basic R interface to PERSiST using external CSV data file

library('Rcpp')
sourceCpp('mobius_r.cpp')

# Keep next few lines as needed for some C++ calls.
none0 <- vector(mode='character', length=0) # It would be nice to use NULL or NA
                                           # instead, but that does not
                                           # interface well with C++ it seems
none1 <- vector(mode='character', length=1)

# Read from standard CSV
upper_tweed <- read.csv("temp_rain_Q_upper.csv", header=TRUE) 
upper_tweed$date <- as.Date(upper_tweed$date, format="%Y-%m-%d")
catchment_area <- 1800

cubic_metres_per_day <- upper_tweed$Q * 86400
flow_mm_per_day <- cubic_metres_per_day / catchment_area

mobius_setup_from_parameter_file_and_input_series('BasinObs_upper_testparameters.dat',
                                                 InputDataStartDate = as.character(upper_tweed$date[1]),
                                                 AirTemperature = upper_tweed$temperature,
                                                 Precipitation = upper_tweed$precipitation)

mobius_set_parameter_double('Terrestrial catchment area', 'UpperTweed', catchment_area)
mobius_set_parameter_uint('Timesteps', none0, length(upper_tweed$date))
mobius_set_parameter_double('Reach length', 'UpperTweed', 80000)


# Run the model
mobius_run_model()

# This is the predicted flow. Q is observed
pred_flow <- mobius_get_result_series('Reach flow (daily mean)', c('UpperTweed'))
plot(upper_tweed$Q, type='l', col='blue')
lines(pred_flow, type='l', col='red')

# Evapotraspiration
evap <- mobius_get_result_series('Evapotranspiration X3', c('All'))
plot(evap, type='l', col='blue')

