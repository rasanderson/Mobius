# Takes the original CSV and reformats it for SimplyQ

# Load the data
data <- read.csv("BasinObs_upper.csv", header = FALSE, row.names = 1)
colnames(data) <- c("date", "precipitation", "temperature", "flow")

# start_date is first date in the dataset
start_dat <- data[1,1] #as.Date(data[1,1], format = "%Y-%d-%d")
# num_recs is the number of rows in the dataset
num_recs <- nrow(data)


cat("start_date : ", start_dat, "\n", file = "BasinObs_upper_datainputs.dat")
cat("timesteps : ", num_recs, "\n", file = "BasinObs_upper_datainputs.dat", append = TRUE)
cat("\nadditional_timeseries : \n", file = "BasinObs_upper_datainputs.dat", append = TRUE)
cat("\"observed Q mm/day\"\n\n", file = "BasinObs_upper_datainputs.dat", append = TRUE)
cat("inputs : \n\n\"Air temperature\" :\n", file = "BasinObs_upper_datainputs.dat", append = TRUE)
cat(data$temperature, sep = "\n", file = "BasinObs_upper_datainputs.dat", append = TRUE)
cat("\n\n\"Precipitation\" :\n", file = "BasinObs_upper_datainputs.dat", append = TRUE)
cat(data$precipitation, sep = "\n", file = "BasinObs_upper_datainputs.dat", append = TRUE)
cat("\n\n\"observed Q mm/day\" :\n", file = "BasinObs_upper_datainputs.dat", append = TRUE)
date_and_flow <- cbind(data$date, data$flow)
write.table(date_and_flow, sep = "\t\t", file = "BasinObs_upper_datainputs.dat",
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)

