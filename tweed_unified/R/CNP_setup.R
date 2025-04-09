# Reformats LOIS data into format suitable for SimplyN
library(dplyr)
library(zoo)

# Function to replace consecutive dates
replace_consecutive_dates <- function(df) {
  df <- df %>%
    mutate(DATE = as.Date(ifelse(duplicated(DATE), as.numeric(DATE) + 1, as.numeric(DATE)), origin = "1970-01-01"))
  return(df)
}



QN_datafile <- "tweed_unified/data/simplyQCNP_datainputs.dat"

lois.raw <- read.csv("tweed_unified/data/lois_majorion-nutrient.csv")
lois.raw <- lois.raw[, c("SITE_NAME", "DATE","Ammonia", "Nitrate", "Carbon.organic.dissolved",
                         "Carbon.organic.particulate", "Phosphorus.total", "Phosphorus.total.dissolved")]
lois.raw <- lois.raw[lois.raw$SITE_NAME == "Tweed at Boleside" | lois.raw$SITE_NAME == "Tweed at Norham",]
lois.raw$Ammonia[is.na(lois.raw$Ammonia)] <- 0
lois.raw$Nitrate[is.na(lois.raw$Nitrate)] <- 0
lois.raw$Carbon.organic.dissolved[is.na(lois.raw$Carbon.organic.dissolved)] <- 0
lois.raw$Carbon.organic.particulate[is.na(lois.raw$Carbon.organic.particulate)] <- 0
lois.raw$Phosphorus.total[is.na(lois.raw$Phosphorus.total)] <- 0
lois.raw$Phosphorus.particulate <- lois.raw$Phosphorus.total - lois.raw$Carbon.organic.dissolved
# Convert DATE column to Date format and omit time
lois.raw$DATE <- as.Date(lois.raw$DATE, format = "%d/%m/%Y %H:%M")
lois.raw$DATE <- format(lois.raw$DATE, "%Y-%m-%d")
# Upper or Lower Tweed for site name
lois.raw$SITE_NAME <- ifelse(lois.raw$SITE_NAME == "Tweed at Boleside", "UpperTweed", "LowerTweed")
# Sort by site and date
lois.raw <- lois.raw[order(lois.raw$SITE, lois.raw$DATE), ]
lois.raw <- replace_consecutive_dates(lois.raw)

# Now need to append nitrate data with correct headings to input data
file.copy("tweed_unified/data/BasinObs_upperlower_datainputs.dat", QN_datafile, overwrite = TRUE)
cat("\n\n\"observed NO3\" {\"LowerTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Nitrate")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
cat("\n\n\"observed NH4\" {\"LowerTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Ammonia")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
cat("\n\n\"observed NO3\" {\"UpperTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "UpperTweed",]
tmp <- tmp[, c("DATE", "Nitrate")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
cat("\n\n\"observed NH4\" {\"UpperTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "UpperTweed",]
tmp <- tmp[, c("DATE", "Ammonia")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
