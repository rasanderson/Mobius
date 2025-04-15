# Reformats LOIS data into format suitable for SimplyN
library(dplyr)
library(lubridate)

QN_datafile <- "tweed_unified/data/simplyQCNP_datainputs.dat"

lois.raw <- read.csv("tweed_unified/data/lois_majorion-nutrient.csv")
lois.raw <- lois.raw[, c("SITE_NAME", "DATE","Ammonia", "Nitrate", "Nitrogen.particulate",
                         "Carbon.organic.dissolved", "Carbon.organic.particulate",
                         "Phosphorus.total", "Phosphorus.total.dissolved")]
lois.raw <- lois.raw[lois.raw$SITE_NAME == "Tweed at Boleside" | lois.raw$SITE_NAME == "Tweed at Norham",]
lois.raw$Ammonia[is.na(lois.raw$Ammonia)] <- 0
lois.raw$Nitrate[is.na(lois.raw$Nitrate)] <- 0
lois.raw$Carbon.organic.dissolved[is.na(lois.raw$Carbon.organic.dissolved)] <- 0
lois.raw$Carbon.organic.particulate[is.na(lois.raw$Carbon.organic.particulate)] <- 0
lois.raw$Phosphorus.total[is.na(lois.raw$Phosphorus.total)] <- 0
lois.raw$Phosphorus.particulate <- lois.raw$Phosphorus.total - lois.raw$Phosphorus.total.dissolved
# We don't have suspended sediment, so this is an approximation
lois.raw$Suspended.sediment <- lois.raw$Carbon.organic.particulate + lois.raw$Nitrogen.particulate
# Convert dates to YYYY-mm-dd, omit hours, but round any hours after 12:00 to
# subsequent day as a couple of dates with an am and pm reading leading to
# duplicates
lois.raw <- lois.raw %>%
  mutate(DATE = dmy_hm(DATE), # Convert to datetime
         DATE = if_else(hour(DATE) >= 12, DATE + days(1), DATE), # Round up if after 12:00
         DATE = format(DATE, "%Y-%m-%d")) # Format as desired


# Upper or Lower Tweed for site name
lois.raw$SITE_NAME <- ifelse(lois.raw$SITE_NAME == "Tweed at Boleside", "UpperTweed", "LowerTweed")
# Sort by site and date
lois.raw <- lois.raw[order(lois.raw$SITE, lois.raw$DATE), ]

# Now need to append nitrate data with correct headings to input data
file.copy("tweed_unified/data/BasinObs_upperlower_datainputs.dat", QN_datafile, overwrite = TRUE)
# Nitrates
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
# Suspended sediment (roughly; from N and C)
cat("\n\n\"observed SS\" {\"LowerTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Suspended.sediment")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
cat("\n\n\"observed SS\" {\"UpperTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Suspended.sediment")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
# Dissolved organic carbon
cat("\n\n\"observed DOC\" {\"LowerTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Carbon.organic.dissolved")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
cat("\n\n\"observed DOC\" {\"UpperTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Carbon.organic.dissolved")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
# Dissolved phosphorus
cat("\n\n\"observed TDP\" {\"LowerTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Phosphorus.total.dissolved")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
cat("\n\n\"observed TDP\" {\"UpperTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Phosphorus.total.dissolved")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
# Particulate phosphorus
cat("\n\n\"observed PP\" {\"LowerTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Phosphorus.particulate")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
cat("\n\n\"observed PP\" {\"UpperTweed\"} :\n", file = QN_datafile, append = TRUE)
tmp <- lois.raw[lois.raw$SITE_NAME == "LowerTweed",]
tmp <- tmp[, c("DATE", "Phosphorus.particulate")]
write.table(tmp, sep = "\t\t", file = QN_datafile,
            append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)

# Manually edit header of QN_datafile to include additional timeseries names
# although not clear if this is essential.