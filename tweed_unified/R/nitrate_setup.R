# Reformats LOIS data into format suitable for SimplyN

QN_datafile <- "tweed_unified/data/simplyQN_datainputs.dat"

lois.raw <- read.csv("tweed_unified/data/lois_majorion-nutrient.csv")
lois.raw <- lois.raw[, c("SITE_NAME", "DATE","Ammonia", "Nitrate")]
lois.raw <- lois.raw[lois.raw$SITE_NAME == "Tweed at Boleside" | lois.raw$SITE_NAME == "Tweed at Norham",]
lois.raw$Ammonia[is.na(lois.raw$Ammonia)] <- 0
lois.raw$Nitrate[is.na(lois.raw$Nitrate)] <- 0
lois.raw$SITE_NAME <- ifelse(lois.raw$SITE_NAME == "Tweed at Boleside", "UpperTweed", "LowerTweed")
# Convert DATE column to Date format and omit time
lois.raw$DATE <- as.Date(lois.raw$DATE, format = "%d/%m/%Y %H:%M")
# Format DATE column to "YYYY-MM-DD"
lois.raw$DATE <- format(lois.raw$DATE, "%Y-%m-%d")
# Sort lois.raw by SITE and DATE
lois.raw <- lois.raw[order(lois.raw$SITE, lois.raw$DATE), ]

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
