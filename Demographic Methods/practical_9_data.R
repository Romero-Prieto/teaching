#Demographic Methods - Practical 9 (Cohort fertility and parity progression), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#
rm(list = ls())
#install.packages("HMDHFDplus")
library(HMDHFDplus) #Uses a library to pull data from the Human Fertility Database and the Human Mortality Database.#
library(data.table)

# Reading the data
pATh                 = "~/Documents/Demographic_Methods/Practical_9/"           #To define the path.#

username             = ""
password             = ""
country              = "SWE"
births               = readHFDweb(country, "birthsTR", username, password)
births               = as.data.table(births[, c("Year", "Cohort", "Age", "Total")])
colnames(births)     = c("Year","Cohort", "x", "B")
exposure             = readHFDweb(country, "exposTR", username, password)
exposure             = as.data.table(exposure[, c("Year", "Cohort", "Age", "Exposure")])
colnames(exposure)   = c("Year","Cohort", "x", "PY")
data                 = merge(births, exposure, by = c("Year","Cohort", "x"), all = TRUE)
sEL                  = is.na(data[, "B"])
data$B[sEL]          = 0
sEL                  = is.na(data[, "PY"])
data$PY[sEL]         = 0
rm(births, exposure, sEL, username, password)
write.csv(as.matrix(data), file = paste0(pATh,"practical_9.csv"), row.names = FALSE)