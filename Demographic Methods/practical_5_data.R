#Demographic Methods - Practical 5 (Fertility and Reproduction), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#
rm(list = ls())
#install.packages("HMDHFDplus")
library(HMDHFDplus) #Uses a library to pull data from the Human Fertility Database and the Human Mortality Database.#

# Reading the data
pATh                 = "~/Documents/Demographic_Methods/Practical_5/"           #To define the path.#

username             = ""
password             = ""
country              = "USA"
HFD                  = readHFDweb(country, "birthsRR", username, password)
HFD                  = as.data.frame(HFD[, c("Year","Age","Total")])
colnames(HFD)        = c("Year", "x", "B")


nLx                  = readHMDweb(country, "fltper_1x1", username, password)
nLx                  = as.data.frame(nLx[,c("Year", "Age", "Lx")])
colnames(nLx)        = c("Year", "x", "nLx")
nNx                  = readHMDweb(country, "Exposures_1x1", username, password)
nNx                  = as.data.frame(nNx[,c("Year", "Age", "Female", "Total")])
colnames(nNx)        = c("Year", "x", "nWx", "nNx_total")

HMD                  = merge(nNx, nLx, by = c("Year", "x"), all = TRUE)
data                 = merge(HMD, HFD, by = c("Year", "x"), all = TRUE)
fertility            = as.matrix(data)
sEL                  = is.na(fertility)
fertility[sEL]       = 0
rm(data, HFD, HMD, nNx, nLx, sEL, username, password)
write.csv(fertility, file = paste0(pATh,"practical_5.csv"), row.names = FALSE)