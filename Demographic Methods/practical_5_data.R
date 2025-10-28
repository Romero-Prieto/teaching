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
B                    = readHFDweb(country, "birthsRR", username, password)
B                    = as.data.frame(B[, c("Year","Age","Total")])
colnames(B)          = c("Year", "x", "B")
nNx                  = readHFDweb(country, "exposRR", username, password)
nNx                  = as.data.frame(nNx[,c("Year", "Age", "Exposure")])
colnames(nNx)        = c("Year", "x", "nNx_f")
HFD                  = B

nLx                  = readHMDweb(country, "fltper_1x1", username, password)
nLx                  = as.data.frame(nLx[,c("Year", "Age", "Lx")])
colnames(nLx)        = c("Year", "x", "nLx")
nNx                  = readHMDweb(country, "Exposures_1x1", username, password)
nNx                  = as.data.frame(nNx[,c("Year", "Age", "Female")])
colnames(nNx)        = c("Year", "x", "nNx")
HMD                  = merge(nNx, nLx, by = c("Year", "x"), all = TRUE)
data                 = merge(HMD, HFD, by = c("Year", "x"), all = TRUE)
fertility            = as.matrix(data)
rm(data, HFD, HMD, nNx, B, nLx, username, password)
save(fertility, file = paste0(pATh,"practical_5.RData"))
