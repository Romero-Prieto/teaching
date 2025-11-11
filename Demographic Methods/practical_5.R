#Demographic Methods - Practical 5 (Fertility and Reproduction), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#
rm(list = ls())
#install.packages("ggplot2")
library(ggplot2)

# Reading the data
GitHub               = "https://raw.githubusercontent.com/Romero-Prieto/teaching/main/Demographic%20Methods/practical_5.csv"
data                 = read.csv(GitHub)
print(data[1:20, ])                                                             #Shows the first 20 rows of the data.

# Data preparation
year                 = 2001
radix                = 100000
data                 = data[data[, "Year"]== year, ]                            #Selects year 2001.
x                    = data[, "x"]
nWx                  = data[, "nWx"]
nNx_t                = data[, "nNx_total"]
nLx                  = data[, "nLx"]
B                    = data[, "B"]
rm(data)

n                    = c(diff(x,1),NA)
sEL                  = !is.na(n)
rEP                  = x >= 15 & x < 50

# Examples
# Data could be used to calculate the life expectancy at birth e0, using all values of nLx and the radix.  
sum(nLx)/radix

# The same mortality data could be used to calculate the number of years a newborn is expecting lo live while in the reproductive ages, i.e., between 15 and 50, dividing the total number of person-years years lived between 15 and 50 by the radix.
# (T15 - T50)/l0
(sum(nLx[x >= 15]) - sum(nLx[x >= 50]))/radix
# alternatively,
sum(nLx[x >= 15 & x < 50])/radix
# which is the same as:
sum(nLx[rEP])/radix

# Exercise 1: Fertility and reproduction question (Compulsory)
# Calculate the following values and comment on your results.
# a. Crude Birth Rate.
sum(B)/sum(nNx_t)*1000

# b. Child-Woman Ratio.
# Hint: use the variable "rEP" to identify observations related to reproductive ages.
sum(nNx_t[x < 5])/sum(nWx[rEP])

# c. General Fertility Rate.
sum(B)/sum(nWx[rEP])*1000

# d. Age specific fertility rates $_nf_x$.
nFx                  = B/nWx

# e. Total Fertility Rate.
TFR                  = sum(n[rEP]*nFx[rEP])
TFR

# f. Gross Reproduction Rate.
# Hint: assume a Sex Ratio at Birth equal to 1.05 males per female. 
SRB                  = 1.05
GRR                  = 1/(1 + SRB)*TFR
GRR

# g. Net Reproduction Rate.
NRR                  = 1/(1 + SRB)*sum(nFx[rEP]*nLx[rEP]/radix)
NRR

# h. Calculate the Mean Age of the Fertility Schedule mu.
age                  = x + n/2
sum(n[rEP]*nFx[rEP]*age[rEP])/TFR

# i. Calculate the probability to survive to the Mean Age of the Fertility Schedule p(mu).
NRR/GRR


# Exercise 2: Fertility and reproduction question (Compulsory)
# Plot the Fertility Schedule (i.e., nfx as a function of x).
# Hint: Rates are plotted at the midpoint of the age interval.

coloUR               = c("A"     = rgb(0.00,0.55,0.65,0.35), 
                         "B"     = rgb(0.45,0.65,0.20,0.65))
sEL                  = !is.na(n)
fertility            = data.frame(age, nFx)
fertility            = fertility[sEL, ]
ggplot(fertility, aes(x = age, y = nFx)) + 
  geom_polygon(fill = coloUR["A"], color = coloUR["B"]) +
  coord_cartesian(xlim = c(10, 50)) +
  scale_x_continuous(breaks = seq(10, 50, by = 5)) +
  ggtitle(paste("Age-specific fertility rates, U.S. population of",year))

# Exercise 3: Fertility and reproduction question (Optional)
# Estimate the TFR and NRR for all available years, and address the following questions.
# In which year after World War II did fertility first fall below replacement level? Between 1933 and 1941, the Total Fertility Rate exceeded two children per woman, yet the net reproduction rate remained below replacement. What accounts for this discrepancy?
# Hint: Use a loop to consolidate the nLx and nFx matrices. Estimate all years simultaneously using a single line of code.
rm(year, B, nWx, nFx, nLx, nNx_t, age, coloUR)
data                 = read.csv(GitHub)
data[,"nFx"]         = data[, "B"]/data[, "nWx"]
data                 = data[, c("Year", "x", "nLx", "nFx")]
Year                 = min(data[, "Year"]):max(data[, "Year"])
nFx                  = matrix(NA, nrow = length(x), ncol = length(Year))
nLx                  = nFx

for (i in 1:length(Year)) {
  temp     = data[data[, "Year"] == Year[i],]
  nFx[, i] = temp[, "nFx"]
  nLx[, i] = temp[, "nLx"]
}

TFR                  = colSums(n[rEP]*nFx[rEP, ]) 
NRR                  = 1/(1 + SRB)*colSums(nLx[rEP, ]*nFx[rEP, ])/radix 
data.frame(Year,TFR,NRR)
