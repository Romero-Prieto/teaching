#Demographic Methods - Practical 2 (Mortality and Standardisation), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#
rm(list = ls())                                                                 #Clearing all generated data if any.#
#install.packages("stringr")#                                                   #Packages should be installed only once. Hashtags can be used to add comments or to deactivate lines.#
#install.packages("ggplot2")#                                                   
library(stringr)                                                                #To work with strings (i.e., any character, including letters, numbers, and symbols).#
library(ggplot2)                                                                #To draw plots.#

# Reading a preparing data #
pATh                 = "~/Documents/Demographic_Methods/Practical_2/"           #To define the path.#
GitHub               = "https://raw.githubusercontent.com/Romero-Prieto/teaching/main/Demographic%20Methods/practical_2.csv"
data                 = read.csv(GitHub)                                         #To pull the data from a GitHub repository.# 
print(data)                                                                     #To print data.#

lISt                 = c("Sweden","Kazakhstan")                                 #To define a list of all countries to be analysed.#
countries            = length(lISt)                                             #To define the number of countries.#
ages                 = nrow(data[data[,"Country"] == lISt[1],])                 #To define the number of ages.#

nNx                  = as.data.frame(matrix(NaN, nrow = ages, ncol = countries))#Creates a data frame for population counts, given one row per age and one column per country.#
colnames(nNx)        = lISt                                                     #Allocates country names to each column.#
nDx                  = nNx                                                      #Creates a data frame for death counts, with the same characteristics.#
W                    = nNx                                                      #Creates a data frame for population weights, with the same characteristics.#

for (c in 1:countries) {                                                        #Consolidates and calculates basic information using a wide-form data.#
  nNx[,lISt[c]] = data[data[,"Country"] == lISt[c],"Population"]                #Fills the data frame with imported data.#
  nDx[,lISt[c]] = data[data[,"Country"] == lISt[c],"Deaths"]                    #Idem.#
  W[,lISt[c]]   = nNx[,lISt[c]]/sum(nNx[,lISt[c]])                              #Calculates age weights for each country.#
}
print(W)                                                                        #To visualise the age weights by country.#
print(nDx)                                                                      #To visualise the age distribution of deaths by country.#
print(W[,"Sweden"])                                                             #Note this command line will return the W values of Sweden.#


# Question 1: Age-specific mortality rates and (direct) age-standardisation.#
#a.	Compute age-specific death rates for both countries and plot those on one graph. Discuss the differences between the two countries.
#R Hint: Age-specific mortality rates can be calculated directly by dividing the number of deaths by the mid-year population for the same calendar year. R does not require any special notation for this calculation.#
nMx                  = nDx/nNx

#Data preparation for plotting age-specific mortality rates.#
data[,"nMx"]         = as.data.frame(matrix(as.matrix(nMx), nrow = ages*countries, ncol = 1)) #Adds a column nMx to the data, containing the age-specific mortality rates for each country.#
data[,"n/2"]         = data[,"n"]/2                                             #To plot nMx at the midpoint of each age interval, this line adds a column n/2 to the data, representing half the length of each age interval.#
sEL                  = (data[,"x"] == data[ages,"x"])                           #Identifies open-ended age intervals, where the value of n is undetermined.#
data[sEL,"n/2"]      = 1/data[sEL,"nMx"]                                        #Those age intervals could last the life expectancy, which is the reciprocal of nMx.#
data[,"Age"]         = data[,"x"] + data[,"n/2"]                                #Calculates the mid-oint of each age interval.#

#R Hint for plotting: the function ggplot has a special structure to control the features of a plot. The basic structure is ggplot('name of the database', aes(x = 'name of the variable, x-axis', y = 'name of the variable, y-axis', color = 'name of the variable defining the groups of the plot', group = 'name of the variable defining the groups of the plot')) +  geom_line() +  geom_point().#
#Use the command "help("ggplot")" to identify some other inputs that will control the features of a plot such as the scale, labels, etc.#
#If plotting a figure becomes challenging or time-consuming, you can either skip this part —to be discussed later during the solution— or ask Copilot to provide a code example.# 
coloUR               = c("Sweden"     = rgb(0.45,0.65,0.20,0.65), 
                         "Kazakhstan" = rgb(0.00,0.55,0.65,0.65))               #Allocates colors for each country, using four numbers between [0,1]. The first three define the colour and the last number to define the transparency.#

lnM                  = ggplot(data, aes(x = Age, y = nMx, color = Country, group = Country)) +  geom_line() +  geom_point() +  labs(title = "Age-Specific Mortality Rates", x = "Age", y = "nMx (log scale)") +  theme_minimal() + theme(legend.position = "bottom", legend.direction = "horizontal") + scale_y_log10() + scale_x_continuous(breaks = seq(0, 90, by = 10)) + scale_color_manual(values = coloUR)
M                    = ggplot(data, aes(x = Age, y = nMx, color = Country, group = Country)) +  geom_line() +  geom_point() +  labs(title = "Age-Specific Mortality Rates", x = "Age", y = "nMx") +  theme_minimal() + theme(legend.position = "bottom", legend.direction = "horizontal") + scale_x_continuous(breaks = seq(0, 90, by = 10)) +  scale_color_manual(values = coloUR)
ggsave(paste0(pATh,"lnM.png"), plot = lnM, width = 5, height = 4)               #Exports the figure to a specific location.#
ggsave(paste0(pATh,"M.png"), plot = M, width = 5, height = 4)                   #Exports the figure to a specific location.#

#b.	Compute the (unstandardised) crude death rates, CDR, for both countries and discuss the results.
#R Hint: Crude Death Rates can be calculated by dividing the total number of deaths by the total number of people. As a convention, death rates could be reported in thousands. You can use the R function "colSums(nDx)" to calculate the total number of deaths for each country, considering each country is represented by one row of nDx.#
CDR                  = colSums(nDx)/colSums(nNx)*1000

#c.	Compute the standardised death rates for both countries (use the average age distribution of the two countries as the standard); and discuss the results. The average age distribution can be computed in the following manner: (i) compute the relative age distribution for each country (i.e., the proportion of the population in each age group); and (ii) take the average of the two proportions for each age group. 
#R Hint: The standard is calculated as the average of W. You can use the R function "rowSums(W)" to calculate the sum by rows and then divide by the number of countries. Finally, standardised rates can be calculated as the weighted average of the age-specific mortality rates.#
standard             = rowSums(W)/countries
SDR                  = colSums(nMx*standard)*1000

#d.	How would the standardised rates be different if we had used the Swedish age distribution as the standard? What if we had used the Kazakh age distribution?
#R Hint: You can repeat the previous step, using as the standard the column of W specific to each country.#
SDR_Sweden          = colSums(nMx*W[,"Sweden"])*1000
SDR_Kazakhstan      = colSums(nMx*W[,"Kazakhstan"])*1000
tABle               = t(data.frame(CDR,SDR,SDR_Sweden,SDR_Kazakhstan))          #To present all results in one table, using the function "data.frame(Result_1,...,Result_n)". The function "t()" returns the transpose of a data frame or a matrix.

# Question 2: Indirect standardisation.#
# Let’s assume that we didn’t know the age distribution of deaths for Kazakhstan, but that we had an estimate of the total number of deaths (64,572), and the age distribution of the population. In such circumstances, we can no longer conduct direct age-standardisation but can still compare the mortality regime in the two populations via indirect standardisation. Compute the Comparative Mortality Ratio, CMR (sometimes also referred to as the Standardised Mortality Ratio, SMR), and interpret the result.
# R hint: You can use the function "sum()" to calculate the "observed number of deaths" in Kazakhstan (i.e., the second column of nDx). To calculate the "expected number of deaths" — assuming Kazakhstan had Sweden’s age-specific mortality rates (nMx) — compute the sum of the product of Kazakhstan’s nNx and Sweden’s nMx.#
SMR                 = sum(nDx[,"Kazakhstan"])/sum(nMx[,"Sweden"]*nNx[,"Kazakhstan"])