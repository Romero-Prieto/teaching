#Cohort Component Model of Projections (female dominant perspective, open to migration), by JE Romero-Prieto, PhD#
#https://github.com/Romero-Prieto/teaching#

rm(list = ls()) #Clear all generated data if any.#
#install.packages("readxl")#
#install.packages("stringr")#
library(readxl) #Uses a library to read excel data.#
library(stringr) #Uses a library to work with strings.#
input                = read_excel("~/Documents/Lectures/Projections/Preston 6.xlsx") #Reads the data of Preston et al. 2001. "Population projections." Pp. 117-137 in Demography: measuring and modeling population processes. Malden, MA: Blackwell Publishers.#
input                = as.matrix(input) #Read the data as a matrix.#
female               = input[,c("age","nLx_f","nFx","nNx_f","nIx_f")] 
View(female)

#Some important definitions#
year                 = 1993.0 #January 1st population.#
ages                 = nrow(female)
radix                = 100000
n                    = 5
SRB                  = 1.05

nLx                  = female[,"nLx_f"]
nSx                  = c(nLx[1]/(n*radix),nLx[2:(ages - 1)]/nLx[1:(ages - 2)],nLx[ages]/sum(nLx[(ages - 1):ages])) #Calculation of survivorship ratios.#
nFx                  = female[,"nFx"]

#Closed population#
nGx                  = (nFx + c(nFx[2:ages]*nSx[2:ages],0))*n/2 #Incorporates specific assumptions about fertility.#
lESliE_C             = matrix(0,ages,ages) #Defines a space for the Leslie matrix.#
lESliE_C[1,]         = nGx*1/(1 + SRB)*nSx[1] #Inserts multipliers to project the first age group—depending on the age distribution of women in reproductive ages.#
lESliE_C[ages,ages]  = nSx[ages] #Inserts multipliers to project (part of) the open-ended age group.#

for (x in 1:(ages - 1)) {
  lESliE_C[x + 1,x]  = nSx[x + 1] #Inserts multipliers to project the remaining age groups.#
}

#Open to migration#
nKx                  = c(female[,"nIx_f"]/female[,"nNx_f"])
nGx                  = ((1 + nKx)*nFx + (1 + nKx/2)*c(nFx[2:ages]*nSx[2:ages],0))*n/2 #Incorporates specific assumptions about fertility and migration.#
nGx[1]               = nGx[1] + nKx[1]/2*(1 + SRB)/nSx[1] #Incorporates additional assumptions about migration of the first age group.#
lESliE_O             = matrix(0,ages,ages) #Defines a space for the Leslie matrix.#
lESliE_O[1,]         = nGx*1/(1 + SRB)*nSx[1] #Inserts multipliers to project the first age group—depending on the age distribution of women in reproductive ages.#
lESliE_O[ages,ages]  = (1 + nKx[ages]/2)*(nSx[ages] + nKx[ages]/2) #Inserts multipliers to project (part of) the open-ended age group accouting for migration.#

for (x in 1:(ages - 1)) {
  lESliE_O[x + 1,x]     = (1 + nKx[x]/2)*nSx[x + 1] #Inserts multipliers to project the remaining age groups.#
  if (x < ages - 1) {
    lESliE_O[x + 1,x + 1] = nKx[x + 1]/2 #Inserts additional multipliers to project the remaining age groups accounting for migration.#
  }
}

current              = female[,"nNx_f"]
projected            = lESliE_O%*%current #Matrix multiplication simplify the projection to one single operation. The Leslie matrix accounts for all migration rates and assumptions. Note the command for matrix multiplication using R.#

#Let's produce a simple table of results, projecting the population 3 consecutive periods holding fertility and mortality constant (i.e., the Leslie matrix is always the same).#
female_O             = input[,c("age","nNx_f")] #Retains only relevant information.#
colnames(female_O)[colnames(female_O) == "nNx_f"] <- str_c("nNx_",sprintf("%.1f", year)) #Renames the current population using the reference year.#
female_C             = female_O #Creates an identical table for a closed population.#


for (t in seq(year, year + 2*n, n)) {
  projected = lESliE_O%*%female_O[,str_c("nNx_",sprintf("%.1f", t))] #Projects the population n years using the most recent data.#
  colnames(projected)[1] <- str_c("nNx_",sprintf("%.1f", t + n)) #Gives a name to the projected data.#
  female_O  = cbind(female_O,projected) #Concatenates the table to include the new set of projections.#
  
  projected = lESliE_C%*%female_C[,str_c("nNx_",sprintf("%.1f", t))] #Repeats the same but using closed-population data.#
  colnames(projected)[1] <- str_c("nNx_",sprintf("%.1f", t + n))
  female_C  = cbind(female_C,projected)
  }

print(female_O)
print(female_C)

#Problem set 3:#
#a. Can we expect life expectancy to change because of migration?#
#b. Can we expect the TFR to change because of migration?#
#c. Can we expect the NRR to change because of migration?#
#d. Calculate the projected number of births from 1993.0 to 1998.0 by age group of the mother and discriminating by migrants and nationals.#
#e. What change can we expect in the projected number of births if net migration is negative (i.e., nIx < 0)?.#
#f. Which line of this code would you modify if migrants brought their own age-specific fertility rates?#
#g. Which line of this code would you modify if migrants imported their own mortality schedule (i.e., nLx)?#


#Solution:#
#a. Can we expect life expectancy to change because of migration?#
#No, unless migrants are selected to survive more or less than the domestic population. We would need a life table of migrants to assess that problem.#
#b. Can we expect the TFR to change because of migration?#
#No, we are assuming migrants reproduce at the same rates, but additional data would help to relax that assumption (i.e., nFx discriminated by migrants and nationals).#
#c. Can we expect the NRR to change because of migration?#
#No, but in practice, populations reproduce more or less due to migration. We would need detailed nLx and nFx to quantify the effect.#
#d. Calculate the projected number of births from 1993.0 to 1998.0 by age group of the mother and discriminating by migrants and nationals.#
femaleS              = female_O[,c("age",str_c("nNx_",sprintf("%.1f", year)),str_c("nNx_",sprintf("%.1f", year + n)))]
B_O                  = matrix(rowSums(femaleS[,c(2,3)])/2*n*nFx,ages,1) #Projects births by age, assuming an open population.#

femaleS              = female_C[,c("age",str_c("nNx_",sprintf("%.1f", year)),str_c("nNx_",sprintf("%.1f", year + n)))]
B_nat                = matrix(rowSums(femaleS[,c(2,3)])/2*n*nFx,ages,1) #Projects births by age, assuming an closed population (i.e., a counterfactual).#
colnames(B_nat)[1] <- str_c("B_nat[",sprintf("%.1f", year),",",sprintf("%.1f", year + n),")")
B_mig                = B_O - B_nat #Calculates migrant births as the "excess of births due to migration".#
colnames(B_mig)[1] <- str_c("B_mig[",sprintf("%.1f", year),",",sprintf("%.1f", year + n),")")
BirthS               = cbind(matrix(input[,"age"],ages),B_nat,B_mig)
#e. What change can we expect in the projected number of births if net migration is negative (i.e., nIx < 0)?.#
#The excess of births would be negative due to outmigration, and the projected number of births would be lower than the counterfactual.#
#f. Which line of this code would you modify if migrants brought their own age-specific fertility rates?#
#Line 35, calculating nGx.#
#g. Which line of this code would you modify if migrants imported their own mortality schedule (i.e., nLx)?#
#All lines depending on nSx.#
