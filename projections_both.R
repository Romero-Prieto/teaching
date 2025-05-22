#Cohort Component Model of Projections (female dominant perspective, both sexes and closed population), by JE Romero-Prieto, PhD#
rm(list = ls()) #Clear all generated data if any.#
#install.packages("readxl")#
#install.packages("stringr")#
library(readxl) #Uses a library to read excel data.#
library(stringr) #Uses a library to work with strings.#
input                = read_excel("~/Documents/Lectures/Projections/Preston 6.xlsx") #Reads the data of Preston et al. 2001. "Population projections." Pp. 117-137 in Demography: measuring and modeling population processes. Malden, MA: Blackwell Publishers.#
input                = as.matrix(input) #Read the data as a matrix.#
female               = input[,c("age","nLx_f","nFx","nNx_f")]
male                 = input[,c("age","nLx_m","nNx_m")]

#Some important definitions#
year                 = 1993.0 #January 1st population.#
ages                 = nrow(female) #The number of age groups.#
radix                = 100000 #The radix of a life table.#
n                    = 5 #The length of the age intervals—which is the same for all.# 
SRB                  = 1.05 #The Sex Ratio at Birth.#

#Female population#
nLx                  = female[,"nLx_f"]
nSx                  = c(nLx[1]/(n*radix),nLx[2:(ages - 1)]/nLx[1:(ages - 2)],nLx[ages]/sum(nLx[(ages - 1):ages])) #Calculation of survivorship ratios.#
nFx                  = female[,"nFx"]
nGx                  = (nFx + c(nFx[2:ages]*nSx[2:ages],0))*n/2 #Incorporates specific assumptions about fertility.#
lESliE               = matrix(0,ages,ages) #Defines a space for the Leslie matrix.#
lESliE[1,]           = nGx*1/(1 + SRB)*nSx[1] #Inserts multipliers to project the first age group—depending on the age distribution of women in reproductive ages.#
lESliE[ages,ages]    = nSx[ages] #Inserts multipliers to project (part of) the open-ended age group.#
for (x in 1:(ages - 1)) {
  lESliE[x + 1,x]    = nSx[x + 1] #Inserts multipliers to project the remaining age groups.#
}

lESliE_ff            = lESliE #Note we use the subscript ff to define a Leslie matrix to project female population using only female information.# 
lESliE_mm            = matrix(0,ages,ages) #Defines a space for the Leslie matrix to project male population using only male information.#
lESliE_fm            = matrix(0,ages,ages) #Defines a space for the Leslie matrix to project male population using only female information (i.e., the projected number of births depends on the number and age distribution of the female population in reproductive ages).#
lESliE_mf            = matrix(0,ages,ages) #Defines a null space for the Leslie matrix to project female population using only male information (i.e., female population does not depend on male information).#

#Male population#
nLx                  = male[,"nLx_m"]
nSx                  = c(nLx[1]/(n*radix),nLx[2:(ages - 1)]/nLx[1:(ages - 2)],nLx[ages]/sum(nLx[(ages - 1):ages])) #Calculation of survivorship ratios is the same as for female population.#
lESliE_fm[1,]        = nGx*SRB/(1 + SRB)*nSx[1] #Inserts multipliers to project the first age group—depending on the age distribution of women in reproductive ages. Note that we have used the same nGx, but male nSx and multiply by the SRB.#
lESliE_mm[ages,ages] = nSx[ages] #Inserts multipliers to project (part of) the open-ended age group.#
for (x in 1:(ages - 1)) {
  lESliE_mm[x + 1,x] = nSx[x + 1] #Inserts multipliers to project the remaining age groups.#
}

lESliE               = rbind(cbind(lESliE_ff,lESliE_mf),cbind(lESliE_fm,lESliE_mm)) #Concatenates the Leslie matrices for the two sexes.#
current              = rbind(matrix(female[,"nNx_f"],ages),matrix(male[,"nNx_m"],ages)) #Concatenates the two sexes, female population on top.#
projected            = lESliE%*%current #Matrix multiplication simplify the projection to one single operation. Note the command for matrix multiplication using R.#


#Let's produce a simple table of results, projecting the population 3 consecutive periods holding fertility and mortality constant (i.e., the Leslie matrix is always the same).#
pop                  = input[,c("age","nNx_m","nNx_f")] #Retains only relevant information for the two sexes.#
colnames(pop)[colnames(pop) == "nNx_f"] <- str_c("nNx_f",sprintf("%.1f",year)) #Renames the current population using the reference year.#
colnames(pop)[colnames(pop) == "nNx_m"] <- str_c("nNx_m",sprintf("%.1f",year))

for (t in seq(year, year + 2*n, n)) {
  current   = rbind(matrix(pop[,str_c("nNx_f",sprintf("%.1f",t))],ages),matrix(pop[,str_c("nNx_m",sprintf("%.1f",t))],ages))
  projected = lESliE%*%current #Projects the population n years using the most recent data.#
  female    = matrix(projected[1:ages,],ages) #Extracts female population.#
  colnames(female)[1] <- str_c("nNx_f",sprintf("%.1f",t + n)) #Gives a name to the projected female population.#
  male      = matrix(projected[(1 + ages):(2*ages),],ages) #Extracts male population.#
  colnames(male)[1] <- str_c("nNx_m",sprintf("%.1f",t + n)) #Gives a name to the projected male population.#
  pop       = cbind(pop,male,female) #Concatenates the table to include the new set of projections for each sex.#
}

print(pop)

#Problem set 2:#
#a. Calculate the life expectancy at birth for each sex.#
#b. Calculate the resulting age-sex distribution of keeping constant the fertility and mortality regimes for a long period of time (i.e., stable-equivalent age distribution, ergodic solution.).#

#Solution:#
#a. Calculate the life expectancy at birth for each sex.#
sum(input[,"nLx_m"])/radix
sum(input[,"nLx_f"])/radix
#b. Calculate the resulting age-sex distribution of keeping constant the fertility and mortality regimes for a long period of time (i.e., stable-equivalent age distribution, ergodic solution.).#
current              = rbind(matrix(input[,"nNx_f"],ages),matrix(input[,"nNx_m"],ages))
current              = current/sum(current)
for (t in 1:100) {
  projected = lESliE%*%current
  current   = projected/sum(projected)
}

female               = matrix(projected[1:ages,],ages)
colnames(female)[1] <- "nNx_f*"
male                 = matrix(projected[(1 + ages):(2*ages),],ages)
colnames(male)[1] <- "nNx_m*"
ergodic              = cbind(input[,"age"],male,female)
print(ergodic)