##########################
#This is an R script to install and demonstrate the EmpiricTherapyIndices package.
#The methods are described in Hughes et al. 2016. Empiric therapy indices: novel measures of the impact of antibiotic resistance on populations.
#The code can be viewed at https://github.com/josie-hughes/EmpiricTherapyIndices/blob/master/R/EmpiricTherapyIndicesFns.R.
#Install the package.
install.packages(c("devtools","plyr","tidyr"))
library(devtools)
devtools::install_github("josie-hughes/EmpiricTherapyIndices")
library(EmpiricTherapyIndices)

#Example calculation.
EmpiricTherapyIndicesDAI(getExampleAntibiogramDAI())

#Revise the antibiogram and recalculate. PDR Acinetobacter example
pdrAcinetoAbg = getExampleAntibiogramDAI()
pdrAcinetoAbg$A.baumannii[!is.na(pdrAcinetoAbg$A.baumannii)]=100
EmpiricTherapyIndicesDAI(pdrAcinetoAbg)

#Save the example antibiogram as a delimited text file that can be edited in Excel
write.table(getExampleAntibiogramDAI(),file="myAntibiogram.txt",sep = "\t",row.names=F)

#Load a revised antibiogram and calculate the indices.
myRevisedAbg = read.table(file="myAntibiogram.txt",header=T)
EmpiricTherapyIndicesDAI(myRevisedAbg)

#Remove reserve drugs

ReserveDrugs = c("merop",)
