##########################
#Example use of EmpiricTherapyIndices package
#Setup package and calculate example
install.packages("devtools")
library(devtools);install_github("josie-hughes/EmpiricTherapyIndices");devtools::document()
library(EmpiricTherapyIndices)
#Example calculation - 2011 in the worst case
EmpiricTherapyIndicesDAI(getExampleAntibiogramDAI())

#Revise the antibiogram and recalculate. PDR Acinetobacter example
pdrAcinetoAbg = getExampleAntibiogramDAI()
pdrAcinetoAbg$A.baumannii[!is.na(pdrAcinetoAbg$A.baumannii)]=100
EmpiricTherapyIndicesDAI(pdrAcinetoAbg)

#Save antibiogram template as a delimited text file - can be edited in Excel
write.table(getExampleAntibiogramDAI(),file="myAntibiogram.txt",sep = "\t",row.names=F)

#Load revised antibiogram and calculate indices
myRevisedAbg = read.table(file="myAntibiogram.txt",header=T)
EmpiricTherapyIndicesDAI(myRevisedAbg)

