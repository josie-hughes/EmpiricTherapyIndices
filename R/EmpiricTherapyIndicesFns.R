#' Calculate empiric therapy indices for device-associated infections.
#' 
#' @param Antibiograms An antibiogram. getExampleAntibiograms() for example. Numbers are percent resistance (or non-susceptibility). One site is included in the example, but the algorithm can handle multiple sites.
#' 
#' @param Basket The basket of infections. getBasketDAI() for example.
#' @param Drugs The set of available drugs. getDrugsDAI() for example.
#' @param mixingPars A list of parameters for mixingFunction(), determining the impact of drug heterogeneity on the evolution of resistance. In the default case list(alpha=0) using multiple drugs does not slow the development of resistance. list(alpha=0.1) means that using 2 drugs instead of 1 slows the developement of resistance by 10\%. 
#' @return Empiric options index (EOI) and empiric coverage index (ECI) for each site.
#' @examples
#'#Example calculation:
#'EmpiricTherapyIndicesDAI(getExampleAntibiogramDAI())
#'
#'#Revise the antibiogram and recalculate. PDR Acinetobacter example
#'pdrAcinetoAbg = getExampleAntibiogramDAI()
#'pdrAcinetoAbg$A.baumannii[!is.na(pdrAcinetoAbg$A.baumannii)]=100
#'EmpiricTherapyIndicesDAI(pdrAcinetoAbg)
#'
#'#Save the example antibiogram as a delimited text file that can be edited in Excel
#'write.table(getExampleAntibiogramDAI(),file="myAntibiogram.txt",sep = "\t",row.names=F)
#'
#Load a revised antibiogram and calculate the indices.
#'myRevisedAbg = read.table(file="myAntibiogram.txt",header=T)
#'EmpiricTherapyIndicesDAI(myRevisedAbg)
#'
#'#Calculate the indices without reserve drugs
#'ReserveDrugs = c("amika","cefta","dapto","eryth","genta","linez","merop","tobra")
#'abgWithoutReserves = subset(getExampleAntibiogramDAI(),!is.element(drug_d,ReserveDrugs))
#'EmpiricTherapyIndicesDAI(abgWithoutReserves)
#' @export
EmpiricTherapyIndicesDAI<-function(Antibiograms,Basket=getBasketDAI(),Drugs=getDrugsDAI(),mixingPars=list(alpha=0),wide=T){
  #Antibiograms=abgWithoutReserves;Basket=getBasketDAI();Drugs=getDrugsDAI();wide=T
  if(wide){dat=EIMakeLong(Antibiograms,Basket,Drugs);Antibiograms=dat$Antibiograms;Basket=dat$Basket;Drugs=dat$Drugs}
  checkDataDAI(Antibiograms,Basket,Drugs)
  coverage = getCoverage(Antibiograms,Basket,Drugs)
  ECIy = ddply(coverage,.(site,syndrome_y,alpha_y),summarise,ECIy=max(V))
  ECI = ddply(ECIy,.(site),summarise,value=100*sum(alpha_y*ECIy))
  ECI$type="ECI"  
  EOIy = ddply(coverage,.(site,syndrome_y,alpha_y),getEOIy,mixingPars=mixingPars)
  EOI = ddply(EOIy,.(site),summarise,value=sum(alpha_y*V1))
  EOI$type="EOI"  
  indices = rbind(EOI,ECI)
  return(indices) 
}
#' An example antibiogram. 
#' @return An example antibiogram. Numbers are percent resistant (or non-susceptible).
#' @examples
#'#Revise the antibiogram and recalculate. PDR Acinetobacter example
#'pdrAcinetoAbg = getExampleAntibiogramDAI()
#'pdrAcinetoAbg$A.baumannii[!is.na(pdrAcinetoAbg$A.baumannii)]=100
#'EmpiricTherapyIndicesDAI(pdrAcinetoAbg)
#'
#'#Save the example antibiogram as a delimited text file that can be edited in Excel
#'write.table(getExampleAntibiogramDAI(),file="myAntibiogram.txt",sep = "\t",row.names=F)
#'
#Load a revised antibiogram and calculate the indices.
#'myRevisedAbg = read.table(file="myAntibiogram.txt",header=T)
#'EmpiricTherapyIndicesDAI(myRevisedAbg)
#'
#'#Calculate the indices without reserve drugs:
#'ReserveDrugs = c("amika","cefta","dapto","eryth","genta","linez","merop","tobra")
#'abgWithoutReserves = subset(getExampleAntibiogramDAI(),!is.element(drug_d,ReserveDrugs))
#'EmpiricTherapyIndicesDAI(abgWithoutReserves)
#' @export
getExampleAntibiogramDAI<-function(){
  #Example antibiogram. Numbers are percent resistance (or non-susceptibility). 
  #2011 data, assuming resistance in cases of uncertainty
  #One site is included in this example, but the algorithm can handle multiple sites.
  AntibiogramText = 
"site,drug_d,A.baumannii,CoNS,E.coli,E.faecalis,E.faecium,Enterobacter,Klebsiella,P.aeruginosa,Proteus,S.aureus,Serratia
A,amika,20,100,0,,,0,0,11,0,100,0
A,amox-,, 81,36,0,100,,14,,14, 15,
A,ampic,,100,67,0,100,,,,14,100,
A,cefaz,, 81,38,,,,100,,100, 15,
A,cefta,100, 81,22,,,41,9,24,0, 15,0
A,cipro,0,100,37,100,,5,6,33,0,100,0
A,clind,, 67,,,,,,,, 45,
A,dapto,,  0,,0,0,,,,,  0,
A,doxyc,100, 26,41,100,50,33,20,,100,  3,100
A,eryth,, 75,,100,100,,,,, 48,
A,fosfo,,100,100,100,,100,100,100,100,100,100
A,genta,0,100,15,100,100,2,6,27,0,100,0
A,linez,,  1,,0,0,,,,,  0,
A,merop,0, 81,0,100,,0,0,23,0, 15,0
A,nitro,,  0,3,100,100,83,60,,,  0,
A,pip-t,0, 81,25,0,100,41,0,18,100, 15,0
A,tmp-s,0, 47,26,,,21,14,,0,  1,0
A,tobra,0,100,16,,,2,6,11,0,100,0
A,vanco,,  0,,0,48,,,,,  0,"
  AntibiogramsWide = read.table(text=AntibiogramText,header=TRUE,sep=",",stringsAsFactors=F)
  return(AntibiogramsWide)
}
#' Available drugs for device associated infections. 
#' 
#' @return The set of available drugs.
#' @examples
#' Drugs=getDrugsDAI()
#' @export
getDrugsDAI<-function(){
  DrugsText = 
"drug_d,CAUTIgn,CAUTIgp,CLABSIgn,CLABSIgp,VAPgn,VAPgp
amika,y,,y,,y,
amox-,y,y,y,y,y,y
ampic,y,y,y,y,y,y
cefaz,y,y,y,y,y,y
cefta,y,,y,,y,
cipro,y,y,y,y,y,y
clind,,,,y,,y
dapto,,,,y,,
doxyc,y,y,,,,
eryth,,,,,,y
fosfo,y,y,,,,
genta,y,,y,,y,
linez,,y,,y,,y
merop,y,y,y,y,y,y
nitro,y,y,,,,
pip-t,y,y,y,y,y,y
tmp-s,y,y,y,y,y,y
tobra,y,,y,,y,
vanco,,y,,y,,y"
  DrugsWide = read.table(text=DrugsText,header=TRUE,sep=",",stringsAsFactors=F)
  return(DrugsWide)
}
#' The basket of device-associated infections. 
#' 
#' @return The basket of device-associate drugs.
#' @examples
#' Basket=getBasketDAI()
#' @export
getBasketDAI<-function(){
  BasketText = 
    "syndrome_y,alpha_y,A.baumannii,CoNS,E.coli,E.faecalis,E.faecium,Enterobacter,Klebsiella,P.aeruginosa,Proteus,S.aureus,Serratia
CAUTIgn,28,1,,45,,,7,19,19,8,,2
CAUTIgp, 7,,15,,49,21,,,,,14,
CLABSIgn,16,8,,16,,,18,31,15,3,,10
CLABSIgp,30,,42,,18,14,,,,,25,
VAPgn,13,12,,11,,,16,19,31,3,,9
VAPgp, 6,,3,,2,1,,,,,93,"
  BasketWide <- read.table(text=BasketText,header=TRUE,sep=",",stringsAsFactors=F)
  return(BasketWide)
}
#' The impact of mixing on the development of resistance, f(d).
#' 
#' @return f(d)
#' @examples
#' #Change the impact of antibiotic mixing by overwriting this function.
#' #This should be decreasing function of $d$, with a value of 1 when $d=1$, and a minimum value of $>0$
#' @export
mixingFunction<-function(d,mixingPars){
  #TO DO: pass function names as an argument 
  return((1-mixingPars$alpha)^(d-1))
}
#################
#internal functions
checkDataDAI<-function(Antibiograms,Basket,Drugs){
  #check for consistency among data sources, and return error if there is a problem
  abgSp = unique(Antibiograms$species_s)
  bSp = unique(Basket$species_s)
  msg = paste("Species in the antibiogram are not in the basket:",paste(setdiff(abgSp,bSp),collapse=","))
  try(if(length(setdiff(abgSp,bSp))>0) stop(msg))
  msg = paste("Species in the basket are not in the antibiogram:",paste(setdiff(bSp,abgSp),collapse=","))
  try(if(length(setdiff(abgSp,bSp))>0) stop(msg))
  abgD = unique(Antibiograms$drug_d)
  bD = unique(Drugs$drug_d)
  msg = paste("Drugs in the antibiogram are not in the set of available drugs, and will be ignored:",paste(setdiff(abgD,bD),collapse=","))
  if(length(setdiff(abgD,bD))>0)(warning(msg))
  #try(if(length(setdiff(abgD,bD))>0) stop(msg))
  msg = paste("Drugs in the available set are not in the antibiogram - the algorithms will assume resistance:",paste(setdiff(bD,abgD),collapse=","))
  if(length(setdiff(bD,abgD))>0)(warning(msg))
  #try(if(length(setdiff(bD,abgD))>0) stop(msg))
  try(if(max(Antibiograms$resistance)>100) stop("Resistance in the antibiogram should not exceed 100%"))
  try(if(max(Antibiograms$resistance)<0) stop("Resistance in the antibiogram should not be less than 0%"))  
}
getEOIy<-function(covBit,mixingPars){
  covBit=covBit[order(-covBit$V),]
  covBit$d = seq(1,nrow(covBit))
  covBit$Vdplus1=c(covBit$V[2:nrow(covBit)],0)
  EOIy = sum(covBit$d*(covBit$V^2-covBit$Vdplus1^2)/mixingFunction(covBit$d,mixingPars))
  return(EOIy)
}
getCoverage<-function(Antibiograms,Basket,Drugs){
  Antibiograms=subset(Antibiograms,!is.na(resistance))
  Temp=merge(Antibiograms,Basket)  
  Temp$inner = (1-Temp$resistance)*Temp$kappa_sy
  Temp=merge(Temp,Drugs) 
  coverage = ddply(Temp,.(site,syndrome_y,alpha_y,drug_d),summarise,V=sum(inner))
  return(coverage)
}
EIMakeLong<-function(AntibiogramsWide,BasketWide,DrugsWide){
  Drugs = gather_(DrugsWide,"syndrome_y","measurement",setdiff(names(DrugsWide),"drug_d"))
  Drugs=subset(Drugs,measurement=="y");Drugs$measurement=NULL
  Basket = gather_(BasketWide,"species_s","kappa_sy",setdiff(names(BasketWide),c("syndrome_y","alpha_y")))
  Basket=subset(Basket,!is.na(kappa_sy))
  Basket$kappa_sy = Basket$kappa_sy/100
  Basket$alpha_y = Basket$alpha_y/100    
  Antibiograms = gather_(AntibiogramsWide,"species_s","resistance",setdiff(names(AntibiogramsWide),c("site","drug_d")))
  Antibiograms=subset(Antibiograms,!is.na(resistance))
  Antibiograms$resistance = Antibiograms$resistance/100
  return(list(Antibiograms=Antibiograms,Basket=Basket,Drugs=Drugs))
}


