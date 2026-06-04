#Average models with no uncertainty
#testing 

#90% adult neuter rate
nyears=10#run for 10 years
lengthoftime=12*nyears
SeasonalReproduction=c(rep(c(rep(0.0366667,3),rep(0.13,6),rep(0.0366667,3)),nyears),0.0366667)#Seasonal reproduction
Total_DLN=matrix(ncol = 10, nrow=lengthoftime)
Owned_DLN = matrix(ncol = 10, nrow=lengthoftime)
Feral_DLN = matrix(ncol = 10, nrow=lengthoftime)
Stray_DLN = matrix(ncol = 10, nrow=lengthoftime)
Shelter_DLN = matrix(ncol = 10, nrow=lengthoftime)

PG_Total_DLN=numeric()
PG_Owned_DLN =numeric()
PG_Feral_DLN=numeric()
PG_Stray_DLN=numeric()
PG_Shelter_DLN =numeric()


#Simulate different degrees of prepubertal neutering of owned cats whilst HOLDING NEUTERING PREVALENCE APPROX CONSTANT
#so simulating a delay to neutering not overall change to neutering
#overall 90% adult neutering prevalence (so proportion of adult cats neutered remains 90%)

for (x in 1:10){
  MATS<-list()
  TOKUtoOKN<-seq(0.05,0.5,length.out=10)[x]##kitten neutering between 0.05 to 0.5
  TOJUtoOJN<-0.13
  #to keep total population neutered constant 
  TOAUtoOAN<-c(0.757249083,0.743762921,0.728690152,0.711733286,0.692515505,0.670552327,
               0.645210198,0.615644382,0.580702962,0.538773258)[x]
  
  TStUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  #to correspond
  TStUtoOJN<-c(0.37442215,0.4073473,0.44027245,0.4731976,0.50612275,0.5390479,0.57197305,0.6048982,0.63782335,0.6707485)[x]
  TStUtoOAN<-0.90
MATS<-list()#create empty list for time-varying matrices

VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)
σFK<-0.81
σFJ<-0.92
σFA<-0.96
σFE<-0.9
TFKUtoFKN<-0
TFUtoFN<-0.01 
TFtoSh<-0.003
TFtoO<-0.02
overallbFJ<-1.5
overallbFA<-2.5
bFJ<-SeasonalReproduction[1]*  overallbFJ
bFA<-SeasonalReproduction[1]*overallbFA

#SHELTER PARAMETERS
σShK<-0.974
σShJ<-0.993
σShA<-0.985
σShE<-0.9
TShtoO<-0.63

#OWNED PARAMETERS
σOK<-0.97
σOJ<-0.995#
σOA<-0.995#
σOE<-0.98

TOtoStK<-0.0009
TOtoStJ<-0.0009
TOtoStA<-0.0009
TOtoStE<-0.0009
TOtoShK<-0.002
TOtoShJ<-0.002
TOtoShA<-0.002
TOtoShE<-0.002

overallbOJ<-1.4
overallbOA<-2.1
bOJ<-SeasonalReproduction[1]*  overallbOJ
bOA<-SeasonalReproduction[1]*overallbOA

#STRAY PARAMETERS
σStK<-0.918
σStJ<-0.97
σStA<-0.97
σStE<-0.9


TSttoSh<-0.03
TSttoO<-0.04
TSttoF<-0.14186
overallbStJ<-1.5
overallbStA<-2.5
bStJ<-SeasonalReproduction[1]*overallbStJ
bStA<-SeasonalReproduction[1]*overallbStA

#Aging parameters - SAS 
p <- 0:6; TFKtoJ<-σFK^5/sum(σFK^p)
p <- 0:6; TFJtoA<-σFJ^5/sum(σFJ^p)
p <- 0:120; TFAtoE<-σFA^119/sum(σFA^p)
p <- 0:6; TStKtoJ<-σStK^5/sum(σStK^p)
p <- 0:6; TStJtoA<-σStJ^5/sum(σStJ^p)
p <- 0:120; TStAtoE<-σStA^119/sum(σStA^p)
p <- 0:6; TShKtoJ<-σShK^5/sum(σShK^p)
p <- 0:6; TShJtoA<-σShJ^5/sum(σShJ^p)
p <- 0:120; TShAtoE<-σShA^119/sum(σShA^p)
p <- 0:6; TOKtoJ<-σOK^5/sum(σOK^p)
p <- 0:6; TOJtoA<-σOJ^5/sum(σOJ^p)
p <- 0:120; TOAtoE<-σOA^119/sum(σOA^p)

########MEGAMODEL
MatU<-matrix(c(σFK * (1-TFKtoJ)*(1- TFtoSh-TFtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoF	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
               σFK * (TFKtoJ)*(1-TFKUtoFKN)	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
               σFK * (TFKtoJ)*(TFKUtoFKN)	,	0	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
               0	,	σFJ * TFJtoA*(1-TFUtoFN)	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
               0	,	σFJ * TFJtoA*TFUtoFN	,	σFJ * TFJtoA	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*(1- TSttoSh-TSttoO-TSttoF)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoStK	,	0	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*(1- TSttoSh-TSttoO)	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoStJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoStJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoStJ	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoStA	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,
               σFK * (1-TFKtoJ)*TFtoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoShK	,	0	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoSh	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoShJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*(1- TShtoO)	,	σShJ* (1-TShJtoA)*(1- TShtoO)	,	σShJ * (1-TShJtoA)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoShJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoShJ	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	σShJ* TShJtoA*(1- TShtoO)	,	σShJ * TShJtoA*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoShA	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	σShA *TShAtoE*(1- TShtoO)	,	σShA *TShAtoE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,
               σFK * (1-TFKtoJ)*TFtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*TShtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*(1- TOtoShK-TOtoStK)	,	0	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *(1-TStUtoOKN)	,	σStJ * (1-TStJtoA)*TSttoO*(1-TStUtoOJN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *TStUtoOKN	,	σStJ * (1-TStJtoA)*TSttoO*TStUtoOJN	,	σStJ * (1-TStJtoA)*TSttoO	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*TOKUtoOKN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*TOJUtoOJN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)	,	0	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*(1-TStUtoOAN)	,	0	,	σStA * (1-TStAtoE)*TSttoO*(1-TStUtoOAN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*TStUtoOAN	,	σStJ * TStJtoA*TSttoO	,	σStA * (1-TStAtoE)*TSttoO*TStUtoOAN	,	σStA * (1-TStAtoE)*TSttoO	,	0	,	0	,	0	,	σShJ * TShJtoA*TShtoO	,	σShJ * TShJtoA*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*TOAUtoOAN	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*(1-TStUtoOAN)	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	,	0	,
               0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*TStUtoOAN	,	σStA * TStAtoE*TSttoO	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	σShA *TShAtoE*TShtoO	,	σShA *TShAtoE*TShtoO	,	σShE*TShtoO	,	σShE*TShtoO	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	
) ,nrow=28,byrow=T)


MatF<-matrix(c(    σFK *TFKtoJ*bFJ *(1- TFtoSh-TFtoO)*(1-TFKUtoFKN)	,	σFJ * bFJ	,	0	,	σFA * bFA	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *(1- TSttoSh-TSttoO)	,	σStJ *bStJ *(1- TSttoSh-TSttoO)	,	0	,	σStA *bStA *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *TOtoStK*(1-TOKUtoOKN)	,	σOJ *bOJ*TOtoStJ*(1-TOJUtoOJN)	,	0	,	σOA *bOA* TOtoStA	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σStK *TStKtoJ*bStJ *TSttoSh)/2	,	(σStJ *bStJ *TSttoSh)/2	,	0	,	(σStA *bStA *TSttoSh)/2	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σOK *TOKtoJ*bOJ *TOtoShK*(1-TOKUtoOKN))/2	,	(σOJ *bOJ*TOtoShJ*(1-TOJUtoOJN))/2	,	0	,	(σOA *bOA* TOtoShA)/2	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *TSttoO	,	σStJ *bStJ *TSttoO	,	0	,	σStA *bStA *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *(1- TOtoShK-TOtoStK)*(1-TOKUtoOKN)	,	σOJ *bOJ*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	σOA *bOA*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	
) ,nrow=28,byrow=T)

MatA<-MatU+MatF
Catvec2<-c(2.60E+03	,
           8.76E+02	,
           8.85E+00	,
           1.60E+03	,
           1.61E+01	,
           1.03E+02	,
           1.04E+00	,
           
           1.10E+02	,
           3.08E+02	,
           1.28E+01	,
           7.24E+02	,
           1.09E+03	,
           2.98E+01	,
           6.96E+01	,
           
           7.76E+01	,
           0.00E+00	,
           7.85E+00	,
           0.00E+00	,
           1.30E+02	,
           0.00E+00	,
           1.68E+01	,
           
           1.94E+03	,
           9.91E+02	,
           1.92E+03	,
           4.76E+03	,
           6.32E+04	,
           3.92E+02	,
           1.90E+04 )
VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)
VectorMatrix[1,]<-Catvec2
MATS[[1]]<-MatA
for (i in 1:119){
  CatsProject<-MATS[[i]]%*% VectorMatrix[i,]
  VectorMatrix[i+1,]<-  CatsProject
  NOwned<-sum(VectorMatrix[i+1,22:28])
  NShelter<-sum(VectorMatrix[i+1,15:21])
  NStray<-sum(VectorMatrix[i+1,8:14])
  NFeral<-sum(VectorMatrix[i+1,1:7])
  
  if(NOwned>sum(Catvec2[22:28])*1.1){
    #can't increase shelter as that is often at capacity already
    TOtoStK<-0.0009*2
    TOtoStJ<-0.0009*2
    TOtoStA<-0.0009*2
    TOtoStE<-0.0009*2
    TFtoO<-0.02*0.01
    TSttoO<-0.04*0.01
  }else if(NOwned>(sum(Catvec2[22:28])*1.01) & NOwned<(sum(Catvec2[22:28])*1.1)){
    TOtoStK<-0.0009*1.25
    TOtoStJ<-0.0009*1.25
    TOtoStA<-0.0009*1.25
    TOtoStE<-0.0009*1.25
    TFtoO<-0.02*0.25#0.0075
    TSttoO<-0.04*0.45
  }else {
    TOtoStK<-0.0009
    TOtoStJ<-0.0009
    TOtoStA<-0.0009
    TOtoStE<-0.0009
    TFtoO<-0.02#0.01
    TSttoO<-0.04
  }
  
  
  if(NShelter>(sum(Catvec2[15:21])*1.1)){
    TFtoSh<-0.003*0.25
    TSttoSh<-0.03*0.25
    TOtoShK<-0.002*0.5#
    TOtoShJ<-0.002*0.5#
    TOtoShA<-0.002*0.5#
    TOtoShE<-0.002*0.5#
    
  }
  else if(NShelter>(sum(Catvec2[15:21])*1.01) & NShelter<(sum(Catvec2[15:21])*1.1)){
    TFtoSh<-0.003*0.75
    TSttoSh<-0.03*0.75
    TOtoShK<-0.002*0.75
    TOtoShJ<-0.002*0.75
    TOtoShA<-0.002*0.75
    TOtoShE<-0.002*0.75
  }
  else {
    TFtoSh<-0.003
    TSttoSh<-0.03
    TOtoShK<-0.002
    TOtoShJ<-0.002
    TOtoShA<-0.002
    TOtoShE<-0.002
  }
  
  
  bFJ<-SeasonalReproduction[i+1]*overallbFJ
  bFA<-SeasonalReproduction[i+1]*overallbFA
  
  bOJ<-SeasonalReproduction[i+1]*overallbOJ
  bOA<-SeasonalReproduction[i+1]*overallbOA
  
  bStJ<-SeasonalReproduction[i+1]*overallbStJ
  bStA<-SeasonalReproduction[i+1]*overallbStA
  
  
  ########MEGAMODEL
  MatU<-matrix(c(σFK * (1-TFKtoJ)*(1- TFtoSh-TFtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoF	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 σFK * (TFKtoJ)*(1-TFKUtoFKN)	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 σFK * (TFKtoJ)*(TFKUtoFKN)	,	0	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	σFJ * TFJtoA*(1-TFUtoFN)	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	σFJ * TFJtoA*TFUtoFN	,	σFJ * TFJtoA	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*(1- TSttoSh-TSttoO-TSttoF)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoStK	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*(1- TSttoSh-TSttoO)	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoStJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoStJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoStJ	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoStA	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,
                 σFK * (1-TFKtoJ)*TFtoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoShK	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoSh	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoShJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*(1- TShtoO)	,	σShJ* (1-TShJtoA)*(1- TShtoO)	,	σShJ * (1-TShJtoA)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoShJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoShJ	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	σShJ* TShJtoA*(1- TShtoO)	,	σShJ * TShJtoA*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoShA	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	σShA *TShAtoE*(1- TShtoO)	,	σShA *TShAtoE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,
                 σFK * (1-TFKtoJ)*TFtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*TShtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*(1- TOtoShK-TOtoStK)	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *(1-TStUtoOKN)	,	σStJ * (1-TStJtoA)*TSttoO*(1-TStUtoOJN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *TStUtoOKN	,	σStJ * (1-TStJtoA)*TSttoO*TStUtoOJN	,	σStJ * (1-TStJtoA)*TSttoO	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*TOKUtoOKN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*TOJUtoOJN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*(1-TStUtoOAN)	,	0	,	σStA * (1-TStAtoE)*TSttoO*(1-TStUtoOAN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*TStUtoOAN	,	σStJ * TStJtoA*TSttoO	,	σStA * (1-TStAtoE)*TSttoO*TStUtoOAN	,	σStA * (1-TStAtoE)*TSttoO	,	0	,	0	,	0	,	σShJ * TShJtoA*TShtoO	,	σShJ * TShJtoA*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*TOAUtoOAN	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*(1-TStUtoOAN)	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*TStUtoOAN	,	σStA * TStAtoE*TSttoO	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	σShA *TShAtoE*TShtoO	,	σShA *TShAtoE*TShtoO	,	σShE*TShtoO	,	σShE*TShtoO	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	
  ) ,nrow=28,byrow=T)
  
  
  MatF<-matrix(c(    σFK *TFKtoJ*bFJ *(1- TFtoSh-TFtoO)*(1-TFKUtoFKN)	,	σFJ * bFJ	,	0	,	σFA * bFA	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *(1- TSttoSh-TSttoO)	,	σStJ *bStJ *(1- TSttoSh-TSttoO)	,	0	,	σStA *bStA *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *TOtoStK*(1-TOKUtoOKN)	,	σOJ *bOJ*TOtoStJ*(1-TOJUtoOJN)	,	0	,	σOA *bOA* TOtoStA	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σStK *TStKtoJ*bStJ *TSttoSh)/2	,	(σStJ *bStJ *TSttoSh)/2	,	0	,	(σStA *bStA *TSttoSh)/2	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σOK *TOKtoJ*bOJ *TOtoShK*(1-TOKUtoOKN))/2	,	(σOJ *bOJ*TOtoShJ*(1-TOJUtoOJN))/2	,	0	,	(σOA *bOA* TOtoShA)/2	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *TSttoO	,	σStJ *bStJ *TSttoO	,	0	,	σStA *bStA *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *(1- TOtoShK-TOtoStK)*(1-TOKUtoOKN)	,	σOJ *bOJ*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	σOA *bOA*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	
  ) ,nrow=28,byrow=T)
  
  MatA<-MatU+MatF
  MATS[[i+1]]<-MatA

}
Total_DLN[,x]<-rowSums(VectorMatrix)
Owned_DLN[,x]<-rowSums(VectorMatrix[,22:28])
Feral_DLN[,x]<-rowSums(VectorMatrix[,1:7])
Stray_DLN[,x]<-rowSums(VectorMatrix[,8:14])
Shelter_DLN[,x]<-rowSums(VectorMatrix[,15:21])
PG_Total_DLN[x] <- sum(Total_DLN[109:120,x])/sum(Total_DLN[1:12,x])
PG_Owned_DLN[x] <-sum(Owned_DLN[109:120,x])/sum(Owned_DLN[1:12,x])
PG_Feral_DLN[x] <-sum(Feral_DLN[109:120,x])/sum(Feral_DLN[1:12,x])
PG_Stray_DLN[x] <-sum(Stray_DLN[109:120,x])/sum(Stray_DLN[1:12,x])
PG_Shelter_DLN[x] <-sum(Shelter_DLN[109:120,x])/sum(Shelter_DLN[1:12,x])
}



#95% adult neuter rate
nyears=10#run for 10 years
lengthoftime=12*nyears
SeasonalReproduction=c(rep(c(rep(0.0366667,3),rep(0.13,6),rep(0.0366667,3)),nyears),0.0366667)#Seasonal reproduction
Total_DAN=matrix(ncol = 10, nrow=lengthoftime)
Owned_DAN = matrix(ncol = 10, nrow=lengthoftime)
Feral_DAN = matrix(ncol = 10, nrow=lengthoftime)
Stray_DAN = matrix(ncol = 10, nrow=lengthoftime)
Shelter_DAN = matrix(ncol = 10, nrow=lengthoftime)

PG_Total_DAN=numeric()
PG_Owned_DAN =numeric()
PG_Feral_DAN=numeric()
PG_Stray_DAN=numeric()
PG_Shelter_DAN =numeric()


#95%adult neutering rate

for (x in 1:10){
   TOKUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TOJUtoOJN<-0.13
  TOAUtoOAN<-c(0.878624542,0.871881461,0.864345076,0.855866643,0.846257753,0.835276164,
               0.822605099,0.807822191,0.790351481,0.769386629)[x]
  
  TStUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TStUtoOJN<-c(0.37442215,0.4073473,0.44027245,0.4731976,0.50612275,0.5390479,0.57197305,0.6048982,0.63782335,0.6707485)[x]
  TStUtoOAN<-0.95
  MATS<-list()#create empty list for time-varying matrices
  
  VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)

  σFK<-0.81
  σFJ<-0.92
  σFA<-0.96
  σFE<-0.9
  TFKUtoFKN<-0
  TFUtoFN<-0.01 
  TFtoSh<-0.003
  TFtoO<-0.02
  overallbFJ<-1.5
  overallbFA<-2.5
  bFJ<-SeasonalReproduction[1]*  overallbFJ
  bFA<-SeasonalReproduction[1]*overallbFA
  
  #SHELTER PARAMETERS
  σShK<-0.974
  σShJ<-0.993
  σShA<-0.985
  σShE<-0.9
  TShtoO<-0.63
  
  #OWNED PARAMETERS
  σOK<-0.97
  σOJ<-0.995#
  σOA<-0.995#
  σOE<-0.98
  
  TOtoStK<-0.0009
  TOtoStJ<-0.0009
  TOtoStA<-0.0009
  TOtoStE<-0.0009
  TOtoShK<-0.002
  TOtoShJ<-0.002
  TOtoShA<-0.002
  TOtoShE<-0.002
  
  overallbOJ<-1.4
  overallbOA<-2.1
  bOJ<-SeasonalReproduction[1]*  overallbOJ
  bOA<-SeasonalReproduction[1]*overallbOA
  
  #STRAY PARAMETERS
  σStK<-0.918
  σStJ<-0.97
  σStA<-0.97
  σStE<-0.9
  
  TSttoSh<-0.03
  TSttoO<-0.04
  TSttoF<-0.14186
  overallbStJ<-1.5
  overallbStA<-2.5
  bStJ<-SeasonalReproduction[1]*overallbStJ
  bStA<-SeasonalReproduction[1]*overallbStA
  
  #Aging parameters - SAS 
  p <- 0:6; TFKtoJ<-σFK^5/sum(σFK^p)
  p <- 0:6; TFJtoA<-σFJ^5/sum(σFJ^p)
  p <- 0:120; TFAtoE<-σFA^119/sum(σFA^p)
  p <- 0:6; TStKtoJ<-σStK^5/sum(σStK^p)
  p <- 0:6; TStJtoA<-σStJ^5/sum(σStJ^p)
  p <- 0:120; TStAtoE<-σStA^119/sum(σStA^p)
  p <- 0:6; TShKtoJ<-σShK^5/sum(σShK^p)
  p <- 0:6; TShJtoA<-σShJ^5/sum(σShJ^p)
  p <- 0:120; TShAtoE<-σShA^119/sum(σShA^p)
  p <- 0:6; TOKtoJ<-σOK^5/sum(σOK^p)
  p <- 0:6; TOJtoA<-σOJ^5/sum(σOJ^p)
  p <- 0:120; TOAtoE<-σOA^119/sum(σOA^p)
  
  ########MEGAMODEL
  MatU<-matrix(c(σFK * (1-TFKtoJ)*(1- TFtoSh-TFtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoF	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 σFK * (TFKtoJ)*(1-TFKUtoFKN)	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 σFK * (TFKtoJ)*(TFKUtoFKN)	,	0	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	σFJ * TFJtoA*(1-TFUtoFN)	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	σFJ * TFJtoA*TFUtoFN	,	σFJ * TFJtoA	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*(1- TSttoSh-TSttoO-TSttoF)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoStK	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*(1- TSttoSh-TSttoO)	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoStJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoStJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoStJ	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoStA	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,
                 σFK * (1-TFKtoJ)*TFtoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoShK	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoSh	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoShJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*(1- TShtoO)	,	σShJ* (1-TShJtoA)*(1- TShtoO)	,	σShJ * (1-TShJtoA)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoShJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoShJ	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	σShJ* TShJtoA*(1- TShtoO)	,	σShJ * TShJtoA*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoShA	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	σShA *TShAtoE*(1- TShtoO)	,	σShA *TShAtoE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,
                 σFK * (1-TFKtoJ)*TFtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*TShtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*(1- TOtoShK-TOtoStK)	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *(1-TStUtoOKN)	,	σStJ * (1-TStJtoA)*TSttoO*(1-TStUtoOJN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *TStUtoOKN	,	σStJ * (1-TStJtoA)*TSttoO*TStUtoOJN	,	σStJ * (1-TStJtoA)*TSttoO	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*TOKUtoOKN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*TOJUtoOJN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*(1-TStUtoOAN)	,	0	,	σStA * (1-TStAtoE)*TSttoO*(1-TStUtoOAN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*TStUtoOAN	,	σStJ * TStJtoA*TSttoO	,	σStA * (1-TStAtoE)*TSttoO*TStUtoOAN	,	σStA * (1-TStAtoE)*TSttoO	,	0	,	0	,	0	,	σShJ * TShJtoA*TShtoO	,	σShJ * TShJtoA*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*TOAUtoOAN	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*(1-TStUtoOAN)	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*TStUtoOAN	,	σStA * TStAtoE*TSttoO	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	σShA *TShAtoE*TShtoO	,	σShA *TShAtoE*TShtoO	,	σShE*TShtoO	,	σShE*TShtoO	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	
  ) ,nrow=28,byrow=T)
  
  MatF<-matrix(c(    σFK *TFKtoJ*bFJ *(1- TFtoSh-TFtoO)*(1-TFKUtoFKN)	,	σFJ * bFJ	,	0	,	σFA * bFA	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *(1- TSttoSh-TSttoO)	,	σStJ *bStJ *(1- TSttoSh-TSttoO)	,	0	,	σStA *bStA *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *TOtoStK*(1-TOKUtoOKN)	,	σOJ *bOJ*TOtoStJ*(1-TOJUtoOJN)	,	0	,	σOA *bOA* TOtoStA	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σStK *TStKtoJ*bStJ *TSttoSh)/2	,	(σStJ *bStJ *TSttoSh)/2	,	0	,	(σStA *bStA *TSttoSh)/2	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σOK *TOKtoJ*bOJ *TOtoShK*(1-TOKUtoOKN))/2	,	(σOJ *bOJ*TOtoShJ*(1-TOJUtoOJN))/2	,	0	,	(σOA *bOA* TOtoShA)/2	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *TSttoO	,	σStJ *bStJ *TSttoO	,	0	,	σStA *bStA *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *(1- TOtoShK-TOtoStK)*(1-TOKUtoOKN)	,	σOJ *bOJ*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	σOA *bOA*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	
  ) ,nrow=28,byrow=T)
  MatA<-MatU+MatF
  
  Catvec2<-c(2.60E+03	,
             8.76E+02	,
             8.85E+00	,
             1.60E+03	,
             1.61E+01	,
             1.03E+02	,
             1.04E+00	,
             
             1.10E+02	,
             3.08E+02	,
             1.28E+01	,
             7.24E+02	,
             1.09E+03	,
             2.98E+01	,
             6.96E+01	,
             
             7.76E+01	,
             0.00E+00	,
             7.85E+00	,
             0.00E+00	,
             1.30E+02	,
             0.00E+00	,
             1.68E+01	,
             
             1.94E+03	,
             9.91E+02	,
             1.92E+03	,
             4.76E+03	,
             6.32E+04	,
             3.92E+02	,
             1.90E+04 )
  VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)
  VectorMatrix[1,]<-Catvec2
  MATS[[1]]<-MatA
  for (i in 1:119){
    CatsProject<-MATS[[i]]%*% VectorMatrix[i,]
    VectorMatrix[i+1,]<-  CatsProject
    NOwned<-sum(VectorMatrix[i+1,22:28])
    NShelter<-sum(VectorMatrix[i+1,15:21])
    NStray<-sum(VectorMatrix[i+1,8:14])
    NFeral<-sum(VectorMatrix[i+1,1:7])
    
    if(NOwned>sum(Catvec2[22:28])*1.1){
      #can't increase shelter as that is often at capacity already
      TOtoStK<-0.0009*2
      TOtoStJ<-0.0009*2
      TOtoStA<-0.0009*2
      TOtoStE<-0.0009*2
      TFtoO<-0.02*0.01
      TSttoO<-0.04*0.01
    }else if(NOwned>(sum(Catvec2[22:28])*1.01) & NOwned<(sum(Catvec2[22:28])*1.1)){
      TOtoStK<-0.0009*1.25
      TOtoStJ<-0.0009*1.25
      TOtoStA<-0.0009*1.25
      TOtoStE<-0.0009*1.25
      TFtoO<-0.02*0.25
      TSttoO<-0.04*0.45
    }else {
      TOtoStK<-0.0009
      TOtoStJ<-0.0009
      TOtoStA<-0.0009
      TOtoStE<-0.0009
      TFtoO<-0.02
      TSttoO<-0.04
    }
    
    
    if(NShelter>(sum(Catvec2[15:21])*1.1)){
      TFtoSh<-0.003*0.25
      TSttoSh<-0.03*0.25
      TOtoShK<-0.002*0.5
      TOtoShJ<-0.002*0.5
      TOtoShA<-0.002*0.5
      TOtoShE<-0.002*0.5
      
    }
    else if(NShelter>(sum(Catvec2[15:21])*1.01) & NShelter<(sum(Catvec2[15:21])*1.1)){
      TFtoSh<-0.003*0.75
      TSttoSh<-0.03*0.75
      TOtoShK<-0.002*0.75
      TOtoShJ<-0.002*0.75
      TOtoShA<-0.002*0.75
      TOtoShE<-0.002*0.75
    }
    else {
      TFtoSh<-0.003
      TSttoSh<-0.03
      TOtoShK<-0.002
      TOtoShJ<-0.002
      TOtoShA<-0.002
      TOtoShE<-0.002
    }
    
    
    bFJ<-SeasonalReproduction[i+1]*overallbFJ
    bFA<-SeasonalReproduction[i+1]*overallbFA
    
    bOJ<-SeasonalReproduction[i+1]*overallbOJ
    bOA<-SeasonalReproduction[i+1]*overallbOA
    
    bStJ<-SeasonalReproduction[i+1]*overallbStJ
    bStA<-SeasonalReproduction[i+1]*overallbStA
    
    
    ########MEGAMODEL
    MatU<-matrix(c(σFK * (1-TFKtoJ)*(1- TFtoSh-TFtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoF	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   σFK * (TFKtoJ)*(1-TFKUtoFKN)	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   σFK * (TFKtoJ)*(TFKUtoFKN)	,	0	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	σFJ * TFJtoA*(1-TFUtoFN)	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	σFJ * TFJtoA*TFUtoFN	,	σFJ * TFJtoA	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*(1- TSttoSh-TSttoO-TSttoF)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoStK	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*(1- TSttoSh-TSttoO)	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoStJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoStJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoStJ	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoStA	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,
                   σFK * (1-TFKtoJ)*TFtoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoShK	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoSh	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoShJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*(1- TShtoO)	,	σShJ* (1-TShJtoA)*(1- TShtoO)	,	σShJ * (1-TShJtoA)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoShJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoShJ	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	σShJ* TShJtoA*(1- TShtoO)	,	σShJ * TShJtoA*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoShA	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	σShA *TShAtoE*(1- TShtoO)	,	σShA *TShAtoE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,
                   σFK * (1-TFKtoJ)*TFtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*TShtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*(1- TOtoShK-TOtoStK)	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *(1-TStUtoOKN)	,	σStJ * (1-TStJtoA)*TSttoO*(1-TStUtoOJN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *TStUtoOKN	,	σStJ * (1-TStJtoA)*TSttoO*TStUtoOJN	,	σStJ * (1-TStJtoA)*TSttoO	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*TOKUtoOKN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*TOJUtoOJN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*(1-TStUtoOAN)	,	0	,	σStA * (1-TStAtoE)*TSttoO*(1-TStUtoOAN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*TStUtoOAN	,	σStJ * TStJtoA*TSttoO	,	σStA * (1-TStAtoE)*TSttoO*TStUtoOAN	,	σStA * (1-TStAtoE)*TSttoO	,	0	,	0	,	0	,	σShJ * TShJtoA*TShtoO	,	σShJ * TShJtoA*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*TOAUtoOAN	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*(1-TStUtoOAN)	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*TStUtoOAN	,	σStA * TStAtoE*TSttoO	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	σShA *TShAtoE*TShtoO	,	σShA *TShAtoE*TShtoO	,	σShE*TShtoO	,	σShE*TShtoO	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	
    ) ,nrow=28,byrow=T)
    
    MatF<-matrix(c(    σFK *TFKtoJ*bFJ *(1- TFtoSh-TFtoO)*(1-TFKUtoFKN)	,	σFJ * bFJ	,	0	,	σFA * bFA	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *(1- TSttoSh-TSttoO)	,	σStJ *bStJ *(1- TSttoSh-TSttoO)	,	0	,	σStA *bStA *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *TOtoStK*(1-TOKUtoOKN)	,	σOJ *bOJ*TOtoStJ*(1-TOJUtoOJN)	,	0	,	σOA *bOA* TOtoStA	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σStK *TStKtoJ*bStJ *TSttoSh)/2	,	(σStJ *bStJ *TSttoSh)/2	,	0	,	(σStA *bStA *TSttoSh)/2	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σOK *TOKtoJ*bOJ *TOtoShK*(1-TOKUtoOKN))/2	,	(σOJ *bOJ*TOtoShJ*(1-TOJUtoOJN))/2	,	0	,	(σOA *bOA* TOtoShA)/2	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *TSttoO	,	σStJ *bStJ *TSttoO	,	0	,	σStA *bStA *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *(1- TOtoShK-TOtoStK)*(1-TOKUtoOKN)	,	σOJ *bOJ*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	σOA *bOA*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                       0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	
    ) ,nrow=28,byrow=T)
    
    MatA<-MatU+MatF
    MATS[[i+1]]<-MatA
    
  }
  Total_DAN[,x]<-rowSums(VectorMatrix)
  Owned_DAN[,x]<-rowSums(VectorMatrix[,22:28])
  Feral_DAN[,x]<-rowSums(VectorMatrix[,1:7])
  Stray_DAN[,x]<-rowSums(VectorMatrix[,8:14])
  Shelter_DAN[,x]<-rowSums(VectorMatrix[,15:21])
  PG_Total_DAN[x] <- sum(Total_DAN[109:120,x])/sum(Total_DAN[1:12,x])
  PG_Owned_DAN[x] <-sum(Owned_DAN[109:120,x])/sum(Owned_DAN[1:12,x])
  PG_Feral_DAN[x] <-sum(Feral_DAN[109:120,x])/sum(Feral_DAN[1:12,x])
  PG_Stray_DAN[x] <-sum(Stray_DAN[109:120,x])/sum(Stray_DAN[1:12,x])
  PG_Shelter_DAN[x] <-sum(Shelter_DAN[109:120,x])/sum(Shelter_DAN[1:12,x])
}




#98% adult neuter rate
nyears=10#run for 10 years
lengthoftime=12*nyears
SeasonalReproduction=c(rep(c(rep(0.0366667,3),rep(0.13,6),rep(0.0366667,3)),nyears),0.0366667)#Seasonal reproduction
Total_DHN=matrix(ncol = 10, nrow=lengthoftime)
Owned_DHN = matrix(ncol = 10, nrow=lengthoftime)
Feral_DHN = matrix(ncol = 10, nrow=lengthoftime)
Stray_DHN = matrix(ncol = 10, nrow=lengthoftime)
Shelter_DHN = matrix(ncol = 10, nrow=lengthoftime)

PG_Total_DHN=numeric()
PG_Owned_DHN =numeric()
PG_Feral_DHN=numeric()
PG_Stray_DHN=numeric()
PG_Shelter_DHN =numeric()


#98%adult neutering rate

for (x in 1:10){
  TOKUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TOJUtoOJN<-0.13
  TOAUtoOAN<-c(0.951449817, 0.948752584, 0.94573803,  0.942346657, 0.938503101, 0.934110465, 0.92904204,0.923128876,
               0.916140592, 0.907754652)[x]
  
  TStUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TStUtoOJN<-c(0.37442215,0.4073473,0.44027245,0.4731976,0.50612275,0.5390479,0.57197305,0.6048982,0.63782335,0.6707485)[x]
  TStUtoOAN<-0.98
  MATS<-list()#create empty list for time-varying matrices
  
  VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)
  
  σFK<-0.81
  σFJ<-0.92
  σFA<-0.96
  σFE<-0.9
  TFKUtoFKN<-0
  TFUtoFN<-0.01 
  TFtoSh<-0.003
  TFtoO<-0.02
  overallbFJ<-1.5
  overallbFA<-2.5
  bFJ<-SeasonalReproduction[1]*  overallbFJ
  bFA<-SeasonalReproduction[1]*overallbFA
  
  #SHELTER PARAMETERS
  σShK<-0.974
  σShJ<-0.993
  σShA<-0.985
  σShE<-0.9
  TShtoO<-0.63
  
  #OWNED PARAMETERS
  σOK<-0.97
  σOJ<-0.995
  σOA<-0.995
  σOE<-0.98
  
  TOtoStK<-0.0009
  TOtoStJ<-0.0009
  TOtoStA<-0.0009
  TOtoStE<-0.0009
  TOtoShK<-0.002
  TOtoShJ<-0.002
  TOtoShA<-0.002
  TOtoShE<-0.002
  
  overallbOJ<-1.4
  overallbOA<-2.1
  bOJ<-SeasonalReproduction[1]*  overallbOJ
  bOA<-SeasonalReproduction[1]*overallbOA
  
  #STRAY PARAMETERS
  σStK<-0.918
  σStJ<-0.97
  σStA<-0.97
  σStE<-0.9
  
  
  TSttoSh<-0.03
  TSttoO<-0.04
  TSttoF<-0.14186
  overallbStJ<-1.5
  overallbStA<-2.5
  bStJ<-SeasonalReproduction[1]*overallbStJ
  bStA<-SeasonalReproduction[1]*overallbStA
  
  #Aging parameters - SAS 
  p <- 0:6; TFKtoJ<-σFK^5/sum(σFK^p)
  p <- 0:6; TFJtoA<-σFJ^5/sum(σFJ^p)
  p <- 0:120; TFAtoE<-σFA^119/sum(σFA^p)
  p <- 0:6; TStKtoJ<-σStK^5/sum(σStK^p)
  p <- 0:6; TStJtoA<-σStJ^5/sum(σStJ^p)
  p <- 0:120; TStAtoE<-σStA^119/sum(σStA^p)
  p <- 0:6; TShKtoJ<-σShK^5/sum(σShK^p)
  p <- 0:6; TShJtoA<-σShJ^5/sum(σShJ^p)
  p <- 0:120; TShAtoE<-σShA^119/sum(σShA^p)
  p <- 0:6; TOKtoJ<-σOK^5/sum(σOK^p)
  p <- 0:6; TOJtoA<-σOJ^5/sum(σOJ^p)
  p <- 0:120; TOAtoE<-σOA^119/sum(σOA^p)
  
  ########MEGAMODEL
  MatU<-matrix(c(σFK * (1-TFKtoJ)*(1- TFtoSh-TFtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoF	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 σFK * (TFKtoJ)*(1-TFKUtoFKN)	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 σFK * (TFKtoJ)*(TFKUtoFKN)	,	0	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	σFJ * TFJtoA*(1-TFUtoFN)	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	σFJ * TFJtoA*TFUtoFN	,	σFJ * TFJtoA	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*(1- TSttoSh-TSttoO-TSttoF)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoStK	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*(1- TSttoSh-TSttoO)	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoStJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoStJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoStJ	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoStA	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,
                 σFK * (1-TFKtoJ)*TFtoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoShK	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoSh	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoShJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*(1- TShtoO)	,	σShJ* (1-TShJtoA)*(1- TShtoO)	,	σShJ * (1-TShJtoA)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoShJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoShJ	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	σShJ* TShJtoA*(1- TShtoO)	,	σShJ * TShJtoA*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoShA	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	σShA *TShAtoE*(1- TShtoO)	,	σShA *TShAtoE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,
                 σFK * (1-TFKtoJ)*TFtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*TShtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*(1- TOtoShK-TOtoStK)	,	0	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *(1-TStUtoOKN)	,	σStJ * (1-TStJtoA)*TSttoO*(1-TStUtoOJN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *TStUtoOKN	,	σStJ * (1-TStJtoA)*TSttoO*TStUtoOJN	,	σStJ * (1-TStJtoA)*TSttoO	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*TOKUtoOKN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*TOJUtoOJN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)	,	0	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*(1-TStUtoOAN)	,	0	,	σStA * (1-TStAtoE)*TSttoO*(1-TStUtoOAN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*TStUtoOAN	,	σStJ * TStJtoA*TSttoO	,	σStA * (1-TStAtoE)*TSttoO*TStUtoOAN	,	σStA * (1-TStAtoE)*TSttoO	,	0	,	0	,	0	,	σShJ * TShJtoA*TShtoO	,	σShJ * TShJtoA*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*TOAUtoOAN	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*(1-TStUtoOAN)	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	,	0	,
                 0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*TStUtoOAN	,	σStA * TStAtoE*TSttoO	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	σShA *TShAtoE*TShtoO	,	σShA *TShAtoE*TShtoO	,	σShE*TShtoO	,	σShE*TShtoO	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	
  ) ,nrow=28,byrow=T)
  
  
  MatF<-matrix(c(  0	,	σFJ * (1-TFUtoFN)*bFJ	,	0	,	σFA * (1-TFUtoFN)*bFA	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *(1- TSttoSh-TSttoO)	,	σStJ *bStJ *(1- TSttoSh-TSttoO)	,	0	,	σStA *bStA *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *TOtoStK*(1-TOKUtoOKN)	,	σOJ *bOJ*TOtoStJ*(1-TOJUtoOJN)	,	0	,	σOA *bOA* TOtoStA	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σStK *TStKtoJ*bStJ *TSttoSh)/2	,	(σStJ *bStJ *TSttoSh)/2	,	0	,	(σStA *bStA *TSttoSh)/2	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σOK *TOKtoJ*bOJ *TOtoShK*(1-TOKUtoOKN))/2	,	(σOJ *bOJ*TOtoShJ*(1-TOJUtoOJN))/2	,	0	,	(σOA *bOA* TOtoShA)/2	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *TSttoO	,	σStJ *bStJ *TSttoO	,	0	,	σStA *bStA *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *(1- TOtoShK-TOtoStK)*(1-TOKUtoOKN)	,	σOJ *bOJ*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	σOA *bOA*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	
  ) ,nrow=28,byrow=T)
  
  MatA<-MatU+MatF
  Catvec2<-c(2.60E+03	,
             8.76E+02	,
             8.85E+00	,
             1.60E+03	,
             1.61E+01	,
             1.03E+02	,
             1.04E+00	,
             
             1.10E+02	,
             3.08E+02	,
             1.28E+01	,
             7.24E+02	,
             1.09E+03	,
             2.98E+01	,
             6.96E+01	,
             
             7.76E+01	,
             0.00E+00	,
             7.85E+00	,
             0.00E+00	,
             1.30E+02	,
             0.00E+00	,
             1.68E+01	,
             
             1.94E+03	,
             9.91E+02	,
             1.92E+03	,
             4.76E+03	,
             6.32E+04	,
             3.92E+02	,
             1.90E+04 )
  VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)
  VectorMatrix[1,]<-Catvec2
  MATS[[1]]<-MatA
  for (i in 1:119){
    CatsProject<-MATS[[i]]%*% VectorMatrix[i,]
    VectorMatrix[i+1,]<-  CatsProject
    NOwned<-sum(VectorMatrix[i+1,22:28])
    NShelter<-sum(VectorMatrix[i+1,15:21])
    NStray<-sum(VectorMatrix[i+1,8:14])
    NFeral<-sum(VectorMatrix[i+1,1:7])
    if(NOwned>sum(Catvec2[22:28])*1.1){
      #can't increase shelter as that is often at capacity already
      TOtoStK<-0.0009*2
      TOtoStJ<-0.0009*2
      TOtoStA<-0.0009*2
      TOtoStE<-0.0009*2
      TFtoO<-0.02*0.01
      TSttoO<-0.04*0.01
    }else if(NOwned>(sum(Catvec2[22:28])*1.01) & NOwned<(sum(Catvec2[22:28])*1.1)){
      TOtoStK<-0.0009*1.25
      TOtoStJ<-0.0009*1.25
      TOtoStA<-0.0009*1.25
      TOtoStE<-0.0009*1.25
      TFtoO<-0.02*0.25
      TSttoO<-0.04*0.45
    }else {
      TOtoStK<-0.0009
      TOtoStJ<-0.0009
      TOtoStA<-0.0009
      TOtoStE<-0.0009
      TFtoO<-0.02
      TSttoO<-0.04
    }
    
    
    if(NShelter>(sum(Catvec2[15:21])*1.1)){
      TFtoSh<-0.003*0.25
      TSttoSh<-0.03*0.25
      TOtoShK<-0.002*0.5
      TOtoShJ<-0.002*0.5
      TOtoShA<-0.002*0.5
      TOtoShE<-0.002*0.5
      
    }
    else if(NShelter>(sum(Catvec2[15:21])*1.01) & NShelter<(sum(Catvec2[15:21])*1.1)){
      TFtoSh<-0.003*0.75
      TSttoSh<-0.03*0.75
      TOtoShK<-0.002*0.75
      TOtoShJ<-0.002*0.75
      TOtoShA<-0.002*0.75
      TOtoShE<-0.002*0.75
    }
    else {
      TFtoSh<-0.003
      TSttoSh<-0.03
      TOtoShK<-0.002
      TOtoShJ<-0.002
      TOtoShA<-0.002
      TOtoShE<-0.002
    }
    bFJ<-SeasonalReproduction[i+1]*overallbFJ
    bFA<-SeasonalReproduction[i+1]*overallbFA
    
    bOJ<-SeasonalReproduction[i+1]*overallbOJ
    bOA<-SeasonalReproduction[i+1]*overallbOA
    
    bStJ<-SeasonalReproduction[i+1]*overallbStJ
    bStA<-SeasonalReproduction[i+1]*overallbStA
    
    
    ########MEGAMODEL
    MatU<-matrix(c(σFK * (1-TFKtoJ)*(1- TFtoSh-TFtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoF	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   σFK * (TFKtoJ)*(1-TFKUtoFKN)	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   σFK * (TFKtoJ)*(TFKUtoFKN)	,	0	,	σFJ * (1-TFJtoA)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	σFJ * TFJtoA*(1-TFUtoFN)	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	σFJ * TFJtoA*TFUtoFN	,	σFJ * TFJtoA	,	0	,	σFA*(1- TFAtoE)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	σFA* TFAtoE	,	0	,	σFE	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*(1- TSttoSh-TSttoO-TSttoF)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoStK	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*(1- TSttoSh-TSttoO)	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoStJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoStJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoStJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoStJ	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*(1- TSttoSh-TSttoO)	,	0	,	σStA * (1-TStAtoE)*(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoStA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoStA	,	0	,	σOA * (1-TOAtoE)*TOtoStA	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*(1- TSttoSh-TSttoO)	,	0	,	σStE *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoStE	,	0	,	σOE * TOtoStE	,
                   σFK * (1-TFKtoJ)*TFtoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*TOtoShK	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoSh	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)* TOtoShJ*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * (1-TStJtoA)*TSttoSh	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*(1- TShtoO)	,	σShJ* (1-TShJtoA)*(1- TShtoO)	,	σShJ * (1-TShJtoA)*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*TOtoShJ*TOKUtoOKN	,	σOJ * (1-TOJtoA)*TOtoShJ*TOJUtoOJN	,	σOJ * (1-TOJtoA)*TOtoShJ	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoSh	,	0	,	σStA * (1-TStAtoE)*TSttoSh	,	0	,	0	,	0	,	σShJ* TShJtoA*(1- TShtoO)	,	σShJ * TShJtoA*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	σShA* (1-TShAtoE)*(1- TShtoO)	,	0	,	0	,	0	,	σOJ * TOJtoA*TOtoShA*TOAUtoOAN	,	σOJ * TOJtoA*TOtoShA	,	0	,	σOA * (1-TOAtoE)*TOtoShA	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoSh	,	0	,	σStE *TSttoSh	,	0	,	0	,	0	,	σShA *TShAtoE*(1- TShtoO)	,	σShA *TShAtoE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	σShE*(1- TShtoO)	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*TOtoShE	,	0	,	σOE * TOtoShE	,
                   σFK * (1-TFKtoJ)*TFtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * (1-TStKtoJ)*TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σShK * (1-TShKtoJ)*TShtoO	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * (1-TOKtoJ)*(1- TOtoShK-TOtoStK)	,	0	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *(1-TStUtoOKN)	,	σStJ * (1-TStJtoA)*TSttoO*(1-TStUtoOJN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*(1-TOKUtoOKN)	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK * TStKtoJ*TSttoO *TStUtoOKN	,	σStJ * (1-TStJtoA)*TSttoO*TStUtoOJN	,	σStJ * (1-TStJtoA)*TSttoO	,	0	,	0	,	0	,	0	,	σShK * TShKtoJ*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	σShJ * (1-TShJtoA)*TShtoO	,	0	,	0	,	0	,	0	,	σOK * TOKtoJ*(1- TOtoShJ-TOtoStJ)*TOKUtoOKN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)*TOJUtoOJN	,	σOJ * (1-TOJtoA)*(1- TOtoShJ-TOtoStJ)	,	0	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*(1-TStUtoOAN)	,	0	,	σStA * (1-TStAtoE)*TSttoO*(1-TStUtoOAN)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*(1-TOAUtoOAN)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStJ * TStJtoA*TSttoO*TStUtoOAN	,	σStJ * TStJtoA*TSttoO	,	σStA * (1-TStAtoE)*TSttoO*TStUtoOAN	,	σStA * (1-TStAtoE)*TSttoO	,	0	,	0	,	0	,	σShJ * TShJtoA*TShtoO	,	σShJ * TShJtoA*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	σShA* (1-TShAtoE)*TShtoO	,	0	,	0	,	0	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)*TOAUtoOAN	,	σOJ * TOJtoA*(1- TOtoShA-TOtoStA)	,	0	,	σOA * (1-TOAtoE)*(1- TOtoShA-TOtoStA)	,	0	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*(1-TStUtoOAN)	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	,	0	,
                   0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStA * TStAtoE*TSttoO*TStUtoOAN	,	σStA * TStAtoE*TSttoO	,	0	,	σStE *TSttoO	,	0	,	0	,	0	,	σShA *TShAtoE*TShtoO	,	σShA *TShAtoE*TShtoO	,	σShE*TShtoO	,	σShE*TShtoO	,	0	,	0	,	0	,	0	,	σOA * TOAtoE*(1- TOtoShE-TOtoStE)	,	0	,	σOE * (1- TOtoShE-TOtoStE)	
    ) ,nrow=28,byrow=T)
    
    
    MatF<-matrix(c(  0	,	σFJ * (1-TFUtoFN)*bFJ	,	0	,	σFA * (1-TFUtoFN)*bFA	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *(1- TSttoSh-TSttoO)	,	σStJ *bStJ *(1- TSttoSh-TSttoO)	,	0	,	σStA *bStA *(1- TSttoSh-TSttoO)	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *TOtoStK*(1-TOKUtoOKN)	,	σOJ *bOJ*TOtoStJ*(1-TOJUtoOJN)	,	0	,	σOA *bOA* TOtoStA	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σStK *TStKtoJ*bStJ *TSttoSh)/2	,	(σStJ *bStJ *TSttoSh)/2	,	0	,	(σStA *bStA *TSttoSh)/2	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	(σOK *TOKtoJ*bOJ *TOtoShK*(1-TOKUtoOKN))/2	,	(σOJ *bOJ*TOtoShJ*(1-TOJUtoOJN))/2	,	0	,	(σOA *bOA* TOtoShA)/2	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	σStK *TStKtoJ*bStJ *TSttoO	,	σStJ *bStJ *TSttoO	,	0	,	σStA *bStA *TSttoO	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	σOK *TOKtoJ*bOJ *(1- TOtoShK-TOtoStK)*(1-TOKUtoOKN)	,	σOJ *bOJ*(1- TOtoShJ-TOtoStJ)*(1-TOJUtoOJN)	,	0	,	σOA *bOA*(1- TOtoShA-TOtoStA)	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,
                     0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	,	0	
    ) ,nrow=28,byrow=T)
    
    MatA<-MatU+MatF
    MATS[[i+1]]<-MatA
    
  }
  Total_DHN[,x]<-rowSums(VectorMatrix)
  Owned_DHN[,x]<-rowSums(VectorMatrix[,22:28])
  Feral_DHN[,x]<-rowSums(VectorMatrix[,1:7])
  Stray_DHN[,x]<-rowSums(VectorMatrix[,8:14])
  Shelter_DHN[,x]<-rowSums(VectorMatrix[,15:21])
  PG_Total_DHN[x] <- sum(Total_DHN[109:120,x])/sum(Total_DHN[1:12,x])
  PG_Owned_DHN[x] <-sum(Owned_DHN[109:120,x])/sum(Owned_DHN[1:12,x])
  PG_Feral_DHN[x] <-sum(Feral_DHN[109:120,x])/sum(Feral_DHN[1:12,x])
  PG_Stray_DHN[x] <-sum(Stray_DHN[109:120,x])/sum(Stray_DHN[1:12,x])
  PG_Shelter_DHN[x] <-sum(Shelter_DHN[109:120,x])/sum(Shelter_DHN[1:12,x])
}


par(mar = c(4,4,1,1))
layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))

library("RColorBrewer")
#display.brewer.all()
colscheme<-brewer.pal(3, "Set2")

plot(PG_Total_DAN~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Total PGR",ylim=c(min(PG_Total_DHN),max(PG_Total_DLN)+0.2),cex.lab=1.2,cex.axis=1.2, cex=1.2)
lines(seq(0.05,0.5,length.out=10),PG_Total_DLN,lty="solid",col=colscheme[1],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Total_DAN,lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Total_DHN,lty="solid",col=colscheme[3],lwd=3)
legend("topright", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0.01),title="owned adult neutering prevalence")

mtext("a",adj=-0.07,mar=TRUE,cex=1.2)

plot(PG_Owned_DAN~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Owned PGR",ylim=c(min(PG_Owned_DHN),max(PG_Owned_DLN)+0.2),cex.lab=1.2,cex.axis=1.2,cex=1.2)
lines(seq(0.05,0.5,length.out=10),PG_Owned_DLN,lty="solid",col=colscheme[1],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Owned_DAN,lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Owned_DHN,lty="solid",col=colscheme[3],lwd=3)
legend("topright", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0.01),title="owned adult neutering prevalence")
mtext("b",adj=-0.15,mar=TRUE,cex=1.2)
plot(PG_Stray_DAN~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Stray PGR",ylim=c(min(PG_Stray_DHN),max(PG_Stray_DLN)+0.2),cex.lab=1.2,cex.axis=1.2,cex=1.2)
lines(seq(0.05,0.5,length.out=10),PG_Stray_DLN,lty="solid",col=colscheme[1],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Stray_DAN,lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Stray_DHN,lty="solid",col=colscheme[3],lwd=3)
legend("topright", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0.01),title="owned adult neutering prevalence")
mtext("c",adj=-0.15,mar=TRUE,cex=1.2)
plot(PG_Feral_DAN~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Feral PGR",ylim=c(min(PG_Feral_DHN),max(PG_Feral_DLN)+0.2),cex.lab=1.2,cex.axis=1.2,cex=1.2)
lines(seq(0.05,0.5,length.out=10),PG_Feral_DLN,lty="solid",col=colscheme[1],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Feral_DAN,lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Feral_DHN,lty="solid",col=colscheme[3],lwd=3)
legend("topright", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0.01),title="owned adult neutering prevalence")
mtext("d",adj=-0.15,mar=TRUE,cex=1.2)
plot(PG_Shelter_DAN~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Shelter PGR",ylim=c(min(PG_Shelter_DHN),max(PG_Shelter_DLN)+0.2),cex.lab=1.2,cex.axis=1.2,cex=1.2)
lines(seq(0.05,0.5,length.out=10),PG_Shelter_DLN,lty="solid",col=colscheme[1],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Shelter_DAN,lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),PG_Shelter_DHN,lty="solid",col=colscheme[3],lwd=3)
legend("topright", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0.01),title="owned adult neutering prevalence")

mtext("e",adj=-0.15,mar=TRUE,cex=1.2)

# W 743 H730
par(mar = c(4,4,4,1))
par(oma=c(3,3,0,0),mar=c(3,3,2,2))

layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))


data <- matrix(c(median(Owned_DLN[109:120,1]),median(Stray_DLN[109:120,1]),median(Feral_DLN[109:120,1]),median(Shelter_DLN[109:120,1]),
                median(Owned_DLN[109:120,2]),median(Stray_DLN[109:120,2]),median(Feral_DLN[109:120,2]),median(Shelter_DLN[109:120,2]),
                median(Owned_DLN[109:120,3]),median(Stray_DLN[109:120,3]),median(Feral_DLN[109:120,3]),median(Shelter_DLN[109:120,3]),
                median(Owned_DLN[109:120,4]),median(Stray_DLN[109:120,4]),median(Feral_DLN[109:120,4]),median(Shelter_DLN[109:120,4]),
                median(Owned_DLN[109:120,5]),median(Stray_DLN[109:120,5]),median(Feral_DLN[109:120,5]),median(Shelter_DLN[109:120,5]),
                median(Owned_DLN[109:120,6]),median(Stray_DLN[109:120,6]),median(Feral_DLN[109:120,6]),median(Shelter_DLN[109:120,6]),
                median(Owned_DLN[109:120,7]),median(Stray_DLN[109:120,7]),median(Feral_DLN[109:120,7]),median(Shelter_DLN[109:120,7]),
                median(Owned_DLN[109:120,8]),median(Stray_DLN[109:120,8]),median(Feral_DLN[109:120,8]),median(Shelter_DLN[109:120,8]),
                median(Owned_DLN[109:120,9]),median(Stray_DLN[109:120,9]),median(Feral_DLN[109:120,9]),median(Shelter_DLN[109:120,9]),
                median(Owned_DLN[109:120,10]),median(Stray_DLN[109:120,10]),median(Feral_DLN[109:120,10]),median(Shelter_DLN[109:120,10])), 
               nrow=4,byrow=F)

colnames(data) <- round(seq(0.05,0.5,length.out=10),2)
rownames(data) <- c("owned","stray","feral","shelter")
barplot(data, col=coul , border="white", xlab="Proportion owned cats neutered prepubertally",ylab="median number of cats",main="low neutering",cex.axis=1.2, cex.names=1.2,cex.lab=1.2,cex.main=1.5,ylim=c(0,200000))

abline(h=sum(Catvec2),xpd=FALSE,lty=3)
colSums(data)[2]/colSums(data)[1]
mtext("a",adj=-0.4,mar=TRUE,cex=1.5)

data2 <- matrix(c(median(Owned_DAN[109:120,1]),median(Stray_DAN[109:120,1]),median(Feral_DAN[109:120,1]),median(Shelter_DAN[109:120,1]),
                 median(Owned_DAN[109:120,2]),median(Stray_DAN[109:120,2]),median(Feral_DAN[109:120,2]),median(Shelter_DAN[109:120,2]),
                 median(Owned_DAN[109:120,3]),median(Stray_DAN[109:120,3]),median(Feral_DAN[109:120,3]),median(Shelter_DAN[109:120,3]),
                 median(Owned_DAN[109:120,4]),median(Stray_DAN[109:120,4]),median(Feral_DAN[109:120,4]),median(Shelter_DAN[109:120,4]),
                 median(Owned_DAN[109:120,5]),median(Stray_DAN[109:120,5]),median(Feral_DAN[109:120,5]),median(Shelter_DAN[109:120,5]),
                 median(Owned_DAN[109:120,6]),median(Stray_DAN[109:120,6]),median(Feral_DAN[109:120,6]),median(Shelter_DAN[109:120,6]),
                 median(Owned_DAN[109:120,7]),median(Stray_DAN[109:120,7]),median(Feral_DAN[109:120,7]),median(Shelter_DAN[109:120,7]),
                 median(Owned_DAN[109:120,8]),median(Stray_DAN[109:120,8]),median(Feral_DAN[109:120,8]),median(Shelter_DAN[109:120,8]),
                 median(Owned_DAN[109:120,9]),median(Stray_DAN[109:120,9]),median(Feral_DAN[109:120,9]),median(Shelter_DAN[109:120,9]),
                 median(Owned_DAN[109:120,10]),median(Stray_DAN[109:120,10]),median(Feral_DAN[109:120,10]),median(Shelter_DAN[109:120,10])), 
               nrow=4,byrow=F)


colnames(data2) <- round(seq(0.05,0.5,length.out=10),2)
rownames(data2) <- c("owned","stray","feral","shelter")
barplot(data2, col=coul , border="white", xlab="Proportion owned cats neutered prepubertally",ylab="median number of cats",main="medium neutering",cex.axis=1.2, cex.names=1.2,cex.lab=1.2,cex.main=1.5,ylim=c(0,200000))
#legend("top", inset=c(0,-0.1),legend = c("owned","stray","feral","shelter") , 
#      col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz =TRUE)
abline(h=sum(Catvec2),xpd=FALSE,lty=3)
mtext("b",adj=-0.4,mar=TRUE,cex=1.5)

data3 <- matrix(c(median(Owned_DHN[109:120,1]),median(Stray_DHN[109:120,1]),median(Feral_DHN[109:120,1]),median(Shelter_DHN[109:120,1]),
                  median(Owned_DHN[109:120,2]),median(Stray_DHN[109:120,2]),median(Feral_DHN[109:120,2]),median(Shelter_DHN[109:120,2]),
                  median(Owned_DHN[109:120,3]),median(Stray_DHN[109:120,3]),median(Feral_DHN[109:120,3]),median(Shelter_DHN[109:120,3]),
                  median(Owned_DHN[109:120,4]),median(Stray_DHN[109:120,4]),median(Feral_DHN[109:120,4]),median(Shelter_DHN[109:120,4]),
                  median(Owned_DHN[109:120,5]),median(Stray_DHN[109:120,5]),median(Feral_DHN[109:120,5]),median(Shelter_DHN[109:120,5]),
                  median(Owned_DHN[109:120,6]),median(Stray_DHN[109:120,6]),median(Feral_DHN[109:120,6]),median(Shelter_DHN[109:120,6]),
                  median(Owned_DHN[109:120,7]),median(Stray_DHN[109:120,7]),median(Feral_DHN[109:120,7]),median(Shelter_DHN[109:120,7]),
                  median(Owned_DHN[109:120,8]),median(Stray_DHN[109:120,8]),median(Feral_DHN[109:120,8]),median(Shelter_DHN[109:120,8]),
                  median(Owned_DHN[109:120,9]),median(Stray_DHN[109:120,9]),median(Feral_DHN[109:120,9]),median(Shelter_DHN[109:120,9]),
                  median(Owned_DHN[109:120,10]),median(Stray_DHN[109:120,10]),median(Feral_DHN[109:120,10]),median(Shelter_DHN[109:120,10])), 
                nrow=4,byrow=F)



colnames(data3) <- round(seq(0.05,0.5,length.out=10),2)
rownames(data3) <- c("owned","stray","feral","shelter")
barplot(data3, col=coul , border="white", xlab="Proportion owned cats neutered prepubertally",ylab="median numbers of cats",main="high neutering",cex.axis=1.2, cex.names=1.2,cex.lab=1.2,cex.main=1.5,ylim=c(0,200000))
#legend("top", inset=c(-0.3,-0.35),legend = c("owned","stray","feral","shelter") , 
#      col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz =TRUE)
legend("topright", inset=c(0,0),legend = c("owned","stray","feral","shelter") , 
       col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, horiz =FALSE)
abline(h=sum(Catvec2),xpd=FALSE,lty=3)
mtext("c",adj=-0.4,mar=TRUE,cex=1.5)




# Transform this data in %
data_percentage <- apply(data, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
barplot(data_percentage[2:4,], col=coul[2:4] , border="white", xlab="Proportion owned cats neutered prepubertally",ylab="",ylim=c(0,15),cex.axis=1.2, cex.names=1.2,cex.lab=1.2)
mtext("d",adj=-0.4,mar=TRUE,cex=1.5)
#legend("top", inset=c(0.5,-0.08),legend = c("owned","stray","feral","shelter") , 
#      col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz =TRUE)


# Transform this data in %
data_percentage2 <- apply(data2, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
barplot(data_percentage2[2:4,], col=coul[2:4] , border="white", xlab="Proportion owned cats neutered prepubertally",ylab="",ylim=c(0,15),cex.axis=1.2, cex.names=1.2,cex.lab=1.2)
mtext("e",adj=-0.4,mar=TRUE,cex=1.5)

#legend("top", inset=c(0.5,-0.08),legend = c("owned","stray","feral","shelter") , 
#      col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz =TRUE)


# Transform this data in %
data_percentage3 <- apply(data3, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
barplot(data_percentage3[2:4,], col=coul[2:4] , border="white", xlab="Proportion owned cats neutered prepubertally",ylab="",ylim=c(0,15),cex.axis=1.2, cex.names=1.2,cex.lab=1.2)
mtext("f",adj=-0.4,mar=TRUE,cex=1.5)

legend("topright", inset=c(0,0),legend = c("stray","feral","shelter") , 
       col = coul[2:4] , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, horiz =FALSE)

mtext(text="proportion owned cats neutered < 6 months",side=1,line=0,outer=TRUE)
mtext(text="percentage",side=2,line=0,outer=TRUE,adj=0.25)
mtext(text="median number of cats",side=2,line=0,outer=TRUE,adj=0.75)

      