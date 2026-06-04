#rm(list=ls())


####AVerage neuter rate 88% overall 95 % adult
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
median(c(0.709,0.7937))
MATS<-list()
nyears=10
lengthoftime=12*nyears
SeasonalReproduction=c(rep(c(rep(0.0366667,3),rep(0.13,6),rep(0.0366667,3)),nyears),0.0366667)
Total = matrix(ncol = lengthoftime, nrow = 20000)
Owned = matrix(ncol = lengthoftime, nrow = 20000)
Feral = matrix(ncol = lengthoftime, nrow = 20000)
Stray = matrix(ncol =lengthoftime, nrow = 20000)
Shelter = matrix(ncol = lengthoftime, nrow = 20000)
TotalNeuterRate = matrix(ncol = lengthoftime, nrow = 20000)
AdultNeuterRate = matrix(ncol = lengthoftime, nrow = 20000)
JuvenileNeuterRate= matrix(ncol =lengthoftime, nrow = 20000)
median(rbeta(100000,estBetaParams(0.95,0.0005)$alpha,estBetaParams(0.95,0.0005)$beta))
PG_Total=numeric()
PG_Owned =numeric()
PG_Feral=numeric()
PG_Stray=numeric()
PG_Shelter =numeric()
VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)



for (j in 1:20000){
  #Survival ferals
  σFK<-rbeta(1,estBetaParams(0.81,0.001)$alpha,estBetaParams(0.81,0.001)$beta)
  σFJ<-rbeta(1,estBetaParams(0.92,0.0005)$alpha,estBetaParams(0.92,0.0005)$beta)
  σFA<-rbeta(1,estBetaParams(0.96,0.0001)$alpha,estBetaParams(0.96,0.0001)$beta)
  σFE<-rbeta(1,estBetaParams(0.9,0.0001)$alpha,estBetaParams(0.9,0.0001)$beta)
  TFKUtoFKN<-0
  TFUtoFN<-0.01
  TFtoSh<-0.003
  TFtoO<-0.02
  overallbFJ<-1.5
    overallbFA<-2.5
  bFJ<-SeasonalReproduction[1]*  overallbFJ
  bFA<-SeasonalReproduction[1]*overallbFA
  
  #SHELTER PARAMETERS
  σShK<-rbeta(1,estBetaParams(0.974,0.00001)$alpha,estBetaParams(0.974,0.00001)$beta)
  σShJ<-rbeta(1,estBetaParams(0.993,0.00001)$alpha,estBetaParams(0.993,0.00001)$beta)
  σShA<-rbeta(1,estBetaParams(0.985,0.00001)$alpha,estBetaParams(0.985,0.00001)$beta)
  σShE<-rbeta(1,estBetaParams(0.9,0.00001)$alpha,estBetaParams(0.9,0.00001)$beta)
  TShtoO<-0.63
  
  #OWNED PARAMETERS
  σOK<-rbeta(1,estBetaParams(0.97,0.000001)$alpha,estBetaParams(0.97,0.000001)$beta)
  σOJ<-rbeta(1,estBetaParams(0.995,0.000001)$alpha,estBetaParams(0.995,0.000001)$beta)
  σOA<-rbeta(1,estBetaParams(0.995,0.000001)$alpha,estBetaParams(0.995,0.000001)$beta)
  σOE<-rbeta(1,estBetaParams(0.98,0.000001)$alpha,estBetaParams(0.98,0.000001)$beta)
  TOKUtoOKN<-0.41#
  TOJUtoOJN<-0.13#
  TOAUtoOAN<-0.804565# THIS IS THE PARAMETER THAT CHANGES TO RECREATE NEUTER PREV
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
  σStK<-rbeta(1,estBetaParams(0.918,0.00003)$alpha,estBetaParams(0.918,0.00003)$beta)
  σStJ<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStA<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStE<-rbeta(1,estBetaParams(0.9,0.00003)$alpha,estBetaParams(0.9,0.00003)$beta)
  
  TStUtoOKN<-0.41
  TStUtoOJN<-0.61
  TStUtoOAN<-0.95
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
             1.90E+04)
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
  Total[j,]<-rowSums(VectorMatrix)
  Owned[j,]<-rowSums(VectorMatrix[,22:28])
  Feral[j,]<-rowSums(VectorMatrix[,1:7])
  Stray[j,]<-rowSums(VectorMatrix[,8:14])
  Shelter[j,]<-rowSums(VectorMatrix[,15:21])
  TotalNeuterRate[j,] <-sum(VectorMatrix[,c(24,26,28)])/((sum(VectorMatrix[,22])*0.794)+sum(VectorMatrix[,c(23,25,27)])+sum(VectorMatrix[,c(24,26,28)]))#juveniles,adults and elderly
  AdultNeuterRate[j,]<-sum(VectorMatrix[,c(26,28)])/(sum(VectorMatrix[,c(25,27)])+sum(VectorMatrix[,c(26,28)]))#96% adult neuter rate
  JuvenileNeuterRate[j,]<-sum(VectorMatrix[,c(24)])/(sum(VectorMatrix[,c(23)])+sum(VectorMatrix[,c(24)]))#56% juvenile neuter rate
  PG_Total[j] <- sum(Total[j,109:120])/sum(Total[j,1:12])
  PG_Owned[j] <-sum(Owned[j,109:120])/sum(Owned[j,1:12])
  PG_Feral[j] <-sum(Feral[j,109:120])/sum(Feral[j,1:12])
  PG_Stray[j] <-sum(Stray[j,109:120])/sum(Stray[j,1:12])
  PG_Shelter[j] <-sum(Shelter[j,109:120])/sum(Shelter[j,1:12])
}

mean(TotalNeuterRate)
mean(AdultNeuterRate)
mean(JuvenileNeuterRate)
#Population growth over 10 years

median(PG_Total)
quantile(PG_Total,probs=c(0.025,0.975))

median(PG_Owned)

quantile(PG_Owned,probs=c(0.025,0.975))

median(PG_Stray)
quantile(PG_Stray,probs=c(0.025,0.975))

median(PG_Feral)
quantile(PG_Feral,probs=c(0.025,0.975))


median(PG_Shelter)
quantile(PG_Shelter,probs=c(0.025,0.975))




####Low neutering rate- deprived area 83% overall

MATS<-list()
nyears=10
lengthoftime=12*nyears
SeasonalReproduction=c(rep(c(rep(0.0366667,3),rep(0.13,6),rep(0.0366667,3)),nyears),0.0366667)
Owned_LN = matrix(ncol = lengthoftime, nrow = 20000)
Feral_LN = matrix(ncol = lengthoftime, nrow = 20000)
Stray_LN = matrix(ncol =lengthoftime, nrow = 20000)
Shelter_LN = matrix(ncol = lengthoftime, nrow = 20000)
TotalNeuterRate_LN = matrix(ncol = lengthoftime, nrow = 20000)
AdultNeuterRate_LN = matrix(ncol = lengthoftime, nrow = 20000)
JuvenileNeuterRate_LN= matrix(ncol =lengthoftime, nrow = 20000)

PG_Total_LN=numeric()
PG_Owned_LN =numeric()
PG_Feral_LN=numeric()
PG_Stray_LN=numeric()
PG_Shelter_LN =numeric()
VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)

for (j in 1:20000){
  #Survival ferals
  σFK<-rbeta(1,estBetaParams(0.81,0.001)$alpha,estBetaParams(0.81,0.001)$beta)
  σFJ<-rbeta(1,estBetaParams(0.92,0.0005)$alpha,estBetaParams(0.92,0.0005)$beta)
  σFA<-rbeta(1,estBetaParams(0.96,0.0001)$alpha,estBetaParams(0.96,0.0001)$beta)
  σFE<-rbeta(1,estBetaParams(0.9,0.0001)$alpha,estBetaParams(0.9,0.0001)$beta)
  TFKUtoFKN<-0
  TFUtoFN<-0.01
  TFtoSh<-0.003
  TFtoO<-0.02
  overallbFJ<-1.5
  overallbFA<-2.5
  bFJ<-SeasonalReproduction[1]*  overallbFJ
  bFA<-SeasonalReproduction[1]*overallbFA
  
  #SHELTER PARAMETERS
  σShK<-rbeta(1,estBetaParams(0.974,0.00001)$alpha,estBetaParams(0.974,0.00001)$beta)
  σShJ<-rbeta(1,estBetaParams(0.993,0.00001)$alpha,estBetaParams(0.993,0.00001)$beta)
  σShA<-rbeta(1,estBetaParams(0.985,0.00001)$alpha,estBetaParams(0.985,0.00001)$beta)
  σShE<-rbeta(1,estBetaParams(0.9,0.00001)$alpha,estBetaParams(0.9,0.00001)$beta)
  TShtoO<-0.63
  
  #OWNED PARAMETERS
  σOK<-rbeta(1,estBetaParams(0.97,0.000001)$alpha,estBetaParams(0.97,0.000001)$beta)
  σOJ<-rbeta(1,estBetaParams(0.995,0.000001)$alpha,estBetaParams(0.995,0.000001)$beta)
  σOA<-rbeta(1,estBetaParams(0.995,0.000001)$alpha,estBetaParams(0.995,0.000001)$beta)
  σOE<-rbeta(1,estBetaParams(0.98,0.000001)$alpha,estBetaParams(0.98,0.000001)$beta)
  TOKUtoOKN<-0.41#
  TOJUtoOJN<-0.13#
  TOAUtoOAN<-0.61## THIS IS THE PARAMETER THAT CHANGES TO RECREATE NEUTER PREV
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
  σStK<-rbeta(1,estBetaParams(0.918,0.00003)$alpha,estBetaParams(0.918,0.00003)$beta)
  σStJ<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStA<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStE<-rbeta(1,estBetaParams(0.9,0.00003)$alpha,estBetaParams(0.9,0.00003)$beta)
  
  TStUtoOKN<-0.41
  TStUtoOJN<-0.61
  TStUtoOAN<-0.90
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
             1.90E+04)
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
  
  Total_LN<-rowSums(VectorMatrix)
  Owned_LN[j,]<-rowSums(VectorMatrix[,22:28])
  Feral_LN[j,]<-rowSums(VectorMatrix[,1:7])
  Stray_LN[j,]<-rowSums(VectorMatrix[,8:14])
  Shelter_LN[j,]<-rowSums(VectorMatrix[,15:21])
  TotalNeuterRate_LN[j,] <-sum(VectorMatrix[,c(24,26,28)])/((sum(VectorMatrix[,22])*0.794)+sum(VectorMatrix[,c(23,25,27)])+sum(VectorMatrix[,c(24,26,28)]))#juveniles,adults and elderly
  AdultNeuterRate_LN[j,]<-sum(VectorMatrix[,c(26,28)])/(sum(VectorMatrix[,c(25,27)])+sum(VectorMatrix[,c(26,28)]))#96% adult neuter rate
  JuvenileNeuterRate_LN[j,]<-sum(VectorMatrix[,c(24)])/(sum(VectorMatrix[,c(23)])+sum(VectorMatrix[,c(24)]))#56% juvenile neuter rate
  PG_Total_LN[j] <- sum(Total_LN[109:120])/sum(Total_LN[1:12])
  PG_Owned_LN[j] <-sum(Owned_LN[j,109:120])/sum(Owned_LN[j,1:12])
  PG_Feral_LN[j] <-sum(Feral_LN[j,109:120])/sum(Feral_LN[j,1:12])
  PG_Stray_LN[j] <-sum(Stray_LN[j,109:120])/sum(Stray_LN[j,1:12])
  PG_Shelter_LN[j] <-sum(Shelter_LN[j,109:120])/sum(Shelter_LN[j,1:12])
}


mean(TotalNeuterRate_LN)
mean(AdultNeuterRate_LN)
mean(JuvenileNeuterRate_LN)
#Population growth over 10 years
median(PG_Total_LN)
quantile(PG_Total_LN,probs=c(0.025,0.975))

median(PG_Owned_LN)
quantile(PG_Owned_LN,probs=c(0.025,0.975))

median(PG_Stray_LN)
quantile(PG_Stray_LN,probs=c(0.025,0.975))
median(PG_Feral_LN)
quantile(PG_Feral_LN,probs=c(0.025,0.975))


median(PG_Shelter_LN)
quantile(PG_Shelter_LN,probs=c(0.025,0.975))



####High neutering rate- affluent area 92% overall 
MATS<-list()
nyears=10
lengthoftime=12*nyears
SeasonalReproduction=c(rep(c(rep(0.0366667,3),rep(0.13,6),rep(0.0366667,3)),nyears),0.0366667)
Owned_HN = matrix(ncol = lengthoftime, nrow = 20000)
Feral_HN = matrix(ncol = lengthoftime, nrow = 20000)
Stray_HN = matrix(ncol =lengthoftime, nrow = 20000)
Shelter_HN = matrix(ncol = lengthoftime, nrow = 20000)
TotalNeuterRate_HN = matrix(ncol = lengthoftime, nrow = 20000)
AdultNeuterRate_HN = matrix(ncol = lengthoftime, nrow = 20000)
JuvenileNeuterRate_HN= matrix(ncol =lengthoftime, nrow = 20000)

PG_Total_HN=numeric()
PG_Owned_HN =numeric()
PG_Feral_HN=numeric()
PG_Stray_HN=numeric()
PG_Shelter_HN =numeric()
VectorMatrix = matrix(ncol = 28, nrow = lengthoftime)
#set.seed(7)
for (j in 1:20000){
  #Survival ferals
  σFK<-rbeta(1,estBetaParams(0.81,0.001)$alpha,estBetaParams(0.81,0.001)$beta)
  σFJ<-rbeta(1,estBetaParams(0.92,0.0005)$alpha,estBetaParams(0.92,0.0005)$beta)
  σFA<-rbeta(1,estBetaParams(0.96,0.0001)$alpha,estBetaParams(0.96,0.0001)$beta)
  σFE<-rbeta(1,estBetaParams(0.9,0.0001)$alpha,estBetaParams(0.9,0.0001)$beta)
  TFKUtoFKN<-0
  TFUtoFN<-0.01
  TFtoSh<-0.003
  TFtoO<-0.02
  overallbFJ<-1.5
  overallbFA<-2.5
  bFJ<-SeasonalReproduction[1]*  overallbFJ
  bFA<-SeasonalReproduction[1]*overallbFA
  
  #SHELTER PARAMETERS
  σShK<-rbeta(1,estBetaParams(0.974,0.00001)$alpha,estBetaParams(0.974,0.00001)$beta)
  σShJ<-rbeta(1,estBetaParams(0.993,0.00001)$alpha,estBetaParams(0.993,0.00001)$beta)
  σShA<-rbeta(1,estBetaParams(0.985,0.00001)$alpha,estBetaParams(0.985,0.00001)$beta)
  σShE<-rbeta(1,estBetaParams(0.9,0.00001)$alpha,estBetaParams(0.9,0.00001)$beta)
  TShtoO<-0.63
  
  #OWNED PARAMETERS
  σOK<-rbeta(1,estBetaParams(0.97,0.000001)$alpha,estBetaParams(0.97,0.000001)$beta)
  σOJ<-rbeta(1,estBetaParams(0.995,0.000001)$alpha,estBetaParams(0.995,0.000001)$beta)
  σOA<-rbeta(1,estBetaParams(0.995,0.000001)$alpha,estBetaParams(0.995,0.000001)$beta)
  σOE<-rbeta(1,estBetaParams(0.98,0.000001)$alpha,estBetaParams(0.98,0.000001)$beta)
  TOKUtoOKN<-0.41#
  TOJUtoOJN<-0.13#
  TOAUtoOAN<-0.92## THIS IS THE PARAMETER THAT CHANGES TO RECREATE NEUTER PREV
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
  σStK<-rbeta(1,estBetaParams(0.918,0.00003)$alpha,estBetaParams(0.918,0.00003)$beta)
  σStJ<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStA<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStE<-rbeta(1,estBetaParams(0.9,0.00003)$alpha,estBetaParams(0.9,0.00003)$beta)
  
  TStUtoOKN<-0.41
  TStUtoOJN<-0.61
  TStUtoOAN<-0.98
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
             1.90E+04)
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
  
  
  Total_HN<-rowSums(VectorMatrix)
  Owned_HN[j,]<-rowSums(VectorMatrix[,22:28])
  Feral_HN[j,]<-rowSums(VectorMatrix[,1:7])
  Stray_HN[j,]<-rowSums(VectorMatrix[,8:14])
  Shelter_HN[j,]<-rowSums(VectorMatrix[,15:21])
  TotalNeuterRate_HN[j,] <-sum(VectorMatrix[,c(24,26,28)])/((sum(VectorMatrix[,22])*0.794)+sum(VectorMatrix[,c(23,25,27)])+sum(VectorMatrix[,c(24,26,28)]))#juveniles,adults and elderly
  AdultNeuterRate_HN[j,]<-sum(VectorMatrix[,c(26,28)])/(sum(VectorMatrix[,c(25,27)])+sum(VectorMatrix[,c(26,28)]))#96% adult neuter rate
  JuvenileNeuterRate_HN[j,]<-sum(VectorMatrix[,c(24)])/(sum(VectorMatrix[,c(23)])+sum(VectorMatrix[,c(24)]))#56% juvenile neuter rate
  PG_Total_HN[j] <- sum(Total_HN[109:120])/sum(Total_HN[1:12])
  PG_Owned_HN[j] <-sum(Owned_HN[j,109:120])/sum(Owned_HN[j,1:12])
  PG_Feral_HN[j] <-sum(Feral_HN[j,109:120])/sum(Feral_HN[j,1:12])
  PG_Stray_HN[j] <-sum(Stray_HN[j,109:120])/sum(Stray_HN[j,1:12])
  PG_Shelter_HN[j] <-sum(Shelter_HN[j,109:120])/sum(Shelter_HN[j,1:12])
}


#(0.794 as assume 31% of cats aged 4 to 6 months nueterind 1- 4/6*0,31)
mean(TotalNeuterRate_HN)
mean(AdultNeuterRate_HN)
mean(JuvenileNeuterRate_HN)
#Population growth over 10 years
median(PG_Total_HN)
quantile(PG_Total_HN,probs=c(0.025,0.975))
median(PG_Owned_HN)
quantile(PG_Owned_HN,probs=c(0.025,0.975))
median(PG_Stray_HN)
quantile(PG_Stray_HN,probs=c(0.025,0.975))
median(PG_Feral_HN)
quantile(PG_Feral_HN,probs=c(0.025,0.975))
median(PG_Shelter_HN)
quantile(PG_Shelter_HN,probs=c(0.025,0.975))




###QUANTILES FOR PLOTTING

QuantilesOwned_HN<-matrix(data=NA,nrow=2,ncol=120)
QuantilesOwned_LN<-matrix(data=NA,nrow=2,ncol=120)
QuantilesOwned<-matrix(data=NA,nrow=2,ncol=120)

QuantilesStray_HN<-matrix(data=NA,nrow=2,ncol=120)
QuantilesStray_LN<-matrix(data=NA,nrow=2,ncol=120)
QuantilesStray<-matrix(data=NA,nrow=2,ncol=120)

QuantilesFeral_HN<-matrix(data=NA,nrow=2,ncol=120)
QuantilesFeral_LN<-matrix(data=NA,nrow=2,ncol=120)
QuantilesFeral<-matrix(data=NA,nrow=2,ncol=120)

QuantilesShelter_HN<-matrix(data=NA,nrow=2,ncol=120)
QuantilesShelter_LN<-matrix(data=NA,nrow=2,ncol=120)
QuantilesShelter<-matrix(data=NA,nrow=2,ncol=120)
for (i in 1:120){
  QuantilesOwned_HN[,i]<-quantile(Owned_HN[,i],probs=c(0.025,0.975))
  QuantilesOwned_LN[,i]<-quantile(Owned_LN[,i],probs=c(0.025,0.975))
  QuantilesOwned[,i]<-quantile(Owned[,i],probs=c(0.025,0.975))
  
  QuantilesStray_HN[,i]<-quantile(Stray_HN[,i],probs=c(0.025,0.975))
  QuantilesStray_LN[,i]<-quantile(Stray_LN[,i],probs=c(0.025,0.975))
  QuantilesStray[,i]<-quantile(Stray[,i],probs=c(0.025,0.975))
  
  QuantilesFeral_HN[,i]<-quantile(Feral_HN[,i],probs=c(0.025,0.975))
  QuantilesFeral_LN[,i]<-quantile(Feral_LN[,i],probs=c(0.025,0.975))
  QuantilesFeral[,i]<-quantile(Feral[,i],probs=c(0.025,0.975))
  
  QuantilesShelter_HN[,i]<-quantile(Shelter_HN[,i],probs=c(0.025,0.975))
  QuantilesShelter_LN[,i]<-quantile(Shelter_LN[,i],probs=c(0.025,0.975))
  QuantilesShelter[,i]<-quantile(Shelter[,i],probs=c(0.025,0.975))
  
}
head(QuantilesOwned_HN)
head(Owned_HN)

library("RColorBrewer")
display.brewer.all()

colscheme<-brewer.pal(3, "Set2")

#####Overall plots
par(mar = c(4,4,1,1))
layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))
## show the regions that have been allocated to each plot
layout.show(5)
#PGR
boxplot(c(PG_Total_LN,PG_Total,PG_Total_HN)~rep(seq(1,3),each=20000),names = c("90%","95%","98%"),ylab="10 year PGR",
        xlab="Owned adult cats neutered",outline=FALSE,notch=TRUE,main="",col=colscheme,cex.lab=1.2,cex.axis=1.2, cex=1.2)
#abline(h=1,col="red",lty=2,lwd=2)
mtext("a",adj=-0.05,mar=TRUE,cex=1.2)

#Owned cats
plot(colMeans(Owned)~seq(1,120,length.out=120), type = 'n',xlab="time(months)",ylab="number of owned cats",ylim=c(min(apply(Owned_HN,2,median)),max(apply(Owned_LN,2,median))),cex.lab=1.2,cex.axis=1.2, cex=1.2)
#plot(colMeans(Owned)~seq(1,120,length.out=120), type = 'n',xlab="time(months)",ylab="number of owned cats",ylim=c(min(QuantilesOwned_HN),max(QuantilesOwned_LN)))
#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesOwned[1,],rev(QuantilesOwned[2,])), col =adjustcolor(colscheme[2],alpha.f=0.1) ,border=NA)
lines(seq(1,120,length.out=120),apply(Owned,2,median),lty="solid",col=colscheme[2],lwd=3)

#lines(seq(1,120,length.out=120),colMeans(Owned),lty="solid",col=colscheme[2],lwd=3)

#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesOwned_LN[1,],rev(QuantilesOwned_LN[2,])), col =adjustcolor(colscheme[1],alpha.f=0.1),border=NA)
lines(seq(1,120,length.out=120),apply(Owned_LN,2,median),lty="solid",col=colscheme[1],plot=FALSE,lwd=3)
#lines(seq(1,120,length.out=120),colMeans(Owned_LN),lty="solid",col=colscheme[1],plot=FALSE,lwd=2)

#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
   #     c(QuantilesOwned_HN[1,],rev(QuantilesOwned_HN[2,])), col =adjustcolor(colscheme[3],alpha.f=0.1),border=NA)
lines(seq(1,120,length.out=120),apply(Owned_HN,2,median),lty="solid",col=colscheme[3],lwd=3)
#lines(seq(1,120,length.out=120),colMeans(Owned_HN),lty="solid",col=colscheme[3],lwd=2)
# Add a legend
legend("topleft", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0))
mtext("b",adj=-0.15,mar=TRUE,cex=1.2)

#Stray cats
plot(colMeans(Stray)~seq(1,120,length.out=120), type = 'n',xlab="time(months)",ylab="number of stray cats",ylim=c(min(apply(Stray_HN,2,median)),max(apply(Stray_LN,2,median))),cex.lab=1.2,cex.axis=1.2, cex=1.2)
#plot(colMeans(Stray)~seq(1,120,length.out=120), type = 'n',xlab="time(months)",ylab="number of stray cats",ylim=c(min(QuantilesStray_HN),max(QuantilesStray_LN)))
#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesStray[1,],rev(QuantilesStray[2,])), col =adjustcolor(colscheme[2],alpha.f=0.1) ,border=NA)
lines(seq(1,120,length.out=120),apply(Stray,2,median),lty="solid",col=colscheme[2],lwd=3)
#lines(seq(1,120,length.out=120),colMeans(Stray),lty="solid",col=colscheme[2],lwd=2)

#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesStray_LN[1,],rev(QuantilesStray_LN[2,])), col =adjustcolor(colscheme[1],alpha.f=0.1),border=NA)
lines(seq(1,120,length.out=120),apply(Stray_LN,2,median),lty="solid",col=colscheme[1],lwd=3)
#lines(seq(1,120,length.out=120),colMeans(Stray_LN),lty="solid",col=colscheme[1],lwd=2)

#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesStray_HN[1,],rev(QuantilesStray_HN[2,])), col =adjustcolor(colscheme[3],alpha.f=0.1),border=NA)
lines(seq(1,120,length.out=120),apply(Stray_HN,2,median),lty="solid",col=colscheme[3],lwd=3)
#lines(seq(1,120,length.out=120),colMeans(Stray_HN),lty="solid",col=colscheme[3],lwd=2)
# Add a legend
legend("topleft", legend = c("90%","95%","98%"), 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0))
mtext("c",adj=-0.15,mar=TRUE,cex=1.2)
#Feral
#plot(colMeans(Feral)~seq(1,120,length.out=120), type = 'n',xlab="time(months)",ylab="number of feral cats",ylim=c(min(QuantilesFeral_HN),max(QuantilesFeral_LN)))
plot(colMeans(Feral)~seq(1,120,length.out=120), type = 'n',xlab="time(months)",ylab="number of feral cats",ylim=c(min(apply(Feral_HN,2,median)),max(apply(Feral_LN,2,median))),cex.lab=1.2,cex.axis=1.2, cex=1.2)
#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesFeral[1,],rev(QuantilesFeral[2,])), col =adjustcolor(colscheme[2],alpha.f=0.1) ,border=NA)
lines(seq(1,120,length.out=120),apply(Feral,2,median),lty="solid",col=colscheme[2],lwd=3)
#lines(seq(1,120,length.out=120),colMeans(Feral),lty="solid",col=colscheme[2],lwd=2)

#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesFeral_LN[1,],rev(QuantilesFeral_LN[2,])), col =adjustcolor(colscheme[1],alpha.f=0.1),border=NA)
lines(seq(1,120,length.out=120),apply(Feral_LN,2,median),lty="solid",col=colscheme[1],lwd=3)
#lines(seq(1,120,length.out=120),colMeans(Feral_LN),lty="solid",col=colscheme[1],lwd=2)

#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesFeral_HN[1,],rev(QuantilesFeral_HN[2,])), col =adjustcolor(colscheme[3],alpha.f=0.1),border=NA)
lines(seq(1,120,length.out=120),apply(Feral_HN,2,median),lty="solid",col=colscheme[3],lwd=3)
#lines(seq(1,120,length.out=120),colMeans(Feral_HN),lty="solid",col=colscheme[3],lwd=2)
# Add a legend
legend("topleft", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0))
mtext("d",adj=-0.15,mar=TRUE,cex=1.2)
#Shelter
plot(colMeans(Shelter)~seq(1,120,length.out=120), type = 'n',xlab="time(months)",ylab="number of shelter cats",ylim=c(min(apply(Shelter_HN,2,median)),max(apply(Shelter_LN,2,median))+20),cex.lab=1.2,cex.axis=1.2, cex=1.2)
#plot(colMeans(Shelter)~seq(1,120,length.out=120), type = 'n',xlab="time(months)",ylab="number of shelter cats",ylim=c(min(QuantilesShelter_HN),max(QuantilesShelter_LN)))
#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesShelter[1,],rev(QuantilesShelter[2,])), col =adjustcolor(colscheme[2],alpha.f=0.1) ,border=NA)
lines(seq(1,120,length.out=120),apply(Shelter,2,median),lty="solid",col=colscheme[2],lwd=2)
#lines(seq(1,120,length.out=120),colMeans(Shelter),lty="solid",col=colscheme[2],lwd=2)

#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesShelter_LN[1,],rev(QuantilesShelter_LN[2,])), col =adjustcolor(colscheme[1],alpha.f=0.1),border=NA)
lines(seq(1,120,length.out=120),apply(Shelter_LN,2,median),lty="solid",col=colscheme[1],lwd=2)
#lines(seq(1,120,length.out=120),colMeans(Shelter_LN),lty="solid",col=colscheme[1],lwd=2)
#polygon(c(seq(1,120,length.out=120), rev(seq(1,120,length.out=120))), 
 #       c(QuantilesShelter_HN[1,],rev(QuantilesShelter_HN[2,])), col =adjustcolor(colscheme[3],alpha.f=0.1),border=NA)
lines(seq(1,120,length.out=120),apply(Shelter_HN,2,median),lty="solid",col=colscheme[3],lwd=2)
#lines(seq(1,120,length.out=120),colMeans(Shelter_HN),lty="solid",col=colscheme[3],lwd=2)
# Add a legend
legend("topleft", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0))
mtext("e",adj=-0.15,mar=TRUE,cex=1.2)

###proportion in each state
library(RColorBrewer)
coul <- brewer.pal(4, "Pastel2") 
par(mar = c(4,4,4,4))
layout(matrix(c(1,2), 2, 2, byrow = TRUE))

data <- matrix(c(                 median(Owned_LN[,120]),median(Stray_LN[,120]),median(Feral_LN[,120]),median(Shelter_LN[,120]),
                                  median(Owned[,120]),median(Stray[,120]),median(Feral[,120]),median(Shelter[,120]),
                 median(Owned_HN[,120]),median(Stray_HN[,120]),median(Feral_HN[,120]),median(Shelter_HN[,120])), 
               nrow=4,byrow=F)
#data <- matrix(c(mean(Owned[,120]),mean(Stray[,120]),mean(Feral[,120]),mean(Shelter[,120]),
 #                mean(Owned_LN[,120]),mean(Stray_LN[,120]),mean(Feral_LN[,120]),mean(Shelter_LN[,120]),
  #               mean(Owned_HN[,120]),mean(Stray_HN[,120]),mean(Feral_HN[,120]),mean(Shelter_HN[,120])), 
   #            nrow=4,byrow=F)
colnames(data) <- c("90%","95%","98%")
rownames(data) <- c("owned","stray","feral","shelter")
barplot(data, col=coul , border="white", xlab="proportion of adult owned cats neutered",ylab="median numbers of cats",cex.axis=1.2, cex.names=1.2,cex.lab=1.2)
legend("topright", inset=c(0,0),legend = c("owned","stray","feral","shelter") , 
       col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, horiz =FALSE)
abline(h=sum(Catvec2),xpd=FALSE,lty=3)
mtext("a",adj=-0.2,mar=TRUE,cex=1.5)
colSums(data)[1]/colSums(data)[2]
data[2,1]/data[2,2]
# create color palette:

# Transform this data in %
data_percentage <- apply(data, 2, function(x){x*100/sum(x,na.rm=T)})


# Make a stacked barplot--> it will be in %!
barplot(data_percentage[2:4,], col=coul[2:4] , border="white", xlab="proportion of adult owned cats neutered",ylab="percentage",cex.axis=1.2, cex.names=1.2,cex.lab=1.2)
mtext("b",adj=-0.2,mar=TRUE,cex=1.5)
legend("topright", inset=c(0,0),legend = c("stray","feral","shelter") , 
       col = coul[2:4] , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, horiz =FALSE)


