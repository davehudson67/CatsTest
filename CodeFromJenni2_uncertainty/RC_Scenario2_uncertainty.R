set.seed(10)
#90% adult neutering rate
nyears=10
lengthoftime=12*nyears
Owned<- array(numeric(),c(20000,lengthoftime,10)) 
Feral <- array(numeric(),c(20000,lengthoftime,10)) 
Stray <- array(numeric(),c(20000,lengthoftime,10)) 
Shelter<- array(numeric(),c(20000,lengthoftime,10)) 
Total<- array(numeric(),c(20000,lengthoftime,10)) 

PG_Total = matrix(ncol = 10, nrow = 20000)
PG_Owned = matrix(ncol = 10, nrow = 20000)
PG_Feral = matrix(ncol = 10, nrow = 20000)
PG_Stray = matrix(ncol =10, nrow = 20000)
PG_Shelter = matrix(ncol = 10, nrow = 20000)

TotalNeuterRate= matrix(ncol = 10, nrow = 20000)
AdultNeuterRate= matrix(ncol = 10, nrow = 20000)
JuvenileNeuterRate= matrix(ncol = 10, nrow = 20000)
set.seed(55)
#90% adult neuter rate
for (x in 1:10){
  MATS<-list()
  TOKUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TOJUtoOJN<-0.13
  TOAUtoOAN<-c(0.757249083,0.743762921,0.728690152,0.711733286,0.692515505,0.670552327,
               0.645210198,0.615644382,0.580702962,0.538773258)[x]
  
  TStUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TStUtoOJN<-c(0.37442215,0.4073473,0.44027245,0.4731976,0.50612275,0.5390479,0.57197305,0.6048982,0.63782335,0.6707485)[x]
  TStUtoOAN<-0.90
  for (j in 1:20000){
  
  #Ferals
    σFK<-rbeta(1,estBetaParams(0.81,0.001)$alpha,estBetaParams(0.81,0.001)$beta)
    σFJ<-rbeta(1,estBetaParams(0.92,0.0005)$alpha,estBetaParams(0.92,0.0005)$beta)
    σFA<-rbeta(1,estBetaParams(0.96,0.0001)$alpha,estBetaParams(0.96,0.0001)$beta)
    σFE<-rbeta(1,estBetaParams(0.9,0.0001)$alpha,estBetaParams(0.9,0.0001)$beta)
  TFKUtoFKN<-0#0.01
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
  σStK<-rbeta(1,estBetaParams(0.918,0.00003)$alpha,estBetaParams(0.918,0.00003)$beta)#CHANGED
  σStJ<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStA<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStE<-rbeta(1,estBetaParams(0.9,0.00003)$alpha,estBetaParams(0.9,0.00003)$beta)
  
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
        TOtoShK<-0.002*0.5#0.0015#1798
        TOtoShJ<-0.002*0.5#0.0015#1798
        TOtoShA<-0.002*0.5#0.0015#1798
        TOtoShE<-0.002*0.5#0.0015#1798
        
      }
      else if(NShelter>(sum(Catvec2[15:21])*1.01) & NShelter<(sum(Catvec2[15:21])*1.1)){
        TFtoSh<-0.003*0.75#0.0015
        TSttoSh<-0.03*0.75#0.015
        TOtoShK<-0.002*0.75#0.0015#1798
        TOtoShJ<-0.002*0.75#0.0015#1798
        TOtoShA<-0.002*0.75#0.0015#1798
        TOtoShE<-0.002*0.75#0.0015#1798
      }
      else {
        TFtoSh<-0.003
        TSttoSh<-0.03
        TOtoShK<-0.002#1798
        TOtoShJ<-0.002#1798
        TOtoShA<-0.002#1798
        TOtoShE<-0.002#1798
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
    Total[j,,x]<-rowSums(VectorMatrix)
    Owned[j,,x]<-rowSums(VectorMatrix[,22:28])
    Feral[j,,x]<-rowSums(VectorMatrix[,1:7])
    Stray[j,,x]<-rowSums(VectorMatrix[,8:14])
    Shelter[j,,x]<-rowSums(VectorMatrix[,15:21])
    PG_Total[j,x] <- sum(Total[j,109:120,x])/sum(Total[j,1:12,x])
    PG_Owned[j,x] <-sum(Owned[j,109:120,x])/sum(Owned[j,1:12,x])
    PG_Feral[j,x] <-sum(Feral[j,109:120,x])/sum(Feral[j,1:12,x])
    PG_Stray[j,x] <-sum(Stray[j,109:120,x])/sum(Stray[j,1:12,x])
    PG_Shelter[j,x] <-sum(Shelter[j,109:120,x])/sum(Shelter[j,1:12,x])
    TotalNeuterRate[j,x] <-sum(VectorMatrix[,c(24,26,28)])/(sum(VectorMatrix[,c(23,25,27)])+sum(VectorMatrix[,c(24,26,28)]))#juveniles,adults and elderly
    AdultNeuterRate[j,x]<-sum(VectorMatrix[,c(26,28)])/(sum(VectorMatrix[,c(25,27)])+sum(VectorMatrix[,c(26,28)]))#90% adult neuter rate
    JuvenileNeuterRate[j,x]<-sum(VectorMatrix[,c(24)])/(sum(VectorMatrix[,c(23)])+sum(VectorMatrix[,c(24)]))#56% juvenile neuter rate
    
  }}

dim(PG_Total)
head(PG_Owned)
#check adult neuter rate similar across models
mean(AdultNeuterRate[,1])
mean(AdultNeuterRate[,2])
mean(AdultNeuterRate[,3])
mean(AdultNeuterRate[,4])
mean(AdultNeuterRate[,5])
mean(AdultNeuterRate[,6])
mean(AdultNeuterRate[,7])
mean(AdultNeuterRate[,8])
mean(AdultNeuterRate[,9])
mean(AdultNeuterRate[,10])



#95% adult neutering rate
Owned_95<- array(numeric(),c(20000,lengthoftime,10)) 
Feral_95 <- array(numeric(),c(20000,lengthoftime,10)) 
Stray_95 <- array(numeric(),c(20000,lengthoftime,10)) 
Shelter_95<- array(numeric(),c(20000,lengthoftime,10)) 
Total_95<- array(numeric(),c(20000,lengthoftime,10)) 


PG_Total_95 = matrix(ncol = 10, nrow = 20000)
PG_Owned_95 = matrix(ncol = 10, nrow = 20000)
PG_Feral_95 = matrix(ncol = 10, nrow = 20000)
PG_Stray_95 = matrix(ncol =10, nrow = 20000)
PG_Shelter_95 = matrix(ncol = 10, nrow = 20000)

TotalNeuterRate_95= matrix(ncol = 10, nrow = 20000)
AdultNeuterRate_95= matrix(ncol = 10, nrow = 20000)
JuvenileNeuterRate_95= matrix(ncol = 10, nrow = 20000)

#95% adult neuter rate
for (x in 1:10){
  MATS<-list()
  TOKUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TOJUtoOJN<-0.13
  TOAUtoOAN<-c(0.878624542,0.871881461,0.864345076,0.855866643,0.846257753,0.835276164,
               0.822605099,0.807822191,0.790351481,0.769386629)[x]
  
  TStUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TStUtoOJN<-c(0.37442215,0.4073473,0.44027245,0.4731976,0.50612275,0.5390479,0.57197305,0.6048982,0.63782335,0.6707485)[x]
  TStUtoOAN<-0.95
  for (j in 1:20000){
  #Ferals
    σFK<-rbeta(1,estBetaParams(0.81,0.001)$alpha,estBetaParams(0.81,0.001)$beta)
    σFJ<-rbeta(1,estBetaParams(0.92,0.0005)$alpha,estBetaParams(0.92,0.0005)$beta)
    σFA<-rbeta(1,estBetaParams(0.96,0.0001)$alpha,estBetaParams(0.96,0.0001)$beta)
    σFE<-rbeta(1,estBetaParams(0.9,0.0001)$alpha,estBetaParams(0.9,0.0001)$beta)
  TFKUtoFKN<-0#0.01
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
  
  
  #STRAY PAR  
  σStK<-rbeta(1,estBetaParams(0.918,0.00003)$alpha,estBetaParams(0.918,0.00003)$beta)#CHANGED
  σStJ<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStA<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
  σStE<-rbeta(1,estBetaParams(0.9,0.00003)$alpha,estBetaParams(0.9,0.00003)$beta)

  
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
  VectorMatrix_95 = matrix(ncol = 28, nrow = lengthoftime)
  VectorMatrix_95[1,]<-Catvec2
  MATS[[1]]<-MatA

    
    for (i in 1:119){
      CatsProject<-MATS[[i]]%*% VectorMatrix_95[i,]
      VectorMatrix_95[i+1,]<-  CatsProject
      NOwned<-sum(VectorMatrix_95[i+1,22:28])
      NShelter<-sum(VectorMatrix_95[i+1,15:21])
      NStray<-sum(VectorMatrix_95[i+1,8:14])
      NFeral<-sum(VectorMatrix_95[i+1,1:7])
      
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
        TOtoShK<-0.002*0.5#0.0015#1798
        TOtoShJ<-0.002*0.5#0.0015#1798
        TOtoShA<-0.002*0.5#0.0015#1798
        TOtoShE<-0.002*0.5#0.0015#1798
        
      }
      else if(NShelter>(sum(Catvec2[15:21])*1.01) & NShelter<(sum(Catvec2[15:21])*1.1)){
        TFtoSh<-0.003*0.75#0.0015
        TSttoSh<-0.03*0.75#0.015
        TOtoShK<-0.002*0.75#0.0015#1798
        TOtoShJ<-0.002*0.75#0.0015#1798
        TOtoShA<-0.002*0.75#0.0015#1798
        TOtoShE<-0.002*0.75#0.0015#1798
      }
      else {
        TFtoSh<-0.003
        TSttoSh<-0.03
        TOtoShK<-0.002#1798
        TOtoShJ<-0.002#1798
        TOtoShA<-0.002#1798
        TOtoShE<-0.002#1798
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
    Total_95[j,,x]<-rowSums(VectorMatrix_95)
    Owned_95[j,,x]<-rowSums(VectorMatrix_95[,22:28])
    Feral_95[j,,x]<-rowSums(VectorMatrix_95[,1:7])
    Stray_95[j,,x]<-rowSums(VectorMatrix_95[,8:14])
    Shelter_95[j,,x]<-rowSums(VectorMatrix_95[,15:21])
    PG_Total_95[j,x] <- sum(Total_95[j,109:120,x])/sum(Total_95[j,1:12,x])
    PG_Owned_95[j,x] <-sum(Owned_95[j,109:120,x])/sum(Owned_95[j,1:12,x])
    PG_Feral_95[j,x] <-sum(Feral_95[j,109:120,x])/sum(Feral_95[j,1:12,x])
    PG_Stray_95[j,x] <-sum(Stray_95[j,109:120,x])/sum(Stray_95[j,1:12,x])
    PG_Shelter_95[j,x] <-sum(Shelter_95[j,109:120,x])/sum(Shelter_95[j,1:12,x])
    TotalNeuterRate_95[j,x] <-sum(VectorMatrix_95[,c(24,26,28)])/(sum(VectorMatrix_95[,c(23,25,27)])+sum(VectorMatrix_95[,c(24,26,28)]))#juveniles,adults and elderly
    AdultNeuterRate_95[j,x]<-sum(VectorMatrix_95[,c(26,28)])/(sum(VectorMatrix_95[,c(25,27)])+sum(VectorMatrix_95[,c(26,28)]))#90% adult neuter rate
    JuvenileNeuterRate_95[j,x]<-sum(VectorMatrix_95[,c(24)])/(sum(VectorMatrix_95[,c(23)])+sum(VectorMatrix_95[,c(24)]))#56% juvenile neuter rate
    
  }}



mean(AdultNeuterRate[,1])
mean(AdultNeuterRate[,2])
mean(AdultNeuterRate[,3])
mean(AdultNeuterRate[,4])
mean(AdultNeuterRate[,5])
mean(AdultNeuterRate[,6])
mean(AdultNeuterRate[,7])
mean(AdultNeuterRate[,8])
mean(AdultNeuterRate[,9])
mean(AdultNeuterRate[,10])




#98% adult neutering rate
Owned_98<- array(numeric(),c(20000,lengthoftime,10)) 
Feral_98 <- array(numeric(),c(20000,lengthoftime,10)) 
Stray_98 <- array(numeric(),c(20000,lengthoftime,10)) 
Shelter_98<- array(numeric(),c(20000,lengthoftime,10)) 
Total_98<- array(numeric(),c(20000,lengthoftime,10)) 


PG_Total_98 = matrix(ncol = 10, nrow = 20000)
PG_Owned_98 = matrix(ncol = 10, nrow = 20000)
PG_Feral_98 = matrix(ncol = 10, nrow = 20000)
PG_Stray_98 = matrix(ncol =10, nrow = 20000)
PG_Shelter_98 = matrix(ncol = 10, nrow = 20000)

TotalNeuterRate_98= matrix(ncol = 10, nrow = 20000)
AdultNeuterRate_98= matrix(ncol = 10, nrow = 20000)
JuvenileNeuterRate_98= matrix(ncol = 10, nrow = 20000)

#98% adult neuter rate
for (x in 1:10){
  MATS<-list()
  TOKUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TOJUtoOJN<-0.13
  TOAUtoOAN<-c(0.951449817, 0.948752584, 0.94573803,  0.942346657, 0.938503101, 0.934110465, 0.92904204,0.923128876,
               0.916140592, 0.907754652)[x]
  
  TStUtoOKN<-seq(0.05,0.5,length.out=10)[x]
  TStUtoOJN<-c(0.37442215,0.4073473,0.44027245,0.4731976,0.50612275,0.5390479,0.57197305,0.6048982,0.63782335,0.6707485)[x]
  TStUtoOAN<-0.98
  for (j in 1:20000){
    #Ferals
    σFK<-rbeta(1,estBetaParams(0.81,0.001)$alpha,estBetaParams(0.81,0.001)$beta)
    σFJ<-rbeta(1,estBetaParams(0.92,0.0005)$alpha,estBetaParams(0.92,0.0005)$beta)
    σFA<-rbeta(1,estBetaParams(0.96,0.0001)$alpha,estBetaParams(0.96,0.0001)$beta)
    σFE<-rbeta(1,estBetaParams(0.9,0.0001)$alpha,estBetaParams(0.9,0.0001)$beta)
    TFKUtoFKN<-0#0.01
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
    σStK<-rbeta(1,estBetaParams(0.918,0.00003)$alpha,estBetaParams(0.918,0.00003)$beta)#CHANGED
    σStJ<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
    σStA<-rbeta(1,estBetaParams(0.97,0.00003)$alpha,estBetaParams(0.97,0.00003)$beta)
    σStE<-rbeta(1,estBetaParams(0.9,0.00003)$alpha,estBetaParams(0.9,0.00003)$beta)
    
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
    VectorMatrix_98 = matrix(ncol = 28, nrow = lengthoftime)
    VectorMatrix_98[1,]<-Catvec2
    MATS[[1]]<-MatA

    
    for (i in 1:119){
      CatsProject<-MATS[[i]]%*% VectorMatrix_98[i,]
      VectorMatrix_98[i+1,]<-  CatsProject
      NOwned<-sum(VectorMatrix_98[i+1,22:28])
      NShelter<-sum(VectorMatrix_98[i+1,15:21])
      NStray<-sum(VectorMatrix_98[i+1,8:14])
      NFeral<-sum(VectorMatrix_98[i+1,1:7])
      
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
        TOtoShK<-0.002*0.5#0.0015#1798
        TOtoShJ<-0.002*0.5#0.0015#1798
        TOtoShA<-0.002*0.5#0.0015#1798
        TOtoShE<-0.002*0.5#0.0015#1798
        
      }
      else if(NShelter>(sum(Catvec2[15:21])*1.01) & NShelter<(sum(Catvec2[15:21])*1.1)){
        TFtoSh<-0.003*0.75#0.0015
        TSttoSh<-0.03*0.75#0.015
        TOtoShK<-0.002*0.75#0.0015#1798
        TOtoShJ<-0.002*0.75#0.0015#1798
        TOtoShA<-0.002*0.75#0.0015#1798
        TOtoShE<-0.002*0.75#0.0015#1798
      }
      else {
        TFtoSh<-0.003
        TSttoSh<-0.03
        TOtoShK<-0.002#1798
        TOtoShJ<-0.002#1798
        TOtoShA<-0.002#1798
        TOtoShE<-0.002#1798
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
    Total_98[j,,x]<-rowSums(VectorMatrix_98)
    Owned_98[j,,x]<-rowSums(VectorMatrix_98[,22:28])
    Feral_98[j,,x]<-rowSums(VectorMatrix_98[,1:7])
    Stray_98[j,,x]<-rowSums(VectorMatrix_98[,8:14])
    Shelter_98[j,,x]<-rowSums(VectorMatrix_98[,15:21])
    PG_Total_98[j,x] <- sum(Total_98[j,109:120,x])/sum(Total_98[j,1:12,x])
    PG_Owned_98[j,x] <-sum(Owned_98[j,109:120,x])/sum(Owned_98[j,1:12,x])
    PG_Feral_98[j,x] <-sum(Feral_98[j,109:120,x])/sum(Feral_98[j,1:12,x])
    PG_Stray_98[j,x] <-sum(Stray_98[j,109:120,x])/sum(Stray_98[j,1:12,x])
    PG_Shelter_98[j,x] <-sum(Shelter_98[j,109:120,x])/sum(Shelter_98[j,1:12,x])
    TotalNeuterRate_98[j,x] <-sum(VectorMatrix_98[,c(24,26,28)])/(sum(VectorMatrix_98[,c(23,25,27)])+sum(VectorMatrix_98[,c(24,26,28)]))#juveniles,adults and elderly
    AdultNeuterRate_98[j,x]<-sum(VectorMatrix_98[,c(26,28)])/(sum(VectorMatrix_98[,c(25,27)])+sum(VectorMatrix_98[,c(26,28)]))#90% adult neuter rate
    JuvenileNeuterRate_98[j,x]<-sum(VectorMatrix_98[,c(24)])/(sum(VectorMatrix_98[,c(23)])+sum(VectorMatrix_98[,c(24)]))#56% juvenile neuter rate
    
  }}



mean(AdultNeuterRate_98[,1])
mean(AdultNeuterRate_98[,2])
mean(AdultNeuterRate_98[,3])
mean(AdultNeuterRate_98[,4])
mean(AdultNeuterRate_98[,5])
mean(AdultNeuterRate_98[,6])
mean(AdultNeuterRate_98[,7])
mean(AdultNeuterRate_98[,8])
mean(AdultNeuterRate_98[,9])
mean(AdultNeuterRate_98[,10])
####PLOTS
library("RColorBrewer")
#display.brewer.all()
colscheme<-brewer.pal(3, "Set2")

par(mar = c(4,4,1,1))
layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))



boxplot(c(PG_Total[,1],PG_Total_95[,1],PG_Total_98[,1],
          PG_Total[,2],PG_Total_95[,2],PG_Total_98[,2],
          PG_Total[,3],PG_Total_95[,3],PG_Total_98[,3],
          PG_Total[,4],PG_Total_95[,4],PG_Total_98[,4],
          PG_Total[,5],PG_Total_95[,5],PG_Total_98[,5],
          PG_Total[,6],PG_Total_95[,6],PG_Total_98[,6],
          PG_Total[,7],PG_Total_95[,7],PG_Total_98[,7],
          PG_Total[,8],PG_Total_95[,8],PG_Total_98[,8],
          PG_Total[,9],PG_Total_95[,9],PG_Total_98[,9],
          PG_Total[,10],PG_Total_95[,10],PG_Total_98[,10])
        ~rep(seq(1,30),each=20000),ylab="10 year PGR",
        xlab="Proportion of owned cats neutered prepubertally",outline=FALSE,notch=TRUE,main="",col=colscheme,
        xaxt="n",cex.lab=1.2,cex.axis=1.2, cex=1.2)
axis(1,at=(seq(2,29,by=3)),labels=round(seq(0.05,0.5,length.out=10),2))
#abline(h=1,col="red",lty=2,lwd=2,xpd=FALSE)
legend("topright", legend = c("90%","95%","98%") , 
       col = colscheme , bty = "n", pch=20 , pt.cex = 3, cex = 1.2, horiz = FALSE, inset = c(0, 0.01),title="owned adult neutering prevalence")
mtext("a",adj=-0.07,mar=TRUE,cex=1.2)

#Owned
apply(PG_Owned,2,median)
QuantilesPGOwned=apply(PG_Owned,2,quantile,probs=c(0.025,0.975))
#colMeans(PG_Owned)
apply(PG_Owned_95,2,median)
QuantilesPGOwned_95=apply(PG_Owned_95,2,quantile,probs=c(0.025,0.975))
#colMeans(PG_Owned_95)
apply(PG_Owned_98,2,median)
QuantilesPGOwned_98=apply(PG_Owned_98,2,quantile,probs=c(0.025,0.975))
plot(colMeans(PG_Owned)~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Owned PGR",ylim=c(min(apply(PG_Owned_98,2,median)),max(apply(PG_Owned,2,median))),cex.lab=1.2,cex.axis=1.2, cex=1.2)
#polygon(c(seq(0.1,0.5,length.out=10), rev(seq(0.1,0.5,length.out=10))), 
 #       c(QuantilesPGOwned[1,],rev(QuantilesPGOwned[2,])), col =adjustcolor(colscheme[1],alpha.f=0.5),border=NA)
#lines(seq(0.1,0.5,length.out=10),colMeans(PG_Owned),lty="solid",col=colscheme[1],lwd=1.5)
lines(seq(0.05,0.5,length.out=10),apply(PG_Owned,2,median),lty="solid",col=colscheme[1],lwd=3)
#abline(h=1,col="red",lty=2,lwd=2,xpd=FALSE)
#polygon(c(seq(0.1,0.5,length.out=10), rev(seq(0.1,0.5,length.out=10))), 
 #       c(QuantilesPGOwned_95[1,],rev(QuantilesPGOwned_95[2,])), col =adjustcolor(colscheme[2],alpha.f=0.5),border=NA)
#lines(seq(0.1,0.5,length.out=10),colMeans(PG_Owned_95),lty="solid",col=colscheme[2],lwd=1.5)
lines(seq(0.05,0.5,length.out=10),apply(PG_Owned_95,2,median),lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),apply(PG_Owned_98,2,median),lty="solid",col=colscheme[3],lwd=3)
mtext("b",adj=-0.15,mar=TRUE,cex=1.2)
#legend("topright", legend =c("90% owned adults neutered","95% of owned adults neutered") , 
#      col = colscheme[1:2] , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0, 0))

#Stray cats

#QuantilesPGStray=apply(PG_Stray,2,quantile,probs=c(0.025,0.975))
#QuantilesPGStray_95=apply(PG_Stray_95,2,quantile,probs=c(0.025,0.975))
#plot(colMeans(PG_Stray)~seq(0.1,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Stray PGR",ylim=c(min(QuantilesPGStray_95),max(QuantilesPGStray)))
#polygon(c(seq(0.1,0.5,length.out=10), rev(seq(0.1,0.5,length.out=10))), 
#        c(QuantilesPGStray[1,],rev(QuantilesPGStray[2,])), col =adjustcolor(colscheme[1],alpha.f=0.5),border=NA)
#lines(seq(0.1,0.5,length.out=10),colMeans(PG_Stray),lty="solid",col=colscheme[1],lwd=1.5)
#abline(h=1,col="red",lty=2,lwd=2,xpd=FALSE)
#polygon(c(seq(0.1,0.5,length.out=10), rev(seq(0.1,0.5,length.out=10))), 
 #       c(QuantilesPGStray_95[1,],rev(QuantilesPGStray_95[2,])), col =adjustcolor(colscheme[2],alpha.f=0.5),border=NA)
#lines(seq(0.1,0.5,length.out=10),colMeans(PG_Stray_95),lty="solid",col=colscheme[2],lwd=1.5)
plot(colMeans(PG_Stray)~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Stray PGR",ylim=c(min(apply(PG_Stray_98,2,median)),max(apply(PG_Stray,2,median))),cex.lab=1.2,cex.axis=1.2, cex=1.2)
lines(seq(0.05,0.5,length.out=10),apply(PG_Stray,2,median),lty="solid",col=colscheme[1],lwd=3)
#abline(h=1,col="red",lty=2,lwd=2,xpd=FALSE)
lines(seq(0.05,0.5,length.out=10),apply(PG_Stray_95,2,median),lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),apply(PG_Stray_98,2,median),lty="solid",col=colscheme[3],lwd=3)

mtext("c",adj=-0.15,mar=TRUE,cex=1.2)
#Feral cats
#QuantilesPGFeral=apply(PG_Feral,2,quantile,probs=c(0.025,0.975))
#QuantilesPGFeral_95=apply(PG_Feral_95,2,quantile,probs=c(0.025,0.975))
#plot(colMeans(PG_Feral)~seq(0.1,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Feral PGR",ylim=c(min(QuantilesPGFeral_95),max(QuantilesPGFeral)))
#polygon(c(seq(0.1,0.5,length.out=10), rev(seq(0.1,0.5,length.out=10))), 
 #       c(QuantilesPGFeral[1,],rev(QuantilesPGFeral[2,])), col =adjustcolor(colscheme[1],alpha.f=0.5),border=NA)
#lines(seq(0.1,0.5,length.out=10),colMeans(PG_Feral),lty="solid",col=(sequential(colscheme[1],plot=FALSE))[21],lwd=1.5)
#abline(h=1,col="red",lty=2,lwd=2,xpd=FALSE)
#polygon(c(seq(0.1,0.5,length.out=10), rev(seq(0.1,0.5,length.out=10))), 
 #       c(QuantilesPGFeral_95[1,],rev(QuantilesPGFeral_95[2,])), col =adjustcolor(colscheme[2],alpha.f=0.5),border=NA)
#lines(seq(0.1,0.5,length.out=10),colMeans(PG_Feral_95),lty="solid",col=(sequential(colscheme[2],plot=FALSE))[21],lwd=1.5)

plot(colMeans(PG_Feral)~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Feral PGR",ylim=c(min(apply(PG_Feral_98,2,median)),max(apply(PG_Feral,2,median))),cex.lab=1.2,cex.axis=1.2, cex=1.2)
lines(seq(0.05,0.5,length.out=10),apply(PG_Feral,2,median),lty="solid",col=colscheme[1],lwd=3)
#abline(h=1,col="red",lty=2,lwd=2,xpd=FALSE)
lines(seq(0.05,0.5,length.out=10),apply(PG_Feral_95,2,median),lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),apply(PG_Feral_98,2,median),lty="solid",col=colscheme[3],lwd=3)


mtext("d",adj=-0.15,mar=TRUE,cex=1.2)

#Shelter cats

#QuantilesPGShelter=apply(PG_Shelter,2,quantile,probs=c(0.025,0.975))
#QuantilesPGShelter_95=apply(PG_Shelter_95,2,quantile,probs=c(0.025,0.975))
#plot(colMeans(PG_Shelter)~seq(0.1,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Shelter PGR",ylim=c(min(QuantilesPGShelter_95),max(QuantilesPGShelter)))
#polygon(c(seq(0.1,0.5,length.out=10), rev(seq(0.1,0.5,length.out=10))), 
 #       c(QuantilesPGShelter[1,],rev(QuantilesPGShelter[2,])), col =adjustcolor(colscheme[1],alpha.f=0.5),border=NA)
#lines(seq(0.1,0.5,length.out=10),colMeans(PG_Shelter),lty="solid",col=(sequential(colscheme[1],plot=FALSE))[21],lwd=1.5)
#abline(h=1,col="red",lty=2,lwd=2,xpd=FALSE)
#polygon(c(seq(0.1,0.5,length.out=10), rev(seq(0.1,0.5,length.out=10))), 
 #       c(QuantilesPGShelter_95[1,],rev(QuantilesPGShelter_95[2,])), col =adjustcolor(colscheme[2],alpha.f=0.5),border=NA)
#lines(seq(0.1,0.5,length.out=10),colMeans(PG_Shelter_95),lty="solid",col=(sequential(colscheme[2],plot=FALSE))[21],lwd=1.5)
plot(colMeans(PG_Shelter)~seq(0.05,0.5,length.out=10), type = 'n',xlab="Proportion of owned cats neutered prepubertally",ylab="Shelter PGR",ylim=c(min(apply(PG_Shelter_98,2,median)),max(apply(PG_Shelter,2,median))),cex.lab=1.2,cex.axis=1.2, cex=1.2)
lines(seq(0.05,0.5,length.out=10),apply(PG_Shelter,2,median),lty="solid",col=colscheme[1],lwd=3)
#abline(h=1,col="red",lty=2,lwd=2,xpd=FALSE)
lines(seq(0.05,0.5,length.out=10),apply(PG_Shelter_95,2,median),lty="solid",col=colscheme[2],lwd=3)
lines(seq(0.05,0.5,length.out=10),apply(PG_Shelter_98,2,median),lty="solid",col=colscheme[3],lwd=3)
mtext("e",adj=-0.15,mar=TRUE,cex=1.2)
#legend("topright", legend =c("90% owned adults neutered","95% of owned adults neutered") , 
#      col = colscheme[1:2] , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz = FALSE, inset = c(0, 0))


# create color palette:
library(RColorBrewer)
coul <- brewer.pal(4, "Pastel2") 

dim(Owned_95)
###proportion in each state
par(mar = c(4,4,4,1))
par(oma=c(3,3,0,0),mar=c(3,3,2,2))

layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE))


data <- matrix(c(median(Owned[,109:120,1]),median(Stray[,109:120,1]),median(Feral[,109:120,1]),median(Shelter[,109:120,1]),
                 median(Owned[,109:120,2]),median(Stray[,109:120,2]),median(Feral[,109:120,2]),median(Shelter[,109:120,2]),
                 median(Owned[,109:120,3]),median(Stray[,109:120,3]),median(Feral[,109:120,3]),median(Shelter[,109:120,3]),
                 median(Owned[,109:120,4]),median(Stray[,109:120,4]),median(Feral[,109:120,4]),median(Shelter[,109:120,4]),
                 median(Owned[,109:120,5]),median(Stray[,109:120,5]),median(Feral[,109:120,5]),median(Shelter[,109:120,5]),
                 median(Owned[,109:120,6]),median(Stray[,109:120,6]),median(Feral[,109:120,6]),median(Shelter[,109:120,6]),
                 median(Owned[,109:120,7]),median(Stray[,109:120,7]),median(Feral[,109:120,7]),median(Shelter[,109:120,7]),
                 median(Owned[,109:120,8]),median(Stray[,109:120,8]),median(Feral[,109:120,8]),median(Shelter[,109:120,8]),
                 median(Owned[,109:120,9]),median(Stray[,109:120,9]),median(Feral[,109:120,9]),median(Shelter[,109:120,9]),
                 median(Owned[,109:120,10]),median(Stray[,109:120,10]),median(Feral[,109:120,10]),median(Shelter[,109:120,10])), 
               nrow=4,byrow=F)

colnames(data) <- round(seq(0.05,0.5,length.out=10),2)
rownames(data) <- c("owned","stray","feral","shelter")
barplot(data, col=coul , border="white", xlab="",ylab="",main="low neutering",cex.axis=1.2, cex.names=1.2,cex.lab=1.2,cex.main=1.5,ylim=c(0,200000))

abline(h=sum(Catvec2),xpd=FALSE,lty=3)
mtext("a",adj=-0.3,mar=TRUE,cex=1.5)

data2 <- matrix(c(median(Owned_95[,109:120,1]),median(Stray_95[,109:120,1]),median(Feral_95[,109:120,1]),median(Shelter_95[,109:120,1]),
                  median(Owned_95[,109:120,2]),median(Stray_95[,109:120,2]),median(Feral_95[,109:120,2]),median(Shelter_95[,109:120,2]),
                  median(Owned_95[,109:120,3]),median(Stray_95[,109:120,3]),median(Feral_95[,109:120,3]),median(Shelter_95[,109:120,3]),
                  median(Owned_95[,109:120,4]),median(Stray_95[,109:120,4]),median(Feral_95[,109:120,4]),median(Shelter_95[,109:120,4]),
                  median(Owned_95[,109:120,5]),median(Stray_95[,109:120,5]),median(Feral_95[,109:120,5]),median(Shelter_95[,109:120,5]),
                  median(Owned_95[,109:120,6]),median(Stray_95[,109:120,6]),median(Feral_95[,109:120,6]),median(Shelter_95[,109:120,6]),
                  median(Owned_95[,109:120,7]),median(Stray_95[,109:120,7]),median(Feral_95[,109:120,7]),median(Shelter_95[,109:120,7]),
                  median(Owned_95[,109:120,8]),median(Stray_95[,109:120,8]),median(Feral_95[,109:120,8]),median(Shelter_95[,109:120,8]),
                  median(Owned_95[,109:120,9]),median(Stray_95[,109:120,9]),median(Feral_95[,109:120,9]),median(Shelter_95[,109:120,9]),
                  median(Owned_95[,109:120,10]),median(Stray_95[,109:120,10]),median(Feral_95[,109:120,10]),median(Shelter_95[,109:120,10])), 
                nrow=4,byrow=F)

colnames(data2) <- round(seq(0.05,0.5,length.out=10),2)
rownames(data2) <- c("owned","stray","feral","shelter")
barplot(data2, col=coul , border="white", xlab="",ylab="",main="medium neutering",,cex.axis=1.2, cex.names=1.2,cex.lab=1.2,cex.main=1.5,ylim=c(0,200000))
#legend("top", inset=c(0,-0.1),legend = c("owned","stray","feral","shelter") , 
 #      col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz =TRUE)
abline(h=sum(Catvec2),xpd=FALSE,lty=3)
mtext("b",adj=-0.3,mar=TRUE,cex=1.5)

data3 <- matrix(c(median(Owned_98[,109:120,1]),median(Stray_98[,109:120,1]),median(Feral_98[,109:120,1]),median(Shelter_98[,109:120,1]),
                  median(Owned_98[,109:120,2]),median(Stray_98[,109:120,2]),median(Feral_98[,109:120,2]),median(Shelter_98[,109:120,2]),
                  median(Owned_98[,109:120,3]),median(Stray_98[,109:120,3]),median(Feral_98[,109:120,3]),median(Shelter_98[,109:120,3]),
                  median(Owned_98[,109:120,4]),median(Stray_98[,109:120,4]),median(Feral_98[,109:120,4]),median(Shelter_98[,109:120,4]),
                  median(Owned_98[,109:120,5]),median(Stray_98[,109:120,5]),median(Feral_98[,109:120,5]),median(Shelter_98[,109:120,5]),
                  median(Owned_98[,109:120,6]),median(Stray_98[,109:120,6]),median(Feral_98[,109:120,6]),median(Shelter_98[,109:120,6]),
                  median(Owned_98[,109:120,7]),median(Stray_98[,109:120,7]),median(Feral_98[,109:120,7]),median(Shelter_98[,109:120,7]),
                  median(Owned_98[,109:120,8]),median(Stray_98[,109:120,8]),median(Feral_98[,109:120,8]),median(Shelter_98[,109:120,8]),
                  median(Owned_98[,109:120,9]),median(Stray_98[,109:120,9]),median(Feral_98[,109:120,9]),median(Shelter_98[,109:120,9]),
                  median(Owned_98[,109:120,10]),median(Stray_98[,109:120,10]),median(Feral_98[,109:120,10]),median(Shelter_98[,109:120,10])), 
                nrow=4,byrow=F)

colnames(data3) <- round(seq(0.05,0.5,length.out=10),2)
rownames(data3) <- c("owned","stray","feral","shelter")
barplot(data3, col=coul , border="white", xlab="",ylab="",main="high neutering",cex.axis=1.2, cex.names=1.2,cex.lab=1.2,cex.main=1.5,ylim=c(0,200000))
#legend("top", inset=c(-0.3,-0.35),legend = c("owned","stray","feral","shelter") , 
 #      col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz =TRUE)
abline(h=sum(Catvec2),xpd=FALSE,lty=3)
legend("topright", inset=c(0,0),legend = c("owned","stray","feral","shelter") , 
       col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, horiz =FALSE)
mtext("c",adj=-0.3,mar=TRUE,cex=1.5)




# Transform this data in %
data_percentage <- apply(data, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
barplot(data_percentage[2:4,], col=coul[2:4] , border="white", xlab="",ylab="",ylim=c(0,15),cex.axis=1.2, cex.names=1.2,cex.lab=1.2)
mtext("d",adj=-0.3,mar=TRUE,cex=1.5)
#legend("top", inset=c(0.5,-0.08),legend = c("owned","stray","feral","shelter") , 
#      col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz =TRUE)


# Transform this data in %
data_percentage2 <- apply(data2, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
barplot(data_percentage2[2:4,], col=coul[2:4] , border="white", xlab="",ylab="",ylim=c(0,15),cex.axis=1.2, cex.names=1.2,cex.lab=1.2)
mtext("e",adj=-0.3,mar=TRUE,cex=1.5)

#legend("top", inset=c(0.5,-0.08),legend = c("owned","stray","feral","shelter") , 
#      col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1, horiz =TRUE)


# Transform this data in %
data_percentage3 <- apply(data3, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot--> it will be in %!
barplot(data_percentage3[2:4,], col=coul[2:4] , border="white", xlab="",ylab="",ylim=c(0,15),cex.axis=1.2, cex.names=1.2,cex.lab=1.2)
mtext("f",adj=-0.3,mar=TRUE,cex=1.5)

legend("topright", inset=c(0,0),legend = c("stray","feral","shelter") , 
       col = coul[2:4] , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, horiz =FALSE)
mtext(text="proportion owned cats neutered < 6 months",side=1,line=0,outer=TRUE)
mtext(text="percentage",side=2,line=0,outer=TRUE,adj=0.25)
mtext(text="median number of cats",side=2,line=0,outer=TRUE,adj=0.75)

#are the averagetrends sig

#values for table
apply(PG_Owned,2,quantile,probs=c(0.025,0.975))
apply(PG_Owned,2,median)
apply(PG_Owned_95,2,quantile,probs=c(0.025,0.975))
apply(PG_Owned_95,2,median)
apply(PG_Owned_98,2,quantile,probs=c(0.025,0.975))
apply(PG_Owned_98,2,median)

apply(PG_Stray,2,quantile,probs=c(0.025,0.975))
apply(PG_Stray,2,median)
apply(PG_Stray_95,2,quantile,probs=c(0.025,0.975))
apply(PG_Stray_95,2,median)
apply(PG_Stray_98,2,quantile,probs=c(0.025,0.975))
apply(PG_Stray_98,2,median)

apply(PG_Feral,2,quantile,probs=c(0.025,0.975))
apply(PG_Feral,2,median)
apply(PG_Feral_95,2,quantile,probs=c(0.025,0.975))
apply(PG_Feral_95,2,median)
apply(PG_Feral_98,2,quantile,probs=c(0.025,0.975))
apply(PG_Feral_98,2,median)


apply(PG_Shelter,2,quantile,probs=c(0.025,0.975))
apply(PG_Shelter,2,median)
apply(PG_Shelter_95,2,quantile,probs=c(0.025,0.975))
apply(PG_Shelter_95,2,median)
apply(PG_Shelter_98,2,quantile,probs=c(0.025,0.975))
apply(PG_Shelter_98,2,median)

apply(PG_Total,2,quantile,probs=c(0.025,0.975))
apply(PG_Total,2,median)
apply(PG_Total_95,2,quantile,probs=c(0.025,0.975))
apply(PG_Total_95,2,median)

apply(PG_Total_98,2,quantile,probs=c(0.025,0.975))
apply(PG_Total_98,2,median)

