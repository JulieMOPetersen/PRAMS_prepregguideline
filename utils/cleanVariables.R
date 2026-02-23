library(dplyr)

#Refactor binary variables with 1:No & 2:Yes
fnGenBinSubt <- function(x){
  as.factor(as.integer(x-1))
}

#Refactor binary variables with 1:Yes & 2:No
fnGenBinMod <- function(x){
  as.factor(as.integer(x%%2))
}

################################################################################
#Preparing primary variables
fnCleanVariables<- function(dfMerged){
  dfMerged <- dfMerged%>%
    mutate(EXER = fnGenBinSubt(PRE_EXER))%>%
    mutate(BMI_OVER = as.factor(as.integer(MOM_BMIG_QX_REV >= 3)))%>%
    # mutate(PRE_DIET_EXER = as.factor(as.integer(
    #   ifelse((MOM_BMIG_BC >= 3) & ((!is.na(PRE_DIET)) | (!is.na(PRE_EXER))),
    #          ((MOM_BMIG_BC >= 3) & ((PRE_DIET == 2) | (PRE_EXER == 2))),
    #          NA))) ,
    #   PRE_DIET_EXER = ifelse((STATE%in%vecMaternalStates) & (MOM_BMIG_BC%in%c(1,2)),2, PRE_DIET_EXER ),
    #   PRE_DIET_EXER = factor(PRE_DIET_EXER, labels = 0:1))%>%
    mutate(PRE_DIET_EXER = as.factor(as.integer(
      ifelse((MOM_BMIG_BC >= 3) & (!is.na(PRE_DIET)), 
             ((MOM_BMIG_BC >= 3) & (PRE_DIET == 2)), 
             NA))) ,
      PRE_DIET_EXER = ifelse((STATE%in%vecMaternalStates) & (MOM_BMIG_BC%in%c(1,2)),2, PRE_DIET_EXER ),
      PRE_DIET_EXER = factor(PRE_DIET_EXER, labels = 0:1))%>%
    mutate(VITAMIN_BIN = as.factor(as.integer(VITAMIN != 1)))%>%
    mutate(PNC_4MTH = as.factor(as.integer(PNC_MTH<5)))%>%
    mutate(FLU_SHOT = as.factor(as.integer(FLUPREG > 1)))%>%
    mutate(ECIG_3B_A = ifelse(ECIG_3B_A==5,1,2))%>%
    mutate(NO_SMK_3B = as.factor(SMK63B_A%%2))%>%
    mutate(NO_ECIG_3B = as.factor(ECIG_3B_A%%2))%>%
    mutate(LESS_DRK_3B = as.factor(ifelse((DRK_2YRS==1)&(!is.na(DRK_2YRS)),1,as.integer(DRK8_3B > 3))))%>%
    mutate(NO_DRK_3B = as.factor(ifelse((DRK_2YRS==1)&(!is.na(DRK_2YRS)),1,as.integer(DRK8_3B == 6))))%>%
    mutate(DENT = ifelse(((PRE_VIST==1) & (is.na(TYP_DDS))), 1, TYP_DDS))%>%
    mutate(DENTAL = as.factor(as.numeric((DENT-1) | (DDS_CLN - 1))))
  
################################################################################ 
#Non-adherers
  dfMerged$NON_ADH = as.integer(apply(
    dfMerged%>%
      select(all_of(vecNonAdh))%>%
      mutate(across(everything(), ~!as.numeric(levels(.x))[.x])),
    1,
    sum, na.rm = TRUE
  ) >= 4) |> 
    as.factor()
  
  dfMerged$NON_ADH3 = as.integer(apply(
    dfMerged%>%
      select(all_of(vecNonAdh))%>%
      mutate(across(everything(), ~!as.numeric(levels(.x))[.x])),
    1,
    sum, na.rm = TRUE
  ) >= 3) |> 
    as.factor()
  
  
  dfMerged$NON_ADH5 = as.integer(apply(
    dfMerged%>%
      select(all_of(vecNonAdh))%>%
      mutate(across(everything(), ~!as.numeric(levels(.x))[.x])),
    1,
    sum, na.rm = TRUE
  ) >= 5) |> 
    as.factor()
  
  vecMatStates = dfMerged%>%
    filter(!is.na(EXER))%>%
    select(STATE)%>%
    unique()%>%
    unlist()
  
  
  
#Adherers
  dfMerged$ADH = as.integer(apply(
    dfMerged%>%
      select(all_of(vecNonAdh))%>%
      mutate(across(everything(), ~as.numeric(levels(.x))[.x])),
    1,
    sum, na.rm = TRUE
  ) >= 4) |> 
    as.factor()
  
#Adding NAs for non-adherers
  #More than or equal to 4 NAs and categorized as '0' or adherers, are now NAs
  
  vecNAsCount = apply(dfMerged%>%
                        select(vecNonAdh)%>%
                        mutate(across(everything(), ~as.integer(is.na(.x)))),
                      1,
                      sum) >=4
  
  dfMerged[((!as.integer(apply(
    dfMerged%>%
      select(vecNonAdh)%>%
      mutate(across(everything(), ~!as.numeric(levels(.x))[.x])),
    1,
    sum, na.rm = TRUE
  )>= 4) ) & (vecNAsCount)), 'NON_ADH'] = NA
  
  #Adding NAs for non-adherers
  #More than or equal to 3 NAs and categorized as '0' or adherers, are now NAs
  
  vecNAsCount3 = apply(dfMerged%>%
                        select(vecNonAdh)%>%
                        mutate(across(everything(), ~as.integer(is.na(.x)))),
                      1,
                      sum) >=3
  
  dfMerged[((!as.integer(apply(
    dfMerged%>%
      select(vecNonAdh)%>%
      mutate(across(everything(), ~!as.numeric(levels(.x))[.x])),
    1,
    sum, na.rm = TRUE
  )>= 3) ) & (vecNAsCount3)), 'NON_ADH'] = NA
  
  #Adding NAs for non-adherers
  #More than or equal to 4 NAs and categorized as '0' or adherers, are now NAs
  
  vecNAsCount5 = apply(dfMerged%>%
                        select(vecNonAdh)%>%
                        mutate(across(everything(), ~as.integer(is.na(.x)))),
                      1,
                      sum) >=5
  
  dfMerged[((!as.integer(apply(
    dfMerged%>%
      select(vecNonAdh)%>%
      mutate(across(everything(), ~!as.numeric(levels(.x))[.x])),
    1,
    sum, na.rm = TRUE
  )>= 5) ) & (vecNAsCount5)), 'NON_ADH'] = NA
  
  
  #Adding NAs for adherers
  #More than or equal to 4 NAs and categorized as '0' or adherers, are now NAs
  
  vecNAsCountadh = apply(dfMerged%>%
                        select(vecNonAdh)%>%
                        mutate(across(everything(), ~as.integer(is.na(.x)))),
                      1,
                      sum) >=4
  
  dfMerged[((!as.integer(apply(
    dfMerged%>%
      select(vecNonAdh)%>%
      mutate(across(everything(), ~as.numeric(levels(.x))[.x])),
    1,
    sum, na.rm = TRUE
  )>= 4) ) & (vecNAsCountadh)), 'ADH'] = NA  

################################################################################
#Adding the new mini-NONADH
  
  dfMerged$NON_ADHGRP1 = dfMerged%>%
    select(vecGrp1)%>%
    mutate(across(everything(), ~!as.numeric(levels(.x))[.x]))%>%
    mutate(NON_ADHGrp1 = ifelse(if_all(everything(), ~is.na(.)),
                                NA,
                                rowSums(across(vecGrp1), na.rm = T)),
           NON_ADHGrp1 = as.factor(as.numeric(NON_ADHGrp1==length(vecGrp1))))%>%
    select(NON_ADHGrp1)%>%
    unlist()
  
  dfMerged$NON_ADHGRP2 = dfMerged%>%
    select(vecGrp2)%>%
    mutate(across(everything(), ~!as.numeric(levels(.x))[.x]))%>%
    mutate(NON_ADHGrp2 = ifelse(if_all(everything(), ~is.na(.)),
                                NA,
                                rowSums(across(vecGrp2), na.rm = T)),
           NON_ADHGrp2 = as.factor(as.numeric(NON_ADHGrp2==length(vecGrp2))))%>%
    select(NON_ADHGrp2)%>%
    unlist()
  
  dfMerged$NON_ADHGRP3 = dfMerged%>%
    select(vecGrp3)%>%
    mutate(across(everything(), ~!as.numeric(levels(.x))[.x]))%>%
    mutate(NON_ADHGrp3 = ifelse(if_all(everything(), ~is.na(.)),
                                NA,
                                rowSums(across(vecGrp3), na.rm = T)),
           NON_ADHGrp3 = as.factor(as.numeric(NON_ADHGrp3==length(vecGrp3))))%>%
    select(NON_ADHGrp3)%>%
    unlist()
  
  dfMerged$NON_ADHGRP4 = dfMerged%>%
    select(vecGrp4)%>%
    mutate(across(everything(), ~!as.numeric(levels(.x))[.x]))%>%
    mutate(NON_ADHGrp4 = ifelse(if_all(everything(), ~is.na(.)),
                                NA,
                                rowSums(across(vecGrp4), na.rm = T)),
           NON_ADHGrp4 = as.factor(as.numeric(NON_ADHGrp4==length(vecGrp4))))%>%
    select(NON_ADHGrp4)%>%
    unlist()
  
################################################################################  
#Preparing covariates
  dfMerged[,c('DIAB', 'HBP', 'DPRS', 'RUR')] = 
    sapply(dfMerged[,c('BPG_DIAB8', 'BPG_HBP8', 'BPG_DEPRS8', 'NCHS_URB_RUR2')], fnGenBinSubt)
  
  dfMerged[,c( 'MARRY', 'HISP')] = 
    sapply(dfMerged[,c('MARRIED', 'HISP_BC')], fnGenBinMod)
  
  dfMerged = dfMerged%>%
    mutate_at(c('DIAB', 'HBP', 'DPRS', 'RUR', 'MARRY', 'HISP'), as.factor)
  
#Race:
#1=White, 2=Black, 3=Others
  dfMerged = dfMerged%>%
    mutate(RACE_OVR = as.factor(MAT_RACE_PU))%>%
    mutate(RACE_AK = as.factor(MAT_RACE_PU_AK))%>%
    mutate(OTH_PREG = as.factor(as.integer(OTH_TERM >= 1)))%>%
    mutate(MOM_BMIG_BC = as.factor(MOM_BMIG_BC))
  
#Pregnancy Trying
  #dfMerged$PREG_TRY = ifelse(dfMerged$PGINTENT==5, NA, dfMerged$PGINTENT)
  dfMerged$PREG_TRY = factor(dfMerged$PGINTENT)
  levels(dfMerged$PREG_TRY) <- list(
    '10' = c('1', '4'),
    '11' = c('5'),
    '12' = c('2', '3')
    
  )

   
#Adding Insurance
  INS_GRP <- dfMerged%>%
    mutate(pvt = 6^((INSWORK8==2 )| (INSPAR == 2) | (INSHCEX == 2)),
           medicaid = 5^(INSMED ==2),
           insno = 2^(INSNONE==2),
           oth = 3^((INSOTH==2)|(INSCHIP==2 )| (INSGOV == 2) | (INSGOV2 == 2) | (INSMIL==2) | (INSIHS == 2)))%>%
    mutate(INS_GRP = (pmax(pvt, medicaid, insno, oth, insno, na.rm = TRUE)))%>%
    mutate(INS_GRP = ifelse(INS_GRP==1, NA, INS_GRP))%>%
    mutate(INS_GRP = factor(INS_GRP))%>%
    select(INS_GRP)
  
  dfMerged <- cbind(dfMerged, INS_GRP)
  
  levels(dfMerged$INS_GRP) <- list(
    None = c(2),
    Others = c(3),
    Medicaid = c(5),
    Private = c(6)
  )
  
#Adding Education
  dfMerged <- dfMerged%>%
    mutate(EDU = factor(MAT_DEG))
  
  levels(dfMerged$EDU) <- list(
    LessHS = c(1,2),
    CompHS = c(3),
    SomeCollege = c(4,5),
    Bachelors = c(6),
    Graduate = c(7,8)
  )
  
#Adding Age
  
  vecAgeBreaks = c(0, 20, 25, 30, 35, Inf)
  
  
  dfMerged = dfMerged%>%
    mutate(AGE_VT = as.factor(MAT_AGE_NAPHSIS_VT), 
           AGE_AK = as.factor(MAT_AGE_NAPHSIS_AK))
  
  levels(dfMerged$AGE_VT) <- list(
    '<20' = c('8'),
    '20-24' = c('3'), 
    '25-29' = c('4'), 
    '30-34' = c('5'), 
    '35+' = c('6', '7')
  )
  
  levels(dfMerged$AGE_AK) <- list(
    '<20' = c('8'),
    '20-24' = c('3'), 
    '25-29' = c('4'), 
    '30-34' = c('5'), 
    '35+' = c('6', '7')
  )
  
  dfMerged = dfMerged%>%
    mutate(AGE_OVR = cut(MAT_AGE_PU, breaks = vecAgeBreaks, right = FALSE, 
                         labels = c('<20', '20-24', '25-29', '30-34', '35+')),
           AGE_GRP = coalesce(AGE_OVR, AGE_AK, AGE_VT))
  
  
  
#Add Race
  levels(dfMerged$RACE_OVR) = list(
    'White' = c('2'),
    'Black' = c('3'),
    'AI/AN' = c('4', '10'),
    'AsHwPI' = c('21', '22'),
    'Other' = c('23')
  )
  
  levels(dfMerged$RACE_AK) = list(
    'White' = c('1'),
    'AI/AN' = c('2'),
    'Other' = c('3')
  )
  
  dfMerged = dfMerged%>%
    mutate('RACE' = coalesce(RACE_AK, RACE_OVR))
  
#Adding poverty
  pov = list(
    '2022' = c(13590, 4720),
    '2021'=	c(12880,	4540),
    '2020'=	c(12760,	4480),
    '2019'=	c(12490,	4420),
    '2018'=	c(12140, 4320),
    '2017' = c(12060, 4180),
    '2016' = c(11880, 4140)
  )
  
  vecBreaks = cumsum(c(0, 16, rep(4,4), rep(8,2), 9, 3, 13, 12, Inf))
  vecBreaksHI = round(vecBreaks*1.15)
  vecBreaksHI[5] = 33
  vecBreaksHI[9] = 65
  vecBreaksAK = round(vecBreaks*1.25)
  vecBreaksAK[12] = 107
  vecBreaksAK[5] = 36
  vecBreaksDC = cumsum(c(0, 16, rep(4,4), rep(8,2), 9, 3, 13, 12, 15, 20, Inf))
  vecBreaksDE = cumsum(c(0, 16, rep(4,4), rep(8,2), 9, 3, 13, 12, 14, 10, Inf))
  
  
  lsPovCodes = list()
  lsPovCodesAK = list()
  lsPovCodesHI = list()
  lsPovCodesDC = list()
  lsPovCodesDE = list()
  for (n in names(pov)){
    l = pov[[n]]
    vecPovVals = cumsum(c(l[1], rep(l[2],30)))
    vecPovValsHI = vecPovVals * 1.15
    vecPovValsAK = vecPovVals * 1.25
    lsPovCodes[[n]] = as.numeric(cut((vecPovVals/1000), breaks = vecBreaks, labels = c(1:12)))
    lsPovCodesAK[[n]] = as.numeric(cut((vecPovValsAK/1000), breaks = vecBreaksAK, labels = c(1:12)))
    lsPovCodesHI[[n]] = as.numeric(cut((vecPovValsHI/1000), breaks = vecBreaksHI, labels = c(1:12)))
    lsPovCodesDE[[n]] = as.numeric(cut((vecPovVals/1000), breaks = vecBreaksDE, labels = c(1:14)))
    lsPovCodesDC[[n]] = as.numeric(cut((vecPovVals/1000), breaks = vecBreaksDC, labels = c(1:14)))
  }
  
  dfMerged[,'INCOME_NEW'] = dfMerged[,'INCOME8']
  dfMerged[dfMerged$INCOME_NEW %in% c(13:14), 'INCOME_NEW' ] = 1
  dfMerged[dfMerged$INCOME_NEW %in% c(17:19), 'INCOME_NEW' ] = dfMerged[dfMerged$INCOME_NEW %in% c(17:19), 'INCOME_NEW' ] - 5
  dfMerged[dfMerged$INCOME_NEW %in% c(20:22), 'INCOME_NEW' ] = dfMerged[dfMerged$INCOME_NEW %in% c(20:22), 'INCOME_NEW' ] - 8
  dfMerged[dfMerged$INCOME_NEW %in% c(200:212), 'INCOME_NEW' ] = dfMerged[dfMerged$INCOME_NEW %in% c(200:212), 'INCOME_NEW' ] - 200
  dfMerged[dfMerged$INCOME_NEW %in% c(100:112), 'INCOME_NEW' ] = dfMerged[dfMerged$INCOME_NEW %in% c(100:112), 'INCOME_NEW' ] - 100
  
  fnPov = function(x){
    if (x['STATE']=='AK'){
      return (as.integer(lsPovCodesAK[[as.character(x['NEST_YR'])]][as.numeric(x['INC_NDEP'])] >= as.numeric(x['INCOME_NEW'])))
    }
    else if(x['STATE']=='HI'){
     return (as.integer(lsPovCodesHI[[as.character(x['NEST_YR'])]][as.numeric(x['INC_NDEP'])] >= as.numeric(x['INCOME_NEW'])))
    }
    else if(x['STATE']=='DE'){
      return (as.integer(lsPovCodesDE[[as.character(x['NEST_YR'])]][as.numeric(x['INC_NDEP'])] >= as.numeric(x['INCOME_NEW'])))
    }
    else if(x['STATE']=='DC'){
      return (as.integer(lsPovCodesDC[[as.character(x['NEST_YR'])]][as.numeric(x['INC_NDEP'])] >= as.numeric(x['INCOME_NEW'])))
    }
    else{
      return (as.integer(lsPovCodes[[as.character(x['NEST_YR'])]][as.numeric(x['INC_NDEP'])] >= as.numeric(x['INCOME_NEW'])))
    }
  }
  
  dfMerged[, 'PVTY'] = as.factor(apply(dfMerged[,c('NEST_YR', 'INC_NDEP', 'INCOME_NEW', 'STATE')], 1, fnPov))

################################################################################  
#Health Care
  dfMerged = dfMerged%>%
    mutate(HLTH_CR_VIST = fnGenBinSubt(PRE_VIST))
  
#Adding Other heatlh care visits 
  dfMerged = dfMerged%>%
    mutate(across(c('TYP_DOCT', 'TYP_ILLN', 'TYP_INJR', 'TYP_MH', 'TYP_OBGN', 'TYP_OTHR',
                    'TYP_DDS', 'TYP_BC'), ~fnGenBinSubt(.x)))
  
  
#Questions asked during the visit
  dfMerged = dfMerged%>%
    mutate(across(vecPre, ~fnGenBinSubt(.x)))
  
#Fixing some inconsistency
#Some patients left the "health care visit" question blank, but proceeded to answer the 
#questions regarding the type of health care visits. We have changed those health care visits to 1
  dfHlthUpd = dfMerged%>%
    filter(is.na(HLTH_CR_VIST)&((TYP_DOCT==1) | (TYP_ILLN==1) | (TYP_INJR==1) | (TYP_MH==1) | TYP_OBGN==1 | 
                                  TYP_OTHR==1 | TYP_DDS==1 | TYP_BC==1))%>%
    mutate(HLTH_CR_VIST = 1)
  
  dfMerged[dfMerged$ID %in% dfHlthUpd$ID, 'HLTH_CR_VIST'] = 1
  
  #Others now includes visits for illnesses, injuries, depression and others
  dfMerged = dfMerged%>%
    mutate(TYP_OTHERS = as.numeric(as.numeric(levels(TYP_ILLN)[(TYP_ILLN)]) | 
                                     as.numeric(levels(TYP_INJR)[(TYP_INJR)]) | 
                                     as.numeric(levels(TYP_MH)[(TYP_MH)]) | 
                                     as.numeric(levels(TYP_OTHR)[(TYP_OTHR)])) )%>%
    mutate(TYP_OTHERS = as.factor(TYP_OTHERS))
  
  
################################################################################  
#Adding regions
  lsRegions = list()
  lsRegions[['West']] = c('AK', 'CO', 'HI', 'NM', 'WA', 'AZ', 'UT', 'WY', 'MP', 'OR', 'MT' )
  lsRegions[['Midwest']] = c('IA', 'IL', 'MI', 'MO', 'NE', 'WI', 'SD', 'ND', 'KS', 'IN', 'MN' )
  lsRegions[['South']] = c('DE',  'AR', 'LA', 'VA', 'WV', 'AL', 'GA', 'KY', 'MS', 'MD', 'PR', 'DC', 'TN' )
  lsRegions[['Northeast']] = c('MA', 'ME', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VT' )
  
  vecMatchStates = rep(names(lsRegions), sapply(lsRegions, length))
  dfMerged$Region = vecMatchStates[match(dfMerged$STATE, unlist(lsRegions))]
  
################################################################################  
#Adding covariates for the glm model
  dfMerged = dfMerged%>%
    mutate(#PREG_INTERVAL = as.factor(ifelse(ILLB_MO<12, 0, 1)),
           PRE_CONDN = as.factor(as.numeric((BPG_DIAB8-1) | (BPG_HBP8-1) | (BPG_DEPRS8-1) | (MOM_BMIG_BC == 4))),
           PRE_COND2 = as.factor(as.numeric((BPG_DIAB8-1) | (BPG_HBP8-1) | (BPG_DEPRS8-1) )),
           P_PRTERM = as.factor(P_PRTERM%%2))
 
################################################################################ 
#Adding outcomes for glm models
  dfMerged$RF_GHYPE = as.factor(dfMerged$RF_GHYPE)
  levels(dfMerged$RF_GHYPE)<-list(
    '1'=c('Y'),
    '0'=c('N')
  )
  
  dfMerged$RF_EHYPE = as.factor(dfMerged$RF_EHYPE)
  levels(dfMerged$RF_EHYPE)<-list(
    '1'=c('Y'),
    '0'=c('N')
  )
  
  dfMerged$RF_GDIAB = as.factor(dfMerged$RF_GDIAB)
  levels(dfMerged$RF_GDIAB)<-list(
    '1'=c('Y'),
    '0'=c('N')
  )
  
  dfMerged$NICU = as.factor(dfMerged$AB_NICU)
  levels(dfMerged$NICU)<-list(
    '0'=c('N'),
    '1'=c('Y')
  )
  
  
  dfMerged = dfMerged%>%
    mutate(KESSNER = factor(ifelse(KESSNER==4,NA, as.numeric(KESSNER%%3 == 0) )),
           HYP_TNS = as.factor(as.numeric( (RF_GHYPE==1) | (RF_EHYPE==1)  )),
           MOM_DIAB = as.factor(as.numeric(MM_DIAB%%2)),
           CSEC = as.factor(as.numeric( (DEL_1CS-1) | (DEL_RCS-1)  )),
           PRE_BIRTH = as.factor(as.numeric(dfMerged$GEST_WK_PU<37)),
           LOW_BTH_WT = as.factor(as.numeric(dfMerged$GRAM_NAPHSIS<2500)),
           HI_BTH_WT = as.factor(MACROSOMIA%%2),
           DEFECT = as.factor(DEFECT%%2),
           SGA = as.factor(SGA_10%%2),
           LGA = as.factor(LGA%%2))
  

  ADH_SUM = apply(dfMerged[,all_of(vecPrimaryVariables)], 1, \(row){
    row%>%
      as.data.frame()%>%
      mutate(across(everything(), ~as.numeric(as.numeric(.x)==0)))%>%
      sum(na.rm=T)
    
  })
  dfMerged = cbind(dfMerged, ADH_SUM)
  dfMerged
}

#declaring outside the function for future use
lsRegions = list()
lsRegions[['West']] = c('AK', 'CO', 'HI', 'NM', 'WA', 'AZ', 'UT', 'WY', 'MP', 'OR', 'MT' )
lsRegions[['Midwest']] = c('IA', 'IL', 'MI', 'MO', 'NE', 'WI', 'SD', 'ND', 'KS', 'IN', 'MN' )
lsRegions[['South']] = c('DE',  'AR', 'LA', 'VA', 'WV', 'AL', 'GA', 'KY', 'MS', 'MD', 'PR', 'DC', 'TN' )
lsRegions[['Northeast']] = c('MA', 'ME', 'NH', 'NJ', 'NY', 'PA', 'RI', 'VT' )

vecMatchStates = rep(names(lsRegions), sapply(lsRegions, length))
