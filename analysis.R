#Read libraries
vecLibraries = readLines('../data/libraries.txt')

#Function to install libraries
fnInstallLibs = function(vecLibraries){
  vecInstall = setdiff(vecLibraries, installed.packages())
  install.packages(vecInstall, character.only = TRUE)
  sapply(vecLibraries, library, character.only = TRUE)
}

fnInstallLibs(vecLibraries)

#Reading custom functions
lapply(list.files("utils", pattern = "*.R", full.names = TRUE), source, echo = FALSE)

################################################################################
#Reading data
dfSurvey <- read_sas('../data/phase8_arf_2016_2022.sas7bdat')
dfMaternalHealth <- read_sas('../data/phase8_2016_2022_std_l.sas7bdat')

dfMerged = merge(dfSurvey, dfMaternalHealth, by = 'ID', all.x = TRUE)

################################################################################
vecMaternalStates = dfMerged%>%
  filter(!is.na(PRE_DIET))%>%
  select(STATE)%>%
  unique()%>%
  unlist()

dfMerged = dfMerged[, colnames(dfMerged)%in%variables]

#Cleaning Variables
dfMerged = fnCleanVariables(dfMerged)
#creating the survey object


################################################################################
#Multiple Imputation
lsGrp = list(vecGrp1, vecGrp2, vecGrp3, vecGrp4)

dfMerged2 = dfMerged%>%
  select(all_of(c(variables, "PRE_CONDN","PRE_COND2")))%>%
  select(-all_of(setdiff(vecDependents, c(vecHealth, "DRK_2YRS","LGA",
                                          "P_PRTERM"))))%>%
  mutate(SGALGA = as.factor(ifelse((SGA==0)&(LGA==0),0,
                                   ifelse(SGA==1,1,2))))%>%
  select(-c(LGA,SGA))

fnStateMice <- function(chrState, dfRawData){
  tryCatch( {
    print(chrState)
    dfRawData = dfRawData%>%
      filter(STATE==chrState)
    dfDataset = dfRawData%>%
      #select(-c(DRK_2YRS, MOM_BMIG_BC))
      select(-c(DRK_2YRS))
    
    imp <- mice(dfDataset, maxit=0)
    matPred <- imp$predictorMatrix
    chrMethod <- imp$method
    matWhere <- imp$where
    
    if (!chrState%in%vecMaternalStates) {
      matPred[c('PRE_DIET_EXER', 'EXER'),]<-0
      matPred[,c('PRE_DIET_EXER', 'EXER')]<-0
      chrMethod[c('PRE_DIET_EXER', 'EXER')] <- ""
    } else{
      matWhere[dfRawData$MOM_BMIG_BC %in%c(1,2) ,'PRE_DIET_EXER']<-FALSE
    }
    
    matPred[vecWeightVariables,]<-0
    matPred[, c('NICU', 'HYP_TNS', 'PRE_DIET_EXER', 'PRE_BIRTH', 'P_PRTERM',
                'EXER', 'LESS_DRK_3B', 'NO_DRK_3B',
                vecHealth, vecPre)] <- 0
    matWhere[(dfDataset$NEST_YR < 2020), c('NICU', 'HYP_TNS')] <- FALSE
    matWhere[which(dfDataset$HLTH_CR_VIST == 0), c(vecHealth, vecPre)] <- FALSE
    matWhere[which(dfRawData$DRK_2YRS == 0), c('NO_DRK_3B', 'LESS_DRK_3B')] <- FALSE
    
    
    
    #matPred[,c('NEST_YR')] <- 0
    objMI <- mice(dfDataset, m=20, maxit = 5, where = matWhere,
                  predictorMatrix = matPred, method = chrMethod,
                  print =  FALSE, seed = 111)
    dfComplete1 = mice::complete(objMI, action="long", include = TRUE)
    
    
    return(dfComplete1)
  },
  error = \(e){
    lsErrorState[[chrState]] <<- append(lsErrorState[[chrState]], chrState)
    return()
  }
  
  )
  
}

fnMode <- function(vec, na.rm=FALSE){
  if(na.rm){vec = vec[!is.na(vec)]}
  vecUniq = unique(vec)
  vecUniq[which.max(tabulate(match(vec, vecUniq)))]
}
# 
# lsErrorState  = list()
# vecPRAMSStates = unique(dfMerged$STATE)
# lsStateMice = lapply(vecPRAMSStates, \(chrState){
#   fnStateMice(chrState, dfMerged2)
# })
# names(lsStateMice) = vecPRAMSStates
#Write it somewhere
# write.csv(lsStateMice%>%
#   bind_rows(), '../output/data_files/PRAMS_Imputed20_2025.12.16.gz')
# 
# dfStateMice  = lapply(lsStateMice, \(dfComplete){
#   dfComplete%>%
#     filter(.imp!=0)%>%
#     summarise(across(everything(), ~fnMode(.x)), .by=all_of(vecWeightVariables))
# })%>%
#   bind_rows()

# write.csv(dfStateMice,
#           file = gzfile('../output/data_files/PRAMS_Imputed_2025_12_16.csv.gz'),
#           row.names = F)
dfStateMice = read.csv('../output/data_files/PRAMS_Imputed_2025_12_16.csv.gz')
colnamesMice = colnames(dfStateMice)
dfStateMice = lapply(colnames(dfStateMice), \(x){
  if (is.factor(dfMerged2[[x]])){
    return (factor(dfStateMice[[x]], levels = levels(dfMerged2[[x]])))
  } else {return (dfStateMice[[x]])}
})%>%
  bind_cols()

colnames(dfStateMice) = colnamesMice
#Updating SGA_LGA and changing PRE_DIET_EXER for newer imputation of bmi
#Similarly, changing the way health care visits are calculated along with fixing
#error of drinking variables' interaction with their conditions
dfStateMice = dfStateMice%>%
  mutate(SGA = as.factor(ifelse(SGALGA==1,1,0)),
         LGA = as.factor(ifelse(SGALGA==2,1,0)),
         PRE_DIET_EXER = ifelse((MOM_BMIG_BC %in% c(1,2))&(STATE%in%vecMaternalStates), 
                                2, PRE_DIET_EXER),
         PRE_DIET_EXER = factor(PRE_DIET_EXER, labels = 0:1),
         HLTH_CR_VIST2 = 
           ifelse(HLTH_CR_VIST==1, 
                  ifelse(if_any(setdiff(vecHealth, "TYP_DDS"), ~(.x==1)),
                         2,1),HLTH_CR_VIST),
         HLTH_CR_VIST2 = factor(HLTH_CR_VIST2, labels = 0:1))

dfStateMice[which(dfMerged$DRK_2YRS==1 & !is.na(dfMerged$DRK_2YRS)),"NO_DRK_3B"]<-"1"
dfStateMice[which(dfMerged$DRK_2YRS==1 & !is.na(dfMerged$DRK_2YRS)),"LESS_DRK_3B"]<-"1"

dfStateMice$NON_ADHGRP1 = dfStateMice%>%
  select(vecGrp1)%>%
  mutate(across(everything(), ~!as.numeric(levels(.x))[.x]))%>%
  mutate(NON_ADHGrp1 = ifelse(if_all(everything(), ~is.na(.)),
                              NA,
                              rowSums(across(vecGrp1), na.rm = T)),
         NON_ADHGrp1 = as.factor(as.numeric(NON_ADHGrp1==length(vecGrp1))))%>%
  select(NON_ADHGrp1)%>%
  unlist()

dfStateMice$NON_ADHGRP2 = dfStateMice%>%
  select(vecGrp2)%>%
  mutate(across(everything(), ~!as.numeric(levels(.x))[.x]))%>%
  mutate(NON_ADHGrp2 = ifelse(if_all(everything(), ~is.na(.)),
                              NA,
                              rowSums(across(vecGrp2), na.rm = T)),
         NON_ADHGrp2 = as.factor(as.numeric(NON_ADHGrp2==length(vecGrp2))))%>%
  select(NON_ADHGrp2)%>%
  unlist()

dfStateMice$NON_ADHGRP3 = dfStateMice%>%
  select(vecGrp3)%>%
  mutate(across(everything(), ~!as.numeric(levels(.x))[.x]))%>%
  mutate(NON_ADHGrp3 = ifelse(if_all(everything(), ~is.na(.)),
                              NA,
                              rowSums(across(vecGrp3), na.rm = T)),
         NON_ADHGrp3 = as.factor(as.numeric(NON_ADHGrp3==length(vecGrp3))))%>%
  select(NON_ADHGrp3)%>%
  unlist()

dfStateMice$NON_ADHGRP4 = dfStateMice%>%
  select(vecGrp4)%>%
  mutate(across(everything(), ~!as.numeric(levels(.x))[.x]))%>%
  mutate(NON_ADHGrp4 = ifelse(if_all(everything(), ~is.na(.)),
                              NA,
                              rowSums(across(vecGrp4), na.rm = T)),
         NON_ADHGrp4 = as.factor(as.numeric(NON_ADHGrp4==length(vecGrp4))))%>%
  select(NON_ADHGrp4)%>%
  unlist()


svyPRAMSMI <- svydesign(ids = ~0, strata = ~SUD_NEST, 
                        fpc = ~TOTCNT, weights = ~WTANAL, data = dfStateMice)


dfAdh1 = svyby(~NON_ADHGRP1, ~STATE, subset(svyPRAMSMI, !is.na(NON_ADHGRP1)), svyciprop, na.rm = TRUE)
dfAdh1%>%
  as.data.frame()%>%
  mutate(across(where(is.double), ~(.x)))%>%
  summarise(Median=round(median(NON_ADHGRP1), 2),
            IQR = paste(format(round(quantile(NON_ADHGRP1, probs = c(0.25, 0.75)), 2), nsmall = 2), collapse = '-'),
            'Min/Max' = paste(format(round(range(NON_ADHGRP1), 2), nsmall = 2), collapse = '-'))

dfAdh2 = svyby(~NON_ADHGRP2, ~STATE, subset(svyPRAMSMI, !is.na(NON_ADHGRP2)), svyciprop, na.rm = TRUE)
dfAdh2%>%
  as.data.frame()%>%
  mutate(across(where(is.double), ~(.x)))%>%
  summarise(Median=round(median(NON_ADHGRP2), 2),
            IQR = paste(format(round(quantile(NON_ADHGRP2, probs = c(0.25, 0.75)), 2), nsmall = 2), collapse = '-'),
            'Min/Max' = paste(format(round(range(NON_ADHGRP2), 2), nsmall = 2), collapse = '-'))

dfAdh3 = svyby(~NON_ADHGRP3, ~STATE, subset(svyPRAMSMI, !is.na(NON_ADHGRP3)), svyciprop, na.rm = TRUE)
dfAdh3%>%
  as.data.frame()%>%
  mutate(across(where(is.double), ~(.x)))%>%
  summarise(Median=round(median(NON_ADHGRP3), 2),
            IQR = paste(format(round(quantile(NON_ADHGRP3, probs = c(0.25, 0.75)), 2), nsmall = 2), collapse = '-'),
            'Min/Max' = paste(format(round(range(NON_ADHGRP3), 2), nsmall = 2), collapse = '-'))

dfAdh4 = svyby(~NON_ADHGRP4, ~STATE, subset(svyPRAMSMI, !is.na(NON_ADHGRP4)), svyciprop, na.rm = TRUE)
dfAdh4%>%
  as.data.frame()%>%
  mutate(across(where(is.double), ~(.x)))%>%
  summarise(Median=round(median(NON_ADHGRP4), 2),
            IQR = paste(format(round(quantile(NON_ADHGRP4, probs = c(0.25, 0.75)), 2), nsmall = 2), collapse = '-'),
            'Min/Max' = paste(format(round(range(NON_ADHGRP4), 2), nsmall = 2), collapse = '-'))

################################################################################
#Association Modeling

dfAssoc2 = dfStateMice

#Releveling variables
levels(dfAssoc2$AGE_GRP) = list(
  "<25" = c("<20", "20-24"),
  "25-34" = c("25-29","30-34"),
  "35+" = "35+"
)

levels(dfAssoc2$EDU) = list(
  "LessHS" = "LessHS",
  "CompHS" = "CompHS",
  "College" = c("SomeCollege", "Bachelors", "Graduate")
)

#New Survey Object
svyPRAMSMI2 <- svydesign(ids = ~0, strata = ~SUD_NEST, fpc = ~TOTCNT, 
                         weights = ~WTANAL, data = dfAssoc2)
################################################################################
lsStates = as.list(unique(dfMerged$STATE))

lsUnadjInfLvl1 <- fnUnadjModel(lsInfOutcomes, vecADH, svyPRAMSMI2)
lsUnadjMomLvl1 <- fnUnadjModel(lsMomOutcomes, vecADH, svyPRAMSMI2)
fnDisplayModelTab(append(lsUnadjMomLvl1, lsUnadjInfLvl1))
lsUnadjInfAdhLvl1 <- fnUnadjModel(lsInfOutcomes, vecNonAdh, svyPRAMSMI2)
lsUnadjMomAdhLvl1 <- fnUnadjModel(lsMomOutcomes, vecNonAdh, svyPRAMSMI2)
fnDisplayModelTab(append(lsUnadjMomAdhLvl1, lsUnadjInfAdhLvl1))
lsAdjInfErr = list()
lsAdjInfLvl1 <- fnAdjModel(lsInfOutcomes, vecAssocCovs5, vecADH, svyPRAMSMI2, lsAdjInfErr)
lsAdjMomErr = list()
lsAdjMomLvl1 <- fnAdjModel(lsMomOutcomes, vecAssocCovs5, vecADH, svyPRAMSMI2, lsAdjMomErr)
fnDisplayModelTab(append(lsAdjMomLvl1, lsAdjInfLvl1))
lsAdjInfErrAssoc = list()
lsInfAssocLvl1  <- fnAdjModel(lsInfOutcomes, vecAssocCovs5, lsAssocCovs, svyPRAMSMI2, lsAdjInfErrAssoc)
lsAdjMomErrAssoc = list()
lsMomAssocLvl1  <- fnAdjModel(lsMomOutcomes, vecAssocCovs5, lsAssocCovs, svyPRAMSMI2, lsAdjMomErrAssoc)
fnDisplayModelTab(append(lsMomAssocLvl1, lsInfAssocLvl1))

#Maybe turn this into function or find a workaround
#Running Crude on states that the adjusted models ran on



lsCrudeInfLvl1 = lapply(lsInfOutcomes, \(variable){
  lsLvl2= lapply(vecADH, \(variable2){
    form = as.formula(paste0(c(variable, reformulate(variable2)), collapse=" "))
    if (variable2 == "NON_ADHGRP1"){
      lsRelStates = as.list(setdiff(vecMaternalStates, lsAdjInfErr[[variable]][[variable2]]))
    } else { lsRelStates = as.list(setdiff(unlist(lsStates), lsAdjInfErr[[variable]][[variable2]]))}
    lsLvl3 = Map(\(x){
      if ((x!="IN")|(!variable%in%c("NICU", "HYP_TNS"))){
        m1 = svyglm(form, design = subset(svyPRAMSMI2, STATE %in% x), family = binomial(link = 'log'))
        return(cbind('State' = x, 'ADH' = variable2, summary(m1)$coeff[,c(1,4)], confint(m1))[-1,])
      } else {return()}
      
    }, lsRelStates)
    bind_rows(lsLvl3)
  })
  bind_rows(lsLvl2)
  
})
#fnDisplayModelTab(lsCrudeInfLvl1)

lsCrudeMomLvl1 = lapply(lsMomOutcomes, \(variable){
  lsLvl2= lapply(vecADH, \(variable2){
    form = as.formula(paste0(c(variable, reformulate(variable2)), collapse=" "))
    if (variable2 == "NON_ADHGRP1"){
      lsRelStates = as.list(setdiff(vecMaternalStates, lsAdjMomErr[[variable]][[variable2]]))
    } else {lsRelStates = as.list(setdiff(unlist(lsStates), lsAdjMomErr[[variable]][[variable2]]))}
    lsLvl3 = Map(\(x){
      if ((x!="IN")|(!variable%in%c("NICU", "HYP_TNS"))){
        m1 = svyglm(form, design = subset(svyPRAMSMI2, STATE %in% x), family = binomial(link = 'log'))
        return(cbind('State' = x, 'ADH' = variable2, summary(m1)$coeff[,c(1,4)], confint(m1))[-1,])
      } else {return()}
      
    }, lsRelStates)
    bind_rows(lsLvl3)
  })
  bind_rows(lsLvl2)
  
})
fnDisplayModelTab(append(lsCrudeMomLvl1, lsCrudeInfLvl1))


lsSensInfErr = list()
lsSensInfLvl1 <- fnAdjModel(lsInfOutcomes, vecAssocCovsSens, vecADH, svyPRAMSMI2, lsSensInfErr)
lsSensMomErr = list()
lsSensMomLvl1 <- fnAdjModel(lsMomOutcomes, vecAssocCovsSens, vecADH, svyPRAMSMI2, lsSensMomErr)
fnDisplayModelTab(append(lsSensMomLvl1, lsSensInfLvl1))


#Adding pre-term history as a covariate
lsSens2InfErr = list()
lsSens2InfLvl1 = lapply(lsInfOutcomes, \(variable){
  #print(variable)
  if (variable=="PRE_BIRTH"){
    vecAssocFn = c(vecAssocCovs5, "P_PRTERM")
  } else {vecAssocFn = vecAssocCovs5}
  #vecAssocFn = vecAssocCovsSens
  lsLvl2= lapply(vecADH, \(variable2){
    #print(variable2)
    form = as.formula(paste0(c(variable, reformulate(c(variable2, vecAssocFn))), collapse=" "))
    if (variable2 == "NON_ADHGRP1"){
      lsRelStates = as.list(vecMaternalStates)
    } else { lsRelStates = lsStates}
    lsLvl3 = Map(\(x){
      #print(x)
      tryCatch({if ((x!="IN")|(!variable%in%c("NICU", "HYP_TNS"))){
        m1 = svyglm(form, design = subset(svyPRAMSMI2, STATE %in% x), family = binomial(link = 'log'))
        return(cbind('State' = x, 'ADH' = variable2, summary(m1)$coeff[,c(1,4)], confint(m1))[2,])
      } else {return()}},
      error = \(e){
        lsSens2InfErr[[variable]][[variable2]] <<- append(lsSens2InfErr[[variable]][[variable2]], x)
        return()
      })
      
    }, lsRelStates)
    bind_rows(lsLvl3)
    
  })
  bind_rows(lsLvl2)
  
})
fnDisplayModelTab(lsSens2InfLvl1)

#Binary Variables by ADH
dfAdhBin = fnGenBinSumm(vecADH, vecBinCov, svyPRAMSMI)
dfAdhMult = fnGenMultSumm(vecADH, vecMultCov, svyPRAMSMI)

dfAdhOutcome = fnGenBinSumm(vecADH, "HLTH_CR_VIST2", svyPRAMSMI)
dfAdhHealth = fnGenBinSumm(vecADH, setdiff(vecHealth,"TYP_DDS"), subset(svyPRAMSMI, HLTH_CR_VIST2==1))
dfAdhPre = fnGenBinSumm(vecADH, vecPre, subset(svyPRAMSMI, HLTH_CR_VIST2==1))
dfAdhMomOutcomes = fnGenBinSumm(vecADH, vecMomOutcomes, svyPRAMSMI)
dfAdhInfOutcomes = fnGenBinSumm(vecADH, vecInfOutcomes, svyPRAMSMI)
dfNonAdhMomOutcomes = fnGenBinSumm(c(vecGrp1, vecGrp2, vecGrp3, vecGrp4), vecMomOutcomes, svyPRAMSMI)
dfNonAdhInfOutcomes = fnGenBinSumm(c(vecGrp1, vecGrp2, vecGrp3, vecGrp4), vecInfOutcomes, svyPRAMSMI)

dfAdhVariables = fnGenBinSumm(vecADH,unlist(lsGrp), svyPRAMSMI)

dfAdhMult%>%
  rbind(dfAdhBin%>%mutate(Level=NA))%>%
  mutate(Covariate = factor(Covariate, levels = vecVarLevels, labels = vecVarLabels),
         Level = factor(Level, levels = vecOrdLevels, labels = vecOrdLabels))%>%
  arrange(Covariate,Level)
Reduce(rbind, list(dfAdhOutcome, dfAdhHealth, dfAdhPre))%>%
  mutate(Covariate = factor(Covariate, 
                            levels = vecHlthLevels,
                            labels = vecHealthLabels))%>%
  arrange(Covariate)

lsAdhGrp = lapply(unlist(lsGrp), \(variable){
  if (variable%in%c("PRE_DIET_EXER", "EXER")){
    vecRelStates = vecMaternalStates
  } else {vecRelStates = unlist(lsStates)}
  dfInt = svyby(reformulate(variable), ~STATE, subset(svyPRAMSMI, STATE%in%vecRelStates), svymean)%>%
    as.data.frame()%>%
    select(!(starts_with("se")|ends_with("0")) )
  
  dfInt%>%
    mutate(across(where(is.numeric), ~.x*100))%>%
    rename(ADH := !!(paste0(variable,"1")))%>%
    summarise(Median = format(round(median(ADH),1), nsmall=1),
              IQR = paste(format(round(quantile(ADH, probs = c(0.25, 0.75)), 1), nsmall = 1), collapse = '-'),
              'Min/Max' = paste(format(round(range(ADH), 1), nsmall = 1), collapse = '-'))%>%
    mutate(Variable = variable)
})

dfAdhGrp=bind_rows(lsAdhGrp)%>%
  mutate(Variable = factor(Variable,
                           levels = vecLvlAdh,
                           labels=vecLabelAdh))
dfAdhGrp = dfAdhGrp[,c(ncol(dfAdhGrp), c(1:(ncol(dfAdhGrp)-1)))]

rbind(dfAdhInfOutcomes, dfAdhMomOutcomes)%>%
  mutate(Covariate = factor(Covariate, 
                            levels = veclvlOutTab,
                            labels = veclblOutTab))%>%
  arrange(Covariate)

rbind(dfNonAdhInfOutcomes, dfNonAdhMomOutcomes)%>%
  mutate(Covariate = factor(Covariate, 
                            levels = veclvlOutTab,
                            labels = veclblOutTab))%>%
  arrange(Covariate)

################################################################################



lsPie = lapply(seq_along(lsGrp), \(intGrp){
  vecGrp = lsGrp[[intGrp]]
  if (intGrp==1){
    lsRelStates = as.list(vecMaternalStates)
  } else { lsRelStates = lsStates}
  
  lsLvl2 = Map(\(x){
    svytable(reformulate(vecGrp), subset(svyPRAMSMI, STATE%in%x))|>
      prop.table()|>
      as.data.frame()%>%
      mutate(State = x)
  }, lsRelStates)
  
  if(intGrp==2){
    dfInt = bind_rows(lsLvl2)%>%
      summarise(Freq = median(Freq), .by=vecGrp)%>%
      mutate(Var = factor(get(vecGrp)),
             label = ifelse(Var==0, paste0("None", intGrp), vecGrp))
  } else {
    dfInt = bind_rows(lsLvl2)%>%
      summarise(Freq = median(Freq), .by=vecGrp)%>%
      mutate(Var = (1)*(as.numeric(as.character(get(vecGrp[[1]])))) 
             + (2)*(as.numeric(as.character(get(vecGrp[[2]])))),
             Var = factor(Var),
             label = ifelse(Var==0, paste0("None", intGrp), 
                            ifelse(Var==(3), paste0("Both", intGrp),
                                   ifelse(Var==1, vecGrp[[1]], vecGrp[[2]]))))
  }
  dfInt%>%
    mutate(grp = intGrp,
           Freq = (1/sum(Freq))*Freq)%>%
    select(!all_of(vecGrp))
})

pltPie = bind_rows(lsPie)%>%
  mutate(txtFreq = format(round(Freq*100,1), nsmall=1),
         txtFreq = ifelse(txtFreq==" 0.0",NA,txtFreq),
         grp = factor(grp, labels=vecLabelAdhGrp),
         label = factor(label, levels = 
                          c(lapply(seq_along(lsGrp), \(x){if(x!=2){c(paste0(c("Title", "None"),x), lsGrp[[x]], paste0("Both",x))}
                            else {c(paste0(c("Title", "None"),x), lsGrp[[x]], " ", "  ")}})%>%unlist())))%>%
  ggplot(aes(x="", y=Freq, fill=label))+
  geom_bar(aes(fill = label),stat="identity", width=0.1, color="black",
           show.legend = FALSE
  )+
  coord_radial("y", expand=FALSE)+
  geom_label(aes(label = paste0(txtFreq,"%"), group=label),position = position_stack(vjust=0.5),
             show.legend = FALSE, fill="white")+
  #geom_label(aes(label = txtFreq), position = position_stack(vjust=0.5))+
  facet_wrap(~grp)+
  # scale_fill_manual(values=c("white", "#EDD1FF", "#F26FA6", "#67A7F0", "#8925DB",
  #                            "white", "#c6f8ff", "#595cff", "white", "white",
  #                            "white", "#E3FFD4", "#26DBDE", "#FDBB2D", "#1EB500",
  #                            "white", "#FAEFCF", "#B2EF91", "#FA9372", "#9C7202"),
  #                   drop=FALSE, labels=fnLabeller)+
  scale_fill_manual(values=c("white", "#f2cedd", "#f289b5", "#c172c8", "#8925DB",
                             "white", "#d9d9ff", "#595cff", "white", "white",
                             "white", "#E3FFD4", "#68b694", "#6dbfbf", "#00594a",
                             "white", "#ffdbd1", "#e68f67", "#b48777", "#57160b"),
                    drop=FALSE, labels=fnLabeller)+
  theme_bw()+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggbldPie <- ggplot_build(pltPie)
ggbldPie$data[[2]]$x = ggbldPie$data[[2]]$x+0.03
#ggbldPie$data[[2]]$x[13] = ggbldPie$data[[2]]$x[13]+0.03
#ggbldPie$data[[2]]$y[13] = ggbldPie$data[[2]]$y[13]+0.03
plot(ggplot_gtable(ggbldPie))


################################################################################
#Boxplot
pltBox = lapply(names(append(lsAdjMomLvl1, lsAdjInfLvl1)), \(x) mutate(append(lsAdjMomLvl1, lsAdjInfLvl1)[[x]], Outcome=x))|>bind_rows()%>%
  mutate(Outcome = factor(Outcome, levels = rev(vecLvlOutcomes), 
                          labels = rev(str_wrap(vecLabelOutcomes2, width=15))),
         Estimate = exp(as.numeric(Estimate)),
         ADH = factor(ADH, labels = vecLabelAdhGrp))%>%
  filter(between(Estimate, 0.2, 4))%>%
  ggplot(aes(y=Outcome, x=Estimate))+
  geom_vline(xintercept = 1)+
  geom_boxplot(aes(fill=ADH), outlier.size = 1, show.legend = FALSE)+
  scale_fill_manual(values = c("#f2cedd","#d9d9ff", "#E3FFD4", "#ffdbd1"))+
  scale_x_continuous(trans = 'log', breaks = c(0.5, 0.33, 1, 2, 3))+
  labs(x='Adjusted Risk Ratio')+
  facet_wrap(~ADH)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 8,hjust = 0, lineheight = 0.9),
        axis.title.x = element_text(size=9, face='bold'))


dfStatesOutcome = fnGenStates(lsAdjMomLvl1, lsAdjInfLvl1)
dfStatesOutcome%>%
  View()
dfStatesOutcome%>%
  pivot_wider(names_from = "ADH", values_from = "States")%>%
  View()

dfStatesSens1Outcome = fnGenStates(lsSensMomLvl1, lsSensInfLvl1)
dfStatesSens1Outcome%>%
  View()
dfStatesSens1Outcome%>%
  pivot_wider(names_from = "ADH", values_from = "States")%>%
  View()

dfStatesSens2Outcome = fnGenStates(lsSens2InfLvl1)
dfStatesSens2Outcome%>%
  filter(Variable=="Preterm Birth")%>%
  View()
dfStatesSens2Outcome%>%
  filter(Variable=="Preterm Birth")%>%
  pivot_wider(names_from = "ADH", values_from = "States")%>%
  View()

fnGenStates(lsMomAssocLvl1, lsInfAssocLvl1)%>%
  pivot_wider(names_from = "ADH", values_from = "States")%>%
  View()

dfStateMice%>%
  select(STATE)%>%
  summarise(Count = n(), .by=STATE)
