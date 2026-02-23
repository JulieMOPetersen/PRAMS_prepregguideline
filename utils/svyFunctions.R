#Generate summary including NAs, CI, N

#vecPrimaryVariables: vector of variables needed to summarise
#prams.svy: the survey object
genSummary <- function(vecPrimaryVariables, prams.svy){
  lsSummary <- 
    lapply(vecPrimaryVariables, 
           \(variable)
           {
            if (variable %in% c('EXER', 'PRE_DIET_EXER')){
              dfSum <- svyby(formula = reformulate(variable), ~STATE, 
                             subset(prams.svy, !is.na(get(variable))), svyciprop, vartype = 'ci', na.rm = TRUE)
            }
            else{
              dfSum <- svyby(formula = reformulate(variable), ~STATE, 
                             prams.svy, svyciprop, vartype = 'ci', na.rm = TRUE)
            }
            dfSum <- dfSum%>%
              as_tibble()%>%
              mutate(across(c(2:4), ~round(.x,2)))%>%
              mutate(ci = paste0('(', ci_l, ', ', ci_u, ')'))%>%
              select(-c(ci_l,ci_u))%>%
              rename_with(~paste0(paste0(variable,"_"), .x, recycle0 = TRUE), starts_with('ci'))
            
            
            dfN <- svytable(formula = reformulate(c(variable, 'STATE')), 
                            prams.svy, na.rm = TRUE)%>%
              as_tibble()%>%
              filter(.[[1]] ==1)%>%
              select(STATE, n)%>%
              rename_with(~paste0(paste0(variable, '_'), .x), starts_with('n'))%>%
              merge(dfSum, by = 'STATE')
            
            dfNAs <- dfMerged%>%
              group_by(STATE)%>%
              summarise(NAs = sum(is.na(get(variable))))%>%
              merge(dfN, by = 'STATE', all.x = TRUE)
            
            dfNAs[,c(1,3,4,5,2)]
          })

  dfSummaries = bind_cols(lsSummary)%>%
    t()%>%
    as_tibble()
  
  colnames(dfSummaries) <- dfSummaries[1,]
  
  dfSummaries = dfSummaries[-seq(1, nrow(dfSummaries),5),]
  
  dfSummaries$var = rep(c('n', 'p', 'ci', 'NA'), length(vecPrimaryVariables))
  
  dfSummaries = dfSummaries[,c(ncol(dfSummaries), c(1:(ncol(dfSummaries)-1)))]
  
  dfSummaries
}
################################################################################
#Gen LSSummary
genLSSummary <- function(vecPrimaryVariables, prams.svy){
lapply(vecPrimaryVariables, 
       \(variable){
         if (variable %in% c('EXER', 'PRE_DIET_EXER')){
           dfSum <- svyby(formula = reformulate(variable), ~STATE, 
                          subset(prams.svy, !is.na(get(variable))), svyciprop, vartype = 'ci', na.rm = TRUE)
         }
         else{
           dfSum <- svyby(formula = reformulate(variable), ~STATE, 
                          prams.svy, svyciprop, vartype = 'ci', na.rm = TRUE)
         }
         dfSum <- dfSum%>%
           as_tibble()%>%
           mutate(across(c(2:4), ~round(.x,2)))%>%
           mutate(ci = paste0('(', ci_l, ', ', ci_u, ')'))%>%
           select(-c(ci_l,ci_u))%>%
           rename_with(~paste0(paste0(variable,"_"), .x, recycle0 = TRUE), starts_with('ci'))
         
         
         dfN <- svytable(formula = reformulate(c(variable, 'STATE')), 
                         prams.svy, na.rm = TRUE)%>%
           as_tibble()%>%
           filter(.[[1]] ==1)%>%
           select(STATE, n)%>%
           rename_with(~paste0(paste0(variable, '_'), .x), starts_with('n'))%>%
           merge(dfSum, by = 'STATE')
         
         dfNAs <- dfMerged%>%
           group_by(STATE)%>%
           summarise(NAs = sum(is.na(get(variable))))%>%
           merge(dfN, by = 'STATE', all.x = TRUE)
         
         dfNAs[,c(1,3,4,5,2)]
       })
}

################################################################################
#Generate summary for Tableau

#lsSummary: list object containing summary
genTableauSummary <- function(lsSummary){
  lsSummaryTableau <- lapply(lsSummary, 
                             \(dfVariable){
                               vari = colnames(dfVariable)[3]
                               colnames(dfVariable) <- c('State', 'n', 'p', 'ci', 'NA')
                               cbind(dfVariable, data.frame(variable = rep(vari, nrow(dfVariable))))
                             })
  
  dfSummaryTableau <- bind_rows(lsSummaryTableau)
  dfSummaryTableau
}



################################################################################
#Generate state-wise adherence proportion for binary variables

#vecVariables: vector of variables needed to summarise
#prams.svy: the survey object

fnGenBinSummary <- function(vecVariables, prams.svy){
  lsTest <- lapply(vecVariables, \(covariate){
    
    #HISP is missing/uncounted for the state of VT
    #RUR is missing for the states of PR, AK
    #Thus, for these variables we subset the dataset for convergence
    
    #calculates proportion of adherents per state per covariate
    if (covariate %in% c('HISP', 'RUR')){
      dfCovariate = svyby(reformulate(covariate), ~(STATE+NON_ADH), subset(prams.svy, !is.na(get(covariate))), svyciprop, vartype = 'ci', na.rm = TRUE)%>%
        as_tibble()
    }
    else{
      dfCovariate = svyby(reformulate(covariate), ~(STATE+NON_ADH), (prams.svy), svyciprop, vartype = 'ci', na.rm = TRUE)%>%
        as_tibble()
    }
    
    #pivots the table to a wide format to show values per state per 
    #adherent state(1 or 0)
    dfCovariate = dfCovariate%>%
      as.data.frame()%>%
      select(!starts_with('ci'))%>%
      filter(NON_ADH!=-1)%>%
      pivot_wider(names_from = c(STATE,NON_ADH), values_from = covariate)%>%
      mutate(Covariate = covariate)
  })
  
  #merging all the results from covariates and rearranging the columns
  dfTest <- bind_rows(lsTest)
  dfTest <- dfTest[,c(ncol(dfTest), seq(1,ncol(dfTest)-1) )]
  dfTest <- dfTest[, c(1, order(colnames(dfTest)[2:ncol(dfTest)]) + 1)]
  
  #calulating median value across states per covariate per adherent state
  dfMed = dfTest%>%
    pivot_longer(cols = (2:ncol(dfTest)), names_to = 'State')%>%
    mutate(ADH = str_extract(State, '(?<=_).*'),
           State = str_extract(State, '(.*)(?=_)'))%>%
    group_by(Covariate, ADH)%>%
    summarise(Median = median(value, na.rm = TRUE))%>%
    ungroup()%>%
    pivot_wider(names_from = c(ADH), values_from = c(Median), names_prefix = 'Median_')
  
  #merging the median value to the previous dataset
  dfTest = merge(dfTest, dfMed, by = c('Covariate'))
}


################################################################################
#Generate state-wise adherence proportion for multivariate variables


#vecVariables: vector of variables needed to summarise
#svyobject: the survey object
fnGenMultSummary <- function(vecVariables, svyobject){
  lsSumm = lapply(vecVariables, \(covariate){
    levelCovariate = length(levels(svyobject$variables[,covariate]))
    
    #calculates proportion of adherents per state per covariate
    
    #Maybe remove this is loop?
    if (covariate %in% c('HYP_TNS', 'NICU')){
      dfCovariate = svyby(reformulate(covariate), ~(STATE+NON_ADH), subset(svyobject, NEST_YR>=2020), 
                          svymean, na.rm = TRUE)
    }
    else{
      dfCovariate = svyby(reformulate(covariate), ~(STATE+NON_ADH), (svyobject), svymean,  na.rm = TRUE)
    }
    
    #pivoting results to a wider format
    dfCovariate = dfCovariate%>%
      as_tibble()%>%
      as.data.frame()%>%
      select(!starts_with('se.'))%>%
      filter(NON_ADH!=-1)%>%
      pivot_longer(c(3:(2+levelCovariate)), names_to = 'Covariate')%>%
      pivot_wider(names_from = c(STATE,NON_ADH), values_from = 'value')%>%
      mutate(Level = str_extract(Covariate, pattern = paste0('(?<=(', covariate, ')).*')))
    
    dfCovariate$Covariate = covariate
    
    dfCovariate
  })
  
  #merging and rearranging the columns
  dfSumm = bind_rows(lsSumm)
  dfSumm = dfSumm[, c(1,ncol(dfSumm), order(colnames(dfSumm)[2:(ncol(dfSumm)-1)]) + 1)]
  
  #adding median proportion across the states per level per covariate per adherent state
  dfMed = dfSumm%>%
    pivot_longer(cols = (3:ncol(dfSumm)), names_to = 'State')%>%
    mutate(ADH = str_extract(State, '(?<=_).*'),
           State = str_extract(State, '(.*)(?=_)'))%>%
    group_by(Covariate, Level, ADH)%>%
    summarise(Median = median(value, na.rm = TRUE))%>%
    ungroup()%>%
    pivot_wider(names_from = c(ADH), values_from = c(Median), names_prefix = 'Median_')
  
  #merging the state results with the medians obtained above
  dfSumm = merge(dfSumm, dfMed, by = c('Covariate', 'Level'))
  
  dfSumm
  
}

################################################################################
#Function to aggregate state values as medians per region

#dfSumm: Dataframe containing adherence summary per state
#lsRegions: List of regions and list of the states within
#lsOrdRegions: Regions arranged in desired order
fnGetRegion <- function(dfSumm, lsRegions, lsOrdRegions){
  
  #For univariate summaries
  if (length(unique(dfSumm$Covariate)) == dim(dfSumm)[1]) {
    dfSumm = dfSumm%>%
      select(!starts_with('Median_'))%>%
      pivot_longer(c(2:(ncol(dfSumm)-2)), names_to = 'State', values_to = 'Values')%>%
      mutate(Adh = str_extract(State, '(?<=_).*'),
             State = str_extract(State, '(.*)(?=_)'),
             Region = vecMatchStates[match(State, unlist(lsRegions))],
             Region = factor(Region, levels = lsOrdRegions[[1]]))%>%
      group_by(Region, Covariate, Adh)%>%
      summarise(Median = median(Values, na.rm = TRUE))%>%
      ungroup()%>%
      pivot_wider(names_from = c(Region, Adh), values_from = Median)
  }
  else{
    #For multivariate summaries
    dfSumm = dfSumm%>%
      select(!starts_with('Median_'))%>%
      pivot_longer(c(3:(ncol(dfSumm)-2)), names_to = 'State', values_to = 'Values')%>%
      mutate(Adh = str_extract(State, '(?<=_).*'),
             State = str_extract(State, '(.*)(?=_)'),
             Region = vecMatchStates[match(State, unlist(lsRegions))],
             Region = factor(Region, levels = lsOrdRegions[[1]]))%>%
      group_by(Region, Covariate, Adh, Level)%>%
      summarise(Median = median(Values, na.rm = TRUE))%>%
      ungroup()%>%
      pivot_wider(names_from = c(Region, Adh), values_from = Median)
  }
  dfSumm
}

################################################################################
#Summaries where "NON_ADH" is in loop

fnGenBinSumm <- function(vecADH, vecVariables, svyPRAMS){
  lsTest <- apply(crossing(vecADH, vecVariables),1, \(vecVars){
    #print(vecVars)
    variable1 = vecVars[[1]]
    variable2 = vecVars[[2]]
    
    #HISP is missing/uncounted for the state of VT
    #RUR is missing for the states of PR, AK
    #Thus, for these variables we subset the dataset for convergence
    
    #calculates proportion of adherents per state per variable2
    if (variable2 %in% c('HISP', 'RUR', 'PRE_DIET_EXER','EXER')){
      dfvariable2 = svyby(reformulate(variable2), reformulate("STATE", variable1), subset(svyPRAMS, !is.na(get(variable2))), svyciprop, vartype = 'ci', na.rm = TRUE)%>%
        as_tibble()
    } else if (variable2 %in% c('HYP_TNS', 'NICU')){
      dfvariable2 = svyby(reformulate(variable2), reformulate("STATE", variable1), subset(svyPRAMS, NEST_YR>=2020), 
                          svyciprop, vartype = 'ci', na.rm = TRUE)
    } else{
      dfvariable2 = svyby(reformulate(variable2), reformulate("STATE", variable1), (svyPRAMS), svyciprop, vartype = 'ci', na.rm = TRUE)%>%
        as_tibble()
    }
    
    #pivots the table to a wide format to show values per state per 
    #adherent state(1 or 0)
    dfvariable2 = dfvariable2%>%
      as.data.frame()%>%
      select(!starts_with('ci'))%>%
      #filter(NON_ADH!=-1)%>%
      pivot_wider(names_from = c("STATE", variable1), values_from = variable2)%>%
      mutate(Covariate = variable2,
             ADH_Grp = variable1)
  })
  
  #merging all the results from variable2s and rearranging the columns
  dfTest <- bind_rows(lsTest)
  if(which(colnames(dfTest)=="ADH_Grp") == ncol(dfTest)){
    dfTest <- dfTest[,c(which(colnames(dfTest)%in%c("Covariate","ADH_Grp")),
                        1:(which(colnames(dfTest)%in%c("Covariate","ADH_Grp"))[1]-1))]
  } else {
    dfTest <- dfTest[,c(which(colnames(dfTest)%in%c("Covariate","ADH_Grp")),
                        1:(which(colnames(dfTest)%in%c("Covariate","ADH_Grp"))[1]-1),
                        (which(colnames(dfTest)%in%c("Covariate","ADH_Grp"))[2]+1):ncol(dfTest))]
  }
  
  dfTest <- dfTest[, c(1:2, order(colnames(dfTest)[3:ncol(dfTest)]) + 2)]
  
  #calulating median value across states per variable2 per adherent state
  dfTest%>%
    pivot_longer(cols = (3:ncol(dfTest)), names_to = 'State')%>%
    mutate(ADH = str_extract(State, '(?<=_).*'),
           State = str_extract(State, '(.*)(?=_)'))%>%
    summarise(Median = median(value, na.rm = TRUE),
              IQR = paste(format(round(quantile(value, probs = c(0.25, 0.75), na.rm = TRUE)*100, 1), nsmall = 1, trim = TRUE), collapse = ' - '),
              .by=c(ADH_Grp, Covariate, ADH))%>%
    pivot_wider(names_from = c(ADH_Grp,ADH), values_from = c(Median, IQR),
                names_vary = "slowest")%>%
    mutate(across(where(is.numeric), ~format(round(.x*100,1), nsmall=1,trim=T)))
  
  #merging the median value to the previous dataset
  #dfTest = merge(dfTest, dfMed, by = c('variable2'))
}

#Multivariate Summary
fnGenMultSumm <- function(vecADH, vecVariables, svyobject){
  lsSumm = apply(crossing(vecADH, vecVariables),1, \(vecVars){
    variable1 = vecVars[[1]]
    variable2 = vecVars[[2]]
    levelCovariate = length(levels(svyobject$variables[,variable2]))
    
    #calculates proportion of adherents per state per variable2
    
    #Maybe remove this is loop?
    if (variable2 %in% c('HYP_TNS', 'NICU')){
      dfCovariate = svyby(reformulate(variable2), reformulate("STATE", variable1), subset(svyobject, NEST_YR>=2020), 
                          svymean, na.rm = TRUE)
    }
    else{
      dfCovariate = svyby(reformulate(variable2), reformulate("STATE", variable1), svyobject, svymean,  na.rm = TRUE)
    }
    
    #pivoting results to a wider format
    dfCovariate = dfCovariate%>%
      as_tibble()%>%
      as.data.frame()%>%
      select(!starts_with('se.'))%>%
      #filter(NON_ADH!=-1)%>%
      pivot_longer(c(3:(2+levelCovariate)), names_to = 'Covariate')%>%
      pivot_wider(names_from = c(STATE,variable1), values_from = 'value')%>%
      mutate(Level = str_extract(Covariate, pattern = paste0('(?<=(', variable2, ')).*')),
             Covariate = variable2,
             ADH_grp = variable1)
    
    dfCovariate
  })
  
  #merging and rearranging the columns
  dfSumm = bind_rows(lsSumm)
  dfSumm = dfSumm[,c(1, which(colnames(dfSumm)%in%c("Level","ADH_grp")),
                     2:(which(colnames(dfSumm)%in%c("Level","ADH_grp"))[1]-1),
                     (which(colnames(dfSumm)%in%c("Level","ADH_grp"))[2]+1):ncol(dfSumm))]
  
  #adding median proportion across the states per level per covariate per adherent state
  dfSumm%>%
    pivot_longer(cols = (4:ncol(dfSumm)), names_to = 'State')%>%
    mutate(ADH = str_extract(State, '(?<=_).*'),
           State = str_extract(State, '(.*)(?=_)'))%>%
    summarise(Median = median(value, na.rm = TRUE),
              IQR = paste(format(round(quantile(value, probs = c(0.25, 0.75), na.rm = TRUE)*100, 1), nsmall = 1, trim = TRUE), collapse = ' - '),
              .by=c(ADH_grp,Covariate, Level, ADH))%>%
    pivot_wider(names_from = c(ADH_grp,ADH), values_from = c(Median, IQR),
                names_vary = "slowest")%>%
    mutate(across(where(is.numeric), ~format(round(.x*100,1), nsmall=1,trim=T)))
  
  #merging the state results with the medians obtained above
  #dfSumm = merge(dfSumm, dfMed, by = c('Covariate', 'Level'))
  
  #dfSumm
  
}

################################################################################
#Displaying outcomes
fnDisplayModelTab <- function(lsTab, varAdh = "ADH"){
  bind_rows(lapply(names(lsTab), \(dff){
    lsTab[[dff]]%>%
      mutate(across(3:6, ~as.numeric(.x)))%>%
      summarise(Median = format(round(exp(median(Estimate)), 2), nsmall=2),
                IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 2), nsmall = 2), collapse = ' - '),
                .by = !!as.symbol(varAdh))%>%
      mutate(Outcome = dff)%>%
      pivot_wider(names_from = varAdh, values_from = c('Median','IQR'), names_vary = "slowest")
  }))%>%
    mutate(Outcome = factor(Outcome, levels = vecLvlOutcomes, labels = vecLabelOutcomes))
}
#Modeling Functions

fnUnadjModel <- function(lsOutcomes, vecADH, svyPRAMS){
  lapply(lsOutcomes, \(variable){
    lsLvl2= lapply(vecADH, \(variable2){
      form = as.formula(paste0(c(variable, reformulate(variable2)), collapse=" "))
      if (variable2 %in% c("PRE_DIET_EXER", "EXER") | variable2 == "NON_ADHGRP1"){
        lsRelStates = as.list(vecMaternalStates)
      } else { lsRelStates = lsStates}
      lsLvl3 = Map(\(x){
        if ((x!="IN")|(!variable%in%c("NICU", "HYP_TNS"))){
          m1 = svyglm(form, design = subset(svyPRAMS, STATE %in% x), family = binomial(link = 'log'))
          return(cbind('State' = x, 'ADH' = variable2, summary(m1)$coeff[,c(1,4)], confint(m1))[-1,])
        } else {return()}
        
      }, lsRelStates)
      bind_rows(lsLvl3)
    })
    bind_rows(lsLvl2)
    
  })
}

fnAdjModel <- function(lsOutcomes, vecAssocFn, vecADH, svyPRAMS, lsErr){
  lapply(lsOutcomes, \(variable){
    lsLvl2= lapply(vecADH, \(variable2){
      #print(variable2)
      form = as.formula(paste0(c(variable, reformulate(c(variable2, vecAssocFn))), collapse=" "))
      if ((variable2 == "NON_ADHGRP1") | (variable2 %in% vecGrp1)){
        lsRelStates = as.list(vecMaternalStates)
      } else { lsRelStates = lsStates}
      lsLvl3 = Map(\(x){
        #print(x)
        tryCatch({if ((x!="IN")|(!variable%in%c("NICU", "HYP_TNS"))){
          m1 = svyglm(form, design = subset(svyPRAMS, STATE %in% x), family = binomial(link = 'log'))
          return(cbind('State' = x, 'ADH' = variable2, summary(m1)$coeff[,c(1,4)], confint(m1))[2,])
        } else {return()}},
        error = \(e){
          lsErr[[variable]][[variable2]] <<- append(lsErr[[variable]][[variable2]], x)
          return()
        })
        
      }, lsRelStates)
      bind_rows(lsLvl3)
      
    })
    bind_rows(lsLvl2)
    
  })
}

fnLabeller <- function(vecLabel){
  lapply(vecLabel, \(Label)
         {if (Label%in%names(lsLabelAdh)){
           return(str_wrap((lsLabelAdh[[as.character(Label)]]), width=15))
         } else {return(as.character(Label))}
  }
  )|>unlist()
}


#States for which the models converged
fnGenStates = function(lsOut1, lsOut2=NULL){
  lapply(names(append(lsOut1, lsOut2)), \(x){
    if (is.null(lsOut2)){
      ls2 = lsOut1[[x]]
    } else {
      ls2 = append(lsOut1, lsOut2)[[x]]
    }
    
    ls2%>%
      mutate(Variable = x)%>%
      summarise(States = paste0(State, collapse = ", "), .by=c(Variable, ADH))
  })|>
    bind_rows()%>%
    mutate(Variable = factor(Variable, levels = vecLvlOutcomes, labels = vecLabelOutcomes))
}
