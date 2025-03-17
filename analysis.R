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

#################################################################################
#delineating the variables
vecWeightVariables = c('SUD_NEST', 'TOTCNT', 'WTANAL', 'STATE', 'ID', 'STRATUMC', 'NEST_YR')


vecPrimaryVariables = c('PRE_DIET_EXER', 'VITAMIN_BIN', 'PNC_4MTH', 'FLU_SHOT', 'EXER',
                        'NO_SMK_3B', 'NO_ECIG_3B', 'NO_DRK_3B', 'DENTAL', 'LESS_DRK_3B')

vecNonAdh = c('PRE_DIET_EXER', 'VITAMIN_BIN', 'PNC_4MTH', 'FLU_SHOT', 'EXER', 
              'NO_SMK_3B', 'NO_ECIG_3B', 'NO_DRK_3B', 'DENTAL')

vecDependents = c('MOM_BMIG_QX_REV', 'VITAMIN', 'PNC_MTH', 'FLUPREG', 'ECIG_3B_A',
                  'SMK63B_A', 'DRK83B_A', 'DRK8_3B', 'TYP_DDS', 'ILLB_MO', 'P_PRTERM',
                  'MOM_BMI', 'INSWORK8', 'INSPAR', 'INSHCEX','INSMED', 'INSNONE', 
                  'INSOTH', 'INSCHIP', 'INSGOV', 'INSGOV2', 'INSMIL', 'INSIHS', 
                  'HISP_BC', 'MARRIED', 'MAT_DEG', 'MAT_RACE_PU', 'NCHS_URB_RUR2', 
                  'OTH_TERM', 'PRE_DIET', 'PRE_EXER', 'BPG_DIAB8', 'BPG_HBP8',
                  'BPG_DEPRS8', 'PGINTENT', 'INCOME8', 'INC_NDEP', 'MAT_AGE_PU',
                  'MAT_AGE_NAPHSIS_VT', 'MAT_AGE_NAPHSIS_AK', 'MAT_RACE_PU_AK',
                  'TYP_DOCT', 'TYP_ILLN', 'TYP_INJR', 'TYP_MH', 'TYP_OBGN', 'TYP_OTHR',
                  'TYP_DDS', 'TYP_BC', 'NON_ADH', 'PRE_VIST', 'DDS_CLN', 'RF_GHYPE', 'RF_EHYPE',
                  'RF_GDIAB', 'MM_DIAB', 'DEL_1CS', 'DEL_RCS', 'GEST_WK_PU', 'GRAM_NAPHSIS',
                  'MACROSOMIA', 'AB_NICU', 'SGA_10', 'LGA', 'PG_GDB8', 'MORB_BP8')

vecCovs = c('MOM_BMIG_BC', 'DIAB', 'HBP', 'DPRS', 'INS_GRP', 'PREG_TRY',
            'PVTY', 'HISP', 'MARRY', 'EDU', 'RACE', 'RUR', 'OTH_PREG', 'AGE_GRP')
vecBinCov = c('DIAB', 'HBP', 'DPRS',  'HISP', 'MARRY','RUR', 'OTH_PREG','PVTY')
vecMultCov <- vecCovs[!vecCovs%in%vecBinCov]

vecPre = c('PRE_HLTH', 'PRE_KIDS', 'PRE_PRBC', 'PRE_SMK', 'PRE_VIT', 'PRE_WT')

vecAssocCovs = c('AGE_GRP', 'MARRY', 'EDU', 'PVTY', 'INS_GRP', 'PREG_TRY', 'RUR',
                  'HLTH_CR_VIST','PRE_CONDN')
vecMomOutcomes = c('KESSNER', 'HYP_TNS', 'MOM_DIAB', 'CSEC')
vecInfOutcomes = c('PRE_BIRTH',  'DEFECT', 'NICU', 'SGA', 'LGA')
vecOutcome = c('HLTH_CR_VIST')
vecHealth = c('TYP_DOCT', 'TYP_OBGN', 'TYP_OTHERS', 'TYP_DDS', 'TYP_BC')

variables = c(vecWeightVariables, vecPrimaryVariables, vecDependents, 
              vecCovs, vecPre, vecMomOutcomes, vecInfOutcomes)

#slicing the columns
################################################################################
#Slicing the columns

#State participation by year
dfSurvey%>%
   group_by(STATE)%>%
   summarise(yr = across(NEST_YR, ~toString(unique(.x))),
             ycnt = n_distinct(NEST_YR))%>%
   arrange(ycnt)


dfMerged = dfMerged[!dfMerged$STATE %in% c('IN', 'MP', 'YC'), 
                    colnames(dfMerged)%in%variables]


################################################################################
#Cleaning Variables
dfMerged = fnCleanVariables(dfMerged)
#creating the survey object

#Design survey object
prams.svy <- svydesign(ids = ~0, strata = ~SUD_NEST, 
                       fpc = ~TOTCNT, weights = ~WTANAL, data = dfMerged)


################################################################################
#Calculates Summary of Adherence Variables


#Including NAs, N, Prop, CI
#Backbone of some important tables
lsSummary <- lapply(vecPrimaryVariables, 
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

dfSummaries <- genSummary(vecPrimaryVariables, prams.svy)
#Summary suitable for plotting in Tableau
dfSummaryTableau = genTableauSummary(lsSummary)

################################################################################
#Graphics for the adherence


#Baseline heatmap
dfAdherence = dfSummaryTableau%>%
  select(State, p, variable)%>%
  mutate(Region = vecMatchStates[match(dfSummaryTableau$State, unlist(lsRegions))])

lsOrdStates = dfAdherence%>%
  group_by(State)%>%
  summarise(p = mean(p, na.rm = TRUE))%>%
  arrange(p)%>%
  select(State)

dfAdherence$State = factor(dfAdherence$State, levels = lsOrdStates[[1]])

#Ranking the regions
lsOrdRegions = dfAdherence%>%
  group_by(State)%>%
  mutate(p = mean(p, na.rm = TRUE))%>%
  ungroup()%>%
  distinct(State, .keep_all = TRUE)%>%
  group_by(Region)%>%
  summarise(p = mean(p))%>%
  arrange(desc(p))%>%
  select(Region)

#Setting variable names
dfAdherence$variable = factor(dfAdherence$variable, levels = list(
   'VITAMIN_BIN',
  'EXER',
  'PRE_DIET_EXER',
  'NO_DRK_3B',
  'LESS_DRK_3B',
  'NO_SMK_3B',
  'NO_ECIG_3B',
  'PNC_4MTH',
  'FLU_SHOT',
  'DENTAL'
),
labels = str_wrap(c(
  'FA/multivitamin (any,1m prior)',
  'Exercise (3+ days, 12m prior)',
  'Among BMI ≥25, dieting or exercising (12m prior)',
  'No drinking (3m prior)',
  'No heavy drinking (3m prior)',
  'No cigarette use (3m prior)',
  'No e-cigarette use (3m prior)',
  'Early prenatal care (within 4m)',
  'Flu shot (12m prior or during pregnancy)',
  'Dental (12m prior or during pregnancy)*'
), width = 17))

#Plotting the heatmap
ggplot(dfAdherence, aes(y = State, x  = variable, fill = p))+
  facet_grid(factor(Region, levels = lsOrdRegions[[1]])~., scales = 'free', space = 'free', switch = 'y')+
  geom_tile(color = 'white')+
  geom_text(aes(label = ifelse(is.na(p), "", format(p, nsmall = 2))), color = 'black', size = 4)+
  theme_bw()+
  scale_fill_gradientn(colors = hcl.colors(20, 'RdBu')[4:17])+
  theme(strip.background = element_rect(fill = '#EEEEEE', color = '#FFFFFF'),
        strip.placement = 'outside',
        legend.position = 'none',
        axis.title = element_blank(), 
        axis.text = element_text(size = 14),
        strip.text = element_text(size=15))

dfAdhOvr = dfAdherence%>%
          group_by(variable)%>%
          summarise(p = median(p, na.rm = TRUE))%>%
          mutate(State = 'Overall\nMedian',
                 Region = 'SMTH')

dfHMPAdherence = dfAdherence%>%
  group_by(Region, variable)%>%
  summarise(p = median(p, na.rm = TRUE))%>%
  mutate(State = 'Median')%>%
  rbind(dfAdherence)%>%
  mutate(State = factor(State, levels = c('Median', lsOrdStates[[1]])),
         label = ifelse(State == 'Median', p, ''),
         colorval = ifelse(State == 'Median', as.character(1), NA))
p1 = dfHMPAdherence%>%
  ggplot(aes(y = State, x  = variable, fill = p))+
  facet_grid(factor(Region, levels = lsOrdRegions[[1]])~., scales = 'free', space = 'free', switch = 'y')+
  geom_tile(aes(color=colorval), height = 0.96, size = 1.1)+
  geom_text(aes(label = ifelse(is.na(label), "", format(label, nsmall = 2))), color = 'black', size = 4)+
  theme_bw()+
  scale_color_manual(values = c('black'), na.value = 'white', guide = 'none')+
  scale_fill_gradientn(colors = hcl.colors(20, 'RdBu')[4:17], name = 'Proportion')+
  theme(strip.background = element_rect(fill = '#EEEEEE', color = '#FFFFFF'),
        strip.placement = 'outside',

        axis.title = element_blank(), 
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size=15),
        axis.text.x = element_blank(),
        legend.title = element_text(size = 20, color = 'black'),
        legend.text = element_text(size = 20, color = 'black'),
        legend.position = 'right',
        legend.key.height = unit(4, 'cm'), legend.key.width = unit(1,'cm'))

p2 = ggplot(dfAdhOvr, aes(y = State, x  = variable, fill = p))+
  geom_tile(aes(y=State, x = variable, fill = p), color = 'black', size = 1.1)+
  facet_grid(factor(Region, levels = lsOrdRegions[[1]])~., scales = 'free', space = 'free', switch = 'y')+
  geom_text(aes(label = ifelse(is.na(p), "", format(p, nsmall = 2))), 
            color = 'black', size = 4, nudge_y = 0.1)+
  theme_bw()+
  scale_fill_gradientn(colors = hcl.colors(20, 'RdBu')[4:17])+
  theme(strip.background = element_rect(fill = '#EEEEEE', color = '#FFFFFF'),
        strip.text = element_blank(),
        legend.position = 'none',
        strip.placement = 'outside',
        axis.title = element_blank(), 
        axis.text = element_text(size = 14))
p1+p2+plot_layout(nrow=2, heights = c(40,1))

#Radar Plot
dfRadar = dfAdherence%>%
  pivot_wider(names_from = variable, values_from = p)%>%
  group_by(Region)%>%
  select(-State)%>%
  summarise(across(everything(), ~median(.x, na.rm = TRUE)))%>%
  as.data.frame()

rownames(dfRadar) = dfRadar$Region
dfRadar = dfRadar[lsOrdRegions[[1]], c(2:ncol(dfRadar))]
dfRadar = rbind(rep(1, length(vecPrimaryVariables)), rep(0.1, length(vecPrimaryVariables)), dfRadar)
dfRadar = dfRadar[,c(2, 9, 4, 3, 7, 6, 10, 8, 1, 5)]

lsRadarLabels = str_wrap(c(
  'FA/multivitamin (any,1m prior)',
  'Dental (12m prior or during pregnancy)*',
  'Flu shot (12m prior or during pregnancy)',
  'Early prenatal care (within 4m)',
  'No e-cigarette use (3m prior)',
  'No cigarette use (3m prior)',
  'No heavy drinking (3m prior)',
  'No drinking (3m prior)',
  'Among BMI ≥25, dieting or exercising (12m prior)',
  'Exercise (3+ days, 12m prior)'
), width = 12)

vecCols = brewer.pal(length(lsRegions), 'Set2')

#Saves the Radarplot in the specified location
#dev.new(width = 2300, height = 2000, unit = 'px', noRStudioGD = TRUE)
#png(filename = "../output/figures/Rplot.png",
#    width = 2300, height = 2000, units = "px")
radarchart(dfRadar, axistype = 1,
           cglcol = "grey", cglty = 1, cglwd = 2,
           vlabels = lsRadarLabels, vlcex = 3.5,
           axislabcol = 'grey',
           caxislabels=c('10%', '', '', '', '100%'), calcex =4,
           pty = 16, pcol = vecCols, plty = 1, plwd = 2.5
)
#dev.off()

legend(x=1.5, y=1, legend = lsOrdRegions[[1]], bty = "o", pch=20 , 
       text.col = "black", col = vecCols, pt.cex=1.5,
       title = 'Regions', inset =0.1)

################################################################################
#Adherence Table
dfAdh = svyby(~ADH, ~STATE, subset(prams.svy, !is.na(ADH)), svyciprop, na.rm = TRUE)

dfAdh%>%
  as.data.frame()%>%
  mutate(Region = vecMatchStates[match(STATE, unlist(lsRegions))])%>%
  group_by(Region)%>%
  summarise(Median=round(median(ADH), 2),
            IQR = paste(format(round(quantile(ADH, probs = c(0.25, 0.75)), 2), nsmall = 2), collapse = '-'),
            'Min/Max' = paste(format(round(range(ADH), 2), nsmall = 2), collapse = '-'))%>%
  mutate(Region = factor(Region, levels = lsOrdRegions[[1]]))%>%
  arrange(Region)


dfAdh%>%
  as.data.frame()%>%
  summarise(Median=round(median(ADH), 2),
            IQR = paste(format(round(quantile(ADH, probs = c(0.25, 0.75)), 2), nsmall = 2), collapse = '-'),
            'Min/Max' = paste(format(round(range(ADH), 2), nsmall = 2), collapse = '-'))

#Generateing three table for each
dfMedian = dfAdherence%>%
  pivot_wider(names_from = variable, values_from = p)%>%
  group_by(Region)%>%
  select(-State)%>%
  summarise(across(everything(), ~format(round(median(.x, na.rm = TRUE), 2), nsmall=2)))%>%
  mutate('Statistic' = 'Median')

dfIQR = dfAdherence%>%
  pivot_wider(names_from = variable, values_from = p)%>%
  group_by(Region)%>%
  select(-State)%>%
  summarise(across(everything(), ~paste(format(round(quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE), 2), nsmall = 2), collapse = '-')))%>%
  mutate('Statistic' = 'IQR')

dfMinMAx = dfAdherence%>%
  pivot_wider(names_from = variable, values_from = p)%>%
  group_by(Region)%>%
  select(-State)%>%
  summarise(across(everything(), ~paste(format(range(.x, na.rm = TRUE), nsmall=2), collapse = "-")))%>%
  mutate('Statistic' = 'Min/Max')

dfTab1 = rbind(dfMedian, dfIQR, dfMinMAx)%>%
  mutate(Region = factor(Region, levels = lsOrdRegions[[1]]),
         Statistic = factor(Statistic, levels = c('Median', 'IQR', 'Min/Max')))%>%
  arrange(Region, Statistic)%>%
  t()

dfAdherence%>%
  group_by(variable)%>%
  summarise(Median=round(median(p, na.rm = TRUE), 2),
            IQR = paste(format(round(quantile(p, probs = c(0.25, 0.75), na.rm = TRUE), 2), nsmall = 2), collapse = '-'),
            'Min/Max' = paste(format(round(range(p, na.rm = TRUE), 2), nsmall = 2), collapse = '-'))


################################################################################
#Covariates for non-adherers
#Binary

dfCovariateSummaries = fnGenBinSummary(vecBinCov, prams.svy)

#MULTIVARIATE
vecStates = length(unique(dfMerged$STATE))
dfMultCovSumm = fnGenMultSummary(vecMultCov, prams.svy)

#Merging binary and multivariate

dfCovariateSummaries[,'Level'] = NA
dfCovariateSummaries = dfCovariateSummaries[,c(1, ncol(dfCovariateSummaries), c(2:(ncol(dfCovariateSummaries)-1)))]
dfFinalCovSumm = rbind(dfCovariateSummaries, dfMultCovSumm)
dfFinalCovSumm$Covariate = factor(dfFinalCovSumm$Covariate, 
       labels =c( 'Age group', 'Diabetic', 'Depressed', 'Education Level', 'High Blood Pressure', 
                  'Hispanic', 'Insurance', 'Married', 'BMI Group', 'Not Primigravida', 
                  'Tried for Pregnancy', 'Poverty', 'Race', 'Rurality'))

factor(vecBinCov, labels = c('Diabetes', 'High Blood Pressure', 'Depressed', 'Hispanic', 
       'Married', 'Rural', 'Previous Pregnancies', 'Below Poverty'))

#Table 2 generated
#incomplete

vecOrdLevels = c('<20', '20-24', '25-29', '30-34', '35+',
                 'LessHS', 'CompHS', 'SomeCollege', 'Bachelors', 'Graduate',
                 '1', '2', '3', '4',
                 'White', 'Black', 'AI/AN', 'AsHwPI', 'Other',
                 'Private', 'Medicaid', 'Others', 'None',
                 '12', '10', '11',
                 NA)
vecOrdLabels = c('<20', '20-24', '25-29', '30-34', '35+',
                 'Less than High School', 'Completed High School', 
                 'Some College', 'Bachelors', 'Graduate',
                 '1', '2', '3', '4',
                 'White', 'Black', 'AI/AN', 'AsHwPi', 'Other',
                 'Private', 'Medicaid', 'Others', 'None',
                 'Yes', 'No', 'Not sure')

tab2 = fnGetRegion(dfFinalCovSumm, lsRegions, lsOrdRegions)%>%
  mutate(Level = factor(Level, levels = vecOrdLevels, labels = vecOrdLabels))%>%
  arrange(Level)%>%
  group_by(Covariate)%>%
  mutate(sortMed = median(Northeast_0))%>%
  ungroup()%>%
  arrange(desc(sortMed), Level)%>%
  select(-sortMed)

dfFinalCovSumm%>%
  select(Covariate, Level, Median_0, Median_1)
################################################################################
#HEALTH OUTCOMES

dfOutcomeSummary <- fnGenBinSummary(vecOutcome, prams.svy)
fnGetRegion(dfOutcomeSummary, lsRegions, lsOrdRegions)

################################################################################
###HEALTHCARE VISIT TYPES

dfHealthCovs <- fnGenBinSummary(vecHealth, prams.svy)
dfHealthCovs%>%
  select(Covariate, Median_0, Median_1)%>%
  mutate(Covariate =  factor(Covariate, 
                             levels = c('TYP_OBGN', 'TYP_DOCT', 'TYP_DDS', 'TYP_BC', 'TYP_OTHERS'),
                             labels = c('OBGYN',
                                        'PCP',
                                        'Dental',
                                        'Family Planning',
                                        'Other')))%>%
  rename('Visit Types' = Covariate)


fnGetRegion(dfHealthCovs, lsRegions, lsOrdRegions)%>%
  mutate(Covariate =  factor(Covariate, 
                             levels = c('TYP_OBGN', 'TYP_DOCT', 'TYP_DDS', 'TYP_BC', 'TYP_OTHERS'),
                             labels = c('OBGYN',
                                        'PCP',
                                        'Dental',
                                        'Family Planning',
                                        'Other')))%>%
  rename('Visit Types' = Covariate)


##HEALTHCARE VISIT TYPES PLOTS
dfHealthCovs%>%
  #filter(Statistic == 'p')%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(c(2:(vecStates*2 + 1)), names_to = "Adh", values_to = 'Percentages')%>%
  mutate(Adh = str_extract(Adh, '(?<=_)(.*)'),
         Percentages = round(Percentages, 2))%>%
  #mutate(Percentages = round(Percentages/100, 2))%>%
  group_by(Covariate, Adh)%>%
  mutate(Med = median(Percentages, na.rm=TRUE),
         Med = ifelse(Med == Percentages, Med, NA))%>%
  ungroup()%>%
  mutate(Covariate = factor(Covariate, 
                            levels = c('TYP_OBGN', 'TYP_DOCT', 'TYP_DDS', 'TYP_BC', 'TYP_OTHERS'),
                            labels = c('OBGYN',
                                       'PCP',
                                       'Dental',
                                       'Family Planning',
                                       'Other')))%>%
  ggplot(aes(x = Covariate, y = Percentages, fill = Adh))+
  geom_boxplot(position = position_dodge(0.9))+
  geom_text(aes(x = Covariate, label = format(Med, nsmall = 2), group = Adh, y=Med), 
            position = position_dodge(width=0.9),
            vjust = -0.3, size = 6)+
  scale_fill_manual(values = c("#B3D0E4", '#EBB0A8'),
                    name = 'Adherence', labels = c('Adherent', 'Non-adherent'))+
  labs(y='Proprtion of Particiapnts', x = 'Types of Health Care Visits (12M Prior)')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(3, 'line'))




################################################################################
#Covariates for questions asked in the surveys

dfPreCovs <- fnGenBinSummary(vecPre, prams.svy)
fnGetRegion(dfPreCovs, lsRegions, lsOrdRegions)

#dfPreCovs = fnGenBinSummary(vecPre, prams.svy)
dfPreCovs$Covariate = factor(dfPreCovs$Covariate,
                             levels = c("PRE_KIDS",
                                        "PRE_PRBC",
                                        "PRE_HLTH",
                                        "PRE_SMK",
                                        "PRE_VIT",
                                        "PRE_WT" ),
                             labels = (
                               c('Desire to have kids',
                                'Birth Control',
                                'Improve Health',
                                'Ask for Smoking',
                                'Vitamin',
                                'Maintaining a Healthy Weight')))


dfPreCovs%>%
  select(Covariate, Median_0, Median_1)

dfPreCovs%>%
  #filter(Statistic == 'p')%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(c(2:(vecStates*2 + 1)), names_to = "Adh", values_to = 'Percentages')%>%
  mutate(Adh = str_extract(Adh, '(?<=_)(.*)'),
         Percentages = round(Percentages, 2))%>%
  #mutate(Percentages = round(Percentages/100, 2))%>%
  group_by(Covariate, Adh)%>%
  mutate(Med = median(Percentages),
         Med = ifelse(Med == Percentages, Med, NA))%>%
  ungroup()%>%
  ggplot(aes(x = factor(Covariate, labels = str_wrap(
    c('Desire to Have Kids',
      'Birth Control',
      'Improve Health before Pregnancy',
      'Smoking',
      'FA Vitamin',
      'Maintaining Healthy Weight'), width=20)), y = Percentages, fill = Adh))+
  geom_boxplot(position = position_dodge(0.9))+
  geom_text(aes( label = Med, group = Adh, y=Med), 
            position = position_dodge(width=0.9),
            vjust = -0.3, size = 6)+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = 'bold'),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.key.size = unit(3, 'line'))+
  scale_fill_manual(values = c("#B3D0E4", '#EBB0A8'),
                    name = 'Adherence', labels = c('Adherent', 'Non-adherent'))+
  labs(y='Proportion Discussed', x = 'Health Topics')


################################################################################
#Big Table

prams.svy2 = prams.svy
vecFacLevels = sapply(prams.svy2$variables[,sapply(prams.svy2$variables, class) %in% c('factor')], levels)
vecFacLevels = sapply(vecFacLevels, c, -1)
mapply(function(x){
  
  print(x)
  levels(prams.svy2$variables[,x]) <<- vecFacLevels[[x]]
   
}, names(vecFacLevels)[!names(vecFacLevels) %in% c('ADH.1')])
 
prams.svy2$variables[is.na(prams.svy2$variables)] = -1

lsStates = as.list(unique(dfMerged$STATE))
#lsStates = as.list(c('AZ'))

lsBigTable <- 
  lapply(lsStates,
         \(x){
           vecLevels <- svytable(~interaction(NON_ADH, PRE_DIET_EXER, VITAMIN_BIN, 
                                              PNC_4MTH, FLU_SHOT, EXER, NO_SMK_3B, 
                                              NO_ECIG_3B, NO_DRK_3B, DENTAL), 
                                 subset(prams.svy2, STATE == x), na.rm = FALSE)
           
      
           dfBigTable <- sapply(names(vecLevels), \(x){
             c(str_extract_all(x, "-?[0-9]")[[1]], vecLevels[[x]])
           })%>%
             t()%>%
             as.data.frame(row.names = FALSE)
           colnames(dfBigTable) <- c('NON_ADH', vecNonAdh, 'Vals')
           
           dfBigTable = dfBigTable%>%
             mutate(Vals = as.numeric(Vals))%>%
             group_by(NON_ADH)%>%
             mutate(Vals = Vals/sum(Vals))%>%
             ungroup()%>%
             filter(Vals!=0)%>%
             mutate(across(c(1:(ncol(dfBigTable)-1)), ~factor(.x, levels = c(0,1,-1), labels = c('0', '1','NA'))))%>%
             arrange(NON_ADH, desc(Vals))
           
           dfBigTable = dfBigTable[,c(1, 3, 6, 2, 9, 7, 8, 4, 5, 10, ncol(dfBigTable))]
           colnames(dfBigTable) = c(
             'Non Adherence',
             'FA/multivitamin (any,1m prior)',
             'Exercise (3+ days, 12m prior)',
             'Among BMI ≥25, dieting or exercising (12m prior)',
             'No drinking (3m prior)',
             'No cigarette use (3m prior)',
             'No e-cigarette use (3m prior)',
             'Early prenatal care (within 4m)',
             'Flu shot (12m prior or during pregnancy)',
             'Dental (12m prior or during pregnancy)*',
             'Value'
           )
           dfBigTable
         })
names(lsBigTable) <- unlist(lsStates)
wb = createWorkbook()
lapply(names(lsBigTable), \(x){
  addWorksheet(wb, x)
  writeData(wb, x, lsBigTable[[x]])
})
saveWorkbook(wb, file = '../output/data_files/Adherence By Variable.xlsx', overwrite=TRUE)


################################################################################
#Association modeling

dfAssoc = dfMerged

#Releveling variables
levels(dfAssoc$AGE_GRP) = list(
  "<25" = c("<20", "20-24"),
  "25-34" = c("25-29","30-34"),
  "35+" = "35+"
)

levels(dfAssoc$EDU) = list(
  "LessHS" = "LessHS",
  "CompHS" = "CompHS",
  "College" = c("SomeCollege", "Bachelors", "Graduate")
)

#New Survey Object
prams.svy3 <- svydesign(ids = ~0, strata = ~SUD_NEST, fpc = ~TOTCNT, weights = ~WTANAL, data = dfAssoc)


################################################################################
#actual models

#Unadjust Infant Model
lsInfOutcomes = as.list(vecInfOutcomes)
names(lsInfOutcomes) = vecInfOutcomes
l1 = lapply(lsInfOutcomes, \(variable){
  form = as.formula(paste0(variable, " ~ NON_ADH"))
  l1 = Map(\(x){
    m1 = svyglm(form, design = subset(prams.svy3, STATE %in% x), family = binomial(link = 'log'))
    return(cbind('State' = x, summary(m1)$coeff[,c(1,4)], confint(m1))[-1,])
  }, lsStates)
  bind_rows(l1)
})

lsCrudeInf = lapply(l1, \(dff){
  dff%>%
    mutate(across(2:5, ~as.numeric(.x)),
           Region = vecMatchStates[match(State, unlist(lsRegions))])%>%
    group_by(Region)%>%
    summarise(MedianEstimate = round(exp(median(Estimate)), 3),
              IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 3), nsmall = 3), collapse = ' - '))
})

dfCrudeInfOvr = bind_rows(lapply(names(l1), \(dff){
  l1[[dff]]%>%
    mutate(across(2:5, ~as.numeric(.x)))%>%
    summarise(MedianEstimate = round(exp(median(Estimate)), 3),
              IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 3), nsmall = 3), collapse = ' - '))%>%
    mutate(Outcome = dff, Region = 'Overall')
}))



dfCrudeInf = bind_rows(lapply(names(lsCrudeInf), \(x){
  lsCrudeInf[[x]]%>%
    mutate(Outcome = x)
}))%>%
  rbind(dfCrudeInfOvr)

#Unadjusted Mom Models
#Mom Outcomes
lsMomOutcomes = as.list(vecMomOutcomes)
names(lsMomOutcomes) = vecMomOutcomes

#Testing the two additional mom outcomes
#lsMomOutcomes = as.list(c(vecMomOutcomes,'MOM_DIAB1','MOM_DIAB2'))
#names(lsMomOutcomes) = c(vecMomOutcomes,'MOM_DIAB1','MOM_DIAB2')

l2 = lapply(lsMomOutcomes, \(variable){
  form = as.formula(paste0(variable, " ~ NON_ADH"))
  l1 = Map(\(x){
    m1 = svyglm(form, design = subset(prams.svy3, STATE %in% x), family = binomial(link = 'log'))
    return(cbind('State' = x, summary(m1)$coeff[,c(1,4)], confint(m1))[-1,])
  }, lsStates)
  bind_rows(l1)
})

lsCrudeMom = lapply(l2, \(dff){
  dff%>%
    mutate(across(2:5, ~as.numeric(.x)),
           Region = vecMatchStates[match(State, unlist(lsRegions))])%>%
    group_by(Region)%>%
    summarise(MedianEstimate = round(exp(median(Estimate)), 3),
              IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 3), nsmall = 3), collapse = ' - '))
})

dfCrudeMomOvr = bind_rows(lapply(names(l2), \(dff){
  l2[[dff]]%>%
    mutate(across(2:5, ~as.numeric(.x)))%>%
    summarise(MedianEstimate = round(exp(median(Estimate)), 3),
              IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 3), nsmall = 3), collapse = ' - '))%>%
    mutate(Outcome = dff, Region = 'Overall')
}))

dfCrudeMom = bind_rows(lapply(names(lsCrudeMom), \(x){
  lsCrudeMom[[x]]%>%
    mutate(Outcome = x)
}))%>%
  rbind(dfCrudeMomOvr)



################################################################################
#Adjusted variables

#Variables to adjust for
vecAssocCovs = c('AGE_GRP', 'EDU', 'PREG_TRY', 'PRE_CONDN', 'INS_GRP')

#Adjusted Infant Models
lsMiss = list()
lsTest = lapply(lsInfOutcomes, \(variable){
  form = as.formula(paste0(c(variable, reformulate(c('NON_ADH', vecAssocCovs))), collapse = " "))
  l1 = Map(\(x){
    tryCatch( {
      m1 = svyglm(form, design = subset(prams.svy3, STATE %in% x), family = binomial(link = 'log'))
      return(cbind('State' = x, summary(m1)$coeff[,c(1,4)], confint(m1))[2,])
      },
      error = \(e)
      { 
        lsMiss[[variable]] <<- append(lsMiss[[variable]], x)
        return()
      }
    )
    
  }, lsStates)
})
#lapply(lsTest, \(x) sum(sapply(x, is.null)))
lsAdjInf = lapply(lsTest, \(dff){
  bind_rows(dff)%>%
    mutate(across(2:5, ~as.numeric(.x)),
           Region = vecMatchStates[match(State, unlist(lsRegions))])%>%
    group_by(Region)%>%
    summarise(MedianEstimate = round(exp(median(Estimate)), 3),
              IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 3), nsmall = 3), collapse = ' - '))
})

dfAdjInfOvr = bind_rows(lapply(names(lsTest), \(dff){
  bind_rows(lsTest[[dff]])%>%
    mutate(across(2:5, ~as.numeric(.x)))%>%
    summarise(MedianEstimate = round(exp(median(Estimate)), 3),
              IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 3), nsmall = 3), collapse = ' - '))%>%
    mutate(Outcome = dff, Region = 'Overall')
}))

dfAdjInf = bind_rows(lapply(names(lsAdjInf), \(x){
  lsAdjInf[[x]]%>%
    mutate(Outcome = x)
}))%>%
  rbind(dfAdjInfOvr)

#Adjusted Mom Models
lsMiss2 = list()
lsTest2 = lapply(lsMomOutcomes, \(variable){
  form = as.formula(paste0(c(variable, reformulate(c('NON_ADH', vecAssocCovs))), collapse = " "))
  l1 = Map(\(x){
    tryCatch( {
      m1 = svyglm(form, design = subset(prams.svy3, STATE %in% x), family = binomial(link = 'log'))
      return(cbind('State' = x, summary(m1)$coeff[,c(1,4)], confint(m1))[2,])
    },
    error = \(e)
    { 
      lsMiss[[variable]] <<- append(lsMiss[[variable]], x)
      return()
    }
    )
    
  }, lsStates)
})
#lapply(lsTest2, \(x) sum(sapply(x, is.null)))
lsAdjMom = lapply(lsTest2, \(dff){
  bind_rows(dff)%>%
    mutate(across(2:5, ~as.numeric(.x)),
           Region = vecMatchStates[match(State, unlist(lsRegions))])%>%
    group_by(Region)%>%
    summarise(MedianEstimate = round(exp(median(Estimate)), 3),
              IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 3), nsmall = 3), collapse = ' - '))
})

dfAdjMomOvr = bind_rows(lapply(names(lsTest2), \(dff){
  bind_rows(lsTest2[[dff]])%>%
    mutate(across(2:5, ~as.numeric(.x)))%>%
    summarise(MedianEstimate = round(exp(median(Estimate)), 3),
              IQR = paste(format(round(exp(quantile(Estimate, probs = c(0.25, 0.75))), 3), nsmall = 3), collapse = ' - '))%>%
    mutate(Outcome = dff, Region = 'Overall')
}))

dfAdjMom = bind_rows(lapply(names(lsAdjMom), \(x){
  lsAdjMom[[x]]%>%
    mutate(Outcome = x)
}))%>%
  rbind(dfAdjMomOvr)

################################################################################
#Merge Model Outcomes
dfMom = merge(dfCrudeMom, dfAdjMom, by = c('Outcome', 'Region'))%>%
  mutate(Region = factor(Region, levels = c(lsOrdRegions[[1]], 'Overall')),
         Outcome = factor(Outcome, levels = c("KESSNER", "CSEC", "HYP_TNS", "MOM_DIAB"),
                          labels = c('Kessner Index', 'C-Section', 'Hyper Tension', 'Diabetes')))%>%
  arrange(Outcome, Region)



dfInf = merge(dfCrudeInf, dfAdjInf, by = c('Outcome', 'Region'))%>%
  mutate(Region = factor(Region, levels = c(lsOrdRegions[[1]], 'Overall')),
         Outcome = factor(Outcome, levels = c("SGA", "PRE_BIRTH", "DEFECT", "NICU", "LGA"),
                          labels = c('SGA', 'Preterm Birth', 'Birth Defect', 'NICU', 'LGA')))%>%
  arrange(Outcome, Region)


################################################################################
#VARIABLES BY ADHERENCE
dfMomAdh = fnGenBinSummary(vecMomOutcomes, prams.svy3)
dfMomAdhRegion  = fnGetRegion(dfMomAdh, lsRegions, lsOrdRegions)

###MOM OUTCOMES
dfMomAdh%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(-c(1), names_to = 'State', values_to = 'Value')%>%
  mutate(Adh = str_extract(State, '(?<=_).*'),
         State = str_extract(State, '.*(?=_)'),
         Region = vecMatchStates[match(State, unlist(lsRegions))],
         Region = factor(Region, levels = lsOrdRegions[[1]]))%>%
  pivot_wider(names_from = Adh, values_from = Value, names_prefix = "NONADH_")%>%
  group_by(Covariate, Region)%>%
  summarise(NONADH_0 = median(NONADH_0),
            NONADH_1 = median(NONADH_1))

dfMomAdh%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(-c(1), names_to = 'State', values_to = 'Value')%>%
  mutate(Adh = str_extract(State, '(?<=_).*'),
         State = str_extract(State, '.*(?=_)'))%>%
  pivot_wider(names_from = Adh, values_from = Value, names_prefix = "NONADH_")%>%
  group_by(Covariate)%>%
  summarise(NONADH_0 = median(NONADH_0),
            NONADH_1 = median(NONADH_1))

dfMomAdh%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(-c(1), names_to = 'State', values_to = 'Value')%>%
  mutate(Adh = str_extract(State, '(?<=_).*'),
         State = str_extract(State, '.*(?=_)'),
         Region = vecMatchStates[match(State, unlist(lsRegions))],
         Region = factor(Region, levels = lsOrdRegions[[1]]))%>%
  pivot_wider(names_from = Adh, values_from = Value, names_prefix = "NONADH_")%>%
  group_by(Covariate, Region)%>%
  summarise(NONADH_0 = paste0(c(round(quantile(NONADH_0, probs = c(0.25, 0.75), na.rm = TRUE),3)), collapse=" - "),
            NONADH_1 = paste0(c(round(quantile(NONADH_1, probs = c(0.25, 0.75), na.rm = TRUE),3)), collapse=" - "))

dfMomAdh%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(-c(1), names_to = 'State', values_to = 'Value')%>%
  mutate(Adh = str_extract(State, '(?<=_).*'),
         State = str_extract(State, '.*(?=_)'))%>%
  pivot_wider(names_from = Adh, values_from = Value, names_prefix = "NONADH_")%>%
  group_by(Covariate)%>%
  summarise(NONADH_0 = paste0(c(round(quantile(NONADH_0, probs = c(0.25, 0.75), na.rm = TRUE),3)), collapse=" - "),
            NONADH_1 = paste0(c(round(quantile(NONADH_1, probs = c(0.25, 0.75), na.rm = TRUE),3)), collapse=" - "))


###INFANT
dfInfAdh = fnGenBinSummary(vecInfOutcomes, prams.svy3)
dfInfAdhRegion  = fnGetRegion(dfInfAdh, lsRegions, lsOrdRegions)

dfInfAdh%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(-c(1), names_to = 'State', values_to = 'Value')%>%
  mutate(Adh = str_extract(State, '(?<=_).*'),
         State = str_extract(State, '.*(?=_)'),
         Region = vecMatchStates[match(State, unlist(lsRegions))],
         Region = factor(Region, levels = lsOrdRegions[[1]]))%>%
  pivot_wider(names_from = Adh, values_from = Value, names_prefix = "NONADH_")%>%
  group_by(Covariate, Region)%>%
  summarise(NONADH_0 = median(NONADH_0),
            NONADH_1 = median(NONADH_1))

dfInfAdh%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(-c(1), names_to = 'State', values_to = 'Value')%>%
  mutate(Adh = str_extract(State, '(?<=_).*'),
         State = str_extract(State, '.*(?=_)'))%>%
  pivot_wider(names_from = Adh, values_from = Value, names_prefix = "NONADH_")%>%
  group_by(Covariate)%>%
  summarise(NONADH_0 = median(NONADH_0),
            NONADH_1 = median(NONADH_1))

dfInfAdh%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(-c(1), names_to = 'State', values_to = 'Value')%>%
  mutate(Adh = str_extract(State, '(?<=_).*'),
         State = str_extract(State, '.*(?=_)'),
         Region = vecMatchStates[match(State, unlist(lsRegions))],
         Region = factor(Region, levels = lsOrdRegions[[1]]))%>%
  pivot_wider(names_from = Adh, values_from = Value, names_prefix = "NONADH_")%>%
  group_by(Covariate, Region)%>%
  summarise(NONADH_0 = paste0(c(round(quantile(NONADH_0, probs = c(0.25, 0.75), na.rm = TRUE),3)), collapse=" - "),
            NONADH_1 = paste0(c(round(quantile(NONADH_1, probs = c(0.25, 0.75), na.rm = TRUE),3)), collapse=" - "))

dfInfAdh%>%
  select(-c(Median_0, Median_1))%>%
  pivot_longer(-c(1), names_to = 'State', values_to = 'Value')%>%
  mutate(Adh = str_extract(State, '(?<=_).*'),
         State = str_extract(State, '.*(?=_)'))%>%
  pivot_wider(names_from = Adh, values_from = Value, names_prefix = "NONADH_")%>%
  group_by(Covariate)%>%
  summarise(NONADH_0 = paste0(c(round(quantile(NONADH_0, probs = c(0.25, 0.75), na.rm = TRUE),3)), collapse=" - "),
            NONADH_1 = paste0(c(round(quantile(NONADH_1, probs = c(0.25, 0.75), na.rm = TRUE),3)), collapse=" - "))

################################################################################
#IQR Plot for model strength


lapply(names(lsTest2), \(var){
  dff = lsTest2[[var]]
  bind_rows(dff)%>%
    mutate(across(2:5, ~as.numeric(.x)),
           Region = vecMatchStates[match(State, unlist(lsRegions))],
           Outcome = var,
           Type = 'Mom Outcome',
           Estimate = exp(Estimate))
}
)%>%
  bind_rows()%>%
  rbind(lapply(names(lsTest), \(var){
    dff = lsTest[[var]]
    bind_rows(dff)%>%
      mutate(across(2:5, ~as.numeric(.x)),
             Region = vecMatchStates[match(State, unlist(lsRegions))],
             Outcome = var,
             Type = 'Infant Outcome',
             Estimate = exp(Estimate))
  }
  )%>%
    bind_rows())%>%
  filter(Estimate >0.1)%>%
  mutate(Outcome = factor(Outcome, 
                          levels = c('DEFECT','LGA','SGA','NICU','PRE_BIRTH',
                                     'CSEC','MOM_DIAB','HYP_TNS','KESSNER'),
                          labels = str_wrap(c('Birth Defect','LGA Birth','SGA Birth',
                                              'NICU Admission', 'Preterm Birth', 
                                              'C-Section','Gestational Diabetes', 
                                              'Hypertensive Disorders of Pregnancy','Inadequate Prenatal Care'), 
                                            width = 20)))%>%
  ggplot(aes(y=Outcome, x=Estimate))+
  geom_vline(xintercept = 1)+
  geom_boxplot(outlier.size = 3)+
  scale_x_continuous(trans = 'log', breaks = c(0.37, 1.00, 2.72))+
  labs(x='Adjusted Risk Ratio')+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14,hjust = 0),
        axis.title.x = element_text(size=16, face='bold'))


################################################################################
#Total survey respondents by state

dfStateTotal = svytotal(~STATE, prams.svy)%>%
  as.data.frame()

dfStateTotal = dfStateTotal%>%
  mutate(State = rownames(dfStateTotal))%>%
  mutate(State = sub("STATE", "", State))%>%
  select(-SE)

rownames(dfStateTotal)<-(1:nrow(dfStateTotal))

dfStateTotal = rbind(dfStateTotal, 
                     dfStateTotal%>%
                       summarise(total = sum(total))%>%
                       mutate(State = 'Overall'))
