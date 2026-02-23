
#################################################################################
#delineating the variables
vecWeightVariables = c('SUD_NEST', 'TOTCNT', 'WTANAL', 'STATE', 'ID', 'STRATUMC', 'NEST_YR')


vecPrimaryVariables = c('PRE_DIET_EXER', 'VITAMIN_BIN', 'PNC_4MTH', 'FLU_SHOT', 'EXER',
                        'NO_SMK_3B', 'NO_ECIG_3B', 'NO_DRK_3B', 'DENTAL', 'LESS_DRK_3B')

vecNonAdh = c('PRE_DIET_EXER', 'VITAMIN_BIN', 'PNC_4MTH', 'FLU_SHOT', 'EXER', 
              'NO_SMK_3B', 'NO_ECIG_3B', 'NO_DRK_3B', 'DENTAL')
vecGrp1 = c('PRE_DIET_EXER', 'EXER')
vecGrp2 = c('VITAMIN_BIN')
#vecGrp3 = c('DENTAL', 'FLU_SHOT', 'PNC_4MTH')
vecGrp3 = c('DENTAL', 'FLU_SHOT')
vecGrp4 = c( 'NO_SMK_3B', 'NO_DRK_3B')


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
                  'MACROSOMIA', 'AB_NICU', 'SGA_10', 'LGA', 'PG_GDB8', 'MORB_BP8', 'DRK_2YRS')

vecCovs = c('MOM_BMIG_BC', 'DIAB', 'HBP', 'DPRS', 'INS_GRP', 'PREG_TRY',
            'PVTY', 'HISP', 'MARRY', 'EDU', 'RACE', 'RUR', 'OTH_PREG', 'AGE_GRP')
vecBinCov = c('DIAB', 'HBP', 'DPRS',  'HISP', 'MARRY','RUR', 'OTH_PREG','PVTY')
vecMultCov <- vecCovs[!vecCovs%in%vecBinCov]

vecPre = c('PRE_HLTH', 'PRE_KIDS', 'PRE_PRBC', 'PRE_SMK', 'PRE_VIT', 'PRE_WT')

vecAssocCovs5 = c('AGE_GRP', 'MARRY', 'EDU', 'PVTY', 'INS_GRP', 'PREG_TRY', 'RUR',
                  'HLTH_CR_VIST','PRE_CONDN')
vecMomOutcomes = c('KESSNER', 'HYP_TNS', 'MOM_DIAB', 'CSEC')
vecInfOutcomes = c('PRE_BIRTH',  'DEFECT', 'NICU', 'SGA', 'LGA')
vecOutcome = c('HLTH_CR_VIST')
vecHealth = c('TYP_DOCT', 'TYP_OBGN', 'TYP_OTHERS', 'TYP_DDS', 'TYP_BC')

variables = c(vecWeightVariables, vecPrimaryVariables, vecDependents, 
              vecCovs, vecPre, vecMomOutcomes, vecInfOutcomes, vecOutcome, vecHealth)

lsInfOutcomes = as.list(vecInfOutcomes)
names(lsInfOutcomes) = vecInfOutcomes

#Mom Outcomes
lsMomOutcomes = as.list(vecMomOutcomes)
names(lsMomOutcomes) = vecMomOutcomes

vecADH = c("NON_ADHGRP1", "NON_ADHGRP2", "NON_ADHGRP3", "NON_ADHGRP4")
vecAssocCovs5 = c('AGE_GRP', 'EDU', 'PREG_TRY', 'PRE_CONDN', 'INS_GRP')
lsAssocCovs = as.list(c(vecGrp1, vecGrp2, vecGrp3, vecGrp4))
names(lsAssocCovs) = c(vecGrp1, vecGrp2, vecGrp3, vecGrp4)
vecAssocCovsSens = c("AGE_GRP", "EDU", "PREG_TRY", "PRE_COND2", "INS_GRP")


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

vecVarLevels = c("AGE_GRP", "RACE","HISP","RUR", "MARRY",
           "EDU", "PVTY","INS_GRP", "MOM_BMIG_BC", 
           "HBP","DIAB", "DPRS","OTH_PREG","PREG_TRY")
vecVarLabels =c( 'Age (Years)',  'Race','Hispanic','Rurality','Married',
           'Education','At or Below Poverty Level','Insurance', 'BMI (kg/m2)',
           'High Blood Pressure','Diabetes (I/II)', 'Depression',   
           'Not Primigravida', 'Intended Pregnancy')

vecHlthLevels = c("HLTH_CR_VIST2", 'TYP_OBGN', 'TYP_DOCT', 
                  'TYP_BC', 'TYP_OTHERS',
                  "PRE_KIDS",
                  "PRE_PRBC",
                  "PRE_HLTH",
                  "PRE_SMK",
                  "PRE_VIT",
                  "PRE_WT" )
vecHealthLabels = c("Had a Health Care Visit", 'OBGYN',
                    'PCP','Family Planning',
                    'Other', 'Desire to have kids',
                    'Birth Control',
                    'Improve Health',
                    'Ask for Smoking',
                    'Vitamin',
                    'Maintaining a Healthy Weight')


vecLvlAdh = c("PRE_DIET_EXER", "EXER", "VITAMIN_BIN",
              "DENTAL", "FLU_SHOT", "NO_SMK_3B",
              "NO_DRK_3B")
vecLabelAdh = c("Among BMI >=25, dieting or exercising (12m prior)",
                "Exercise (3+ days, 12m prior)", "FA/multivitamin (any,1m prior)",
                "Dental (12m prior or during pregnancy)",
                "Flu shot (12m prior or during pregnancy)",
                "No cigarette use (3m prior)", "No drinking (3m prior)")

lsLabelAdh = as.list(vecLabelAdh)
names(lsLabelAdh) = vecLvlAdh


vecLvlOutcomes = c("PRE_BIRTH", "NICU", "SGA", "LGA","DEFECT", "KESSNER", "HYP_TNS", "MOM_DIAB", "CSEC")
vecLabelOutcomes = rev(c('C-Section','Gestational Diabetes', 
                         'Hypertensive Disorders of Pregnancy','Inadequate Prenatal Care',
                         'Congenital Anomaly','LGA Birth','SGA Birth',
                         'NICU Admission', 'Preterm Birth'))

vecLabelOutcomes2 = rev(c('C-Section','Gestational Diabetes',
                          'Hypertension',
                          'Inadequate PNC','Congenital Anomaly',
                          'LGA Birth','SGA Birth','NICU Admission', 'Preterm Birth'))

veclvlOutTab = rev(c('DEFECT','LGA','SGA','NICU','PRE_BIRTH',
               'CSEC','MOM_DIAB','HYP_TNS','KESSNER'))
veclblOutTab = rev(c('Congenital Anomaly','LGA Birth','SGA Birth',
               'NICU Admission', 'Preterm Birth', 
               'C-Section','Gestational Diabetes', 
               'Hypertensive Disorders of Pregnancy','Inadequate Prenatal Care'))
vecLabelAdhGrp=c("Diet/Physical Activity", 
                 "FA/Multivitamin", 
                 "Health Care", 
                 "Substance Use")
