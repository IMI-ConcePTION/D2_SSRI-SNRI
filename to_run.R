#-------------------------------
# sample script for IMI ConcePTION Demonstration Projects

# authors: Rosa Gini, Claudia Bartolini, Olga Paoletti, Davide Messina, Giorgio Limoncella
# based on previous scripts 

# v 2.0 - 25 September 2022
# Improve of the scriptbased on CVM script 

# v 1.0 - 27 June 2022
# Initial release

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#-------------------------------
# @DAP: please modify the following parameter
# dirinput: set it to the directory where your CDM instance is stored
dirinput <- "/home/rosgin/FEPI/ConcePTION/CDMInstances/DataCharacterisationUpdate210928/"

# dirpregnancyinput: set it to the directory where your CDM instance is stored
dirpregnancyinput <- "/home/rosgin/FEPI/ConcePTION/StudyScripts/pregnancy_20221205_3.1/g_output/"

#----------------
#LOAD PARAMTETERS
#----------------


source(paste0(thisdir,"/p_parameters/01_parameters_program.R")) #GENERAL
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R")) #CDM
source(paste0(thisdir,"/p_parameters/03_concept_sets.R")) #CONCEPTSETS
# source(paste0(thisdir,"/p_parameters/04_itemsets.R")) #ITEMSETS
# source(paste0(thisdir,"/p_parameters/05_variable_lists.R")) #OUTCOMES AND COVARIATES
source(paste0(thisdir,"/p_parameters/06_algorithms.R")) #ALGORITHMS
source(paste0(thisdir,"/p_parameters/07_study_design.R")) #STUDY DESIGN
source(paste0(thisdir,"/p_parameters/99_saving_all_parameters.R")) #SAVING AND CLEANING PARAMETERS


#----------------
# RUN STEPS
#----------------

# 01 RETRIEVE RECORDS FRM CDM

# CREATE EXCLUSION CRITERIA and CHECK CORRECT DATE OF BIRTH
launch_step("p_steps/01_T2_1_create_persons.R")

#COMPUTE SPELLS OF TIME FROM OBSERVATION_PERIODS
launch_step("p_steps/01_T2_2_apply_CreateSpells.R")

# APPLY THE FUNCTION CreateConceptSetDatasets TO CREATE ONE DATASET PER CONCEPT SET CONTAINING ONLY RECORDS WITH CODES OF INTEREST
launch_step("p_steps/01_T2_3_CreateConceptSetDatasets.R")

# CLEAN THE SPELLS
launch_step("p_steps/01_T2_4_clean_spells.R")

# CREATE EXCLUSION CRITERIA for persons/spells
launch_step("p_steps/01_T2_5_selection_criteria_from_PERSON_to_study_population.R")

launch_step("p_steps/02_T3_1_create_study_population.R")

#definition of baseline information and algorithms and variables 

launch_step("p_steps/03_T2_1_diagnostic_setting.R")
launch_step("p_steps/03_T2_2_number_of_diagnosis.R")
launch_step("p_steps/03_T2_3_number_of_medication_bydiagnosis.R")


launch_step("p_steps/03_T2_4_algorithms.R")

launch_step("03_T3_5_compute_period_prevalence.R")
