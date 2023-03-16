#-----------------------------------
# Parameters specific for each study
#-----------------------------------

#----------------------------
# admissible gap between observation periods (DAP-specific)
admissible_gap_obs_periods <- vector(mode = "list")
admissible_gap_obs_periods[['ARS']] <- 365
admissible_gap_obs_periods[['TEST']] <- 365
#admissible_gap_obs_periods[['BIPS']] <- 30

gap_days <- ifelse(thisdatasource %not in% names(admissible_gap_obs_periods),
               1, admissible_gap_obs_periods[[thisdatasource]])


# define number of days a spells should not be shorter
min_spell_lenght <- 28

# define from when the data is valid
start_data_availability <- vector(mode = "list")
start_data_availability[['ARS']] <- ymd(20030101)  #should be 2010 but for testing is 2003
start_data_availability[['TEST']] <- ymd(20030101)
start_data_availability[['SAIL']] <- ymd(20000101)
start_data_availability[['THL']] <- ymd(19960101)
start_data_availability[['EFEMERIS']] <- ymd(20040101)
start_data_availability[['POMME']] <- ymd(20100101) #ask anna-belle the 2 different acronym in the 2 extractions
start_data_availability[['ER']] <- ymd(20100101)
start_data_availability[['UOSL']] <- ymd(20080101)


study_start <- start_data_availability[[thisdatasource]]
rm(start_data_availability)

#end of data availability

study_end<- ymd(20191231)

if (thisdatasource=="SAIL") study_end <- ymd(20201231)

