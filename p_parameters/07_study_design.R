#-----------------------------------
# Parameters specific for each study
#-----------------------------------

#study_years_datasource
end_date <- vector(mode = "list")
end_date[['ARS']] <- ymd(20191231)

study_end <- min(instance_creation, recommended_end_date,end_date[[thisdatasource]], na.rm = T)
rm(recommended_end_date)


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
recommended_start_date <- vector(mode = "list")
recommended_start_date[['ARS']] <- ymd(20030101)
recommended_start_date[['TEST']] <- ymd(20030101)
recommended_start_date[['SAIL']] <- ymd(20050101)
recommended_start_date[['THL']] <- ymd(19960101)
recommended_start_date[['EFEMERIS']] <- ymd(20040101)
recommended_start_date[['POMME']] <- ymd(20100101)
recommended_start_date[['ER']] <- ymd(20100101)
recommended_start_date[['UOSL']] <- ymd(20050101)



# recommended_start_date <- fifelse(thisdatasource %not in% names(recommended_start_date_vect),
#                                   ymd(19960101), recommended_start_date_vect[[thisdatasource]])
#rm(recommended_start_date_vect)


study_start <- recommended_start_date[[thisdatasource]]
