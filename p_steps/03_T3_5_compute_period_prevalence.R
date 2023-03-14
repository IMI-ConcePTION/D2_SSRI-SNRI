

OUTCOME_variables<-c("ADHD","ASD","ID")

for (outcome in OUTCOME_variables) {

    assign(outcome,get(load(paste0(dirconceptsets, outcome, ".RData"))[[1]])[, .(person_id, date, meaning_renamed)][, concept := outcome])
  
}

all_concepts<-rbind(ADHD,ASD,ID,fill=T)

  assign("meaning_occurences", MergeFilterAndCollapse(list(all_concepts),
                                             condition = "!is.na(person_id)",
                                             strata = c( "concept","meaning_renamed"),
                                             summarystat = list(c("count", "person_id", "count"))))

fwrite(meaning_occurences,file=paste0(direxp,"D5_meaning_occurences.csv"))


smart_load("D4_study_population", diroutput,extension=extension)
D4_study_population<-D4_study_population[year(birth_date)>=1996 & year(birth_date)<=1999,Birth_cohort:="1996-1999"][year(birth_date)>=2000 & year(birth_date)<=2004,Birth_cohort:="2000-2004"][year(birth_date)>=2005 & year(birth_date)<=2009,Birth_cohort:="2005-2009"][year(birth_date)>=2010 & year(birth_date)<=2014,Birth_cohort:="2010-2014"][year(birth_date)>=2015 & year(birth_date)<=2019,Birth_cohort:="2015-2019"]


           

for (outcome in OUTCOME_variables) {
  algo_df<-smart_load(paste("D3_algorithms", outcome, sep = "_"), dirtemp,  extension = extension)
  
  print(paste0("Computing period prevalence for ",outcome))
  
  assign(paste0("period_prevalence_",outcome),CountPrevalence(D4_study_population, algo_df, c("person_id"),
                                       Start_date = "study_entry_date",
                                       End_date = "study_exit_date", Birth_date = "birth_date",
                                       Name_condition = "algorithm", Date_condition = "date",
                                       Type_prevalence = "period", Periods_of_time = list(
                                         list(str_replace_all(as.character(study_start), "-",""), str_replace_all(as.character(study_end), "-",""))),
                                       Start_study_time = study_start, End_study_time = study_end,
                                       Conditions = unique(algo_df[, algorithm]),
                                       Strata=c("Birth_cohort"),
                                       Aggregate = T))
  
  assign(paste0("period_prevalence_",outcome),get(paste0("period_prevalence_",outcome))[,datasource:=thisdatasource])

  
  fwrite(get(paste0("period_prevalence_", outcome)),file=paste0(direxp,"D4_prevalence_", outcome,".csv"))
  
  age_bands<-list(c(0,4),c(0,6),c(0,12),c(0,17))
  assign(paste0("D4_prevalence_", outcome,"_byage"),data.table())
  for (i in 1:4) {
    ageband_definition<-age_bands[[i]]
    name_dataset<-paste0("period_prevalence_",outcome,"_",paste(as.character(gsub("([0-9]+).*$", "\\1", ageband_definition)), collapse="_"))
    
    print(paste0("Computing prevalence for ",outcome," for age band ",paste(as.character(gsub("([0-9]+).*$", "\\1", ageband_definition)), collapse="_")))
    
    assign(name_dataset,CountPrevalence(D4_study_population, algo_df, c("person_id"),
                                                                Start_date = "study_entry_date",
                                                                End_date = "study_exit_date", Birth_date = "birth_date",
                                                                Name_condition = "algorithm", Date_condition = "date",
                                                                Type_prevalence = "period", Periods_of_time = list(
                                                                  list(str_replace_all(as.character(study_start), "-",""), str_replace_all(as.character(study_end), "-",""))),
                                                                Start_study_time = study_start, End_study_time = study_end,
                                                                Conditions = unique(algo_df[, algorithm]),
                                                                include_remaning_ages = F,
                                                                Age_bands = ageband_definition, Aggregate = T))
    
  assign(paste0("D4_prevalence_", outcome,"_byage"),rbind(get(paste0("D4_prevalence_", outcome,"_byage")),get(name_dataset),fill=T))
  }
  
  assign(paste0("D4_prevalence_", outcome,"_byage"),get(paste0("D4_prevalence_", outcome,"_byage"))[,datasource:=thisdatasource])
         
  fwrite(get(paste0("D4_prevalence_", outcome,"_byage")),file=paste0(direxp,"D4_prevalence_", outcome,"_byage.csv"))
}
  
