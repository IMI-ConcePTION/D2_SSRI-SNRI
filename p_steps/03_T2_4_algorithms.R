
#list of DPs per algorithm
DAPS_alg1<-c("THL","RHE","FERR","UOSL","ARS")
DAPS_alg2<-c("THL","ARS")
DAPS_alg3<-c()
DAPS_alg4<-c("EFEMERIS","POMME","RHE","FERR","UOSL","SAIL","ARS")

OUTCOME_variables<-c("ADHD","ASD","ID")

smart_load("D4_study_population", diroutput)
D4_study_population <- unique(D4_study_population[, .(person_id, study_entry_date,
                                                      study_exit_date)])

#ADHD
#Algorithm 1: One specialist code for ADHD identified in the data source
# Index date: Date of specialist diagnosis (may occur during hospital admission or specialist outpatient clinic visit, for example); any diagnosis with an explicit qualifier of “ruled out,” “suspected,” or “family history” should not be considered. The data partner may consider diagnoses within the hospital as a specialist diagnosis if applicable for their data source.


#fill which meanings in your DAP to use to specify SPECIALIST diagnosis---------
#for example:
# -outpatient_hospital_visit
# -hospital_diagnosis
# -hospitalisation


if (thisdatasource %in% DAPS_alg1) {

  for (outcome in OUTCOME_variables) {
  load(paste0(dirconceptsets,outcome,".RData"))
  
  #  condition = "date>=study_start & date<=study_end & meaning_renamed=='outpatient_hospital_visit' | meaning_renamed=='hospital_diagnosis' | meaning_renamed=='hospitalisation'",
  
  assign(paste0(outcome,"_ALG1"),MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    condition = paste0("(date>=study_start & date<=study_end ) & (",condmeaning[["INPATIENT"]]," | ",condmeaning[["OUTPATIENT_NO_PC"]] ," | " ,condmeaning[["LONGTERM"]]," )"),
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id"),
    summarystat = list(list(c("first"),"date","date"))
  ))
  
  get(paste0(outcome,"_ALG1"))[,algorithm:=paste0(outcome,"_1")][,ALGO_1:=1]
  }
} 


#---------------------------------------
#Algorithm 2:  Two non-specialist codes for ADHD that are at least 28 days apart, but within 1.5 years
# Index date: Date of second non-specialist diagnosis. Where available and applicable, any diagnosis with an explicit qualifier of “ruled out,” “suspected,” or “family history” should not be considered

#fill which meanings in your DAP to use to specify NOT-SPECIALIST diagnosis---------
#for example:
#-primary_care

if (thisdatasource %in% DAPS_alg2) {
  for (outcome in OUTCOME_variables) {
    load(paste0(dirconceptsets,outcome,".RData"))

  assign(paste0(outcome,"_ALG2"),MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    paste0("(date>=study_start & date<=study_end ) & (",condmeaning[["PC"]]," )"),
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id"),
    summarystat = list(list(c("first"),"date","first_diagnosis"),list(c("second"),"date","second_diagnosis"))
  ))
  
  
  get(paste0(outcome,"_ALG2"))[,algorithm:=paste0(outcome,"_2")][,ALGO_2:=1]
  
  
  get(paste0(outcome,"_ALG2"))[,time_btw_1_2_diagnosis:=difftime(second_diagnosis,first_diagnosis,units = "days")]
  assign(paste0(outcome,"_ALG2"),get(paste0(outcome,"_ALG2"))[time_btw_1_2_diagnosis>=28 & time_btw_1_2_diagnosis<=548,][,first_diagnosis:=NULL])
  
  setnames( get(paste0(outcome,"_ALG2")),"second_diagnosis","date")
  }
}



# #---------------------------------------
# #Algorithm 3: One non-specialist code and 1 at least dispensing/prescription for ADHD medication within a year of diagnosis
# # Index date: The later of either the non-specialist code or medication dispensing; any diagnosis with an explicit qualifier of “ruled out,” “suspected,” or “family history” should not be considered.
# if (thisdatasource %in% DAPS_alg3) {
#   
# ADHD_ALG3 <- MergeFilterAndCollapse(
#   listdatasetL= list(ADHD),
#   condition = "date>=study_start & date<=study_end & meaning_renamed=='primary_care'",
#   key = c("person_id"),
#   datasetS = D4_study_population[,.(person_id)],
#   # saveintermediatedataset = T,
#   # nameintermediatedataset = paste0(dirtemp,'tempfile'),
#   sorting = c("person_id","date"),
#   strata = c("person_id"),
#   summarystat = list(list(c("first"),"date","ALGO_3"))
# )
# 
# load(paste0(dirconceptsets,"ADHD_DRUGS.RData"))
# ADHD_ALG3<-merge(ADHD_ALG3, unique(ADHD_DRUGS[,.(person_id,date)]), by="person_id", all.x=T)
# ADHD_ALG3[diagnosis_date<=date & date<=diagnosis_date+365,]
# }

#---------------------------------------
#Algorithm 4: One specialist dispensing/prescription for ADHD medication. 
#Index date: Date of the first prescription / medication dispensing. 

if (thisdatasource %in% DAPS_alg4) {
  load(paste0(dirconceptsets,"ADHD_DRUGS.RData"))
  
  if (nrow(ADHD_DRUGS)>0){
    ADHD_ALG4 <- MergeFilterAndCollapse(
      listdatasetL= list(ADHD_DRUGS),
      condition = "date>=study_start & date<=study_end ",
      key = c("person_id"),
      datasetS = D4_study_population[,.(person_id)],
      # saveintermediatedataset = T,
      # nameintermediatedataset = paste0(dirtemp,'tempfile'),
      sorting = c("person_id","date"),
      strata = c("person_id"),
      summarystat = list(list(c("first"),"date","date"))
    )
    
  }
  
  print(paste0(nrow(ADHD_ALG4)," children with at least one drug dispensing/prescription of one of the drugs in the ADHD codelist were identified"))
  
  ADHD_ALG4[,algorithm:="ADHD_4"][,ALGO_4:=1]
}

OUTCOME_variables<-c("ADHD","ASD","ID")




for (outcome in OUTCOME_variables) {
  Pattern1<-grep(paste0("^",outcome,"_A"),names(.GlobalEnv),value=TRUE)
  Pattern1_list<-do.call("list",mget(Pattern1))   
  
  assign(paste("D3_algorithms", outcome, sep = "_"),rbindlist(Pattern1_list,fill=T)[,.(person_id,date,algorithm)])
  
  smart_save(get(paste("D3_algorithms", outcome, sep = "_")), dirtemp,override_name = paste("D3_algorithms", outcome, sep = "_"),  extension = extension)

  #merge all data frames together
  assign(paste0("algorithms_",outcome),Reduce(function(x, y) merge(x, y, all=TRUE,by="person_id"), Pattern1_list))
  assign(paste0("algorithms_",outcome),get(paste0("algorithms_",outcome))[, grep("^ALGO_|person", names( get(paste0("algorithms_",outcome)))), with = FALSE])

  assign(paste0("Algorithms_",outcome),setDT(get(paste0("algorithms_",outcome)))[,list(Count=.N) ,c(grep(paste0("^ALGO"),names(get(paste0("algorithms_",outcome))),value=TRUE))])

}

Algorithms_ADHD[is.na(Algorithms_ADHD)] <- 0
Algorithms_ASD[is.na(Algorithms_ASD)] <- 0
Algorithms_ID[is.na(Algorithms_ID)] <- 0

fwrite(Algorithms_ADHD,
       paste0(direxp, "D5_algorithms_combinations_ADHD.csv"))

fwrite(Algorithms_ASD,
       paste0(direxp, "D5_algorithms_combinations_ASD.csv"))

fwrite(Algorithms_ID,
       paste0(direxp, "D5_algorithms_combinations_ID.csv"))
