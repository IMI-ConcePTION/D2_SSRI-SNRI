
#list of DPs per algorithm
DAPS_alg1<-c("THL","RHE","FERR","UOSL","ARS")
DAPS_alg2<-c("THL","ARS")
DAPS_alg3<-c()
DAPS_alg4<-c("EFEMERIS","POMME","RHE","FERR","UOSL","SAIL","ARS")

smart_load("D4_study_population", diroutput,extension=extension)

OUTCOME_variables<-c("ADHD","ASD","ID")

for (outcome in OUTCOME_variables) {
  
  baseline_info1<-D4_study_population[, .(person_id,sex_at_instance_creation,birth_date,study_exit_date)]
  baseline_info2<-D4_study_population[, .(person_id,sex_at_instance_creation,birth_date,study_exit_date)][,sex_at_instance_creation:="all"]
  baseline_info<-rbind(baseline_info1,baseline_info2)
  
  load(paste0(dirconceptsets,outcome,".RData"))
  
  specialist_meanings<-c(meanings_of_this_study[["INPATIENT"]],meanings_of_this_study[["OUTPATIENT_NO_PC"]],meanings_of_this_study[["LONGTERM"]])
  
  assign(outcome,unique(get(outcome)[, .(person_id, date, meaning_renamed)]))
  get(outcome)[meaning_renamed %in% specialist_meanings,type:="specialist"][meaning_renamed %in%  meanings_of_this_study[["PC"]],type:="non-specialist"]
  assign(outcome,unique(get(outcome)[!is.na(type),]))
  
  
  assign(paste0(outcome,"_counts"), MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    condition = "date>=study_start & date<=study_end",
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id"),
    summarystat = list(list(c("count"),"date","number_of_diagnosis"))
  ))
  
  get(paste0(outcome,"_counts"))[,Number_diagnostic_codes_detected:=as.character(number_of_diagnosis)][number_of_diagnosis>2,Number_diagnostic_codes_detected:="3+"][,number_of_diagnosis:=NULL]
  baseline_info<-merge(baseline_info,get(paste0(outcome,"_counts")),all.x=T,by="person_id")
  
  
  assign(paste0(outcome,"_first"),MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    condition = "date>=study_start & date<=study_end",
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id"),
    summarystat = list(list(c("first"),"date","first_diagnosis"))
  ))
  
    assign(paste0(outcome,"_first"),get(paste0(outcome,"_first"))[!is.na(first_diagnosis),])
  
  
  assign(paste0(outcome,"_second"),MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    condition = "date>=study_start & date<=study_end",
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id"),
    summarystat = list(list(c("second"),"date","second_diagnosis"))
  ))
  

  assign(paste0(outcome,"_second"),get(paste0(outcome,"_second"))[!is.na(second_diagnosis),])
  
  
  assign(paste0(outcome,"_third"), MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    condition = "date>=study_start & date<=study_end",
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id"),
    summarystat = list(list(c("third"),"date","third_diagnosis"))
  ))
  
  assign(paste0(outcome,"_third"),get(paste0(outcome,"_third"))[!is.na(third_diagnosis),])
  
  time_between_diagnosis<-merge(get(paste0(outcome,"_first")),get(paste0(outcome,"_second")),all=T,by="person_id")[,time_btw_diagnosis:=abs(interval(first_diagnosis,second_diagnosis) %/% months(1))]
  
  time_between_diagnosis<-merge(time_between_diagnosis,get(paste0(outcome,"_third")),all=T,by="person_id")[!is.na(third_diagnosis),time_btw_diagnosis:=abs(interval(first_diagnosis,third_diagnosis) %/% months(1)),by=c("person_id")]

  # assign(paste0(outcome,"_diagnosis"),rbind(get(paste0(outcome,"_first")),get(paste0(outcome,"_second")),get(paste0(outcome,"_third")),fill=T))
  
   assign("baseline_info",get("baseline_info")[is.na(Number_diagnostic_codes_detected),Number_diagnostic_codes_detected:=0])
  
  baseline_info<-merge(baseline_info,time_between_diagnosis,all.x=T,by="person_id")
  
  baseline_info[,age_at_first_diagnosis:=age_fast(birth_date,first_diagnosis)][,time_from_study_entry_to_first_diagnosis:=abs(interval(first_diagnosis,study_exit_date) %/% months(1))][,first_diagnosis:=NULL][,second_diagnosis:=NULL][,third_diagnosis:=NULL]

  fwrite(get("baseline_info"),
         paste0(dirtemp, "number_of_diagnosis_base_",outcome,".csv"))
  
  baseline_info[,Median_Time_in_study:=median(time_from_study_entry_to_first_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,IQR_Median_Time_in_study:=IQR(time_from_study_entry_to_first_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,Mean_Time_in_study:=mean(time_from_study_entry_to_first_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,SD_Mean_Time_in_study:=sd(time_from_study_entry_to_first_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")]
  
  baseline_info[,Median_age_at_first_diagnosis:=median(age_at_first_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,IQR_Median_age_at_first_diagnosis:=IQR(age_at_first_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,Mean_age_at_first_diagnosis:=mean(age_at_first_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,SD_Mean_age_at_first_diagnosis:=sd(age_at_first_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")]
  
  baseline_info[,Median_time_btw_codes:=median(time_btw_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,IQR_time_btw_codes:=IQR(time_btw_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,Mean_time_btw_codes:=mean(time_btw_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,SD_time_btw_codes:=sd(time_btw_diagnosis,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")]
  

  assign(paste0("D5_number_of_diagnosis_",outcome),get("baseline_info")[,.N,by=c("Number_diagnostic_codes_detected","sex_at_instance_creation","Median_Time_in_study", "IQR_Median_Time_in_study", "Mean_Time_in_study" , "SD_Mean_Time_in_study" ,  "Median_age_at_first_diagnosis" ,"IQR_Median_age_at_first_diagnosis" , "Mean_age_at_first_diagnosis" , "SD_Mean_age_at_first_diagnosis" , "Median_time_btw_codes" , "IQR_time_btw_codes" ,"Mean_time_btw_codes" ,                     "SD_time_btw_codes"  )])
  
  assign(paste0("D5_number_of_diagnosis_",outcome), get(paste0("D5_number_of_diagnosis_",outcome))[,datasource:=thisdatasource])
  
  fwrite(get(paste0("D5_number_of_diagnosis_",outcome)),
         paste0(direxp, "D5_number_of_diagnosis_",outcome,".csv"))
  
}
