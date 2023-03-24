
#list of DPs per algorithm
DAPS_alg1<-c("THL","RHE","FERR","UOSL","ARS")
DAPS_alg2<-c("THL","ARS")
DAPS_alg3<-c()
DAPS_alg4<-c("EFEMERIS","POMME","RHE","FERR","UOSL","SAIL","ARS")

smart_load("D4_study_population", diroutput,extension=extension)

OUTCOME_variables<-c("ADHD")

if (thisdatasource %in% DAPS_alg4) {
  for (outcome in OUTCOME_variables) {
  
  load(paste0(dirconceptsets,outcome,"_DRUGS.RData"))
    
  baseline_info1<-D4_study_population[, .(person_id,sex_at_instance_creation)]
  baseline_info2<-D4_study_population[, .(person_id,sex_at_instance_creation)][,sex_at_instance_creation:="Total"]
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
  
  assign("baseline_info",get("baseline_info")[is.na(Number_diagnostic_codes_detected),Number_diagnostic_codes_detected:=0])
  
  if (nrow(get(paste0(outcome,"_DRUGS")))>0){
  assign(paste0(outcome,"_drugs_counts"), MergeFilterAndCollapse(
    listdatasetL= list(get(paste0(outcome,"_DRUGS"))),
    condition = "date>=study_start & date<=study_end",
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id"),
    summarystat = list(list(c("exist"),"date","number_of_medications"))
  ))
  }else{
    stop("No children with at least one drug dispensing/prescription of drugs indicating ADHD were identified because the conceptset ADHD_DRUGS in empty")
  }
  

  baseline_info<-merge(baseline_info,get(paste0(outcome,"_drugs_counts")),all.x=T,by="person_id")
  
  assign("baseline_info",get("baseline_info")[is.na(number_of_medications),number_of_medications:=0])
  
  
  #drugs information

      if (nrow(get(paste0(outcome,"_DRUGS")))>0){
        assign(paste0(outcome,"_first_drug"), MergeFilterAndCollapse(
          listdatasetL= list(get(paste0(outcome,"_DRUGS"))),
          condition = "date>=study_start & date<=study_end",
          key = c("person_id"),
          datasetS = D4_study_population[,.(person_id)],
          # saveintermediatedataset = T,
          # nameintermediatedataset = paste0(dirtemp,'tempfile'),
          sorting = c("person_id","date"),
          strata = c("person_id"),
          summarystat = list(list(c("first"),"date","first_medication"))
        ))
        
      }
      baseline_info<-merge(baseline_info,get(paste0(outcome,"_first_drug")),all.x=T,by="person_id")

      
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
      baseline_info<-merge(baseline_info,get(paste0(outcome,"_first")),all.x=T,by="person_id")
      
      
  
      baseline_info<-baseline_info[,time_btw_diagnosis_medication:=abs(interval(first_diagnosis,first_medication) %/% months(1))]
  
  # time_between_diagnosis<-merge(time_between_diagnosis,get(paste0(outcome,"_third")),all=T,by="person_id")[!is.na(third_diagnosis),time_btw_diagnosis:=(interval(first_diagnosis,third_diagnosis) %/% months(1)),by=c("person_id")]
  # 
  # # assign(paste0(outcome,"_diagnosis"),rbind(get(paste0(outcome,"_first")),get(paste0(outcome,"_second")),get(paste0(outcome,"_third")),fill=T))
  # 
  # assign("baseline_info",get("baseline_info")[is.na(Number_diagnostic_codes_detected),Number_diagnostic_codes_detected:=0])
  # 
  # baseline_info<-merge(baseline_info,time_between_diagnosis,all.x=T,by="person_id")
  # 
  # baseline_info[,age_at_first_diagnosis:=age_fast(birth_date,first_diagnosis)][,time_from_study_entry_to_first_diagnosis:=(interval(first_diagnosis,study_exit_date) %/% months(1))][,first_diagnosis:=NULL][,second_diagnosis:=NULL][,third_diagnosis:=NULL]
  
  fwrite(get("baseline_info"),
         paste0(dirtemp, "number_of_diagnosis_base_",outcome,".csv"))
  
  baseline_info[,Median_Time_btw_diagnosis_medication:=median(time_btw_diagnosis_medication,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,IQR_Time_btw_diagnosis_medication:=IQR(time_btw_diagnosis_medication,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,Mean_Time_btw_diagnosis_medication:=mean(time_btw_diagnosis_medication,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")][,SD_Time_btw_diagnosis_medication:=sd(time_btw_diagnosis_medication,na.rm=T),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation")]
  
  
  
  assign(paste0("D5_number_of_medication_bydiagnosis_",outcome),get("baseline_info")[,.N,by=c("Number_diagnostic_codes_detected","sex_at_instance_creation","Median_Time_btw_diagnosis_medication", "IQR_Time_btw_diagnosis_medication", "Mean_Time_btw_diagnosis_medication" , "SD_Time_btw_diagnosis_medication" )])
  
  assign("temp",baseline_info[,sum(number_of_medications),by=c("Number_diagnostic_codes_detected","sex_at_instance_creation" )])
  setnames(temp,"V1","N_children_with_medication")
  
  assign(paste0("D5_number_of_medication_bydiagnosis_",outcome),merge(get(paste0("D5_number_of_medication_bydiagnosis_",outcome)),temp,by=c("Number_diagnostic_codes_detected","sex_at_instance_creation" )))
  
  preferred.order<-c("0","1","2","3+")
  preferred.order_sex<-c("M","F","Total")
  get(paste0("D5_number_of_medication_bydiagnosis_",outcome))[, Number_diagnostic_codes_detected := factor(Number_diagnostic_codes_detected, levels=preferred.order)][, sex_at_instance_creation := factor(sex_at_instance_creation, levels=preferred.order_sex)]
  setorderv(get(paste0("D5_number_of_medication_bydiagnosis_",outcome)), c("Number_diagnostic_codes_detected","sex_at_instance_creation"))
  
  
  assign(paste0("D5_number_of_medication_bydiagnosis_",outcome), get(paste0("D5_number_of_medication_bydiagnosis_",outcome))[,datasource:=thisdatasource])
  
  fwrite(get(paste0("D5_number_of_medication_bydiagnosis_",outcome)),
         paste0(direxp, "D5_number_of_medication_bydiagnosis_",outcome,".csv"))
  
  }
}

  
  

  
  
  

