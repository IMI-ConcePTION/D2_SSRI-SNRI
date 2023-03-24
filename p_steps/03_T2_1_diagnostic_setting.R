
#list of DPs per algorithm
DAPS_alg1<-c("THL","RHE","FERR","UOSL","ARS")
DAPS_alg2<-c("THL","ARS")
DAPS_alg3<-c()
DAPS_alg4<-c("EFEMERIS","POMME","RHE","FERR","UOSL","SAIL","ARS")

smart_load("D4_study_population", diroutput,extension=extension)

OUTCOME_variables<-c("ADHD","ASD","ID")

for (outcome in OUTCOME_variables) {
  
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

  
  assign(paste0(outcome,"_first"),MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    condition = "date>=study_start & date<=study_end",
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id","type"),
    summarystat = list(list(c("first"),"date","first_diagnosis"))
  ))
  
  
  setnames(get(paste0(outcome,"_first")),"type","type_of_diagnostic_code")
  assign(paste0(outcome,"_first"),get(paste0(outcome,"_first"))[!is.na(first_diagnosis),][,Number_of_diagnosis:="First"][,first_diagnosis:=NULL])
  
  
  assign(paste0(outcome,"_second"),MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    condition = "date>=study_start & date<=study_end",
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id","type"),
    summarystat = list(list(c("second"),"date","second_diagnosis"))
  ))
  
  setnames(get(paste0(outcome,"_second")),"type","type_of_diagnostic_code")
  assign(paste0(outcome,"_second"),get(paste0(outcome,"_second"))[!is.na(second_diagnosis),][,Number_of_diagnosis:="Second"][,second_diagnosis:=NULL])
  
  
  assign(paste0(outcome,"_third"), MergeFilterAndCollapse(
    listdatasetL= list(get(outcome)),
    condition = "date>=study_start & date<=study_end",
    key = c("person_id"),
    datasetS = D4_study_population[,.(person_id)],
    # saveintermediatedataset = T,
    # nameintermediatedataset = paste0(dirtemp,'tempfile'),
    sorting = c("person_id","date"),
    strata = c("person_id","type"),
    summarystat = list(list(c("third"),"date","third_diagnosis"))
  ))
  
  setnames(get(paste0(outcome,"_third")),"type","type_of_diagnostic_code")
  assign(paste0(outcome,"_third"),get(paste0(outcome,"_third"))[!is.na(third_diagnosis),][,Number_of_diagnosis:="Third"][,third_diagnosis:=NULL])
  
  print(paste0(length(unique(get(paste0(outcome,"_counts"))$person_id))," children with at least one diagnosis of ",outcome," were identified"))
  
  assign(paste0(outcome,"_diagnosis"),rbind(get(paste0(outcome,"_first")),get(paste0(outcome,"_second")),get(paste0(outcome,"_third"))))

  assign("baseline_info",get("baseline_info")[!is.na(Number_diagnostic_codes_detected),])
  
  baseline_info<-merge(baseline_info,get(paste0(outcome,"_diagnosis")),all.x=T,by="person_id",allow.cartesian=T)



  fwrite(get("baseline_info"),
         paste0(dirtemp, "diagnostic_setting_base_",outcome,".csv"))
  

  assign(paste0("D5_diagnostic_setting_",outcome),get("baseline_info")[,.N,by=c("Number_diagnostic_codes_detected","sex_at_instance_creation","Number_of_diagnosis","type_of_diagnostic_code")])
  
  preferred.order<-c("0","1","2","3+")
  preferred.order_sex<-c("M","F","Total")
  preferred.order_num_diagn<-c("First","Second","Third")
  get(paste0("D5_diagnostic_setting_",outcome))[, Number_diagnostic_codes_detected := factor(Number_diagnostic_codes_detected, levels=preferred.order)][, sex_at_instance_creation := factor(sex_at_instance_creation, levels=preferred.order_sex)][, Number_of_diagnosis := factor(Number_of_diagnosis, levels=preferred.order_num_diagn)]
  setorderv(get(paste0("D5_diagnostic_setting_",outcome)), c("Number_diagnostic_codes_detected","sex_at_instance_creation","Number_of_diagnosis"))
  
  
  assign(paste0("D5_diagnostic_setting_",outcome), get(paste0("D5_diagnostic_setting_",outcome))[,datasource:=thisdatasource])
         
  fwrite(get(paste0("D5_diagnostic_setting_",outcome)),
         paste0(direxp, "D5_diagnostic_setting_",outcome,".csv"))
  
}
 