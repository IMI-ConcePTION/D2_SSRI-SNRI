
#list of DPs per algorithm
DAPS_alg1<-c("THL","RHE","FERR","UOSL","ARS")
DAPS_alg2<-c("THL","ARS")
DAPS_alg3<-c()
DAPS_alg4<-c("EFEMERIS","POMME","RHE","FERR","UOSL","SAIL","ARS")

smart_load("D4_study_population", diroutput)
D4_study_population <- unique(D4_study_population[, .(person_id, study_entry_date,
                                               study_exit_date)])

print(paste0("The study population includes ",nrow(D4_study_population), " subjects"))

baseline_info<-D4_study_population[,.(person_id)]

#ADHD
 load(paste0(dirconceptsets,"ADHD.RData"))
 
 specialist_meanings<-c(meanings_of_this_study[["HOSP"]],meanings_of_this_study[["SPECIALIST"]],meanings_of_this_study[["LONGTERM"]])
 
 ADHD<-unique(ADHD[, .(person_id, date, meaning_renamed)])
 ADHD[meaning_renamed %in% specialist_meanings,type:="specialist"]
 ADHD[meaning_renamed %in% meanings_of_this_study[["PC"]],type:="non-specialist"]


ADHD_counts <- MergeFilterAndCollapse(
  listdatasetL= list(ADHD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("count"),"date","number_of_diagnosis"))
)

ADHD_first <- MergeFilterAndCollapse(
  listdatasetL= list(ADHD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("first"),"date","first_diagnosis"))
)

setnames(ADHD_first,"type","type_of_first_diagnostic_code")


ADHD_second<- MergeFilterAndCollapse(
  listdatasetL= list(ADHD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("second"),"date","second_diagnosis"))
)

setnames(ADHD_second,"type","type_of_second_diagnostic_code")


ADHD_third<- MergeFilterAndCollapse(
  listdatasetL= list(ADHD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("third"),"date","third_diagnosis"))
)

setnames(ADHD_third,"type","type_of_third_diagnostic_code")

print(paste0(length(unique(ADHD_counts$person_id))," children with at least one diagnosis of ADHD were identified"))

baseline_info<-merge(baseline_info,ADHD_counts,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ADHD_first,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ADHD_second,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ADHD_third,all.x=T,by="person_id")

#drugs information
if (thisdatasource %in% DAPS_alg4) {
  load(paste0(dirconceptsets,"ADHD_DRUGS.RData"))
  
  if (nrow(ADHD_DRUGS)>0){
    ADHD_drugs_counts <- MergeFilterAndCollapse(
      listdatasetL= list(ADHD_DRUGS),
      condition = "date>=study_start & date<=study_end",
      key = c("person_id"),
      datasetS = D4_study_population[,.(person_id)],
      # saveintermediatedataset = T,
      # nameintermediatedataset = paste0(dirtemp,'tempfile'),
      sorting = c("person_id","date"),
      strata = c("person_id"),
      summarystat = list(list(c("first"),"date","first_medication"))
    )
    
    print(paste0(nrow(ADHD_drugs_counts)," children with at least one drug dispensing/prescription of drugs indicating ADHD were identified"))
  }else{
    print("No children with at least one drug dispensing/prescription of drugs indicating ADHD were identified because the conceptset ADHD_DRUGS in empty")
  }
  
}


baseline_info<-D4_study_population[,.(person_id)]

#ASD (no drugs in the codelist)
load(paste0(dirconceptsets,"ASD.RData"))

ASD<-unique(ASD[, .(person_id, date, meaning_renamed)])
ASD[meaning_renamed %in% specialist_meanings,type:="specialist"]
ASD[meaning_renamed %in% meanings_of_this_study[["PC"]],type:="non-specialist"]


ASD_counts <- MergeFilterAndCollapse(
  listdatasetL= list(ASD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("count"),"date","number_of_diagnosis"))
)

ASD_first <- MergeFilterAndCollapse(
  listdatasetL= list(ASD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("first"),"date","first_diagnosis"))
)

setnames(ASD_first,"type","type_of_first_diagnostic_code")


ASD_second<- MergeFilterAndCollapse(
  listdatasetL= list(ASD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("second"),"date","second_diagnosis"))
)

setnames(ASD_second,"type","type_of_second_diagnostic_code")


ASD_third<- MergeFilterAndCollapse(
  listdatasetL= list(ASD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("third"),"date","third_diagnosis"))
)

setnames(ASD_third,"type","type_of_third_diagnostic_code")

print(paste0(length(unique(ASD_counts$person_id))," children with at least one diagnosis of ASD were identified"))

baseline_info<-merge(baseline_info,ASD_counts,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ASD_first,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ASD_second,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ASD_third,all.x=T,by="person_id")


#ID (no drugs in the codelist)
baseline_info<-D4_study_population[,.(person_id)]


load(paste0(dirconceptsets,"ID.RData"))

ID<-unique(ID[, .(person_id, date, meaning_renamed)])
ID[meaning_renamed %in% specialist_meanings,type:="specialist"]
ID[meaning_renamed %in% meanings_of_this_study[["PC"]],type:="non-specialist"]


ID_counts <- MergeFilterAndCollapse(
  listdatasetL= list(ID),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("count"),"date","number_of_diagnosis"))
)

ID_first <- MergeFilterAndCollapse(
  listdatasetL= list(ID),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("first"),"date","first_diagnosis"))
)

setnames(ID_first,"type","type_of_first_diagnostic_code")


ID_second<- MergeFilterAndCollapse(
  listdatasetL= list(ID),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("second"),"date","second_diagnosis"))
)

setnames(ID_second,"type","type_of_second_diagnostic_code")


ID_third<- MergeFilterAndCollapse(
  listdatasetL= list(ID),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id","type"),
  summarystat = list(list(c("third"),"date","third_diagnosis"))
)

setnames(ID_third,"type","type_of_third_diagnostic_code")

print(paste0(length(unique(ID_counts$person_id))," children with at least one diagnosis of ID were identified"))

baseline_info<-merge(baseline_info,ID_counts,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ID_first,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ID_second,all.x=T,by="person_id")
baseline_info<-merge(baseline_info,ID_third,all.x=T,by="person_id")

baseline_info<-baseline_info[number_of_diagnosis>0,]
