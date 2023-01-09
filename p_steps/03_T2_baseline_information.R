
smart_load("D4_study_population", diroutput)
print(paste0("The study population includes ",nrow(D4_study_population), " subjects"))

#ADHD
 load(paste0(dirconceptsets,"ADHD.RData"))


ADHD_counts <- MergeFilterAndCollapse(
  listdatasetL= list(ADHD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("count"),"codvar","number_of_diagnosis"),list(c("first"),"date","first_diagnosis"),list(c("second"),"date","second_diagnosis"),list(c("third"),"date","third_diagnosis"))
)

print(paste0(nrow(ADHD_counts)," children with at least one diagnosis of ADHD were identified"))

ADHD_counts[,time_btw_1_2_diagnosis:=difftime(second_diagnosis,first_diagnosis,units = "days")]
ADHD_counts[,time_btw_1_3_diagnosis:=difftime(third_diagnosis,first_diagnosis,units = "days")]



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


#ASD (no drugs in the codelist)
load(paste0(dirconceptsets,"ASD.RData"))

ASD_counts <- MergeFilterAndCollapse(
  listdatasetL= list(ASD),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("count"),"codvar","number_of_diagnosis"),list(c("first"),"date","first_diagnosis"),list(c("second"),"date","second_diagnosis"),list(c("third"),"date","third_diagnosis"))
)

print(paste0(nrow(ASD_counts)," children with at least one diagnosis of ASD were identified"))



#ASD (no drugs in the codelist)
load(paste0(dirconceptsets,"ID.RData"))

ID_counts <- MergeFilterAndCollapse(
  listdatasetL= list(ID),
  condition = "date>=study_start & date<=study_end",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("count"),"codvar","number_of_diagnosis"),list(c("first"),"date","first_diagnosis"),list(c("second"),"date","second_diagnosis"),list(c("third"),"date","third_diagnosis"))
)

print(paste0(nrow(ID_counts)," children with at least one diagnosis of ID were identified"))
