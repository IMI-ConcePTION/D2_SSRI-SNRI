
#Construct the algorithms
smart_load("D4_study_population", diroutput)

#ADHD
#Algorithm 1: One specialist code for ADHD identified in the data source
# Index date: Date of specialist diagnosis (may occur during hospital admission or specialist outpatient clinic visit, for example); any diagnosis with an explicit qualifier of “ruled out,” “suspected,” or “family history” should not be considered. The data partner may consider diagnoses within the hospital as a specialist diagnosis if applicable for their data source.


#fill which meanings in your DAP to use to specify SPECIALIST diagnosis---------
#for example:
# -outpatient_hospital_visit
# -hospital_diagnosis
# -hospitalisation


load(paste0(dirconceptsets,"ADHD.RData"))


ADHD_ALG1 <- MergeFilterAndCollapse(
  listdatasetL= list(ADHD),
  condition = "date>=study_start & date<=study_end & meaning_renamed=='outpatient_hospital_visit' | meaning_renamed=='hospital_diagnosis' | meaning_renamed=='hospitalisation'",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("count"),"codvar","number_of_diagnosis"),list(c("first"),"date","first_diagnosis"),list(c("second"),"date","second_diagnosis"),list(c("third"),"date","third_diagnosis"))
)


#---------------------------------------
#Algorithm 2:  Two non-specialist codes for ADHD that are at least 28 days apart, but within 1.5 years
# Index date: Date of second non-specialist diagnosis. Where available and applicable, any diagnosis with an explicit qualifier of “ruled out,” “suspected,” or “family history” should not be considered

#fill which meanings in your DAP to use to specify NOT-SPECIALIST diagnosis---------
#for example:
#-primary_care

ADHD_ALG2 <- MergeFilterAndCollapse(
  listdatasetL= list(ADHD),
  condition = "date>=study_start & date<=study_end & meaning_renamed=='primary_care'",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("first"),"date","first_diagnosis"),list(c("second"),"date","second_diagnosis"))
)

ADHD_ALG2[,time_btw_1_2_diagnosis:=difftime(second_diagnosis,first_diagnosis,units = "days")]

ADHD_ALG2<-ADHD_ALG2[time_btw_1_2_diagnosis>=28 & time_btw_1_2_diagnosis<=548,]



#---------------------------------------
#Algorithm 3: One non-specialist code and 1 at least dispensing/prescription for ADHD medication within a year of diagnosis
# Index date: The later of either the non-specialist code or medication dispensing; any diagnosis with an explicit qualifier of “ruled out,” “suspected,” or “family history” should not be considered.

ADHD_ALG3 <- MergeFilterAndCollapse(
  listdatasetL= list(ADHD),
  condition = "date>=study_start & date<=study_end & meaning_renamed=='primary_care'",
  key = c("person_id"),
  datasetS = D4_study_population[,.(person_id)],
  # saveintermediatedataset = T,
  # nameintermediatedataset = paste0(dirtemp,'tempfile'),
  sorting = c("person_id","date"),
  strata = c("person_id"),
  summarystat = list(list(c("exist"),"date","diagnosis_date"))
)

load(paste0(dirconceptsets,"ADHD_DRUGS.RData"))
ADHD_ALG3<-merge(ADHD_ALG3, unique(ADHD_DRUGS[,.(person_id,date)]), by="person_id", all.x=T)
ADHD_ALG3[diagnosis_date<=date & date<=diagnosis_date+365,]


#---------------------------------------
#Algorithm 4: One specialist dispensing/prescription for ADHD medication. 
#Index date: Date of the first prescription / medication dispensing. 

print(table(ADHD_DRUGS$codvar))

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
    summarystat = list(list(c("exist"),"date","medication_date"))
  )
  
}

print(paste0(nrow(ADHD_ALG4)," children with at least one drug dispensing/prescription of one of the drugs in the ADHD codelist were identified"))

#& prescriber_speciality!='GP'
