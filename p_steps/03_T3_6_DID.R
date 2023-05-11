
#LOAD the study population
smart_load("D4_study_population", diroutput,extension=extension)
SURVEY_OBSERVATIONS <- read_CDM_tables("SURVEY_OBSERVATIONS")

summary(D4_study_population$birth_date) #min should be 2005 for EFEMERIS

#complete list of motor and language
origin_columns_names<-c("M24_OBEIT_ORDRE","M24_NOMME_IMAGE","M24_ASSOCIE_2_MOTS","M24_SUPERPOSE_OBJET","M24_MOTRICITE_MEMBRES", "M24_MARCHE_ACQUISE","M24_AGE_MARCHE_ACQUISE")

Social_and_language<-c("M24_OBEIT_ORDRE","M24_NOMME_IMAGE","M24_ASSOCIE_2_MOTS")
Motor<-c("M24_SUPERPOSE_OBJET","M24_MOTRICITE_MEMBRES", "M24_MARCHE_ACQUISE")

#for testing
# origin_columns_names<-c("ABORTI","ANONIMO","CAUSA","M24_MARCHE_ACQUISE","MALFOR","DATAINT"," ETA_NEO")
# Social_and_language<-c("ABORTI","MALFOR")
# Motor<-c("ANONIMO","CAUSA","DATAINT")

#filter the SURVEY_OBSERVATIONS keeping only items indicating a motor, language or social delay, 
SURVEY_OBSERVATIONS<-SURVEY_OBSERVATIONS[so_source_column %in% origin_columns_names,][,.(person_id,so_source_column,so_source_value)]


#rescrict to children  born from July 2005 to the end of December 2017 with infant development assessed at 24 months from 2007-2019
D4_study_population<-D4_study_population[birth_date>=ymd(20050101) & birth_date<=ymd(20171231),][year(birth_date)>=2005 & year(birth_date)<=2009,birth_cohort:="2005-2009"][year(birth_date)>=2010 & year(birth_date)<=2014,birth_cohort:="2010-2014"][year(birth_date)>=2015 & year(birth_date)<=2017,birth_cohort:="2015-2017"][,datasource:=thisdatasource][,study_entry_date:=NULL][,study_exit_date:=NULL]

#merge population to survey_observation table
DID_base<-merge(D4_study_population, SURVEY_OBSERVATIONS,by="person_id",all.x=T)

setnames(DID_base,"so_source_column","Item")  
#DID_base[Item=="CAUSA",Item:=NA]   #REMOVE

#create the column "age_at_first_walking"
eta_database<-DID_base[Item=="M24_MARCHE_ACQUISE",][,.(person_id,so_source_value)][,so_source_value:=as.numeric(so_source_value)]  
setnames(eta_database,"so_source_value","age_at_first_walking")

DID_base<-DID_base[Item %in% Social_and_language | Item %in% Motor ,]
DID_base<-merge(DID_base,eta_database,by="person_id",all.x = T)

DID_base[Item %in% Social_and_language, macro_group:="Social_and_language"][Item %in% Motor, macro_group:="Motor"]

#check how many questions children answered
DID_base[!is.na(Item),Nitems_answered:=.N,by=c("person_id")]  #check number of Nitems_answered is correct
DID_base[is.na(Nitems_answered),Nitems_answered:=0][,maxitems_answered:=max(Nitems_answered),,by=c("person_id")]
children_0or1items_answered<-length(unique(DID_base[maxitems_answered<2,"person_id"])$person_id)   
total_children_inpopulation<-length(unique(DID_base$person_id))
percentage<-children_0or1items_answered/total_children_inpopulation*100

data <- data.table(children_0or1items_answered=children_0or1items_answered, total_children_inpopulation=total_children_inpopulation, percentage=percentage)
data

#save the prcentage of children answering only o or 1 questions (among the one of interest)
fwrite(data,
       paste0(direxp, "D5_number_of_children_excluded_aswered_only0or1_items.csv"))

#remove children aswered only 0 or 1 items from the dataset
DID_base<-DID_base[maxitems_answered>0,]   #change to >1


#copy the dataset to create the "All" birth cohort
copy_tmp<-copy(DID_base)
copy_tmp[,birth_cohort:="All"]
DID_base<-rbind(DID_base,copy_tmp)


#create table 1: stratified per birth cohort
table1a<-unique(DID_base[so_source_value=="0" & !is.na(macro_group) ,count:=.N,by=c("birth_cohort","Item","macro_group")][!is.na(macro_group),denominator:=.N,by=c("birth_cohort","Item","macro_group")][!is.na(macro_group) ,.(birth_cohort,Item,count,denominator,datasource)])  

#restrict to chldren with at least 2 "no" answered
table1b<-unique(DID_base[so_source_value=="0" & macro_group=="Motor",N:=.N,by=c("person_id")][N>1,children_at_least_2_no:=.N,by="birth_cohort"][N>1,mean_age_at_first_walking:=mean(age_at_first_walking,na.rm=T),by="birth_cohort"][,median_age_at_first_walking:=median(age_at_first_walking,na.rm=T),by=c("birth_cohort")][,IQR_median_age_at_first_walking:=IQR(age_at_first_walking,na.rm=T),by=c("birth_cohort")] [,.(birth_cohort,children_at_least_2_no,mean_age_at_first_walking,median_age_at_first_walking,IQR_median_age_at_first_walking,datasource)])

table1<-rbind(table1a,table1b,fill=T)

fwrite(table1,
       paste0(direxp, "D5_number_of_children_with_motor_language_social_delay_bybirthcohort.csv"))



#create table 2: stratified per sex
table2a<-unique(DID_base[so_source_value=="0" & !is.na(macro_group) ,count:=.N,by=c("sex_at_instance_creation","Item","macro_group")][!is.na(macro_group),denominator:=.N,by=c("sex_at_instance_creation","Item","macro_group")][!is.na(macro_group) ,.(sex_at_instance_creation,Item,count,denominator,datasource)])  #metti no invece di 0

table2b<-unique(DID_base[so_source_value=="0" & macro_group=="Motor",N:=.N,by=c("person_id")][N>1,children_at_least_2_no:=.N,by="sex_at_instance_creation"][N>1,mean_age_at_first_walking:=mean(age_at_first_walking,na.rm=T),by="sex_at_instance_creation"][,median_age_at_first_walking:=median(age_at_first_walking,na.rm=T),by=c("sex_at_instance_creation")][,IQR_median_age_at_first_walking:=IQR(age_at_first_walking,na.rm=T),by=c("sex_at_instance_creation")] [,.(sex_at_instance_creation,children_at_least_2_no,mean_age_at_first_walking,median_age_at_first_walking,IQR_median_age_at_first_walking,datasource)])

table2<-rbind(table2a,table2b,fill=T)

fwrite(table2,
       paste0(direxp, "D5_number_of_children_with_motor_language_social_delay_bysex.csv"))

