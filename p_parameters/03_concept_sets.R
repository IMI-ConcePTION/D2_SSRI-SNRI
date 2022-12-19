# the output of this step are several lists and lists of lists to be used by CreateConceptsetDatasets

# concept_set_domains
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 domain

# concept_set_codes_our_study,
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 coding system
# level 3 list of codes incuded in that conceptset for that coding system

# concept_set_codes_our_study_excl
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 coding system
# level 3 list of codes to be excluded from that conceptset for that coding system

# the lists of all conceptsets 
# concept_sets_of_our_study

# input: the VAC4EU spreadsheets, restricted to the conceptsets associated with this study

ADHD_codelist <- fread(paste0(thisdir,"/p_parameters/archive_parameters/DP2_ADHD_RB_reviewed_23_11_22.csv"), header=T)
ADHD_codelist <- ADHD_codelist[, .(coding_system, code, event_abbreviation)]
#ADHD_codelist <- ADHD_codelist[type != "PrA"]
ADHD_codelist <- ADHD_codelist[code != "", ][, event_abbreviation := toupper(event_abbreviation)]

ASD_codelist <- fread(paste0(thisdir,"/p_parameters/archive_parameters/DP2_ASD_RB_reviewed_23_11_22.csv"), header=T)
ASD_codelist <- ASD_codelist[, .(coding_system, code, event_abbreviation)]
#ASD_codelist <- ASD_codelist[type != "PrA"]
ASD_codelist <- ASD_codelist[code != "", ][, event_abbreviation := toupper(event_abbreviation)]

ID_codelist <- fread(paste0(thisdir,"/p_parameters/archive_parameters/DP2_ID_RB_reviewed_23_11_22.csv"), header=T)
ID_codelist <- ID_codelist[, .(coding_system, code, event_abbreviation)]
#ID_codelist <- ID_codelist[type != "PrA"]
ID_codelist <- ID_codelist[code != "", ][, event_abbreviation := toupper(event_abbreviation)]



concept_sets_of_our_study<-unique(c(ADHD_codelist$event_abbreviation,ASD_codelist$event_abbreviation,ID_codelist$event_abbreviation))

concept_set_codes_our_study <- c(lapply(split(ADHD_codelist, by = "event_abbreviation", keep.by = F),
                                      split, by = "coding_system", keep.by = F),
                                 lapply(split(ASD_codelist, by = "event_abbreviation", keep.by = F),
                                      split, by = "coding_system", keep.by = F),
                                 lapply(split(ID_codelist, by = "event_abbreviation", keep.by = F),
                                      split, by = "coding_system", keep.by = F))

concept_set_codes_our_study <- lapply(concept_set_codes_our_study, sapply, unlist, use.names = F, simplify = F)


# concept_set_codes_our_study_excl <- vector(mode="list")
# 
# concept_set_codes_our_study_excl[["EPILEPSY_DRUGS"]][["ATC"]] <- c("N03AX16", "N03AX12","N03AF01")
# concept_set_codes_our_study_excl[["GAD"]][["ATC"]] <- c("N06AA", "N06AX") #N06AX21

## to do when ATC are included
concept_set_domains<- vector(mode="list")
for (concept in names(concept_set_codes_our_study)) {
  #if (!grepl("DRUGS", concept) & concept!="GABAPENTIN" & concept!="PREGABALIN") {
    concept_set_domains[[concept]] = "Diagnosis"
  #}else{
    #concept_set_domains[[concept]] = "Medicines"
  #}
}



#rm(concept)
rm(ADHD_codelist,ASD_codelist,ID_codelist)
