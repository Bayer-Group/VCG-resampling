#not_for_submission_get_legacy_study.R

#*This script extracts the control group data and dose group data from the legacy
#*study which is used to assess the performance ot the VCGs. The following
#*parameters are extracted from the legacy study:
#* - serum calcium
#* - serum potassium
#* - serum inorganic phosphate
#* - reed blood cell count
#* - body weights
#*********************************************************************************;
# retrieve the current study path
rootpath <- sub("*\\/Programs", "",dirname(rstudioapi::getSourceEditorContext()$path))

# path to folder derived data sets
der <- paste(rootpath,'/Data/Derived', sep='')
#******************************************************************************
#Load libraries----------------------------------------------------------------
#******************************************************************************
#******************************************************************************
library(data.table)
library(tidyverse)
#******************************************************************************
#Establish FROG access---------------------------------------------------------
#******************************************************************************
#******************************************************************************
#establish connection to athena AWS
source(paste0(rootpath, "/old_programs/01_establish_frog_access.R"))
#load a function which adds trial information to the selected data set
#(this is temporary for as long as the FROG team is cleaning its data set)
source(paste0(getwd(), "/old_programs/02-1_add_temporary_mappings.R"))
#*******************************************************************************
#Extract legacy study data from FROG--------------------------------------------
#*******************************************************************************
#*******************************************************************************
#select narcosis methods for a set of studies
narcosis <- fread(paste0(rootpath, "/Data/Original/narcosis.csv")) %>%
  #turn all empty cells into NAs
  mutate_all(na_if,"") %>%
  #transform NA of anesthetics to "unknown"
  mutate(anesthetics = case_when(is.na(anesthetics) ~ "unknown",
                                 TRUE ~ anesthetics)) %>%
  #interpolatew the "unknown" anesthetics to "CO2/air" if the study was started
  #before 2017 and to "isoflurance" for studies after 2017
  mutate(anesthetics = case_when(anesthetics == "unknown" & start_year < 2017 ~ "CO2/air",
                                 anesthetics == "unknown" & start_year >= 2017 ~ "isoflurane",
                                 anesthetics == "CO2/O2" ~ "CO2/air",
                                 TRUE ~ anesthetics)) %>%
  rename(SPREFID = study_id)

narcosis_selection_list <- paste0("'", narcosis %>% pull(SPREFID) %>% unique(), "'", collapse = " OR ts.study_id = ")
narcosis_selection_list_bw <- paste0("'", narcosis %>% pull(SPREFID) %>% unique(), "'", collapse = " OR study_id = ")

#Extract all frog LB data from Amazon Athena AWS and save it to the Data/Derived folder
studydata_query_all_lbts <- dbGetQuery(con, paste0("
SELECT
  ts.study_id AS SPREFID,
  ts.species AS SPECIES,
  ts.strain AS STRAIN,
  ts.treatment_vehicle AS TRTV,
  ts.test_facility_location AS TSTFLOC,
  ts.adjusted_duration AS DOSDUR,
  ts.title AS STITLE,
  ts.test_subject_supplier AS SPLRNAM,
  ts.route_of_administration AS ROUTE,
  SUBSTRING(ts.start_date, 1, 4) AS START_YEAR,
  lb.test_definition_name_short AS LBTESTCD,
  lb.test_definition_category AS LBCAT,
  lb.test_result_numerical_value AS LBORRES,
  lb.test_result_unit AS LBORRESU,
  lb.test_definition_method AS LBMETHOD,
  lb.test_definition_specimen AS LBSPEC,
  lb.test_definition_relative_study_day AS LBDY,
  lb.trial_set_description,
  lb.animal_sex AS SEX,
  lb.study_id || '-' || lb.animal_id AS USUBJID
FROM
  frog_sink_catalogdb.tox_study ts
  RIGHT JOIN frog_sink_catalogdb.lab_test_results lb ON ts.study_id = lb.study_id
WHERE
  ts.species = 'RAT'
  AND lb.test_definition_relative_study_day BETWEEN 1 AND 35
AND lb.dosage_group_is_control_group = TRUE
AND (
ts.study_id = ", narcosis_selection_list, "
)
;"))
# studydata_query_all_lbts <- dbGetQuery(con, paste0("
# SELECT
#   ts.study_id AS SPREFID,
#   ts.species AS SPECIES,
#   ts.strain AS STRAIN,
#   ts.treatment_vehicle AS TRTV,
#   ts.test_facility_location AS TSTFLOC,
#   ts.adjusted_duration AS DOSDUR,
#   ts.title AS STITLE,
#   ts.test_subject_supplier AS SPLRNAM,
#   ts.route_of_administration AS ROUTE,
#   SUBSTRING(ts.start_date, 1, 4) AS START_YEAR,
#   lb.test_definition_name_short AS LBTESTCD,
#   lb.test_definition_category AS LBCAT,
#   lb.test_result_numerical_value AS LBORRES,
#   lb.test_result_unit AS LBORRESU,
#   lb.test_definition_method AS LBMETHOD,
#   lb.test_definition_specimen AS LBSPEC,
#   lb.test_definition_relative_study_day AS LBDY,
#   lb.trial_set_description,
#   lb.animal_sex AS SEX,
#   lb.study_id || '-' || lb.animal_id AS USUBJID
# FROM
#   frog_sink_catalogdb.tox_study ts
#   RIGHT JOIN frog_sink_catalogdb.lab_test_results lb ON ts.study_id = lb.study_id
# WHERE
#   ts.species = 'RAT'
#   AND lb.test_definition_relative_study_day BETWEEN 1 AND 35
# AND NOT (
#   ts.adjusted_duration LIKE '%d%' OR
#   ts.adjusted_duration = '1 week' OR
#   ts.adjusted_duration = '2 week' OR
#   ts.adjusted_duration = '3 week'
# )
# AND lb.dosage_group_is_control_group = TRUE
# AND NOT(
#   ts.adjusted_duration IS NULL OR
#   ts.adjusted_duration = '' OR
#   lb.test_result_numerical_value IS NULL OR
#   lb.test_result_unit IS NULL
# )
# ;"))

#Get body weiht values of interest for all used LB studies
#Usually, body weight is not part of LB domain but in order to make the code work
#more conveniantely, I rename BWORRES to LBORRES and BWDY to LBDY
studydata_query_all_bw <- dbGetQuery(con, paste0("
SELECT
  study_id || '-' || animal_id as USUBJID,
  body_weight_day AS LBDY,
  body_weight AS LBORRES,
  body_weight_unit AS LBORRESU
FROM
  frog_sink_catalogdb.body_weights
WHERE
 NOT(
    body_weight IS NULL OR
    body_weight_unit IS NULL
      )
 AND
  body_weight_day = 1
  OR body_weight_day = 14
  OR body_weight_day = 28
AND (
study_id = ", narcosis_selection_list_bw, "
)
;")) %>%
  #remove animal data by IDs not present in the LB domain
filter(USUBJID %in% unique(studydata_query_all_lbts$USUBJID)) %>%
  #add domain name and BWDY as "LBTESTCD"
  mutate(LBTESTCD = paste0("BW_D", LBDY)) %>%
  #make all BWORRES into g
  mutate(LBORRES = case_when(grepl("kg", LBORRESU) ~ LBORRES * 1000,
                             grepl("mg", LBORRESU) ~ LBORRES / 1000,
                             TRUE ~ LBORRES),
         LBORRESU = "g") %>%
  #add relevant parameters from the TS/LB domain performing a join
  inner_join(
    studydata_query_all_lbts %>%
      select(-LBDY, -LBORRES, -LBORRESU, -LBTESTCD, -LBSPEC, -LBMETHOD, -LBCAT) %>%
      unique(),
    by = "USUBJID")

#*Create initial body weigh, i.e. the body weight value of study day 1
#*as a separate column
initbwinfo <- studydata_query_all_bw %>%
  filter(LBDY == 1) %>%
  #make all BWORRES into g
  mutate(INITBW = LBORRES) %>%
  select(USUBJID, INITBW)


#*attach bw to lb domain to remove all values which do not have an according
#*bw-value. An improvised "rolling join" will be used for that. I.e., if not the
#*same day was taken, the nearest measure day will be used.
studydata_all <- bind_rows(studydata_query_all_lbts, studydata_query_all_bw) %>%
  left_join(initbwinfo, by = "USUBJID")

#clean studydata
studydata_cleaned_all <- studydata_all %>%
  #clean adjusted duration
  mutate(DOSDUR = case_when(grepl("T182D", DOSDUR, fixed = T) ~ "26 week",
                                       grepl("P6M", DOSDUR, fixed = T) ~ "26 week",
                                       TRUE ~ DOSDUR)) %>%
  #add a cutoff where you split the studies in "4 weeks" and "> 4 weeks"
  mutate(duration_cutoff = case_when(DOSDUR == "4 week" ~ "4 weeks",
                                     TRUE ~ "> 4 weeks")) %>%
  #Unify test_definition_name_short names
  mutate(LBTESTCD = case_when(grepl("T-SH", LBTESTCD, fixed = T) ~ "TSH",
                                                grepl("U_CRE", LBTESTCD, fixed = T) ~ "CREAT",
                                                grepl("LYMAT", LBTESTCD, fixed = T) ~ "ATYP",
                                                grepl("UCREUVOc", LBTESTCD, fixed = T) ~ "CREAT",
                                                TRUE ~ LBTESTCD))
#add temporary mappings to the ts-domain for strain, supplier, and TRTV based
#on the study ID
# studydata_ts <- add_ts_information(studydata = studydata_cleaned_all)
studydata_ts_dm <- add_dm_information(studydata = studydata_cleaned_all)

studydata <- studydata_ts_dm

studydata_to_test <- studydata %>% filter(
  # grepl("WIST", STRAIN, ignore.case = T),
  # grepl("GAVAGE|GASTR", ROUTE, ignore.case = T) |
  #   grepl("GAVAGE|GASTR", STITLE, ignore.case = T),
  # SPLRNAM == "CHARLES RIVER" | SPLRNAM == "HARLAN",
  # grepl("KOL|SOL", TRTV, ignore.case = T),
  # !grepl("PEG", TRTV),
  # between(AGE, 6, 10),
  # between(INITBW, 100, 250),
  # grepl("wuppert", TSTFLOC, ignore.case = T),
  SPREFID %in% narcosis$SPREFID,
  SEX == "M",
  (LBTESTCD %in% c("CA", "K", "SODIUM", "PHOS") & LBSPEC == "SERUM") |
  (LBTESTCD == "RBC" & LBSPEC == "PLASMA") |
  (LBTESTCD %in% c("BW_D1", "BW_D14", "BW_D28"))
) %>%
  inner_join(narcosis %>% select(-start_year), by = "SPREFID") %>%
  #remove all unneccessary data
  select(
    USUBJID,
    SPREFID,
    LBTESTCD,
    LBCAT,
    LBORRES,
    LBORRESU,
    LBDY,
    LBSPEC,
    SEX,
    INITBW,
    SPECIES,
    STRAIN,
    TRTV,
    START_YEAR,
    AGE,
    anesthetics
  )

studydata_to_test <- studydata_to_test %>%
  #bugfix: remove entries which are NA in the vehicle column
  filter(!is.na(TRTV))
#*******************************************************************************
#Write control group values as CSV into the Data/Derived folder
#*******************************************************************************
#*******************************************************************************
fwrite(studydata_to_test, paste0(der, "/all_lb_parameters_4wk_rat.csv"))
