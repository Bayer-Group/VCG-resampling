#03_apply_dunnett_test_to_legacy_study.R

#*This program is the sweet output of the previous preparation-steps.
#*The functions of the previous R-script are executed here with respect
#*to the entered values.
#*     From the "02_make_dunnett_test.R"-script, the Dunnett's-test-function
#*     is imported. The Dunnett-test can be executed with respect to the entered
#*     (original) values and afterwards, the original control group (OCG) is
#*     replaced with randomly sampled K-values from my CO2- or isoflurane-
#*     sub-group. This step is repeated 500 times (22.08.2022). 500 is chosen
#*     because more iterations were not altering the outcoming results anymore.
#*For testing, an in-house legacy study was selected where a treatment-related
#*increase in calcium was reported serving as evidence for hypercalcemia.

#Load libraries
require(data.table)
require(tidyverse)
#Get the function which calculates Dunnett-test for each resampled value
source(paste0(rootpath, "/Programs/02_make_dunnett_test.R"))

#read electrolyte values from ViCoG table from data/Derived folder
studydata_to_test <- fread(paste0(der, "/electrolyte_parameters_4wk_rat.csv")) %>%
  #summarize "CO2/air" and "CO2/O2" to "CO2"
  mutate(ANESTHETICS = case_when(
    ANESTHETICS == "CO2/air" ~ "CO2",
    ANESTHETICS == "CO2/O2" ~ "CO2",
    ANESTHETICS == "isoflurane" ~ "isoflurane"
    ))

#*******************************************************************************
#*Make statistical tests
#Pull mean and sd of the respective samples (change the parameters if needed)
test_set <- studydata_to_test %>%
  filter(LBTESTCD == "CA",
         START_YEAR < 2017,
         #anesthetics == "CO2"
         ) %>%
  pull(LBORRES)
#get mean and sd
paste0("mean: ", test_set %>% mean() %>% signif(3) %>% format(n_small = 2), " mmol/L ± ",
       test_set %>% sd() %>% signif(2) %>% format(n_small = 2), "mmol/L"      
)
#get variance
paste0("variance: ", test_set %>% var() %>% signif(3) %>% format(n_small = 2))
#get 95 % confidence interval
paste0(test_set %>% mean() %>% signif(3) %>% format(n_small = 2), ", 95 % CI [",
  confint(lm(test_set ~ 1), level=0.95)[1] %>% signif(3) %>% format(n_small = 2),
  ", ", confint(lm(test_set ~ 1), level=0.95)[2] %>% signif(3) %>% format(n_small = 2),
  "]"
)
#make a Welch-t-test between the two groups
t.test(
  LBORRES ~ ANESTHETICS,
  data = studydata_to_test %>% filter(LBTESTCD == "CA"),
  var.equal = FALSE
  )
#*******************************************************************************
#Remove case-study from the set which you use for resampling
sample_population <- studydata_to_test %>% filter(STUDYID != "T103901-4")

baycal_with_initbw <- fread(paste0(der, "/legacy_calcium_study.csv"))

#Extract potassium serum levels from this study
baycal_ca_levels <- baycal_with_initbw %>%
  #rename the dose groups into control group (cg), low dose (ld), mid dose (md) and high dose (hd)
  mutate(trial_set_description = case_when(endsWith(trial_set_description, "1S1") ~ "Control",
                                           endsWith(trial_set_description, "1S2") ~ "Control",
                                           endsWith(trial_set_description, "2S1") ~ "Dose group 1",
                                           endsWith(trial_set_description, "2S2") ~ "Dose group 1",
                                           endsWith(trial_set_description, "3S1") ~ "Dose group 2",
                                           endsWith(trial_set_description, "3S2") ~ "Dose group 2",
                                           endsWith(trial_set_description, "4S1") ~ "Dose group 3",
                                           endsWith(trial_set_description, "4S2") ~ "Dose group 3"),
         trial_set_description = factor(trial_set_description, levels = c("Control", "Dose group 1", "Dose group 2", "Dose group 3")))

#******************************************************************************
#*Assess resampling performance for an equal number from both subsets in males
baycal1_both_m_all <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = c("isoflurane" ,"CO2"),
  electrolyte = "CA",
  replacement_aim = "all"
  )
baycal1_both_m_all[["res_vs_original"]]

baycal1_both_m_all_but_two <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = c("isoflurane" ,"CO2"),
  electrolyte = "CA",
  replacement_aim = "all_but_two"
)
baycal1_both_m_all_but_two[["res_vs_original"]]

baycal1_both_m_half <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = c("isoflurane" ,"CO2"),
  electrolyte = "CA",
  replacement_aim = "half"
)
baycal1_both_m_half[["res_vs_original"]]
#******************************************************************************
#*Assess resampling performance for isoflurane subset in males
baycal1_iso_m_all <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = "isoflurane",
  electrolyte = "CA",
  replacement_aim = "all"
)
baycal1_iso_m_all[["res_vs_original"]]

baycal1_iso_m_all_but_two <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = "isoflurane",
  electrolyte = "CA",
  replacement_aim = "all_but_two"
)
baycal1_iso_m_all_but_two[["res_vs_original"]]

baycal1_iso_m_half <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = "isoflurane",
  electrolyte = "CA",
  replacement_aim = "half"
)
baycal1_iso_m_half[["res_vs_original"]]
