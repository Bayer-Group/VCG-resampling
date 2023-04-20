#K6_apply_resampling_method.R

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
source(paste0(rootpath, "/man/02_make_dunnett_test.R"))

#read electrolyte values from VCG table from data/Derived folder
studydata_to_test <- fread(paste0(der, "/all_lb_parameters_4wk_rat.csv")) %>%
  rename(ANESTHETICS = anesthetics) %>%
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
         # START_YEAR < 2017,
         # ANESTHETICS == "CO2/air"
         ) %>%
  pull(LBORRES)
#get mean and sd
paste0("mean: ", test_set %>% mean() %>% signif(3) %>% format(n_small = 2), " mmol/L ± ",
       test_set %>% sd() %>% signif(2) %>% format(n_small = 2), " mmol/L"
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
sample_population <- studydata_to_test %>% filter(SPREFID != "T103901-4")

sample_population %>% filter(LBTESTCD == "CA") %>% pull(SPREFID) %>% n_distinct()

baycal_with_initbw <- fread(paste0(der, "/legacy_calcium_study.csv"))

#Extract electrolyte serum levels from this study
baycal_ca_levels <- baycal_with_initbw %>%
  #rename the dose groups into control group (control), Dose group 1 (low dose),
  #Dose group 2 (mid dose), and Dose group 3 (high dose)
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
#*******************************************************************************
#*******************************************************************************
#*******************************************************************************
bay_both_m_all <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = c("isoflurane", "CO2"),
  electrolyte = c("CA", "K", "SODIUM", "PHOS", "RBC", "BW_D1", "BW_D14", "BW_D28"),
  replacement_aim = "all"
)


bay_both_m_all_but_two <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = c("isoflurane", "CO2"),
  electrolyte = c("CA", "K", "SODIUM", "PHOS", "RBC", "BW_D1", "BW_D14", "BW_D28"),
  replacement_aim = "all_but_two"
)

bay_both_m_half <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = c("isoflurane", "CO2"),
  electrolyte = c("CA", "K", "SODIUM", "PHOS", "RBC", "BW_D1", "BW_D14", "BW_D28"),
  replacement_aim = "half"
)

bay_iso_m_all <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = "isoflurane",
  electrolyte = c("CA", "K", "SODIUM", "PHOS", "RBC", "BW_D1", "BW_D14", "BW_D28"),
  replacement_aim = "all"
)

bay_iso_m_all_but_two <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = "isoflurane",
  electrolyte = c("CA", "K", "SODIUM", "PHOS", "RBC", "BW_D1", "BW_D14", "BW_D28"),
  replacement_aim = "all_but_two"
)

bay_iso_m_half <- vcg_resampling_dunnett(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 500,
  narcosis_sample = "isoflurane",
  electrolyte = c("CA", "K", "SODIUM", "PHOS", "RBC", "BW_D1", "BW_D14", "BW_D28"),
  replacement_aim = "half"
)

#*******************************************************************************
#*observe results
#Calcium
bay_both_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "CA", consistency_flag  == "con")
bay_both_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "CA", consistency_flag  == "con")
bay_both_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "CA", consistency_flag  == "con")
bay_iso_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "CA", consistency_flag  == "con")
bay_iso_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "CA", consistency_flag  == "con")
bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "CA", consistency_flag  == "con")

bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "CA")
baycal_ca_levels %>% filter(LBTESTCD == "CA", SEX == "M", trial_set_description == "Control") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))
sample_population %>% filter(LBTESTCD == "CA", SEX == "M") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))
sample_population %>% filter(LBTESTCD == "CA", SEX == "M", ANESTHETICS == "isoflurane") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))

#Potassium
bay_both_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "K", consistency_flag  == "con")
bay_both_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "K", consistency_flag  == "con")
bay_both_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "K", consistency_flag  == "con")
bay_iso_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "K", consistency_flag  == "con")
bay_iso_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "K", consistency_flag  == "con")
bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "K", consistency_flag  == "con")

#Sodium
bay_both_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "SODIUM", consistency_flag  == "con")
bay_both_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "SODIUM", consistency_flag  == "con")
bay_both_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "SODIUM", consistency_flag  == "con")
bay_iso_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "SODIUM", consistency_flag  == "con")
bay_iso_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "SODIUM", consistency_flag  == "con")
bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "SODIUM", consistency_flag  == "con")

#Phosphate
bay_both_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "PHOS", consistency_flag  == "con")
bay_both_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "PHOS", consistency_flag  == "con")
bay_both_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "PHOS", consistency_flag  == "con")
bay_iso_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "PHOS", consistency_flag  == "con")
bay_iso_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "PHOS", consistency_flag  == "con")
bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "PHOS", consistency_flag  == "con")

bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "PHOS")
baycal_ca_levels %>% filter(LBTESTCD == "PHOS", SEX == "M", trial_set_description == "Control") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))
sample_population %>% filter(LBTESTCD == "PHOS", SEX == "M") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))
sample_population %>% filter(LBTESTCD == "PHOS", SEX == "M", ANESTHETICS == "isoflurane") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))

#Erythrocytes
bay_both_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "RBC", consistency_flag  == "con")
bay_both_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "RBC", consistency_flag  == "con")
bay_both_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "RBC", consistency_flag  == "con")
bay_iso_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "RBC", consistency_flag  == "con")
bay_iso_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "RBC", consistency_flag  == "con")
bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "RBC", consistency_flag  == "con")

#Body weight day 1
bay_both_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D1", consistency_flag  == "con")
bay_both_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D1", consistency_flag  == "con")
bay_both_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D1", consistency_flag  == "con")
bay_iso_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D1", consistency_flag  == "con")
bay_iso_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D1", consistency_flag  == "con")
bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D1", consistency_flag  == "con")

#Body weight day 14
bay_both_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D14", consistency_flag  == "con")
bay_both_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D14", consistency_flag  == "con")
bay_both_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D14", consistency_flag  == "con")
bay_iso_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D14", consistency_flag  == "con")
bay_iso_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D14", consistency_flag  == "con")
bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D14", consistency_flag  == "con")

#Body weight day 28
bay_both_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D28", consistency_flag  == "con")
bay_both_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D28", consistency_flag  == "con")
bay_both_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D28", consistency_flag  == "con")
bay_iso_m_all[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D28", consistency_flag  == "con")
bay_iso_m_all_but_two[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D28", consistency_flag  == "con")
bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D28", consistency_flag  == "con")

bay_iso_m_half[["res_vs_original"]] %>% filter(LBTESTCD == "BW_D28")
baycal_ca_levels %>% filter(LBTESTCD == "BW_D28", SEX == "M", trial_set_description == "Control") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))
sample_population %>% filter(LBTESTCD == "BW_D28", SEX == "M") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))
sample_population %>% filter(LBTESTCD == "BW_D28", SEX == "M", ANESTHETICS == "isoflurane") %>% summarise(m = mean(LBORRES), s = sd(LBORRES))

#*******************************************************************************
#*******************************************************************************
#*Write results as CSV and store in Data/Derived folder-------------------------
#*******************************************************************************
#*uncomment, if needed
# fwrite(bay_both_m_all, paste0(der, "/bay_both_m_all.csv"))
# fwrite(bay_both_m_all_but_two, paste0(der, "/bay_both_m_all_but_two.csv"))
# fwrite(bay_both_m_half, paste0(der, "/bay_both_m_half.csv"))
# fwrite(bay_iso_m_all, paste0(der, "/bay_iso_m_all.csv"))
# fwrite(bay_iso_m_all_but_two, paste0(der, "/bay_iso_m_all_but_two.csv"))
# fwrite(bay_iso_m_half, paste0(der, "/bay_iso_m_half.csv"))
