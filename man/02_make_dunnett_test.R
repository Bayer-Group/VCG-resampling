#*Create a function which does the following:
#*  - Calculate a Dunnett test using the individual values
#*  - Replace the original control group values with a random sample (without
#*    replacement) of values from rats which were anesthetized with either
#*    isoflurane or CO2 or both and recalculate the Dunnett test
#*  - Repeat previous step n times
#*  - Summarize results in a list giving the percentage of how often the
#*    original result was reproduced (per dose group)
#*    as well as the mean values of each sample of the VCGs
#*    
#*  - Give the 95% CI for the mean distance between the k-th VCG (k being a
#*    selected numer) and the respective dose group.
#*******************************************************************************
#Check if packages are installed------------------------------------------------
#*******************************************************************************
#*******************************************************************************
require(tidyverse)
require(DescTools)
#*******************************************************************************
#*Beginning of function---------------------------------------------------------
#*******************************************************************************
#*******************************************************************************
vcg_resampling_dunnett <- function(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 50,
  narcosis_sample = c("isoflurane", "CO2"),
  electrolyte = c("CA", "K"),
  replacement_aim = "all_but_two"
  )
{

  #set seed for reproducibility of results
  set.seed(1)
  
  #Filter the data you want to observe for selected sex and parameter of interest
  studydata_sex <- case_study %>% filter(
    SEX == sex,
    LBTESTCD %in% c(electrolyte)
    )
  
  
  #*****************************************************************************
  #*get location parameters of CCG and attatch them to results list-------------
  #*****************************************************************************
  ccg_location_parameters <- studydata_sex %>%
    filter(trial_set_description == "Control") %>%
    group_by(LBTESTCD) %>%
    summarize(
      ccg_mean = mean(LBORRES),
      ccg_sd = sd(LBORRES),
      ccg_population = n()
    )
  
  #*****************************************************************************
  #*get location parameters of dose groups of legacy study and attatch them to
  #*results list----------------------------------------------------------------
  #*****************************************************************************
  dose_location_parameters <- studydata_sex %>%
    filter(trial_set_description != "Control") %>%
    group_by(trial_set_description, LBTESTCD) %>%
      summarize(
        dose_mean = mean(LBORRES),
        dose_sd = sd(LBORRES),
        dose_population = n()
      ) %>%
      rename(dose_group = trial_set_description)
  
  #*****************************************************************************
  #*****************************************************************************
  #*Calculate Dunnett's test with CCGs------------------------------------------
  #*****************************************************************************
  #*calculate number of animals in the control group
  #*If there are an unequal number of measured endpoints (e.g., if there are 10
  #*body weight values and 9 ERY values) the highest number is taken as this 
  #*represents the total number of animals in the set.
  anim_per_group <- studydata_sex %>%
    group_by(LBTESTCD) %>%
    filter(trial_set_description == "Control") %>%
    summarize(ccg_count = n()) %>%
    pull(ccg_count) %>%
    max()
  #*****************************************************************************
  #*Turn data frame into a nested list of the following structure:
  #* 1) each list contains the values of each observed endpoint
  #* 2) within this list, all values are split into respective dose groups
  studydata_list_by_endpoint <- split.data.frame(studydata_sex, studydata_sex$LBTESTCD)
  
  studydata_nested_list <- lapply(
    studydata_list_by_endpoint, function(x) split(x$LBORRES, x$trial_set_description)
    )
  #*****************************************************************************
  #Calculate Dunnett's test with concurrent control group
  ccg_dunnett <- lapply(studydata_nested_list, DunnettTest)
  
  #Extract the results of the Dunnett's test
  ccg_dunnett_res <- lapply(ccg_dunnett, function(x) as_tibble(x[[1]]))
  
  #Attach dose group names to the results of the Dunnett's test
  ccg_dunnett_res_dose_names <- lapply(
    ccg_dunnett_res,
    function(x) {
      x %>%
        mutate(
          dose_group = paste0(
            "Dose group ",
            sapply(ccg_dunnett_res, function(x) seq_along(1:nrow(x)))[,1])
          )
        }
    )
  #*****************************************************************************
  #Attach description to the p-val results and the direction of the significance----
  #*****************************************************************************
  ccg_significance <- lapply(
    ccg_dunnett_res_dose_names,
    function(x) {
      x %>%
        mutate(
          #flag if p-value is below 0.05 with "s", otherwise "ns"
          ccg_significance = case_when(
            pval < 0.05 ~ "s",
            TRUE ~ "ns"),
          #state in which direction the difference goes in comparison to the CG
          ccg_direction = case_when(
            diff < 0 ~ "decrease",
            diff > 0 ~ "increase",
            TRUE ~ "no_change"
          )
          )
               }
    )
  
  
  #Trensform results of Dunnett's test into tibble
  ccg_results <- map_df(ccg_significance, as_tibble) %>%
    mutate(
      LBTESTCD = paste0(
      rep(
        names(studydata_nested_list),
        each = studydata_sex %>% filter(trial_set_description != "Control") %>% pull(trial_set_description) %>% n_distinct()
        )
      ),
      #Add an identifier for later joining to the VCG results
      joiner_id = paste(LBTESTCD, dose_group, sep = "_")
    ) %>%
    rename(
      ccg_diff = diff,
      ccg_lwr.ci = lwr.ci,
      ccg_upr.ci = upr.ci,
      ccg_pval = pval
    ) %>%
    #Add CCG location parameters to results
    left_join(ccg_location_parameters, by = "LBTESTCD") %>%
    #Add location parameters of dose groups to results
    left_join(dose_location_parameters, by = c("dose_group", "LBTESTCD"))
    
    
  

  #*****************************************************************************
  #*****************************************************************************
  #Keep sentinel animals--------------------------------------------------------
  #*****************************************************************************
  #*The VCG sample population is selected to match the CA values of the sentinel
  #*animals kept.
  
  sentinel_animals_selection <- studydata_sex %>% filter(LBTESTCD == "CA")
  #*Remove individuals from the original control group, based on selection.
  #*If "all" was selected, all ccg animals were removed. If "half" was selected,
  #*a rounded number of animals will be removed so that an even number of animals
  #*remain on both extremes (selected by initial body weight). If "all_but_two"
  #*were selected, only the heaviest and the lightest rat remain in the data set.
  
  #How many animals do you want to keep?
  n_keep <- if(replacement_aim == "all"){
    0
  } else if(replacement_aim == "all_but_two"){
    2
  } else if(replacement_aim == "half"){
    floor(anim_per_group / 2)
  }
  
  #Get unique subject IDs from control group of the case study ordered by
  #initial body weight (INITBW)
  sentinel_animals_USUBJIDs <- if(replacement_aim == "all") {
    NULL
  } else if(replacement_aim == "all_but_two") {
    sentinel_animals_selection %>% filter(trial_set_description == "Control") %>%
      arrange(desc(INITBW)) %>%
      slice(1:(n_keep / 2), (anim_per_group - (n_keep / 2) + 1):anim_per_group)
  } else if(replacement_aim == "half" & !is.integer(n_keep / 2)) {
    #get one animal from the middle if the amount of sentinel animals is odd
    sentinel_animals_selection %>% filter(trial_set_description == "Control") %>%
      arrange(desc(INITBW)) %>%
      slice(1:floor(n_keep / 2), (ceiling(anim_per_group / 2)), (anim_per_group - floor(n_keep / 2) + 1):anim_per_group)
  } else if(replacement_aim == "half" & is.integer(n_keep / 2)) {
    #if the amount of sentinel animals is even, get only the extremes
    sentinel_animals_selection %>% filter(trial_set_description == "Control") %>%
      arrange(desc(INITBW)) %>%
      slice(1:floor(n_keep / 2), (anim_per_group - floor(n_keep / 2) + 1):anim_per_group)
  }
  
  #select sentinel animals of the actual observed parameter based on the USUBJIDs
  sentinel_animals <- studydata_sex %>% filter(USUBJID %in% sentinel_animals_USUBJIDs$USUBJID)
  
  #Get all animals remaining control-group animals which are not assigned to
  #be sentinel animals. These are important for later visualization.
  ccgs_to_be_removed <- studydata_sex %>% filter(
    trial_set_description == "Control",
    !USUBJID %in% sentinel_animals$USUBJID
    )
  #*****************************************************************************
  #*****************************************************************************
  #*Apply selected ranges of sentinel animals to the VCG sample population------
  #*****************************************************************************  
  #get a vector of all values you want to sample from
  sampling_group_with_duplicates <- vcg_population %>%
    filter(
      SEX == sex,
      !is.na(LBORRES),
      ANESTHETICS %in% narcosis_sample,
      LBTESTCD %in% electrolyte
    ) %>%
    #create an identifer to filter out duplicates in next step
    mutate(dup_identifier = paste0(USUBJID, LBTESTCD, LBDY))
  
  #In the set, animal values from different laboratory days might be present.
  #In this case, take the one values closest to the LBDY of the values from
  #the legacy study.
  #Create a list of values which will be removed from the sample population
  check_duplicates <- sampling_group_with_duplicates %>%
    group_by(USUBJID, LBTESTCD) %>%
    summarize(
      population = n(),
      LBDY = LBDY,
      dup_identifier = dup_identifier
    ) %>%
    filter(
      population > 1
    ) %>%
    summarise(
      LBDY = nth(LBDY, which.max(abs(LBDY - studydata_sex %>% pull(LBDY) %>% min()))),
      dup_identifier = dup_identifier
      ) %>%
    pull(dup_identifier)
  
  #Remove the values from the sampling group from the duplicates list
  sampling_group <- sampling_group_with_duplicates %>%
    filter(!dup_identifier %in% check_duplicates) %>%
    select(-dup_identifier, ANESTHETICS, LBORRES, USUBJID, LBTESTCD)
  
  #*Get the ranges in which you want to filter down your LB-parameters.
  #*If sentinel animals are kept in the ccg, their value distribution is used
  #*to estimate a range in which VCG-values are going to be recruited. Right now,
  #*the mean +- 2*sd is used as a range but this can be adjusted here in future.
  #*If no sentinel animals were selected, all animals from the VCG samples are
  #*selected (i.e. the numerical ranges are the limits)
  #get upper limit
  sentinel_ul <- if(replacement_aim == "all"){
    sampling_group %>%
      filter(LBTESTCD == "CA") %>%
      pull(LBORRES) %>%
      max()
  } else {
    mean(sentinel_animals %>% filter(LBTESTCD == "CA") %>% pull(LBORRES)) +
      2 *
      sd(sentinel_animals %>% filter(LBTESTCD == "CA") %>% pull(LBORRES))
  }
  
  #get lower limit
  sentinel_ll <- if(replacement_aim == "all"){
    sampling_group %>%
      filter(LBTESTCD == "CA") %>%
      pull(LBORRES) %>%
      min()
  } else {
    mean(sentinel_animals %>% filter(LBTESTCD == "CA") %>% pull(LBORRES)) -
      2 *
      sd(sentinel_animals %>% filter(LBTESTCD == "CA") %>% pull(LBORRES))
  }
  #*****************************************************************************
  #*Keep only individual animals in the VCG sample population who match the
  #*filters set for the CA values
  vcg_sample_population_matching_individuals <- if(replacement_aim == "all"){
    sampling_group %>%
      pull(USUBJID)
    }else{
    sampling_group %>%
    filter(
      LBTESTCD == "CA",
      between(LBORRES, sentinel_ll, sentinel_ul)
      ) %>%
    pull(USUBJID)
    }
  
  vcg_sample_population <- sampling_group %>%
    filter(USUBJID %in% vcg_sample_population_matching_individuals)
  
  #*****************************************************************************
  #*****************************************************************************
  #*Calculate Dunnett's test after resampling-----------------------------------
  #*****************************************************************************
  #make a loop where you sample from the selected subset and recalculate the Dunnett test
  collected_vcg_samples <- list()
  resampling_dunnett_results <- list()
  
  #*sample n animals from the sampling group with n being the number of animals per group
  #*make a stratified sampling so each group is equally represented in the sample
  for (i in 1:iterations) {
    #*Calculate the number of filtered VCG animals.
    #*If the number is smaller than the CCG-group, in at least one of the
    #*subgroups, a warning message will be returned.
    vcg_sampling_function <- function(){
      if(
        nrow(vcg_sample_population %>% filter(LBTESTCD == "CA")) < (anim_per_group - n_keep)
      ){
        warning(
          paste0(
            "Not enough animal data within the selected range.
        Out of the required ", anim_per_group - n_keep, " VCGs, ",
            nrow(vcg_sample_population %>% filter(LBTESTCD == "CA")),
            " VCGs can be derived."
          )
        )
        all_values <- vcg_sample_population
        return(all_values)
      }else{
        #Get equal size from each group. If number is not adding up, in the next
        #step, values are going to be removed randomly
        USUBJIDs <- vcg_sample_population %>%
          filter(LBTESTCD == "CA") %>%
          slice_sample(., n = (anim_per_group - n_keep), replace = F) %>%
          pull(USUBJID)
        
        drawn_values <- vcg_sample_population %>%
          filter(
            USUBJID %in% USUBJIDs,
            LBTESTCD %in% electrolyte,     
            )
        return(drawn_values)
      }
    }
    vcg_sample <- vcg_sampling_function()
    
    #Add the values of the sentinel animals to the set
    vcg_sample_with_sentinel <- bind_rows(
      vcg_sample %>% select(LBORRES, USUBJID, LBTESTCD),
      sentinel_animals %>% select(LBORRES, USUBJID, LBTESTCD)
      ) %>%
      mutate(trial_set_description = "Control")
    
    #sample(sampling_group, anim_per_group, replace = FALSE)
    collected_vcg_samples[[i]] <- vcg_sample_with_sentinel
  }

  #*****************************************************************************
  #*****************************************************************************
  #*Replace CCG of legacy study with VCGs---------------------------------------
  #*****************************************************************************
  for (i in 1:iterations) {
    #Replace control group of study with VCG
    studydata_with_vcgs <- studydata_sex %>%
      filter(trial_set_description != "Control") %>%
      bind_rows(collected_vcg_samples[[i]])
    
    # print(studydata_with_vcgs %>% filter(trial_set_description == "Control") %>% pull(LBORRES) %>% mean())
    
    #Transform the data frame into a nested list, same as with the CCGs
    #***************************************************************************
    #*Turn data frame into a nested list of the following structure:
    #* 1) each list contains the values of each observed endpoint
    #* 2) within this list, all values are split into respective dose groups
    studydata_vcg_list_by_endpoint <- split.data.frame(
      studydata_with_vcgs,
      studydata_with_vcgs$LBTESTCD
      )
    
    studydata_vcg_nested_list <- lapply(
      studydata_vcg_list_by_endpoint, function(x) split(x$LBORRES, x$trial_set_description)
    )
    #***************************************************************************
    #Calculate Dunnett's test with virtual control group
    vcg_dunnett <- lapply(studydata_vcg_nested_list, DunnettTest)
    
    #Extract the results of the Dunnett's test
    vcg_dunnett_res <- lapply(vcg_dunnett, function(x) as_tibble(x[[1]]))
    
    #Attach dose group names to the results of the Dunnett's test
    vcg_dunnett_res_dose_names <- lapply(
      vcg_dunnett_res,
      function(x) {
        x %>%
          mutate(
            dose_group = paste0(
              "Dose group ",
              sapply(vcg_dunnett_res, function(x) seq_along(1:nrow(x)))[,1])
          )
      }
    )
    
    #***************************************************************************
    #Attach description to the p-val results and the direction of the significance----
    #***************************************************************************
    vcg_significance <- lapply(
      vcg_dunnett_res_dose_names,
      function(x) {
        x %>%
          mutate(
            #flag if p-value is below 0.05 with "s", otherwise "ns"
            vcg_significance = case_when(
              pval < 0.05 ~ "s",
              TRUE ~ "ns"),
            #state in which direction the difference goes in comparison to the CG
            vcg_direction = case_when(
              diff < 0 ~ "decrease",
              diff > 0 ~ "increase",
              TRUE ~ "no_change"
            )
          )
      }
    )
    
    #store results from each iteration in list
    resampling_dunnett_results[[i]] <- vcg_significance#iteration_results
  }
  #end of loop
  #*****************************************************************************
  #*****************************************************************************
  #*Collect all results from the resampling approach----------------------------
  #*****************************************************************************
  #get location parameters from VCGs of each iteration per electrolyte
  vcg_location_parameters_list <- lapply(
    collected_vcg_samples,
    function(x) x %>%
        select(LBORRES, LBTESTCD) %>%
        group_by(LBTESTCD) %>%
        summarize(
          vcg_mean = mean(LBORRES),
          vcg_sd = sd(LBORRES),
          vcg_population = n()
        )
  )
  
  #transform list into tibble and attatch iteration number
  vcg_location_parameters <- map_df(vcg_location_parameters_list, as_tibble) %>%
    mutate(
      iteration = paste0(
        rep(
          1:iterations,
          each = n_distinct(electrolyte)
        )
      )
    )
  
  vcg_results_flatten <- lapply(
    resampling_dunnett_results,
    function(x) map_df(x, as_tibble) %>%
      mutate(
        LBTESTCD = paste0(
          rep(
            names(studydata_nested_list),
            each = studydata_sex %>% filter(trial_set_description != "Control") %>% pull(trial_set_description) %>% n_distinct()
              )
          )
        )
    )
  
  vcg_results <- map_df(vcg_results_flatten, as_tibble) %>%
    mutate(
      iteration = paste0(
        rep(
          1:iterations,
          each = studydata_sex %>% filter(trial_set_description != "Control") %>% pull(trial_set_description) %>% n_distinct() * n_distinct(electrolyte)
          )
        ),
      #Add an identifier for later joining to the VCG results
      joiner_id = paste(LBTESTCD, dose_group, sep = "_")
    ) %>%
    rename(
      vcg_diff = diff,
      vcg_lwr.ci = lwr.ci,
      vcg_upr.ci = upr.ci,
      vcg_pval = pval
    )
  
  #*****************************************************************************
  #*****************************************************************************
  #*Sort results by consistency-------------------------------------------------
  #*****************************************************************************
  #Get all vcg iterations leading to a result consistent with original result (along with correct direction)
  #*If the results are significant, it also plays a role whether the dose group
  #*significantly increased or decreased compared to the VCG.
  #*If there is no significant difference, the direction does not play a role.
  
  #Join the ccg_significance and the ccg_direction to the results table
  vcg_and_ccg_results <- merge(vcg_results, ccg_results %>% select(-dose_group, -LBTESTCD), by = "joiner_id") %>%
    select(-joiner_id) %>%
    #Add VCG location parameters of each VCG iteration to the table
    left_join(vcg_location_parameters, by = c("LBTESTCD", "iteration"))
  #*****************************************************************************
  consistency_flagged_results <- vcg_and_ccg_results %>%
    mutate(
      consistency_flag = case_when(
        #*The results are consistent with the legacy study if both are not
        #*significant or both are significant and the direction is the same.
        vcg_significance == "ns" & ccg_significance == "ns" ~ "con",
        (vcg_significance == "s" & ccg_significance == "s") & (vcg_direction == ccg_direction) ~ "con",
        #*Not significant results while CCG results are significant are inconsistently non significant
        vcg_significance == "ns" & ccg_significance == "s" ~ "incon_non_sig",
        #*Significant results while CCG results are not significant are inconsistently significant
        vcg_significance == "s" & ccg_significance == "ns" ~ "incon_sig",
        #*If both are significant but the direction is wrong the results are inverse significant
        (vcg_significance == "s" & ccg_significance == "s") & (vcg_direction != ccg_direction) ~ "inv_sig",
      )
    )
  #*****************************************************************************
  #*****************************************************************************
  #*Calculate consistency percentage--------------------------------------------
  #*****************************************************************************
  #*Calculate the percentage with respect to each dose group and each observed
  #*endpoint which were leading to a consistent (or inconsistent) result
  #*respectively.
  consistency_percentage <- consistency_flagged_results %>%
    group_by(LBTESTCD, dose_group, consistency_flag) %>%
    summarize(
      percentage = round(n() / iterations * 100)
    )
  #*****************************************************************************
  #*Append zero consistencies.
  #*If there is a complete dose group missing (e.g. Dose group 1 had absolutely
  #*no values leading to a consistent result), the row needs to be added
  #*manually with a consistency percentage of 0
  
  #*Create a data frame with zeros as their consistencies
  consistency_percentage_with_zeros <- tibble(
    LBTESTCD = rep(
      names(studydata_nested_list),
      each = studydata_sex %>%
        filter(trial_set_description != "Control") %>%
        pull(trial_set_description) %>%
        n_distinct() *
        4
      ),
    dose_group = rep(
      studydata_sex %>%
        filter(trial_set_description != "Control") %>%
        pull(trial_set_description) %>%
        unique(),
      n_distinct(electrolyte),
      each = 4
      ),
    consistency_flag = rep(
      c("con", "incon_sig", "incon_non_sig", "inv_sig"),
      studydata_sex %>%
        filter(trial_set_description != "Control") %>%
        pull(trial_set_description) %>%
        n_distinct() *
        n_distinct(electrolyte)
      )
  ) %>%
    #Add the consistency percentage results by a left join and fill NA values with zeros
    left_join(consistency_percentage, by = c("LBTESTCD", "dose_group", "consistency_flag")) %>%
    mutate(percentage = replace_na(percentage, 0))
  
  #*****************************************************************************
  #*****************************************************************************
  #*Collect all results into a list which is returned in this function----------
  #*****************************************************************************
  res_list <- list(
    #vcg_individual_values,
    consistency_flagged_results,
    consistency_percentage_with_zeros,
    sampling_group,
    sentinel_animals,
    ccgs_to_be_removed
    )
  
  names(res_list) <- c(
    #"all_VCGs",
    "res_all",
    "res_vs_original",
    "sampling_group",
    "sentinel_animals",
    "ccgs_to_be_removed"
  )
  return(res_list)
}
