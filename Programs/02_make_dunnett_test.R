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
#*    selected number) and the respective dose group.

vcg_resampling_dunnett <- function(
  case_study = baycal_ca_levels,
  vcg_population = sample_population,
  sex = "M",
  iterations = 50,
  narcosis_sample = c("isoflurane", "CO2"),
  electrolyte = "CA",
  replacement_aim = "all"
  )
{
  #Check if packages are installed
  require(tidyverse)
  require(DescTools)
  
  #Filter the data you want to observe for selected sex
  studydata_sex <- case_study %>% filter(SEX == sex)
  
  #calculate number of animals in the control group
  anim_per_group <- studydata_sex %>% filter(trial_set_description == "Control") %>% nrow()
  
  #Turn data frame into list divided into respective dose groups
  studydata_list <- split(studydata_sex$LBORRES, studydata_sex$trial_set_description)
  
  #Calculate Dunnett test with concurrent control group
  ccg_dunn_res <- DunnettTest(studydata_list)[[1]]
  #rename the rows with respect to the length of the matrix
  rownames(ccg_dunn_res) <- sapply(1:nrow(ccg_dunn_res), function(i) paste0("Dose group ", i))
  
  #flag if p-value is below 0.05 with "s", otherwise "ns"
  ccg_significance <- ifelse(ccg_dunn_res[,"pval"] < 0.05, "s", "ns")
  #state in which direction the difference goes in comparison to the CG
  ccg_direction <- ifelse(ccg_dunn_res[,"diff"] < 0, "decrease",
                      ifelse(ccg_dunn_res[,"diff"] > 0, "increase", "no_change"))
  
  #get mean, sd, and population and attatch them to results list
  original_means <- do.call(rbind, lapply(studydata_list, mean))
  original_sd <- do.call(rbind, lapply(studydata_list, sd))
  original_population <- do.call(rbind, lapply(studydata_list, length))
  
  original_dunnett_results <- cbind(
    ccg_dunn_res,
    ccg_significance,
    ccg_direction,
    ccg_mean = original_means["Control",],
    ccg_sd = original_sd["Control",],
    ccg_population = original_population["Control",],
    dose_mean = original_means[which(rownames(original_population) != "Control"),],
    dose_sd = original_sd[which(rownames(original_population) != "Control"),],
    dose_population = original_population[which(rownames(original_population) != "Control"),]
    )
  #Make rownames to column
  original_dunnett_results <- cbind(dose_group = rownames(original_dunnett_results), original_dunnett_results)
  
  
  #get a vector of all values you want to sample from
  sampling_group <- vcg_population %>%
    filter(
      SEX == sex,
      !is.na(LBORRES),
      ANESTHETICS %in% narcosis_sample,
      LBTESTCD == electrolyte
    ) %>%
    select(ANESTHETICS, LBORRES)
  
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
  sentinel_animals <- if(replacement_aim == "all") {
    NULL
    } else if(replacement_aim == "all_but_two") {
      studydata_sex %>% filter(trial_set_description == "Control") %>%
        arrange(desc(INITBW)) %>%
        slice(1:(n_keep / 2), (anim_per_group - (n_keep / 2) + 1):anim_per_group)
    } else if(replacement_aim == "half" & !is.integer(n_keep / 2)) {
      #get one animal from the middle if the amount of sentinel animals is odd
      studydata_sex %>% filter(trial_set_description == "Control") %>%
        arrange(desc(INITBW)) %>%
        slice(1:floor(n_keep / 2), (ceiling(anim_per_group / 2)), (anim_per_group - floor(n_keep / 2) + 1):anim_per_group)
    } else if(replacement_aim == "half" & is.integer(n_keep / 2)) {
      #if the amount of sentinel animals is even, get only the extremes
      studydata_sex %>% filter(trial_set_description == "Control") %>%
        arrange(desc(INITBW)) %>%
        slice(1:floor(n_keep / 2), (anim_per_group - floor(n_keep / 2) + 1):anim_per_group)
    }
  #Get all animals remaining control-group animals which are not assigned to
  #be sentinel animals. These are important for later visualization.
  ccgs_to_be_removed <- studydata_sex %>% filter(
    trial_set_description == "Control",
    !USUBJID %in% sentinel_animals$USUBJID
    )
    
  #*Get the ranges in which you want to filter down your LB-parameters.
  #*If sentinel animals are kept in the ccg, their value distribution is used
  #*to estimate a range in which VCG-values are going to be recruited. Right now,
  #*the mean +- 2*sd is used as a range but this can be adjusted here in future.
  #*If no sentinel animals were selected, all animals from the VCG samples are
  #*selected (i.e. the numerical ranges are the limits)
  sentinel_ul <- ifelse(replacement_aim == "all", max(sampling_group$LBORRES), mean(sentinel_animals$LBORRES) + 2 * sd(sentinel_animals$LBORRES))
  sentinel_ll <- ifelse(replacement_aim == "all", min(sampling_group$LBORRES), mean(sentinel_animals$LBORRES) - 2 * sd(sentinel_animals$LBORRES))
  
  #set seed for reproducibility of results
  set.seed(1)
  #make a loop where you sample from the selected subset and recalculate the Dunnett test
  collected_vcg_samples <- list()
  resampling_dunnett_results <- list()
  
  for (i in 1:iterations) {
    #*sample n animals from the sampling group with n being the number of animals per group
    #*make a stratified sampling so each group is equally represented in the sample
    vcg_sample_population <- sampling_group %>% filter(
      between(LBORRES, sentinel_ll, sentinel_ul)
      )
      
    #*Calculate the number of filtered VCG animals.
    #*If the number is smaller than the CCG-group, in at least one of the
    #*subgroups, a warning message will be returned.
    vcg_sample <- if(nrow(vcg_sample_population) < (anim_per_group - n_keep)){
      warning(
      paste0(
      "Not enough animal data within the selected range.
      Out of the required ", anim_per_group - n_keep, " VCGs, ", nrow(vcg_sample_population), " VCGs can be derived."
      ))
      vcg_sample_population %>% pull(LBORRES)
    } else{
      #Get equal size from each group. If number is not adding up, in the next
      #step, values are going to be removed randomly
      vcg_sample_population %>%
        pull(LBORRES) %>%
        sample(., size = anim_per_group - n_keep, replace = F)
    }
    
    #Add the values of the sentinel animals to the set
    vcg_sample_with_sentinel <- c(vcg_sample, sentinel_animals$LBORRES)
    
    #sample(sampling_group, anim_per_group, replace = FALSE)
    collected_vcg_samples[[i]] <- vcg_sample_with_sentinel
  }

  #For some reason it does not work if I do this in one loop. Therfore I split
  #this into two separate loops. One gets the random values the other one calculates
  #the Dunnett test based on these sampled VCGs
  
  for (i in 1:iterations) {
    #Replace control group of study with VCG
    studydata_list[["Control"]] <- collected_vcg_samples[[i]]
    # print(studydata_list[["Control"]])
    #Recalculate the Dunnett test, but now with VCG instead of original CG
    vcg_dunn_res <- DunnettTest(studydata_list)[[1]]
    #rename rows with respect to the length of the resulting matrix of Dunnett test
    rownames(vcg_dunn_res) <- sapply(1:nrow(ccg_dunn_res), function(i) paste0("Dose group ", i))
    
    #flag if p-value is below 0.05 with "s", otherwise "ns"
    vcg_significance <- ifelse(vcg_dunn_res[,"pval"] < 0.05, "s", "ns")
    #state in which direction the difference goes in comparison to the CG
    vcg_direction <- ifelse(vcg_dunn_res[,"diff"] < 0, "decrease",
                        ifelse(vcg_dunn_res[,"diff"] > 0, "increase", "no_change"))
    
    iteration_results <- cbind(
      vcg_dunn_res,
      vcg_significance,
      vcg_direction,
      vcg_mean = mean(collected_vcg_samples[[i]]),
      vcg_sd = sd(collected_vcg_samples[[i]]),
      vcg_population = length(collected_vcg_samples[[i]]),
      iteration_id = paste0("iteration_", i)
      )
    #Maker row names of matrix to column
    iteration_results <- cbind(dose_group = rownames(iteration_results), iteration_results)
    
    #store results from each iteration in list
    resampling_dunnett_results[[i]] <- iteration_results
  }
  
  #get all VCG mean values
  iteration_means <- do.call(rbind, lapply(resampling_dunnett_results, function(x) x[1,c("vcg_mean", "vcg_sd", "iteration_id")])) %>% as_tibble()
  
  #Get all vcg iterations leading to a result consistent with original result (along with correct direction)
  consistent_results <- lapply(resampling_dunnett_results, function(x) x[(x[,"vcg_significance"] == "ns" & original_dunnett_results[,"ccg_significance"] == "ns")
                                                                         | (x[,"vcg_significance"] == original_dunnett_results[,"ccg_significance"] & x[,"vcg_direction"] == original_dunnett_results[,"ccg_direction"]),, drop = F])
  #Get all inconsistent results
  inconsistent_results <- lapply(resampling_dunnett_results, function(x) x[x[,"vcg_significance"] != original_dunnett_results[,"ccg_significance"] | x[,"vcg_direction"] != original_dunnett_results[,"ccg_direction"],, drop = F])
  #Get all iterations leading to inconsistently significant results
  incon_sig <- lapply(resampling_dunnett_results, function(x) x[x[,"vcg_significance"] == "s" & x[,"vcg_significance"] != original_dunnett_results[,"ccg_significance"],, drop = F])
  
  #Get all iterations leading to inconsistently non-significant results
  incon_non_sig <- lapply(resampling_dunnett_results, function(x) x[x[,"vcg_significance"] == "ns" & x[,"vcg_significance"] != original_dunnett_results[,"ccg_significance"],, drop = F])
  
  #Get the iterations leading to inverse significant results (consistently significant but into the wrong direction)
  inv_sig <- lapply(resampling_dunnett_results, function(x) x[x[,"vcg_significance"] == "s" & original_dunnett_results[,"ccg_significance"] == "s" & x[,"vcg_direction"] != original_dunnett_results[,"ccg_direction"],, drop = F])
  
  #get the mean values of all iterations with respect to consistency
  res_con <- do.call(rbind, lapply(consistent_results, function(x) x[,colnames(x)]))
  res_con <- if(length(res_con) != 0) {cbind(res_con, results_type = NA)}
  rownames(res_con) <- NULL
  if(!is.null(res_con)){
    res_con <- as_tibble(res_con) %>%
      arrange(., dose_group, vcg_mean) %>%
      group_by(dose_group) %>%
      mutate(results_row_id = row_number(),
             results_index = paste0("con", row_number()))
  }
  
  res_incon <- do.call(rbind, lapply(inconsistent_results, function(x) x[,colnames(x)]))
  res_incon <- if(length(res_incon) != 0) {cbind(res_incon, results_type = NA)}
  rownames(res_incon) <- NULL
  if(!is.null(res_incon)){
    res_incon <- as_tibble(res_incon) %>%
      arrange(., dose_group, vcg_mean) %>%
      group_by(dose_group) %>%
      mutate(results_row_id = row_number(),
             results_index = paste0("incon", row_number()))
  }
  
  res_incon_sig <- do.call(rbind, lapply(incon_sig, function(x) x[,colnames(x)]))
  res_incon_sig <- if(length(res_incon_sig) != 0) {cbind(res_incon_sig, results_type = "incon_sig")}
  rownames(res_incon_sig) <- NULL
  if(!is.null(res_incon_sig)){
    res_incon_sig <- as_tibble(res_incon_sig) %>%
      arrange(., dose_group, vcg_mean) %>%
      group_by(dose_group) %>%
      mutate(results_row_id = row_number(),
             results_index = paste0("ics", row_number()))
  }
  
  res_incon_non_sig <- do.call(rbind, lapply(incon_non_sig, function(x) x[,colnames(x)]))
  res_incon_non_sig <- if(length(res_incon_non_sig) != 0) {cbind(res_incon_non_sig, results_type = "incon_non_sig")}
  rownames(res_incon_non_sig) <- NULL
  #enumerate rows with respect to dose group and ordered by mean value (from lowest to highest)
  if(!is.null(res_incon_non_sig)){
    res_incon_non_sig <- as_tibble(res_incon_non_sig) %>%
      arrange(., dose_group, vcg_mean) %>%
      group_by(dose_group) %>%
      mutate(results_row_id = row_number(),
             results_index = paste0("ins", row_number()))
  }
  
  res_inv_sig <- do.call(rbind, lapply(inv_sig, function(x) x[,colnames(x)]))
  res_inv_sig <- if(length(res_inv_sig) != 0) {cbind(res_inv_sig, results_type = "inverse_sig")}
  rownames(res_inv_sig) <- NULL
  #enumerate rows with respect to dose group and ordered by mean value (from lowest to highest)
  if(!is.null(res_inv_sig)){
    res_inv_sig <- as_tibble(res_inv_sig) %>%
      arrange(., dose_group, vcg_mean) %>%
      group_by(dose_group) %>%
      mutate(results_row_id = row_number(),
             results_index = paste0("ivs", row_number()))
  }
  
  #get percentage (if the consistency is not NULL)
  consistency_percentage <- if(!is.null(res_con)) {res_con %>% as_tibble() %>% group_by(dose_group) %>% summarize(consistency_percentage = round(n() / iterations * 100))}
  #*If there is a complete dose group missing (e.g. Dose group 1 had absolutely
  #*no values leading to a consistent result), the row needs to be added
  #*manually with a consistency percentage of 0
  append_zero_consistencies <- if(!is.null(res_incon)){res_incon %>% filter(!dose_group %in% consistency_percentage$dose_group) %>% group_by(dose_group) %>% summarize(consistency_percentage = 0)}
  
  consistency_percentage_with_zeros <- {if(nrow(as_tibble(append_zero_consistencies)) != 0) {
    rbind(consistency_percentage, append_zero_consistencies) %>% arrange(dose_group)}
  else {consistency_percentage}}

  #Add the population of the VCGs. The first iteration is taken for that
  n_vcg <- resampling_dunnett_results[[1]][,"vcg_population"]
  
  
  #Add the calculated percentage. Add 0 if there was no consistency at all
  res_vs_original <- if(!is.null(consistency_percentage_with_zeros)){
    merge(original_dunnett_results %>% as_tibble(), consistency_percentage_with_zeros, by = "dose_group")
  } else {
    original_dunnett_results %>% as_tibble() %>% mutate(consistency_percentage = 0)
  }

  #****************************************************************************
  #*Calculate the percentage leading to the respective inconsistency. If no
  #*iterations led to this specific error, add a 0 instead.
  inc_sig_precentage <- if(is.null(res_incon_sig)){res_vs_original %>% select(dose_group) %>% mutate(inc_sig_percentage = 0)}else{res_incon_sig %>% group_by(dose_group) %>% summarize(inc_sig_percentage = round(n() / iterations * 100))}
  inc_non_sig_precentage <- if(is.null(res_incon_non_sig)){res_vs_original %>% select(dose_group) %>% mutate(inc_non_sig_percentage = 0)}else{res_incon_non_sig %>% group_by(dose_group) %>% summarize(inc_non_sig_precentage = round(n() / iterations * 100))}
  inv_sig_percentage <- if(is.null(res_inv_sig)){res_vs_original %>% select(dose_group) %>% mutate(inv_sig_percentage = 0)}else{res_inv_sig %>% group_by(dose_group) %>% summarize(inv_sig_percentage = round(n() / iterations * 100))}
  
  res_vs_original_specified <- res_vs_original %>%
    left_join(inc_sig_precentage, by = "dose_group") %>%
    left_join(inc_non_sig_precentage, by = "dose_group") %>%
    left_join(inv_sig_percentage, by = "dose_group") %>%
    mutate(across(ends_with("percentage"), replace_na, 0)) %>%
    #add the population of the VCGs
    mutate(vcg_population = n_vcg)


  res_list <- list(iteration_means, resampling_dunnett_results, res_con, res_incon_sig, res_incon_non_sig, res_inv_sig, res_vs_original_specified, sampling_group, sentinel_animals, ccgs_to_be_removed)
  names(res_list) <- c("all_VCGs", "res_all", "res_con", "res_incon_sig", "res_incon_non_sig", "res_inv_sig", "res_vs_original", "sampling_group", "sentinel_animals", "ccgs_to_be_removed")
  return(res_list)
}
