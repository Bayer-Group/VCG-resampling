#*********************************************************************************;
#* Program Identification ***********************    RCSS / Bayer AG             **;
#*;
# clear the environment first
# remove (almost) everything in the working environment
rm(list = ls(all.names = TRUE))
# name of the program
program <- "08_statistical_evaluation_of_VCG_sample_population.R"
#* Version:           <1 [2023_02_21]>;
#* Project:           <VCG>;
#* Author:            <Alexander Gurjanov>;
#*;
#*********************************************************************************;
#* Program Description  **********************************************************;
#*;
#*This program evaluates the whether samples from the VCG sample population might
#*lead to the same statistical results as between the highest dose group and
#*the concurrent control (CCG) of the legacy study. More details below.
#*;
#*********************************************************************************;
#*********************************************************************************;
#*********************************************************************************;
#* Program Specification  ********************************************************;
#*;
#* This program takes the laboratory parameter values of the sample population
#* extracted by the experiment and extracts the n highest and lowest LB values.
#* On these, a two sample t-test is performed and the distance in means and CI is
#* extracted.
#* The sample population is further split into two categories:
#* a population where a confounder is present and a population where the values
#* affected by the confounder were removed.
#* Next, the LB values from the legacy study are extracted, namely from the highest
#* dose group and the CCG and the same t-test is calculated on these values.
#* The results of the two t-tests are compared.;
#* <input:  VCG sample population with a present confounder
#*          VCG sample population where the values affected by the confounder were removed
#*          control group values and highest-dose group values of the legacy study> ;
#* <steps:  calculate two-sample t-test between all and compare the results> ;
#* <output: Results of the two-sample t-test, i.e. p-value and distance in means> ;
#*;
#*********************************************************************************;
#*********************************************************************************;
# retrieve the current study path
rootpath <- sub("*\\/man", "",dirname(rstudioapi::getSourceEditorContext()$path))

# path to folder for results / graphs / derived data sets
path_res <- paste(rootpath,'/Results', sep='')
#* 
#* DO NOT EDIT BELOW THESE LINES
#*

# path to folder derived data sets
der <- paste(rootpath,'/Data/Derived', sep='')

cat("---------------------",program,"---------------------","\n\n")
cat(date(),"\n\n")
cat("\n",version$version.string,"\n\n")

setwd(path_res)

cat("\n Results will be stored in folder:",getwd()," \n")

#* 
#* DO NOT EDIT ABOVE THESE LINES
#*

##### program starts here
#******************************************************************************
#******************************************************************************
#******************************************************************************
#Load libraries
library(tidyverse)
library(plotly)
library(DescTools)
library(webshot)
#*******************************************************************************
#Extract the parameters stired in the Data/Derived folder:
#Of the VCG sample population (confounder present)
sample_population_con_pres <- read.csv(paste0(der, "/electrolyte_parameters_4wk_rat.csv")) %>%
  #summarize "CO2/air" and "CO2/O2" to "CO2"
  mutate(ANESTHETICS = case_when(
    ANESTHETICS == "CO2/air" ~ "CO2",
    ANESTHETICS == "CO2/O2" ~ "CO2",
    ANESTHETICS == "isoflurane" ~ "isoflurane"
  )) %>%
  filter(
    #remove the legacy study from the sample population
    STUDYID != "T103901-4",
    #select calcium as the observed parameter
    LBTESTCD == "CA",
    #only male rats are observed
    SEX == "M"
  )

#Of the legacy study
legacy_study_groups <- read.csv(paste0(der, "/legacy_calcium_study.csv"))%>%
  #rename the dose groups into control group (control), Dose group 1 (low dose),
  #Dose group 2 (mid dose), and Dose group 3 (high dose)
  mutate(
    trial_set_description = as.character(trial_set_description),
    trial_set_description = case_when(endsWith(trial_set_description, "1S1") ~ "Control",
                                           endsWith(trial_set_description, "1S2") ~ "Control",
                                           endsWith(trial_set_description, "2S1") ~ "Dose group 1",
                                           endsWith(trial_set_description, "2S2") ~ "Dose group 1",
                                           endsWith(trial_set_description, "3S1") ~ "Dose group 2",
                                           endsWith(trial_set_description, "3S2") ~ "Dose group 2",
                                           endsWith(trial_set_description, "4S1") ~ "Dose group 3",
                                           endsWith(trial_set_description, "4S2") ~ "Dose group 3"),
                                   )
#*******************************************************************************
#take the 10 highest values of the sample
highest_values_of_sample_pop_con_pres <- sample_population_con_pres %>%
  arrange(desc(LBORRES)) %>%
  slice(1:10) %>%
  pull(LBORRES)

#take the 10 lowest values of the sample
lowest_values_of_sample_pop_con_pres <- sample_population_con_pres %>%
  arrange(LBORRES) %>%
  slice(1:10) %>%
  pull(LBORRES)
#******************************************************************************
#*Create another data frame for the VCG sample population where the values affected
#*by the confounder were removed (i.e., remove values from animals anesthetized
#*with CO2 instead of isoflurane)
sample_population_con_rem <- sample_population_con_pres %>% filter(ANESTHETICS == "isoflurane")

#take the 10 highest values of the sample
highest_values_of_sample_pop_con_rem <- sample_population_con_rem %>%
  arrange(desc(LBORRES)) %>%
  slice(1:10) %>%
  pull(LBORRES)

#take the 10 lowest values of the sample
lowest_values_of_sample_pop_con_rem <- sample_population_con_rem %>%
  arrange(LBORRES) %>%
  slice(1:10) %>%
  pull(LBORRES)
#******************************************************************************
#*Create a third data frame for the VCG sample population where the values affected
#*where the VCG sample population is filtered to match the mean +- 2*sd values
#*of the CA parameters of the legacy study. 5 sentinel animals are kept and these
#*are selected by body weight. I.e., the 2 heaviest, the 2 lightest and 1 middle
#*rat is kept in the set.

#*Extract the 2 heaviest, 2 lightest and 1 middle animal from the CCG set of the
#*legacy study
#*CAUTION: This code only works if 10 control group animals are present in 
#*the set! You might consider to change the slice()-statement to match the
#*set more properly.
sentinel_animals_filterrange <- legacy_study_groups %>%
  filter(
    LBTESTCD == "CA",
    SEX == "M",
    trial_set_description == "Control"
  ) %>%
  arrange(desc(INITBW)) %>%
  slice(1,2,5,9,10) %>%
  #get the ranges by mean +- 2 * sd from the set
  summarise(
    minval = mean(LBORRES) - 2 * sd(LBORRES),
    maxval = mean(LBORRES) + 2 * sd(LBORRES)
  )

#filter sample population to match the selected range filters
sample_population_sent_pres <- sample_population_con_pres %>%
  filter(between(LBORRES, sentinel_animals_filterrange[1], sentinel_animals_filterrange[2]))

#take the 10 highest values of the sample
highest_values_of_sample_pop_sent_ani <- sample_population_sent_pres %>%
  arrange(desc(LBORRES)) %>%
  slice(1:10) %>%
  pull(LBORRES)

#take the 10 lowest values of the sample
lowest_values_of_sample_pop_sent_ani <- sample_population_sent_pres %>%
  arrange(LBORRES) %>%
  slice(1:10) %>%
  pull(LBORRES)

#*******************************************************************************
#*Select the control group values and the highest dose-group values of the
#*legacy study
#Select the control group CA values of the legacy study
legacy_study_control <- legacy_study_groups %>%
  filter(LBTESTCD == "CA", trial_set_description == "Control", SEX == "M") %>% pull(LBORRES)

legacy_study_dose_3 <- legacy_study_groups %>%
  filter(LBTESTCD == "CA", trial_set_description == "Dose group 3", SEX == "M") %>% pull(LBORRES)
#*******************************************************************************
#*******************************************************************************
#Calculate the effect sizes-----------------------------------------------------
#*******************************************************************************
#Between the 10 highest and 10 lowest values of the VCG sample population with
#the confounder present:
effect_size_sample_pop_con_pres <- CohenD(
  highest_values_of_sample_pop_con_pres,
  lowest_values_of_sample_pop_con_pres
)
#Between the 10 highest and 10 lowest values of the VCG sample population with
#the confounder removed:
effect_size_sample_pop_con_rem <- CohenD(
  highest_values_of_sample_pop_con_rem,
  lowest_values_of_sample_pop_con_rem
)
#Between the 10 highest and 10 lowest values of the VCG sample population with
#sentinel animals as an additional filter range:
effect_size_sample_pop_sent_ani <- CohenD(
  highest_values_of_sample_pop_sent_ani,
  lowest_values_of_sample_pop_sent_ani
)
#Between the control group and Dose group 3 of the legacy study:
effect_size_legacy_study <- CohenD(
  legacy_study_dose_3,
  legacy_study_control
)
#*******************************************************************************
#*******************************************************************************
#Calculate the two-sample t-tests-----------------------------------------------
#*******************************************************************************
#Between the 10 highest and 10 lowest values of the VCG sample population with
#the confounder present:
t_test_sample_pop_con_pres <- t.test(
  highest_values_of_sample_pop_con_pres, lowest_values_of_sample_pop_con_pres,
  var.equal = T
)
#Between the 10 highest and 10 lowest values of the VCG sample population with
#the confounder removed:
t_test_sample_pop_con_rem <- t.test(
  highest_values_of_sample_pop_con_rem, lowest_values_of_sample_pop_con_rem,
  var.equal = T
)
#Between the 10 highest and 10 lowest values of the VCG sample population with
#the sentinel animals as an additional filter:
t_test_sample_pop_sent_ani <- t.test(
  highest_values_of_sample_pop_sent_ani, lowest_values_of_sample_pop_sent_ani,
  var.equal = T
)
#Between the control group and Dose group 3 of the legacy study:
t_test_legacy_study <- t.test(
  legacy_study_dose_3, legacy_study_control,
  var.equal = T
)

#*******************************************************************************
#*******************************************************************************
#*Visualize results as scatter plots
#*******************************************************************************
#Customize lower range of y-axis with respect to the selected electrolyte
distance_plot <- plot_ly() %>%
  #Make these dummy-traces in order to have a visible legend
  add_trace(
    type = "scatter",
    mode = "markers",
    x = "legacy study",
    y = -5,
    marker = list(
      color = "#000000",
      symbol = "cross",
      line = list(color = "#000000"),
      opacity = 1
      ),
    size = 10,
    name = "VCG sample population: 10 highest values"
    ) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    x = "legacy study",
    y = -5,
    marker = list(
      color = "#ffffff",
      opacity = 1,
      line = list(color = "#000000", width = 2)
      ),
    size = 10,
    name = "VCG sample population: 10 lowest values"
  ) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    x = "legacy study",
    y = -5,
    marker = list(
      color = "#000000",
      opacity = 1,
      symbol = "triangle-up",
      line = list(color = "#000000")
      ),
    size = 10,
    name = "legacy study: Dose group 3"
  ) %>%
  add_trace(
    type = "scatter",
    mode = "markers",
    x = "legacy study",
    y = -5,
    marker = list(
      color = "#ffffff",
      opacity = 1,
      line = list(color = "#000000", width = 2),
      symbol = "square"
      ),
    size = 10,
    name = "legacy study: CCG"
  ) %>%
  #*****************************************************************************
  #*****************************************************************************
  #*Add the scattered points----------------------------------------------------
  #*****************************************************************************
  #Confounder present
  add_boxplot(
    x = "confounder present",
    y = highest_values_of_sample_pop_con_pres,
    marker = list(
      color = "#000000",
      symbol = "cross",
      size = 10
      ),
    boxpoints = "all",
    jitter = 1,
    pointpos = 0,
    name = "VCG sample population",
    fillcolor = "rgba(255,255,255,0)",
    line = list(color = "rgba(255,255,255,0)"),
    showlegend = F
  ) %>%
  add_boxplot(
    x = "confounder present",
    y = lowest_values_of_sample_pop_con_pres,
    marker = list(
      color = "#ffffff",
      line = list(color = "#000000", width = 2),
      size = 10
      ),
    boxpoints = "all",
    jitter = 1,
    pointpos = 0,
    name = "VCG sample population",
    fillcolor = "rgba(255,255,255,0)",
    line = list(color = "rgba(255,255,255,0)"),
    showlegend = F
  ) %>%
  #*****************************************************************************
  #Confounder filtered
  add_boxplot(
    x = "confounder removed",
    y = highest_values_of_sample_pop_con_rem,
    marker = list(
      color = "#000000",
      symbol = "cross",
      size = 10
      ),
    boxpoints = "all",
    jitter = 1,
    pointpos = 0,
    name = "VCG sample population",
    fillcolor = "rgba(255,255,255,0)",
    line = list(color = "rgba(255,255,255,0)"),
    showlegend = F
  ) %>%
  add_boxplot(
    x = "confounder removed",
    y = lowest_values_of_sample_pop_con_rem,
    marker = list(
      color = "#ffffff",
      line = list(color = "#000000", width = 2),
      size = 10
      ),
    boxpoints = "all",
    jitter = 1,
    pointpos = 0,
    name = "VCG sample population",
    fillcolor = "rgba(255,255,255,0)",
    line = list(color = "rgba(255,255,255,0)"),
    showlegend = F
  ) %>%
  #*****************************************************************************
  #*****************************************************************************
  #Legacy study
  add_boxplot(
    x = "legacy study",
    y = legacy_study_dose_3,
    marker = list(
      color = "#000000",
      symbol = "triangle-up",
      size = 10
      ),
    boxpoints = "all",
    jitter = 1,
    pointpos = 0,
    name = "legacy study Dose group 3",
    fillcolor = "rgba(255,255,255,0)",
    line = list(color = "rgba(255,255,255,0)"),
    showlegend = F
  ) %>%
  add_boxplot(
    x = "legacy study",
    y = legacy_study_control,
    marker = list(
      color = "#ffffff",
      line = list(color = "#000000", width = 2),
      symbol = "square",
      size = 10
      ),
    boxpoints = "all",
    jitter = 1,
    pointpos = -.2,
    name = "legacy study control group",
    fillcolor = "rgba(255,255,255,0)",
    line = list(color = "rgba(255,255,255,0)"),
    showlegend = F
  ) %>%
  #*****************************************************************************
  #*Add annotations to mark difference in means and 95 % CI
  #*****************************************************************************
  #*confounder present
  add_annotations(
    text = ~paste0(
      # (signif(mean(highest_values_of_sample_pop_con_pres) - mean(lowest_values_of_sample_pop_con_pres), 3)),
      "95 % CI [",
      signif(t_test_sample_pop_con_pres[["conf.int"]][1], 3),
      "; ",
      signif(t_test_sample_pop_con_pres[["conf.int"]][2], 3),
      "]"
    ),
    size = 8,
    x = 0.03,
    xanchor = "top",
    xref = "paper",
    y = mean(lowest_values_of_sample_pop_con_pres) + (mean(highest_values_of_sample_pop_con_pres) - mean(lowest_values_of_sample_pop_con_pres)) / 2,
    yanchor = "center",
    textangle = 270,
    showarrow = FALSE
  ) %>%
  #*****************************************************************************
  #*confounder removed
  add_annotations(
    text = ~paste0(
      # (signif(mean(highest_values_of_sample_pop_con_rem) - mean(lowest_values_of_sample_pop_con_rem), 3)),
      "95 % CI [",
      signif(t_test_sample_pop_con_rem[["conf.int"]][1], 3),
      "; ",
      signif(t_test_sample_pop_con_rem[["conf.int"]][2], 3),
      "]"
    ),
    size = 8,
    x = .37,
    xanchor = "top",
    xref = "paper",
    y = mean(lowest_values_of_sample_pop_con_rem) + (mean(highest_values_of_sample_pop_con_rem) - mean(lowest_values_of_sample_pop_con_rem)) / 2,
    yanchor = "center",
    textangle = 270,
    showarrow = FALSE
  ) %>%
  #*****************************************************************************
  #*****************************************************************************
  #*legacy study
  add_annotations(
    text = ~paste0(
      # (signif(mean(highest_values_of_sample_pop_con_rem) - mean(lowest_values_of_sample_pop_con_rem), 3)),
      "95 % CI [",
      signif(t_test_legacy_study[["conf.int"]][1], 2),
      "; ",
      signif(t_test_legacy_study[["conf.int"]][2], 3),
      "]"
    ),
    size = 8,
    x = .73, #if 4 groups: 0.75
    xanchor = "top",
    xref = "paper",
    y = mean(legacy_study_control) + (mean(legacy_study_dose_3) - mean(legacy_study_dose_3)) / 2,
    yanchor = "center",
    textangle = 270,
    showarrow = FALSE
  ) %>%
  layout(
    font = list(family = "times new roman", size = 22),
    #add lines to show difference in means between each group
    shapes = list(
      #*************************************************************************
      #*confounder present
      list(
        type = "line",
        x0 = .07,
        x1 = .07,
        xref = "paper",
        y0 = mean(lowest_values_of_sample_pop_con_pres),
        y1 = mean(highest_values_of_sample_pop_con_pres),
        line = list(color = "#000000")
      ),
      list(
        type = "line",
        x0 = .07,
        x1 = .09,
        xref = "paper",
        y0 = mean(lowest_values_of_sample_pop_con_pres),
        y1 = mean(lowest_values_of_sample_pop_con_pres),
        line = list(color = "#000000")
      ),
      list(
        type = "line",
        x0 = .07,
        x1 = .09,
        xref = "paper",
        y0 = mean(highest_values_of_sample_pop_con_pres),
        y1 = mean(highest_values_of_sample_pop_con_pres),
        line = list(color = "#000000")
      ),
      #*************************************************************************
      #*confounder removed
      list(
        type = "line",
        x0 = .4,
        x1 = .4,
        xref = "paper",
        y0 = mean(lowest_values_of_sample_pop_con_rem),
        y1 = mean(highest_values_of_sample_pop_con_rem),
        line = list(color = "#000000")
      ),
      list(
        type = "line",
        x0 = .4,
        x1 = .42,
        xref = "paper",
        y0 = mean(lowest_values_of_sample_pop_con_rem),
        y1 = mean(lowest_values_of_sample_pop_con_rem),
        line = list(color = "#000000")
      ),
      list(
        type = "line",
        x0 = .4,
        x1 = .42,
        xref = "paper",
        y0 = mean(highest_values_of_sample_pop_con_rem),
        y1 = mean(highest_values_of_sample_pop_con_rem),
        line = list(color = "#000000")
      ),
      #*************************************************************************
      #*************************************************************************
      #*legacy study
      list(
        type = "line",
        x0 = .73,
        x1 = .73,
        xref = "paper",
        y0 = mean(legacy_study_dose_3),
        y1 = mean(legacy_study_control),
        line = list(color = "#000000")
      ),
      list(
        type = "line",
        x0 = .73,
        x1 = .75,
        xref = "paper",
        y0 = mean(legacy_study_dose_3),
        y1 = mean(legacy_study_dose_3),
        line = list(color = "#000000")
      ),
      list(
        type = "line",
        x0 = .73,
        x1 = .75,
        xref = "paper",
        y0 = mean(legacy_study_control),
        y1 = mean(legacy_study_control),
        line = list(color = "#000000")
      )
  ),
    xaxis = list(
      title = "",#"VCG sample population",
      showline = T,
      linewidth = 2,
      ticks = "outside",
      tickwidth = 2,
      ticklen = 10,
      tickangle = 30,
      mirror = T
    ),
    yaxis = list(
      title = "Ca<sup>2+</sup>-concentration in serum [mmol/L]",
      showline = T,
      linewidth = 2,
      ticks = "outside",
      tickwidth = 2,
      ticklen = 10,
      mirror = T,
      range = c(2.1, 3.5)
    )
  )
  
distance_plot
#*******************************************************************************
#*******************************************************************************
#*Export plot-------------------------------------------------------------------
#*******************************************************************************
#export the plot as HTML
htmlwidgets::saveWidget(
  distance_plot,
  paste0(path_res, "/HTML/", deparse(substitute(distance_plot)), ".html")
)
#export the plot as JPEG
webshot(
  paste0(path_res, "/HTML/", deparse(substitute(distance_plot)), ".html"),
  paste0(path_res, "/JPEG/", deparse(substitute(distance_plot)), ".jpeg"),
  vwidth = 1920, vheight = 800, zoom = 2
)
#export plot as PDF
webshot(
  paste0(path_res, "/HTML/", deparse(substitute(distance_plot)), ".html"),
  paste0(path_res, "/PDF/", deparse(substitute(distance_plot)), ".pdf"),
  vwidth = 1920, vheight = 800, zoom = 2
)

##### program ends here
save.image(file = paste(rootpath,"\\Programs\\",program,".RData",sep=""))
#* 
#* DO NOT EDIT BELOW THESE LINES
#*
savehistory(file = paste(rootpath,"\\Programs\\",program,".Rhistory",sep=""))
sessionInfo()
