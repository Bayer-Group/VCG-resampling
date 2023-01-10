#*********************************************************************************;
#* Program Identification ***********************    RCSS / Bayer AG             **;
#*;
# clear the environment first
# remove (almost) everything in the working environment
rm(list = ls(all.names = TRUE))
# name of the program
program <- "00_master.R"
#* Version:           <10 [2022_12_29]>;
#* Project:           <ViCoG>;
#* Author:            <Alexander Gurjanov>;
#*;
#*********************************************************************************;
#* Program Description  **********************************************************;
#*;
#*This program is the execution file of the calculation and visualization steps
#*of the remaining R-scripts.
#*The functions of the R-scripts are executed here with respect
#*to the entered values.
#*;
#*********************************************************************************;
#*********************************************************************************;
#*********************************************************************************;
#* Program Specification  ********************************************************;
#*;
#* This program reads and executes the function from all other R-scripts:
#* "02_make_dunnett_test.R" to excecute the resampling approach and (re)calculate
#*  the Dunnett's test,
#* "03_apply_dunnett_test_to_legacy_study.R" to apply the Dunnett's test to the
#* legacy study using a resampling approach with respect to the selected scenarios.
#* The results are then visualized by the following scripts:
#* "04_visualize_electrolytes.R" to plot the distributions of the electrolyte values
#* "05_visualize_scenarios.R" to plot the CC animals and show which ones are kept
#* as sentinel animals.
#* "06_visualize_picked_iterations.R" to plot the results of a single handpicked
#* iteration of the resampling experiment.
#* "07_visualize_resampling_results.R" to plot all results of all iterations of
#* the resampling experiment.;
#* <input:  Electrolyte values of the VCG data set and legacy study> ;
#* <steps:  calculate Dunnett's test using CCG and with VCGs. Then visualize result> ;
#* <output: Plots illustrating distribution of electrolyte values of the
#*          VCG data set and the resampling approach.> ;
#*;
#*********************************************************************************;
#*********************************************************************************;
# retrieve the current study path
rootpath <- sub("*\\/Programs", "",dirname(rstudioapi::getSourceEditorContext()$path))

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
library(data.table)
library(tidyverse)
library(DescTools)
library(plotly)
library(webshot)
#*******************************************************************************
#Create a function doing a resampling approach and calculating the Dunnett's test
source(paste0(rootpath, "/Programs/02_make_dunnett_test.R"))
#Source the script in order to obtain the resampling_results
source(paste0(rootpath, "/Programs/03_apply_dunnett_test_to_legacy_study.R"))
#*******************************************************************************
#Source the visualization scripts:
#The plots will be stored in the "Results"-folder as HTML files and as JPEGs
#Get the visualizations of the electrolytes
source(paste0(rootpath, "/Programs/04_visualize_electrolytes.R"))
#Get the function which visualizes the scenarios
source(paste0(rootpath, "/Programs/05_visualize_scenarios.R"))
#Get the function which visualizes results from certain selected iterations
source(paste0(rootpath, "/Programs/06_visualize_picked_iterations.R"))
#Get the function which visualizes the results of the previous script
source(paste0(rootpath, "/Programs/07_visualize_resampling_results.R"))
#Visualize all results using the visualization master script
source(paste0(rootpath, "/Programs/01_visualization_master.R"))
#*******************************************************************************
##### program ends here
save.image(file = paste(rootpath,"\\Programs\\",program,".RData",sep=""))
#* 
#* DO NOT EDIT BELOW THESE LINES
#*
savehistory(file = paste(rootpath,"\\Programs\\",program,".Rhistory",sep=""))
sessionInfo()
