#*This program is the execution file of all visualization steps
#*The functions of the R-scripts are executed here with respect
#*to the entered values.
#******************************************************************************
#******************************************************************************
#******************************************************************************
#Load libraries
require(data.table)
require(tidyverse)
require(DescTools)
require(plotly)
require(webshot)
#*******************************************************************************
#*******************************************************************************
#*Visualize the distribution of the selected electrolyte values
#Create the plots which were defined in the batch_plots()-function
#*******************************************************************************
#Do this for potassium
#*******************************************************************************
#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_k_uncolored <- subplot(
  batch_plots(studydata_to_test, "M", "K", "hist_all"),
  batch_plots(studydata_to_test, "M", "K", "box_all"),
  nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T
)
# histbox_of_k_uncolored

#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_k <- subplot(
  batch_plots(studydata_to_test, "M", "K", "hist_narcosis"),
  batch_plots(studydata_to_test, "M", "K", "box_year_narcosis"),
  nrows = 1, shareY = F, shareX = F, titleX = T, titleY = T
)
# histbox_of_k

#A box plot showing the difference in the numerical values with respect to the narcosis
box_of_k_narcosis_colored <- batch_plots(studydata_to_test, "M", "K", "box_narcosis")
# box_of_k_narcosis_colored
#*******************************************************************************
# Do this for calcium
#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_ca_uncolored <- subplot(
  batch_plots(studydata_to_test, "M", "CA", "hist_all"),
  batch_plots(studydata_to_test, "M", "CA", "box_all"),
  nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T
)
# histbox_of_ca_uncolored

#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_ca <- subplot(
  batch_plots(studydata_to_test, "M", "CA", "hist_narcosis"),
  batch_plots(studydata_to_test, "M", "CA", "box_year_narcosis"),
  nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T
)
# histbox_of_ca

#A box plot showing the difference in the numerical values with respect to the narcosis
box_of_ca_narcosis_colored <- batch_plots(studydata_to_test, "M", "CA", "box_narcosis")
# box_of_ca_narcosis_colored
#*******************************************************************************
# Do this for sodium
#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_na_uncolored <- subplot(
  batch_plots(studydata_to_test, "M", "SODIUM", "hist_all"),
  batch_plots(studydata_to_test, "M", "SODIUM", "box_all"),
  nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T
)
# histbox_of_na_uncolored

#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_na <- subplot(
  batch_plots(studydata_to_test, "M", "SODIUM", "hist_narcosis"),
  batch_plots(studydata_to_test, "M", "SODIUM", "box_year_narcosis"),
  nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T
)
# histbox_of_na

#A box plot showing the difference in the numerical values with respect to the narcosis
box_of_na_narcosis_colored <- batch_plots(studydata_to_test, "M", "SODIUM", "box_narcosis")
# box_of_na_narcosis_colored
#*******************************************************************************
# Do this for inorganic phosphate
#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_po4_uncolored <- subplot(
  batch_plots(studydata_to_test, "M", "PHOS", "hist_all"),
  batch_plots(studydata_to_test, "M", "PHOS", "box_all"),
  nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T
)
# histbox_of_po4_uncolored

#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_po4 <- subplot(
  batch_plots(studydata_to_test, "M", "PHOS", "hist_narcosis"),
  batch_plots(studydata_to_test, "M", "PHOS", "box_year_narcosis"),
  nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T
)
# histbox_of_po4

#A box plot showing the difference in the numerical values with respect to the narcosis
box_of_po4_narcosis_colored <- batch_plots(studydata_to_test, "M", "PHOS", "box_narcosis")
# box_of_po4_narcosis_colored
#*******************************************************************************
#*******************************************************************************
#*Substudy: check whether there is a difference between CO2/air and CO2/ot in the
#*electrolytes
#*******************************************************************************
#*Potassium
#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_k_o2vsair <- subplot(
  batch_plots(studydata_to_test, "M", "K", "hist_narcosis", anesthetic1 = "CO2/O2"),
  batch_plots(studydata_to_test, "M", "K", "box_narcosis", anesthetic1 = "CO2/O2"),
  nrows = 1, shareY = F, shareX = F, titleX = T, titleY = T
)
#*Calcium
#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_ca_o2vsair <- subplot(
  batch_plots(studydata_to_test, "M", "CA", "hist_narcosis", anesthetic1 = "CO2/O2"),
  batch_plots(studydata_to_test, "M", "CA", "box_narcosis", anesthetic1 = "CO2/O2"),
  nrows = 1, shareY = F, shareX = F, titleX = T, titleY = T
)
#*Sodium
#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_na_o2vsair <- subplot(
  batch_plots(studydata_to_test, "M", "SODIUM", "hist_narcosis", anesthetic1 = "CO2/O2"),
  batch_plots(studydata_to_test, "M", "SODIUM", "box_narcosis", anesthetic1 = "CO2/O2"),
  nrows = 1, shareY = F, shareX = F, titleX = T, titleY = T
)
#*Inorganic phosphate
#A histogram and a sideways layed box plot showing the drop in the electrolyte
#along with its distribution
histbox_of_po4_o2vsair <- subplot(
  batch_plots(studydata_to_test, "M", "PHOS", "hist_narcosis", anesthetic1 = "CO2/O2"),
  batch_plots(studydata_to_test, "M", "PHOS", "box_narcosis", anesthetic1 = "CO2/O2"),
  nrows = 1, shareY = F, shareX = F, titleX = T, titleY = T
)
#*******************************************************************************
#Visualize the original results from the Dunnett's test of the selected study
baycal_original <- resampling_plot(
  plot_data = baycal1_both_m_all,
  which_VCG = "center",
  of_dose = "Dose group 1",
  res_leading_to = "res_con",
  electrolyte = "CA",
  sex = "males",
  whichplot = "original"
)
baycal_original
#*******************************************************************************
#*******************************************************************************
#Visualize the scenarios applying to each study
#*******************************************************************************
#Resampling from both subgroups
#*******************************************************************************
#Males
both_m_all_recruitment <- scenarios_plot(baycal1_both_m_all, "CA")
both_m_all_recruitment

both_m_all_but_two_recruitment <- scenarios_plot(baycal1_both_m_all_but_two, "CA")
both_m_all_but_two_recruitment

both_m_half_recruitment <- scenarios_plot(baycal1_both_m_half, "CA")
both_m_half_recruitment
#*******************************************************************************
#Resampling from isoflurane subgroup
#*******************************************************************************
#Males
iso_m_all_recruitment <- scenarios_plot(baycal1_iso_m_all, "CA")
iso_m_all_recruitment

iso_m_all_but_two_recruitment <- scenarios_plot(baycal1_iso_m_all_but_two, "CA")
iso_m_all_but_two_recruitment

iso_m_half_recruitment <- scenarios_plot(baycal1_iso_m_half, "CA")
iso_m_half_recruitment

all_recruitment_scenarios <- subplot(
  scenarios_plot(baycal1_both_m_all, "CA", F), scenarios_plot(baycal1_iso_m_all, "CA", F),
  scenarios_plot(baycal1_both_m_all_but_two, "CA", F), scenarios_plot(baycal1_iso_m_all_but_two, "CA", F),
  scenarios_plot(baycal1_both_m_half, "CA", T), scenarios_plot(baycal1_iso_m_half, "CA", F),
  nrows = 3, margin = 0.04, titleX = T, titleY = T
)
#*******************************************************************************
#*show results from picked iterations
#*******************************************************************************
#*Both subroups, all OCGs replaced
# source(paste0(getwd(), "/Programs/K5_2_plots_OCG_vs_VCG.R"))
picked_both_m_all <- resampling_plot(
  plot_data = baycal1_both_m_all,
  which_VCG = "center",
  of_dose = "Dose group 2",
  res_leading_to = "res_inv_sig",
  electrolyte = "CA",
  whichplot = "res",
  showlegendhere = T
)
picked_both_m_all
#*******************************************************************************
#iso subgroup, all animals replaced
picked_iso_m_all <- resampling_plot(
  plot_data = baycal1_iso_m_all,
  which_VCG = "center",
  of_dose = "Dose group 1",
  res_leading_to = "res_con",
  electrolyte = "CA",
  whichplot = "res",
  showlegendhere = T
)
picked_iso_m_all
#*******************************************************************************
#*Both subroups, all but two OCGs replaced
picked_both_m_half <- resampling_plot(
  plot_data = baycal1_both_m_half,
  which_VCG = "center",
  of_dose = "Dose group 2",
  res_leading_to = "res_con",
  electrolyte = "CA",
  whichplot = "res",
  showlegendhere = T
)
picked_both_m_half
#*******************************************************************************
#*******************************************************************************
#Visualize all iteration results
#*******************************************************************************
#Resampling from both subgroups
#*******************************************************************************
#Males
baycal1_both_m_all_plot <- subplot(
  resampling_result_baycal_plot(baycal1_both_m_all, electrolyte = "CA", whichplot = "VCGs"),
  resampling_result_baycal_plot(baycal1_both_m_all, "Dose group 1", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_both_m_all, "Dose group 2", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_both_m_all, "Dose group 3", "CA", whichplot = "resampling"),
  shareY = T, titleX = F, titleY = T
  )
baycal1_both_m_all_plot

baycal1_both_m_all_but_two_plot <- subplot(
  resampling_result_baycal_plot(baycal1_both_m_all_but_two, electrolyte = "CA", whichplot = "VCGs"),
  resampling_result_baycal_plot(baycal1_both_m_all_but_two, "Dose group 1", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_both_m_all_but_two, "Dose group 2", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_both_m_all_but_two, "Dose group 3", "CA", whichplot = "resampling"),
  shareY = T, titleX = F, titleY = T
  )
baycal1_both_m_all_but_two_plot

baycal1_both_m_half_plot <- subplot(
  resampling_result_baycal_plot(baycal1_both_m_half, electrolyte = "CA", whichplot = "VCGs"),
  resampling_result_baycal_plot(baycal1_both_m_half, "Dose group 1", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_both_m_half, "Dose group 2", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_both_m_half, "Dose group 3", "CA", whichplot = "resampling"),
  shareY = T, titleX = F, titleY = T
  )
baycal1_both_m_half_plot
#*******************************************************************************
#*******************************************************************************
#Resampling from isoflurane subgroup
#*******************************************************************************
#Males
baycal1_iso_m_all_plot <- subplot(
  resampling_result_baycal_plot(baycal1_iso_m_all, electrolyte = "CA", whichplot = "VCGs"),
  resampling_result_baycal_plot(baycal1_iso_m_all, "Dose group 1", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_iso_m_all, "Dose group 2", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_iso_m_all, "Dose group 3", "CA", whichplot = "resampling"),
  shareY = T, titleX = F, titleY = T
)
baycal1_iso_m_all_plot

baycal1_iso_m_all_but_two_plot <- subplot(
  resampling_result_baycal_plot(baycal1_iso_m_all_but_two, electrolyte = "CA", whichplot = "VCGs"),
  resampling_result_baycal_plot(baycal1_iso_m_all_but_two, "Dose group 1", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_iso_m_all_but_two, "Dose group 2", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_iso_m_all_but_two, "Dose group 3", "CA", whichplot = "resampling"),
  shareY = T, titleX = F, titleY = T
)
baycal1_iso_m_all_but_two_plot

baycal1_iso_m_half_plot <- subplot(
  resampling_result_baycal_plot(baycal1_iso_m_half, electrolyte = "CA", whichplot = "VCGs"),
  resampling_result_baycal_plot(baycal1_iso_m_half, "Dose group 1", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_iso_m_half, "Dose group 2", "CA", whichplot = "resampling"),
  resampling_result_baycal_plot(baycal1_iso_m_half, "Dose group 3", "CA", whichplot = "resampling"),
  shareY = T, titleX = F, titleY = T
)
baycal1_iso_m_half_plot
#*******************************************************************************
#*******************************************************************************
#*Write plots as HTML and as JPEG
#*******************************************************************************
#Make a function to export HTMLs
export_as_html <- function(plotlyplot){
  htmlwidgets::saveWidget(
    plotlyplot,
    paste0(path_res, "/HTML/", deparse(substitute(plotlyplot)), ".html")
  )
}
#Export HTMLs
#Electrolyte distributions
#potassium
export_as_html(histbox_of_k_uncolored)
export_as_html(histbox_of_k)
export_as_html(box_of_k_narcosis_colored)
#calcium
export_as_html(histbox_of_ca_uncolored)
export_as_html(histbox_of_ca)
export_as_html(box_of_ca_narcosis_colored)
#sodium
export_as_html(histbox_of_na_uncolored)
export_as_html(histbox_of_na)
export_as_html(box_of_na_narcosis_colored)
#inorganic phosphate
export_as_html(histbox_of_po4_uncolored)
export_as_html(histbox_of_po4)
export_as_html(box_of_po4_narcosis_colored)
#Do the same for the substudies where the difference between air and O2 was compared
export_as_html(histbox_of_k_o2vsair)
export_as_html(histbox_of_ca_o2vsair)
export_as_html(histbox_of_na_o2vsair)
export_as_html(histbox_of_po4_o2vsair)
#Original mean values
export_as_html(baycal_original)
#All recruitment scenarios in one panel
export_as_html(all_recruitment_scenarios)
#Picked scenarios
export_as_html(picked_both_m_all)
export_as_html(picked_iso_m_all)
export_as_html(picked_both_m_half)
#Results from all iterations
#Confounder not controlled, no sentinel animals
export_as_html(baycal1_both_m_all_plot)
#Confounder controlled, no sentinel animals
export_as_html(baycal1_iso_m_all_plot)
#Confounder not controlled, two sentinel animals
export_as_html(baycal1_both_m_all_but_two_plot)
#Confounder not controlled, half of the set kept as sentinal animals
export_as_html(baycal1_both_m_half_plot)
#Confounder controlled, two sentinel animals
export_as_html(baycal1_iso_m_all_but_two_plot)
#Confounder controlled, half of the set kept as sentinel animals
export_as_html(baycal1_iso_m_half_plot)
#*******************************************************************************
#*Export everything as JPEGs
#*******************************************************************************
#Make a function to export JPEGs
export_as_jpeg <- function(plotlyplot, width = 960, height = 540){
  webshot(
    paste0(path_res, "/HTML/", deparse(substitute(plotlyplot)), ".html"),
    paste0(path_res, "/JPEG/", deparse(substitute(plotlyplot)), ".jpeg"),
    vwidth = width, vheight = height, zoom = 2
  )
}
#Export JPEGs
#Electrolyte distributions
#potassium
export_as_jpeg(histbox_of_k_uncolored)
export_as_jpeg(histbox_of_k)
export_as_jpeg(box_of_k_narcosis_colored)
#calcium
export_as_jpeg(histbox_of_ca_uncolored)
export_as_jpeg(histbox_of_ca)
export_as_jpeg(box_of_ca_narcosis_colored)
#sodium
export_as_jpeg(histbox_of_na_uncolored)
export_as_jpeg(histbox_of_na)
export_as_jpeg(box_of_na_narcosis_colored)
#inorganic phosphate
export_as_jpeg(histbox_of_po4_uncolored)
export_as_jpeg(histbox_of_po4)
export_as_jpeg(box_of_po4_narcosis_colored)
#Do the same for the substudies where the difference between air and O2 was compared
export_as_jpeg(histbox_of_k_o2vsair)
export_as_jpeg(histbox_of_ca_o2vsair)
export_as_jpeg(histbox_of_na_o2vsair)
export_as_jpeg(histbox_of_po4_o2vsair)
#Original mean values
export_as_jpeg(baycal_original)
#All recruitment scenarios in one panel
export_as_jpeg(all_recruitment_scenarios, width = 960, height = 1080)
#Picked scenarios
export_as_jpeg(picked_both_m_all)
export_as_jpeg(picked_iso_m_all)
export_as_jpeg(picked_both_m_half)
#Results from all iterations
#Confounder not controlled, no sentinel animals
export_as_jpeg(baycal1_both_m_all_plot, width = 1920, height = 540)
#Confounder controlled, no sentinel animals
export_as_jpeg(baycal1_iso_m_all_plot, width = 1920, height = 540)
#Confounder not controlled, two sentinel animals
export_as_jpeg(baycal1_both_m_all_but_two_plot, width = 1920, height = 540)
#Confounder not controlled, half of the set kept as sentinal animals
export_as_jpeg(baycal1_both_m_half_plot, width = 1920, height = 540)
#Confounder controlled, two sentinel animals
export_as_jpeg(baycal1_iso_m_all_but_two_plot, width = 1920, height = 540)
#Confounder controlled, half of the set kept as sentinel animals
export_as_jpeg(baycal1_iso_m_half_plot, width = 1920, height = 540)