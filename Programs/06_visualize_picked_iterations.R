#*Add program description
##### program starts here
#*********************************************************************************;
#*********************************************************************************;
#*********************************************************************************;
#load libraries
library(tidyverse)
library(plotly)
#*********************************************************************************;
resampling_plot <- function(
  plot_data = baycal1_both_m_all,
  sex = "males",
  which_VCG = "center",
  of_dose = "Dose group 2",
  res_leading_to = "res_inv_sig",
  electrolyte = "CA",
  whichplot,
  showlegendhere = F
    )
  {
  
  #Customize lower range of y-axis
  yaxislowerrange <- if(electrolyte == "K") {
    2.5
  } else if(electrolyte == "CA") {
    2.1
  } else if(electrolyte == "SODIUM") {
    135
  } else if(electrolyte == "PHOS") {
    .5
  }
  #Customize upper range of y-axis
  yaxisupperrange <- if(electrolyte == "K") {
    8.5
  } else if(electrolyte == "CA") {
    3.5
  } else if(electrolyte == "SODIUM") {
    155
  } else if(electrolyte == "PHOS") {
    5
  }
  #Customize binsize to be 0.1-wise for K, 0.025-wise for Ca, 1-wise for Na
  xbinsize <- if(electrolyte == "K"){
    .1
  } else if(electrolyte == "CA") {
    .025
  } else if (electrolyte == "SODIUM") {
    1
  } else if(electrolyte == "PHOS") {
    .1
  }
  #Customize where the bins need to start
  xbinstart <- if(electrolyte == "K") {
    -.05
  } else if(electrolyte == "CA") {
    -.0125
  } else if (electrolyte == "SODIUM") {
    -.5
  }  else if(electrolyte == "PHOS") {
    -.05
  }
  #Customize tick-distance
  xticks <- if(electrolyte == "K") {
    .5
  } else if(electrolyte == "CA") {
    .2
  } else if(electrolyte == "SODIUM") {
    2
  } else if(electrolyte == "PHOS") {
    .5
  }
  
  #define words for the axis title
  yaxistitle <- ifelse(electrolyte == "K", "K-concentration in serum [mmol/L]", "Ca-concentration in serum [mmol/L]")
  
  #*Get mean and sd values of concurrent control group (ccg) of selected study
  original_data_without_cg <- plot_data[["res_vs_original"]] %>% as_tibble() %>%
    mutate(
      ccg_significance = case_when(ccg_significance == "ns" ~ "not significant",
                                   ccg_significance == "s" ~ "significant"),
      across(c(diff, lwr.ci, upr.ci, pval, ccg_mean, ccg_sd, ccg_population, dose_mean, dose_sd, dose_population, consistency_percentage), as.numeric),
      row_id = row_number()
    ) %>%
    select(dose_group, ccg_significance, ccg_direction, ccg_mean, ccg_sd, ccg_population,
           dose_mean, dose_sd, dose_population)
  ccg_mean <- original_data_without_cg %>% filter(dose_group == "Dose group 1") %>% pull(ccg_mean)
  ccg_sd <- original_data_without_cg %>% filter(dose_group == "Dose group 1") %>% pull(ccg_sd)
  ccg_population <- original_data_without_cg %>% filter(dose_group == "Dose group 1") %>% pull(ccg_population)
  
  #Add control group information as a row entry to the original-data data frame
  #Otherwise, the coloring of the plotly graph is not colorizable
  ccg_information <- tibble(dose_group = "Control",
                            # diff = 0,
                            # lwr.ci = 0,
                            # upr.ci = 0,
                            # pval = 0,
                            ccg_significance = "concurrent control",
                            ccg_direction = "control",
                            ccg_mean = as.numeric(ccg_mean),
                            ccg_sd = as.numeric(ccg_sd),
                            ccg_population = as.numeric(ccg_population),
                            dose_mean = as.numeric(ccg_mean),
                            dose_sd = as.numeric(ccg_sd),
                            dose_population = as.numeric(ccg_population)
  )
                            # consistency_percentage = 0,
                            # row_id = 0,
                            # vcg_population = 0)

  original_data <- rbind.data.frame(ccg_information, original_data_without_cg) %>%
    mutate(ccg_significance = factor(ccg_significance, levels = c("concurrent control", "not significant", "significant")))
  
  #extract the distance in means and put each one in a list, so that these can
  #be added to the plot
  # lapply(original_data_without_cg, tibble(type = "line", y0 = ccg_mean, y1 = original_data_without_cg$dose_mean, x1 = dose_group, x2 = dose_group))
  # ?sapply
  # ccg_mean_distances <- split(original_data_without_cg, seq(nrow(original_data_without_cg)))
  # ccg_mean_distance_annotations <- lapply(
  #   ccg_mean_distances, function(ccg_data) list(type = "line",
  #                                               y0 = ccg_data$ccg_mean,
  #                                               y1 = ccg_data$dose_mean,
  #                                               x0 = 1 / nrow(original_data) * ccg_data$row_id + 0.12,
  #                                               x1 = 1 / nrow(original_data) * ccg_data$row_id + 0.12,
  #                                               xref = "paper",
  #                                               line = list(color = "#000000", opacity = 0.8)
  #   )
  # )
  # 
  # ccg_mean_distance <- tibble(
  #   xstart = rep(
  #     1 / nrow(original_data) * original_data_without_cg$row_id + 0.12, 3
  #   ),
  #   xend = c(
  #     1 / nrow(original_data) * original_data_without_cg$row_id + 0.12,
  #     rep(1 / nrow(original_data) * original_data_without_cg$row_id + 1, 2)
  #   ),
  #   ystart = c(rep(ccg_mean, 6), original_data_without_cg$dose_mean),
  #   yend = c(original_data_without_cg$dose_mean, rep(ccg_mean, 3), original_data_without_cg$dose_mean),
  #   annotated_text = c(paste0(signif(original_data_without_cg$diff, 3)), rep("", 6))
  # )
  
  #extract vector of IDs of the results type of interest with respect to dose group
  VCG_res_id <- plot_data[[res_leading_to]] %>% filter(dose_group == of_dose) %>% pull(results_row_id)

  #terminate function if the selected vector is empty
  # try(if(length(VCG_res_id == 0)) stop("Selection leads to empty vector"))

  #*select the hightest, lowest or middle VCG (rounded to higher integer) leading
  #*to the selected result in the selected dose group
  VCG_dis_rank <- if(which_VCG == "lowest"){
    plot_data[[res_leading_to]] %>% filter(dose_group == of_dose, results_row_id == min(VCG_res_id)) %>% pull(iteration_id)
  } else if (which_VCG == "highest") {
    plot_data[[res_leading_to]] %>% filter(dose_group == of_dose, results_row_id == max(VCG_res_id)) %>% pull(iteration_id)
  } else if ((which_VCG == "center")) {
    plot_data[[res_leading_to]] %>% filter(dose_group == of_dose, results_row_id == ceiling(mean(VCG_res_id))) %>% pull(iteration_id)
  }

  #Get the selected iteration from the nested list of the VCG results
  selected_vcg <- plot_data[["res_all"]][[as.numeric(gsub("iteration_", "", VCG_dis_rank))]] %>% as_tibble() %>%
    mutate(
      vcg_significance = case_when(vcg_significance == "ns" ~ "not significant",
                                   vcg_significance == "s" ~ "significant"),
      restext = case_when(
        original_data_without_cg$ccg_significance == "not significant" & vcg_significance == "not significant" ~ "consistently non-significant",
        original_data_without_cg$ccg_significance == "significant" & vcg_significance == "significant" & original_data_without_cg$ccg_direction == vcg_direction ~ "consistently significant",
        original_data_without_cg$ccg_significance == "not significant" & vcg_significance == "significant" ~ "inconsistently significant",
        original_data_without_cg$ccg_significance == "significant" & vcg_significance == "not significant" ~ "inconsistently non-significant",
        original_data_without_cg$ccg_significance == vcg_significance & original_data_without_cg$ccg_direction != vcg_direction ~ "inverted significant"
        ),
      dose_mean = original_data_without_cg$dose_mean,
      dose_sd = original_data_without_cg$dose_sd
      )

  # original_data$colorcode <- selected_vcg$colorcode
  # original_data$significance <- selected_vcg$significance
  #
  #Get VCG mean and SD from these selected values
  vcg_mean <- selected_vcg %>% filter(dose_group == "Dose group 1") %>% pull(vcg_mean)
  vcg_sd <- selected_vcg %>% filter(dose_group == "Dose group 1") %>% pull(vcg_sd)
  
  #Create VCG and ccg information which will be attached to the data frame
  vcg_attachment <- tibble(
    dose_group = c("Control", "Control"),
    dose_mean = c(ccg_information$ccg_mean, as.numeric(selected_vcg$vcg_mean[1])),
    dose_sd = c(ccg_information$ccg_sd, as.numeric(selected_vcg$vcg_sd[1])),
    diff = c(0, 0),
    lwr.ci = c(0, 0),
    upr.ci = c(0, 0),
    pval = c(0, 0),
    vcg_significance = c("concurrent control", "virtual control"),
    vcg_direction = c("control", "control"),
    vcg_mean = c(ccg_information$ccg_mean, as.numeric(selected_vcg$vcg_mean[1])),
    vcg_sd = c(ccg_information$ccg_sd, as.numeric(selected_vcg$vcg_sd[1])),
    vcg_population = c(ccg_information$ccg_population, as.numeric(selected_vcg$vcg_population[1])),
    iteration_id = rep(as.numeric(selected_vcg$vcg_population[1]), 2),
    restext = c("concurrent control", "virtual control")
  )
  
  #Add ccg-row to VCG results in order to be able to add the colors in the plot
  vcg_data_for_plot <- rbind.data.frame(vcg_attachment, selected_vcg) %>%
    #make the results text as a factor so that it weill be shown in a certain order
    #on the plot legend
    mutate(restext = factor(restext, levels = c(
      "concurrent control",
      "virtual control",
      "consistently non-significant",
      "consistently significant",
      "inconsistently significant",
      "inconsistently non-significant",
      "inverted significant"
      )))
  
  #*****************************************************************************
  #Create a function visualizing the respective values of the substance and sex
  ccg_plot <- plot_ly(
    data = original_data,
    type = "scatter",
    mode = "markers",
    x = ~dose_group,
    y = ~dose_mean,
    marker = list(size = 14),
    error_y = ~list(array = dose_sd),
    color = ~ccg_significance,
    colors = c(
      "concurrent control" = "#ff7f00",
      "not significant" = "#1f78b4",
      "significant" = "#e31a1c"
    ),
    symbol = ~ccg_significance,
    symbols = c(
      "concurrent control" = "cross",
      "not significant" = "circle",
      "significant" = "x"
    ),
    showlegend = ifelse(sex == "males", T, F)
    ) %>%
    #add a star as an annotation to a mean value if the result is significant
    add_annotations(
      data = original_data %>% filter(ccg_significance == "significant"),
      text = "<b>*</b>",
      x = ~dose_group,
      y = ~dose_mean,
      showarrow = F,
      xanchor = "right",
      yanchor = "bottom"
    )  %>%
    layout(
      font = list(size = 22),
      shapes = list(
        #add ccg mean value as a dashed orange line
        list(
          type = "line", y0 = ccg_mean, y1 = ccg_mean, x0 = 0, x1 = 1, xref = "paper",
          line = list(color = "#ff7f00", dash = "dot", opacity = 0.5)
        )#,
        #add distance between dose group and ccg as a solid black line
        #*Note: this bit only works for studies with three dose groups as each
        #*dose group had to be added manually
        # ccg_mean_distance_annotations[[2]]
      ),
      xaxis = list(title = "Legacy study",
                   zerolinecolor = "#000000",
                   zerolinewidth = 2,
                   # gridcolor = "#000000",
                   ticks = "outside",
                   tickwidth = 2,
                   ticklen = 10,
                   tickangle = 90),
      yaxis = list(title = "",#paste0(yaxistitle),
                   zerolinecolor = "#000000",
                   zerolinewidth = 2,
                   # gridcolor = "#000000",
                   range = c(yaxislowerrange, yaxisupperrange))
    )
  ccg_plot
  #Add the distribution of the VCG sample population as a sideways histogram
  vcg_sample_distribution <- plot_ly(
    type = "histogram",
    ybins = list(size = xbinsize, start = xbinstart),
    opacity = 0.5,
    data = plot_data[["sampling_group"]],
    y = ~LBORRES,
    marker = list(color = "#a5a5a5", line = list(color = "#000000", width = 2)),
    name = "VCG sample population"
  ) %>%
    layout(
      plot_bgcolor="#e5ecf6",
      font = list(size = 18),
      # annotations = list(list(x = 0.1 , y = 0.95, text = titlesex, showarrow = F, xref =  "paper", yref = "paper")),
      #title = paste0(titleelectrolyte, " concentration of ", titlesex, " Wistar-rats"),
      yaxis = list(title = yaxistitle,
                   zerolinecolor = "#ffff", 
                   zerolinewidth = 2, 
                   gridcolor = "ffff",
                   ticks = "inside",
                   tickwidth = 2,
                   ticklen = 10,
                   dtick = xticks,
                   range = c(yaxislowerrange, yaxisupperrange)),
      xaxis = list(title = "",
                   zerolinecolor = "#ffff", 
                   zerolinewidth = 2, 
                   gridcolor = "ffff",
                   autorange = "reversed"),
      showlegend = F
    )
  vcg_sample_distribution
  
  vcg_sample_mean <- plot_ly(
    data = plot_data[["sampling_group"]],
    type = "scatter",
    mode = "markers",
    x = "VCG sample",
    y = ~mean(LBORRES),
    marker = list(
      size = 14, color = "#c5c8cd", symbol = "square",
      line = list(color = "#73767b", width = 2)
      ),
    error_y = ~list(array = sd(LBORRES), color = "#73767b"),
    showlegend = F,
    legendgroup = "VCG"
  ) %>%
    layout(
      plot_bgcolor = "#e5ecf6",
      font = list(size = 22),
      xaxis = list(title = "",
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff",
                   ticks = "outside",
                   tickwidth = 2,
                   ticklen = 10,
                   tickangle = 90),
      yaxis = list(title = "",#paste0(yaxistitle),
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff",
                   range = c(yaxislowerrange, yaxisupperrange))
    )
  vcg_sample_mean
  
  
  #Show both plots of sample distribution and ccg results
  ccg_and_vcg_sample <- subplot(
    vcg_sample_distribution, vcg_sample_mean, ccg_plot,
    nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T,
    widths = c(0.3, 0.1, 0.6)
  )
  ccg_and_vcg_sample
  #*****************************************************************************
  #*Create a box plot illustrating the different scenarios.
  #*Scenario 1: No ccgs are used. The data from the whole sample population is
  #*taken for sampling.
  #*Scenario 2: The 2 VCGs on the extreme side are used. A mean +- 2*sd is
  #*calculated from this and these are used as ranges to where recruit from.
  #*Scenario 3: Like scenario 2 but instead of two ccgs, the half of the ccgs
  #*are kept.
  vcg_res_plot <- plot_ly(
    data = vcg_data_for_plot,
    type = "scatter",
    mode = "markers",
    x = ~dose_group,
    y = ~dose_mean,
    marker = ~list(size = 14),
    error_y = ~list(array = dose_sd),
    color = ~restext,
    colors = c(
      "concurrent control" = "#a5a5a5",
      "virtual control" = "#6a3d9a",
      "consistently non-significant" = "#1f78b4",
      "consistently significant" = "#1f78b4",
      "inconsistently significant" = "#e31a1c",
      "inconsistently non-significant" = "#ff7f00",
      "inverted significant" = "#b15928"
    ),
    symbol = ~restext,
    symbols = c(
      "concurrent control" = "cross",
      "virtual control" = "square",
      "consistently non-significant" = "circle",
      "consistently significant" = "x",
      "inconsistently significant" = "triangle-up",
      "inconsistently non-significant" = "triangle-down",
      "inverted significant" = "triangle-right"
    ),
    showlegend = ifelse(showlegendhere == T, T, F)
  ) %>%
    #add a star as an annotation to a mean value if the result is significant
    add_annotations(
      data = vcg_data_for_plot %>% filter(grepl(" significant", restext)),
      text = "<b>*</b>",
      x = ~dose_group,
      y = ~dose_mean,
      showarrow = F,
      xanchor = "right",
      yanchor = "bottom"
    )  %>%
    layout(
      plot_bgcolor = "#e5ecf6",
      font = list(size = 22),
      shapes = list(
        #add ccg mean value as a dashed gray line
        list(
          type = "line", y0 = ccg_mean, y1 = ccg_mean, x0 = 0, x1 = 1, xref = "paper",
          line = list(color = "#a5a5a5", dash = "dot", opacity = 0.5)
        ),
        #add VCG mean value as a dashed violet line
        list(
          type = "line", y0 = vcg_mean, y1 = vcg_mean, x0 = 0, x1 = 1, xref = "paper",
          line = list(color = "#6a3d9a", dash = "dot")
        )
      ),
      xaxis = list(title = "",
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff",
                   ticks = "outside",
                   tickwidth = 2,
                   ticklen = 10),
      yaxis = list(title = "",#paste0(yaxistitle),
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff",
                   range = c(yaxislowerrange, yaxisupperrange))
    )
  # vcg_res_plot
  
  ifelse(whichplot == "original", return(ccg_and_vcg_sample), return(vcg_res_plot))
}
#*********************************************************************************;
#*********************************************************************************;
#*********************************************************************************;
##### program ends here

# save.image(file = paste(rootpath,"\\Programs\\",program,".RData",sep=""))
# 
# 
# #*
# #* DO NOT EDIT BELOW THESE LINES
# #*
# savehistory(file = paste(rootpath,"\\Programs\\",program,".Rhistory",sep=""))
# sessionInfo()
