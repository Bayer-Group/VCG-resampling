#*Add program description
##### program starts here
#*********************************************************************************;
#*********************************************************************************;
#*********************************************************************************;
#load libraries
require(tidyverse)
require(plotly)
#*********************************************************************************;
resampling_plot <- function(
  plot_data = baycal1_both_m_all,
  sex = "males",
  which_VCG = "center",
  of_dose = "Dose group 2",
  res_leading_to = "con",
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
  yaxistitle <- ifelse(electrolyte == "K", "K<sup>+</sup>-concentration in serum [mmol/L]", "Ca<sup>2+</sup>-concentration in serum [mmol/L]")
  
  #*Get mean and sd values of concurrent control group (ccg) of selected study
  original_data_without_cg <- plot_data[["res_all"]] %>%
    filter(LBTESTCD == electrolyte) %>%
    mutate(
      ccg_significance = case_when(ccg_significance == "ns" ~ "not significant",
                                   ccg_significance == "s" ~ "significant"),
      # across(c(diff, lwr.ci, upr.ci, pval, ccg_mean, ccg_sd, ccg_population, dose_mean, dose_sd, dose_population, consistency_percentage), as.numeric),
      row_id = row_number()
    ) %>%
    select(
      dose_group,
      ccg_significance,
      ccg_direction,
      ccg_mean,
      ccg_sd,
      ccg_population,
      dose_mean,
      dose_sd,
      dose_population
      )
  #Extract location parameters of concurrent control group of legacy study
  ccg_mean <- original_data_without_cg %>% pull(ccg_mean) %>% unique()
  ccg_sd <- original_data_without_cg %>% pull(ccg_sd) %>% unique()
  ccg_population <- original_data_without_cg %>% pull(ccg_population) %>% unique()
  
  #Add control group information as a row entry to the original-data data frame
  #Otherwise, the coloring of the plotly graph is not colorizable
  ccg_information <- tibble(dose_group = "Concurrent control",
                            ccg_significance = "concurrent control",
                            ccg_direction = "control",
                            ccg_mean = as.numeric(ccg_mean),
                            ccg_sd = as.numeric(ccg_sd),
                            ccg_population = as.numeric(ccg_population),
                            dose_mean = as.numeric(ccg_mean),
                            dose_sd = as.numeric(ccg_sd),
                            dose_population = as.numeric(ccg_population)
  )

  original_data <- rbind.data.frame(ccg_information, original_data_without_cg) %>%
    mutate(ccg_significance = factor(ccg_significance, levels = c("concurrent control", "not significant", "significant"))) %>%
    unique()

  
  # #extract vector of IDs of the results type of interest with respect to dose group
  # VCG_res_id <- plot_data[["res_all"]] %>%
  #   filter(
  #     consistency_flag == res_leading_to,
  #     dose_group == of_dose,
  #     LBTESTCD == electrolyte
  #     )


  #*select the hightest, lowest or middle VCG (rounded to higher integer) leading
  #*to the selected result in the selected dose group
  VCG_dis_rank <- if(which_VCG == "lowest"){
    plot_data[["res_all"]] %>%
      filter(
        LBTESTCD == electrolyte,
        consistency_flag == res_leading_to,
        dose_group == of_dose
        ) %>%
          slice(which.min(vcg_mean)) %>%
      pull(iteration)
  } else if (which_VCG == "highest") {
    plot_data[["res_all"]] %>%
      filter(
        LBTESTCD == electrolyte,
        consistency_flag == res_leading_to,
        dose_group == of_dose
        ) %>%
      slice(which.max(vcg_mean)) %>%
      pull(iteration)
  } else if ((which_VCG == "center")) {
    plot_data[["res_all"]] %>%
      filter(
        LBTESTCD == electrolyte,
        consistency_flag == res_leading_to,
        dose_group == of_dose
      ) %>%
      arrange(desc(vcg_mean)) %>%
      slice(
        ceiling(
          plot_data[["res_all"]] %>%
            filter(
              LBTESTCD == electrolyte,
              consistency_flag == res_leading_to,
              dose_group == of_dose
                      )
          %>% nrow() / 2)) %>%
      pull(iteration)
  }

  #Get the selected iteration from the nested list of the VCG results
  selected_vcg <- plot_data[["res_all"]] %>%
    filter(
      LBTESTCD == electrolyte,
      iteration %in% VCG_dis_rank
    ) %>%
    mutate(
      #spell out the iteration results in full text
      restext = case_when(
        ccg_significance == "ns" & vcg_significance == "ns" ~ "consistently non-significant",
        ccg_significance == "s" & vcg_significance == "s" & ccg_direction == vcg_direction ~ "consistently significant",
        ccg_significance == "ns" & vcg_significance == "s" ~ "inconsistently significant",
        ccg_significance == "s" & vcg_significance == "ns" ~ "inconsistently non-significant",
        ccg_significance == "s" & vcg_significance == "s" & ccg_direction != vcg_direction ~ "inverted significant"
        )
      )

  # original_data$colorcode <- selected_vcg$colorcode
  # original_data$significance <- selected_vcg$significance
  #
  #Get VCG mean and SD from these selected values
  vcg_mean <- selected_vcg %>% filter(dose_group == "Dose group 1") %>% pull(vcg_mean)
  vcg_sd <- selected_vcg %>% filter(dose_group == "Dose group 1") %>% pull(vcg_sd)
  
  #Create VCG and ccg information which will be attached to the data frame
  vcg_attachment <- tibble(
    dose_group = c("Virtual control", "Concurrent control"),
    dose_mean = c(as.numeric(selected_vcg$vcg_mean[1]), ccg_information$ccg_mean),
    dose_sd = c(as.numeric(selected_vcg$vcg_sd[1]), ccg_information$ccg_sd),
    vcg_significance = c("virtual control", "concurrent control"),
    vcg_direction = c("control", "control"),
    vcg_mean = c(as.numeric(selected_vcg$vcg_mean[1]), ccg_information$ccg_mean),
    vcg_sd = c(as.numeric(selected_vcg$vcg_sd[1]), ccg_information$ccg_sd),
    vcg_population = c(as.numeric(selected_vcg$vcg_population[1]), ccg_information$ccg_population),
    iteration_id = rep(as.numeric(selected_vcg$vcg_population[1]), 2),
    restext = c("virtual control", "concurrent control")
  )
  
  #Add ccg-row to VCG results in order to be able to add the colors in the plot
  vcg_data_for_plot <- bind_rows(vcg_attachment, selected_vcg) %>%
    #make the results text as a factor so that it weill be shown in a certain order
    #on the plot legend
    mutate(
      restext = factor(restext, levels = c(
      "virtual control",
      "concurrent control",
      "consistently non-significant",
      "consistently significant",
      "inconsistently significant",
      "inconsistently non-significant",
      "inverted significant"
      )),
      #turn control groups into factor so that they are shown in the correct
      #order on the x-axis
      dose_group = factor(dose_group, levels = c(
        "Virtual control",
        "Concurrent control",
        "Dose group 1",
        "Dose group 2",
        "Dose group 3"
      ))
      )
  
  #*Extract the highest mean value of a dose-mean leading to a significant result
  #*add the sd to it and another vallue (e.g. 5 %) on top. This value will serve
  #*as a position where the "*" should be put at signifying that this is a 
  #*significant result there.
  #For the original results:
  signif_y_posistion_original <- original_data %>%
    filter(ccg_significance == "significant") %>%
    summarize(y_pos = (max(dose_mean) + max(dose_sd)) + 0.05 * (max(dose_mean) + max(dose_sd))) %>%
    pull(y_pos)
    
  #For the VCGs:
  signif_y_position_VCG <- vcg_data_for_plot %>%
    filter(grepl(" significant", restext)) %>%
    summarize(y_pos = (max(dose_mean) + max(dose_sd)) + 0.05 * (max(dose_mean) + max(dose_sd))) %>%
    pull(y_pos)
  
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
      y = signif_y_posistion_original,
      showarrow = F,
      xanchor = "middle",
      yanchor = "bottom"
    )  %>%
    #Add a "C" above the figure to show the panel description
    add_annotations(
      text = "<b>C</b>",
      x = 0.05,
      y = 1,
      xref = "paper",
      yref = "paper",
      showarrow = F,
      xanchor = "middle",
      yanchor = "bottom"
    )  %>%
    layout(
      font = list(family = "times new roman", size = 22),
      shapes = list(
        #add ccg mean value as a dashed orange line
        list(
          type = "line", y0 = ccg_mean, y1 = ccg_mean, x0 = 0, x1 = 1, xref = "paper",
          line = list(color = "#ff7f00", dash = "dot", opacity = 0.5)
        )
      ),
      xaxis = list(
        # #add non-breakable spaces to align axis titles in the sub plots
        title = "",#paste0(c(
        #   rep("\n&nbsp;", 2),
        #   rep("&nbsp;", 10),
        #   "Legacy study",
        #   rep("&nbsp;", 10)
        # ),
        # collapse = ""
        # ),
        showline = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        tickangle = 90,
        mirror = T
        ),
      yaxis = list(
        title = "",#paste0(yaxistitle),
        showline = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        side = "right",
        range = c(yaxislowerrange, yaxisupperrange),
        mirror = T
        )
    )
  ccg_plot
  #Add the distribution of the VCG sample population as a sideways histogram
  vcg_sample_distribution <- plot_ly(
    type = "histogram",
    ybins = list(size = xbinsize, start = xbinstart),
    opacity = 0.5,
    data = plot_data[["sampling_group"]] %>% filter(LBTESTCD == "CA"),
    y = ~LBORRES,
    marker = list(color = "#a5a5a5", line = list(color = "#000000", width = 2)),
    name = "VCG sample population"
  ) %>%
    #Add an "A" above the figure to show the panel description
    add_annotations(
      text = "<b>A</b>",
      x = 0.05,
      y = 1,
      xref = "paper",
      yref = "paper",
      showarrow = F,
      xanchor = "middle",
      yanchor = "bottom"
    )  %>%
    layout(
      font = list(family = "times new roman", size = 18),
      #title = paste0(titleelectrolyte, " concentration of ", titlesex, " Wistar-rats"),
      yaxis = list(
        title = paste0(c(
          rep("&nbsp;", 10),
          yaxistitle,
          rep("&nbsp;", 25)
        ),
          collapse = ""
        ),
        showline = T,
        showgrid = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        dtick = xticks,
        range = c(yaxislowerrange, yaxisupperrange),
        mirror = T
      ),
      xaxis = list(
        #Add non breakable lines so that the title is aligned with the
        #title from the legacy study
        # title = paste0(c(
        #   rep("\n&nbsp;", 5),
        #   rep("&nbsp;", 10),
        #   "VCG sample population",
        #   rep("&nbsp;", 10)
        #   ),
        #   collapse = ""
        # ),
        showline = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        autorange = "reversed",
        mirror = T
                   ),
      showlegend = F
    )
  vcg_sample_distribution
  
  vcg_sample_mean <- plot_ly(
    data = plot_data[["sampling_group"]] %>% filter(LBTESTCD == electrolyte),
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
    #Add a "B" above the figure to show the panel description
    add_annotations(
      text = "<b>B</b>",
      x = 0.1,
      y = 1,
      xref = "paper",
      yref = "paper",
      showarrow = F,
      xanchor = "middle",
      yanchor = "bottom"
    )  %>%
    layout(
      font = list(family = "times new roman", size = 18),
      xaxis = list(
        title = "",
        showline = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        tickangle = 90,
        mirror = T
        ),
      yaxis = list(
        title = "",#paste0(yaxistitle),
        showline = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        showticklabels = F,
        range = c(yaxislowerrange, yaxisupperrange),
        mirror = T
        )
    )
  vcg_sample_mean
  
  
  #Show both plots of sample distribution and ccg results
  ccg_and_vcg_sample <- subplot(
    vcg_sample_distribution, vcg_sample_mean, ccg_plot,
    nrows = 1 ,shareY = F, shareX = F, titleX = T, titleY = T,
    widths = c(0.3, 0.1, 0.3)
  )
  ccg_and_vcg_sample
  #*****************************************************************************
  #*Create a box plot illustrating the different scenarios----------------------
  #*****************************************************************************
  #*****************************************************************************
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
      y = signif_y_position_VCG,
      showarrow = F,
      xanchor = "middle",
      yanchor = "bottom"
    )  %>%
    layout(
      font = list(family = "times new roman", size = 22),
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
      xaxis = list(
        title = "",
        showline = T,
        linewidth = 2,
        ticks = "outside",
        tickangle = 90,
        tickwidth = 2,
        ticklen = 10,
        mirror = T
                   ),
      yaxis = list(
        title = "",#paste0(yaxistitle),
        showline = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        side = "right",
        range = c(yaxislowerrange, yaxisupperrange),
        mirror = T
                   )
    )
  vcg_res_plot
  
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
