#*This function creates a plot displaying mean electrolyte-value of the dose-
#*group, the original control group and the mean of the sampled VCGs from each
#*iteration with respect to whether they've led to a consistent result or not

#load libraries
library(plotly)
library(tidyverse)


resampling_result_baycal_plot <- function(
  plot_data = bay_both_m_all_but_two,
  dose_group_to_plot = "Dose group 1",
  electrolyte = "CA",
  whichplot
  ) 
  {
  #Extract the values from the resampling results list of selected dose group
  #all values
  vcg_means <- plot_data[["res_all"]] %>%
    filter(LBTESTCD == electrolyte) %>%
    select(iteration, vcg_mean) %>%
    unique() %>%
    pull(vcg_mean)
  
  #Values which led to a consistent result
  vcg_con <- if(
    is.null(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "con")
      ) |
    identical(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "con"),
      character(0)
      )
  ) {
    NULL
  } else {
    plot_data[["res_all"]] %>%
      filter(
      LBTESTCD == electrolyte,
      consistency_flag == "con"
      ) %>%
      filter(dose_group == dose_group_to_plot) %>%
      pull(vcg_mean)
    }
  #Values which led to a inconsistent result
  #Inconsistently significant
  vcg_inc_sig <- if(
    is.null(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_sig")
    ) |
    identical(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_sig"),
      character(0)
    )
  ) {
    NULL
  } else {
    plot_data[["res_all"]] %>%
      filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_sig"
      ) %>%
      filter(dose_group == dose_group_to_plot) %>%
      pull(vcg_mean)
  }
  #Inconsistently non significant
  vcg_inc_non_sig <- if(
    is.null(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_non_sig")
    ) |
    identical(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_non_sig"),
      character(0)
    )
  ) {
    NULL
  } else {
    plot_data[["res_all"]] %>%
      filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_non_sig"
      ) %>%
      filter(dose_group == dose_group_to_plot) %>%
      pull(vcg_mean)
  }
  #Inverse significant
  vcg_inv_sig <- if(
    is.null(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "inv_sig")
    ) |
    identical(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "inv_sig"),
      character(0)
    )
  ) {
    NULL
  } else {
    plot_data[["res_all"]] %>%
      filter(
        LBTESTCD == electrolyte,
        consistency_flag == "inv_sig"
      ) %>%
      filter(dose_group == dose_group_to_plot) %>%
      pull(vcg_mean)
  }
  #Get the mean value of the electrolyte with respect to dose group
  original_mean <- plot_data[["res_all"]] %>%
    filter(
      LBTESTCD == electrolyte,
      dose_group == dose_group_to_plot
      ) %>%
    select(dose_mean, ccg_mean) %>%
    unique()
  
  #Create switch to turn scatter points invisible if the incoming list is empty
  scatter_visibility_con <- ifelse(length(vcg_con) == 0, F, T)
  scatter_visibility_incon_sig <- ifelse(length(vcg_inc_sig) == 0, F, T)
  scatter_visibility_incon_non_sig <- ifelse(length(vcg_inc_non_sig) == 0, F, T)
  scatter_visibility_inv_sig <- ifelse(length(vcg_inv_sig) == 0, F, T)

  #*While the above boolean values check whether values are present in general,
  #*there's a need to check whether the values are present with respect to the
  #*selected dose group (in order to turn on/off the "consistency-area-boxes")
  consistencybox_visibility_con <- ifelse(
    nrow(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "con",
        dose_group == dose_group_to_plot
        )
    ) == 0,
    F,
    T
  )
  #*specify whether there are any VCG values above or below the dose groups
  #*leading to a inconsistently significant result (above dose group = ad, below dose group = bd)
  consistencybox_visibility_inc_sig_ad <- ifelse(
    nrow(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_sig",
        dose_group == dose_group_to_plot,
        vcg_mean >= original_mean$dose_mean
        )
    ) == 0,
    F,
    T
  )
    
  consistencybox_visibility_inc_sig_bd <- ifelse(
    nrow(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_sig",
        dose_group == dose_group_to_plot,
        vcg_mean < original_mean$dose_mean
      )
    ) == 0,
    F,
    T
  )

  consistencybox_visibility_inc_non_sig <- ifelse(
    nrow(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "incon_non_sig",
        dose_group == dose_group_to_plot
      )
    ) == 0,
    F,
    T
  )
  consistencybox_visibility_inv_sig <- ifelse(
    nrow(
      plot_data[["res_all"]] %>% filter(
        LBTESTCD == electrolyte,
        consistency_flag == "inv_sig",
        dose_group == dose_group_to_plot
      )
    ) == 0,
    F,
    T
  )
  
  #*****************************************************************************
  #*****************************************************************************
  #*Check for each consistencybox which dose groups are present for this box.
  #*This is mandatory for the "showlegend"-option in the visualization. Otherwise
  #*the legend shows several same entries.
  dose_groups_having_con <- plot_data[["res_all"]] %>% filter(
    LBTESTCD == electrolyte,
    consistency_flag == "con"
    ) %>%
    select(dose_group) %>%
    unique() %>%
    filter(row_number() == 1) %>%
    pull(dose_group)
  
  dose_groups_having_incon_sig_ad <- plot_data[["res_all"]] %>% filter(
    LBTESTCD == electrolyte,
    consistency_flag == "incon_sig",
    vcg_mean >= original_mean$dose_mean
  ) %>%
    select(dose_group) %>%
    unique() %>%
    filter(row_number() == 1) %>%
    pull(dose_group)
  
  dose_groups_having_incon_sig_bd <- plot_data[["res_all"]] %>% filter(
    LBTESTCD == electrolyte,
    consistency_flag == "incon_sig",
    vcg_mean < original_mean$dose_mean
  ) %>%
    select(dose_group) %>%
    unique() %>%
    filter(row_number() == 1) %>%
    pull(dose_group)
  
  dose_groups_having_incon_non_sig <- plot_data[["res_all"]] %>% filter(
    LBTESTCD == electrolyte,
    consistency_flag == "incon_non_sig"
  ) %>%
    select(dose_group) %>%
    unique() %>%
    filter(row_number() == 1) %>%
    pull(dose_group)
  
  dose_groups_having_inv_sig <- plot_data[["res_all"]] %>% filter(
    LBTESTCD == electrolyte,
    consistency_flag == "inv_sig"
  ) %>%
    select(dose_group) %>%
    unique() %>%
    filter(row_number() == 1) %>%
    pull(dose_group)
  #*****************************************************************************

  #set the y-axis range respectiveley to the selected electrolyte
  yaxislowerrange <- if(electrolyte == "K") {
    0
  } else if(electrolyte == "CA") {
    2
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
  
  #*Get the data of all values from the CO2-group and the isoflurane-group which
  #*were used for generating VCGs
  sampling_group <- plot_data[["vcg_sample"]]

  #define words for the axis title
  yaxistitle <- if(electrolyte == "K"){
    "K<sup>+</sup>-concentration in serum [mmol/L]"
  }else if(electrolyte == "CA"){
    "Ca<sup>2+</sup>-concentration in serum [mmol/L]"
  }else{
    paste0(electrolyte)
  }
  
  #Get max and min mean values leading to consistent results or to respective errors
  maxcon <- ifelse(length(vcg_con) != 0, max(vcg_con), -5)
  mincon <- ifelse(length(vcg_con) != 0, min(vcg_con), -5)
  #*For incon sig results it also plays a role "on which side" of the dose mean the
  #*VCG mean is. Therefore, we split this into two groups:
  #* "above dose group" (ad), and "below dose group" (bd)
  maxincsig_ad <- ifelse(
    length(vcg_inc_sig[vcg_inc_sig >= original_mean$dose_mean]) != 0,
    max(vcg_inc_sig[vcg_inc_sig >=  original_mean$dose_mean]),
    -5
    )
  minincsig_ad <- ifelse(
    length(vcg_inc_sig[vcg_inc_sig >= original_mean$dose_mean]) != 0,
    min(vcg_inc_sig[vcg_inc_sig >=  original_mean$dose_mean]),
    -5
    )
  maxincsig_bd <- ifelse(
    length(vcg_inc_sig[vcg_inc_sig < original_mean$dose_mean]) != 0,
    max(vcg_inc_sig[vcg_inc_sig <  original_mean$dose_mean]),
    -5
    )
  minincsig_bd <- ifelse(
    length(vcg_inc_sig[vcg_inc_sig < original_mean$dose_mean]) != 0,
    min(vcg_inc_sig[vcg_inc_sig <  original_mean$dose_mean]),
    -5
    )
  maxincnonsig <- ifelse(length(vcg_inc_non_sig) != 0, max(vcg_inc_non_sig), -5)
  minincnonsig <- ifelse(length(vcg_inc_non_sig) != 0, min(vcg_inc_non_sig), -5)
  maxinvsig <- ifelse(length(vcg_inv_sig) != 0, max(vcg_inv_sig), -5)
  mininvsig <- ifelse(length(vcg_inv_sig) != 0, min(vcg_inv_sig), -5)
 
  
  #*Make a function to show the mean value of the CCG across all the dose groups
  ccgline <- function(){
    if(dose_group_to_plot == "Dose group 1"){
      list(type = "line", y0 = ~original_mean %>% pull(ccg_mean),
           y1 = ~original_mean %>% pull(ccg_mean), x0 = 0, x1 = 3.5, xref = "paper",
           line = list(color = "black", dash = "dot"))
    }
    else{
      list(type = "line", y0 = ~original_mean %>% pull(ccg_mean),
           y1 = ~original_mean %>% pull(ccg_mean), x0 = 0, x1 = 0.001, xref = "paper",
           line = list(color = "black", dash = "dot"))
    }
  }

  #*Make a function to toggle the "result boxes" off if there are no results
  #*leading to a respective error (e.g., if there are no VCG values leading to
  #*an inconsistent result, there "box" will hide under the CCG mean value (this 
  #*workaround is necessary as "NULL" values lead to appearance of random boxes
  #*in the plot))
  consistencybox <- function(restype, minval, maxval, boxcolor){
    if(restype == F){
      list(type = "line", y0 = ~original_mean %>% pull(ccg_mean),
        y1 = ~original_mean %>% pull(ccg_mean), x0 = 0, x1 = 1, xref = "paper",
        line = list(color = "black", dash = "dot"))
    }
    else{
      list(
        type = "rect", y0 = minval, y1 = maxval, x0 = 0, x1 = 1, xref = "paper",
        fillcolor = boxcolor, opacity = .3,
        pattern = "x"
           )
    }
  }
  
  #Add shapes and patterns with respect to consistency result
  
  #plot dots of electrolyte-values
  plot_resampling_results <- plot_ly() %>%
    #Add original dose group value
    add_trace(
      type = "scatter",
      mode = "markers",
      x = dose_group_to_plot,
      y = ~original_mean %>% pull(dose_mean),
      marker = list(color = "rgba(155,155,155,0.8)", size = 14, opacity = 1,
                    line = list(color = "darkred", width = 2), symbol = "x"),
      name = "dose group mean value",
      legendgroup = "DG",
      showlegend = ifelse(dose_group_to_plot == "Dose group 1", T, F)
    ) %>%
    #add "consistent zones" as blue boxes
    add_trace(
      x = dose_group_to_plot,
      type = "bar",
      base = mincon,
      y = as.numeric(maxcon) - as.numeric(mincon),
      marker = list(
        pattern = list(shape = "."),
        line = list(color = "#1f78b4", width = 2),
        color = "#1f78b4",
        opacity = 0.5
      ),
      name = "sampled VCG mean values\nleading to a consistent result",
      legendgroup = "VCG_con",
      showlegend = ifelse(dose_group_to_plot == dose_groups_having_con, T, F),
      visible = scatter_visibility_con
  ) %>%
    #add inconsistently significant results as red boxes
    #above dose group mean
    add_trace(
      x = dose_group_to_plot,
      type = "bar",
      base = minincsig_ad,
      y = as.numeric(maxincsig_ad) - as.numeric(minincsig_ad),
      marker = list(
        pattern = list(shape = "x"),
        line = list(color = "#e31a1c", width = 2),
        color = "#e31a1c",
        opacity = 0.5
      ),
      name = "sampled VCG mean values leading to\nan inconsistently significant result",
      legendgroup = "VCG_con",
      showlegend = ifelse(
        dose_group_to_plot == dose_groups_having_incon_sig_ad &
          scatter_visibility_incon_sig == T,
        T,
        F
        ),
      visible = consistencybox_visibility_inc_sig_ad
    ) %>%
    #below dose group mean
    add_trace(
      x = dose_group_to_plot,
      type = "bar",
      base = minincsig_bd,
      y = as.numeric(maxincsig_bd) - as.numeric(minincsig_bd),
      marker = list(
        pattern = list(shape = "x"),
        line = list(color = "#e31a1c", width = 2),
        color = "#e31a1c",
        opacity = 0.5
      ),
      name = "sampled VCG mean values leading to\nan inconsistently significant result",
      legendgroup = "VCG_con",
      showlegend = ifelse(
        #Show this legend only if no inconsistently significant values above dosage are shown
        dose_group_to_plot == dose_groups_having_incon_sig_bd &
          consistencybox_visibility_inc_sig_ad == F &
          consistencybox_visibility_inc_sig_bd == T,
        T,
        F
        ),
      visible = consistencybox_visibility_inc_sig_bd
    ) %>%
    #add inconsistently non-significant results orange boxes
    add_trace(
      x = dose_group_to_plot,
      type = "bar",
      base = minincnonsig,
      y = as.numeric(maxincnonsig) - as.numeric(minincnonsig),
      marker = list(
        pattern = list(shape = "+"),
        line = list(color = "#ff7f00", width = 2),
        color = "#ff7f00",
        opacity = 0.5
      ),
      name = "sampled VCG mean values leading to\nan inconsistently non-significant result",
      legendgroup = "VCG_con",
      showlegend = ifelse(dose_group_to_plot == dose_groups_having_incon_non_sig, T, F),
      visible = scatter_visibility_incon_non_sig
    ) %>%
    #add inverse significant results
    add_trace(
      x = dose_group_to_plot,
      type = "bar",
      base = mininvsig,
      y = as.numeric(maxinvsig) - as.numeric(mininvsig),
      marker = list(
        pattern = list(shape = "/"),
        line = list(color = "#f653a6", width = 2),
        color = "#f653a6",
        opacity = 0.5
      ),
      name = "sampled VCG mean values leading\nto an inverse significant result",
      legendgroup = "VCG_con",
      showlegend = ifelse(dose_group_to_plot == dose_groups_having_inv_sig, T, F),
      visible = scatter_visibility_inv_sig
    ) %>%
    layout(
      font = list(family = "times new roman", size = 22),
      barmode = "overlay",
      #add mean value of CCG as a horizontal dashed line
      # shapes = list(ccgline()),
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
        # showline = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        showticklabels = F,
        mirror = T,
        range = c(yaxislowerrange, yaxisupperrange)
        )
      )
  #*****************************************************************************
  #*****************************************************************************
  #*****************************************************************************
  #*Make a box plot showing all mean values of all VCGs along with mean value of CCG
  plot_ccg_vs_VCGs <- plot_ly() %>%
    #Make these dummy-traces in order to have a visible legend
    add_trace(type = "scatter", mode = "markers", x = "Control", y = -5, marker = list(color = "#6a3d9a", size = 7, opacity = 0.5, line = list(color = "#000000", width = 2)), name = "all sampled VCG mean values", legendgroup = "VCG_all", showlegend = ifelse(dose_group_to_plot == "Dose group 1", T, F)) %>%
    #add VCG mean values of all iterations
    add_trace(
      type = "box", boxpoints = "all",
      x = "Control",
      y = vcg_means,
      pointpos = 0,
      jitter = 1,
      marker = list(color = "#6a3d9a", opacity = 0.5, size = 7,
                    line = list(color = "#000000", width = 2)),
      fillcolor = "rgba(255,255,255,0)",
      line = list(color = "rgba(255,255,255,0)"),
      name = "all sampled VCG mean values",
      legendgroup = "VCG_all",
      showlegend = FALSE
    ) %>%
    #Add mean value of CCG
    add_trace(
      type = "scatter", mode = "markers",
      x = "Control",
      y = original_mean %>% pull(ccg_mean),
      marker = list(
        color = "#4affcd", symbol = "cross",
        opacity = 1,
        size = 14,
        line = list(color = "#000000", width = 2)
        ),
      name = "CCG mean value",
      legendgroup = "CCG"
    )%>%
    #Add the amount of iterations
    add_annotations(
      text = ~paste0("<i>n</i><sub>iterations</sub>: ", length(vcg_means)),
      size = 18,
      x = 0.5,
      y = 1,
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "top",
      align = "right",
      showarrow = FALSE
    ) %>%
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
        title = paste0(yaxistitle),
        showline = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        showticklabels = T,
        range = c(yaxislowerrange, yaxisupperrange),
        mirror = T
        )
    )
  
  #return requested plot
  if (whichplot == "VCGs") {return(plot_ccg_vs_VCGs)}
  else if(whichplot == "resampling") {return(plot_resampling_results)}
}