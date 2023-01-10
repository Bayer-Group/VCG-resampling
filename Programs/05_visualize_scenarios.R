#*This script generates scatter plots showing in which ranges VCG-values
#*were derived with respect to the selected scenario.
#*One plot shows the individual VCG values colored by which anesthetic was used
#*in the study design.
#*The other plot shows the OCG animals colored with respect to whether they
#*were removed from the group or kept as sentinel animals.
#*The dashed line shows the ranges from which the VCG animals were derived from.
#*Both plots are combined into one subplot.
##### program starts here
#*********************************************************************************;
#*********************************************************************************;
#*********************************************************************************;

#*********************************************************************************;
scenarios_plot <- function(
  plot_data = baycal1_both_m_all_but_two,
  electrolyte = "CA",
  general_legend_on_switch = T
)
  {
  #load libraries
  require(tidyverse)
  require(plotly)
  #Retrieve data from VCG population which was user for resampling
  vcg_sample <- plot_data[["sampling_group"]]
  #Retrieve sentinel animal data
  sentinel_animals <- plot_data[["sentinel_animals"]]$LBORRES
  #Retrieve OCG animal data which was removed
  removed_CCG_animals <- plot_data[["ccgs_to_be_removed"]]$LBORRES
  #*****************************************************************************
  #Customize lower range of y-axis with respect to the selected electrolyte
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
  
  #Add visibility switches
  #Render respective subgroups invisible if they are not present
  scatter_visibility_co2 <- ifelse("CO2" %in% vcg_sample$ANESTHETICS, T, F)
  scatter_visibility_iso <- ifelse("isoflurane" %in% vcg_sample$ANESTHETICS, T, F)
  
  #Render sentinel animals invisible if none were used in the scenario
  scatter_visibility_sentinel <- ifelse(length(sentinel_animals) == 0 | is.null(sentinel_animals), F, T)
  
  #*Add the ranges from which animals were derived.
  #*If sentinel animals were used, the mean +- 2*sd range of these are used
  #*as ranges from which animals were picked. If not, the entire sample population
  #*of the VCG repository was used.
  filterrange_upper <- ifelse(
    length(sentinel_animals) == 0 | is.null(sentinel_animals),
    max(vcg_sample$LBORRES),
    mean(sentinel_animals) + 2 * sd(sentinel_animals)
    )
  filterrange_lower <- ifelse(
    length(sentinel_animals) == 0 | is.null(sentinel_animals),
    min(vcg_sample$LBORRES),
    mean(sentinel_animals) - 2 * sd(sentinel_animals)
    )
  
  #*****************************************************************************
  #*Define shapes of markers in plot
  #Define shape of points for the CO2-subset here
  co2_markers <- list(
    color = "#cab2d6",
    size = 10,
    opacity = 0.5,
    line = list(color = "#000000", width = 2),
    symbol = "triangle-up"
  )
  #Define shape of points for the isoflurane-subset here
  iso_markers <- list(
    color = "#6a3d9a",
    size = 10,
    opacity = 0.5,
    line = list(color = "#000000", width = 2),
    symbol = "pentagon"
  )
  #Define shape of points for the removed animals
  removed_animals_markers <- list(
    color = "#000000",
    size = 14,
    opacity = 0.7,
    line = list(color = "#000000", width = 2),
    symbol = "x"
  )
  #Define shape of points for the sentinel animals
  sentinel_animals_markers <- list(
    color = "#ff7f00",
    size = 14,
    opacity = 0.8,
    line = list(color = "#000000", width = 2),
    symbol = "cross"
  )
  #*****************************************************************************
  #*Count the number of data points in the sets
  #Count the number of data points in the sample population within range of
  #the filters
  n_vcg_sample <- paste0(
    "n<sub>VCG sample population within filter range</sub>: ",
    vcg_sample %>%
      filter(
        between(LBORRES, filterrange_lower, filterrange_upper)
        ) %>% nrow()
    )
  #Count the number of removed CCG animals
  n_removed_animals <- paste0(
    "n<sub>removed CCG animals</sub>: ", length(removed_CCG_animals)
    )
  #Count the number of sentinel animals
  n_sentinel <- paste0(
    "n<sub>sentinel animals</sub>: ", length(sentinel_animals)
    )
  #*****************************************************************************
  #Make a scattered dot-plot with the VCG sample colored by ANESTHETICS
  #plot dots of electrolyte-values
  plot_sample_population <- plot_ly() %>%
    #Make these dummy-traces in order to have a visible legend
    add_trace(type = "scatter",
              mode = "markers",
              x = "",
              y = -5,
              marker = co2_markers,
              name = "CO<sub>2</sub>-subset",
              legendgroup = "co2_subset",
              showlegend = (scatter_visibility_co2 == general_legend_on_switch),
              visible = scatter_visibility_co2) %>%
    add_trace(type = "scatter",
              mode = "markers",
              x = "",
              y = -5,
              marker = iso_markers,
              name = "isoflurane-subset",
              legendgroup = "iso_subset",
              showlegend = (scatter_visibility_iso == general_legend_on_switch),
              visible = scatter_visibility_iso) %>%
    add_segments(x = "", xend = "",
                 y = -5, yend = -5,
                 name = "VCG filter range",
                 line = list(color = "black", dash = "dot"),
                 showlegend = general_legend_on_switch) %>%
    #add points from CO2 subset
    add_trace(
      type = "box", boxpoints = "all",
      x = "",
      y = vcg_sample %>% filter(ANESTHETICS == "CO2") %>% pull(LBORRES),
      pointpos = 0,
      jitter = 1,
      marker = co2_markers,
      fillcolor = "rgba(255,255,255,0)",
      line = list(color = "rgba(255,255,255,0)"),
      name = "CO<sub>2</sub>-subset",
      legendgroup = "co2_subset",
      showlegend = FALSE,
      visible = scatter_visibility_co2
    ) %>%
    #add points from isoflurane subset
    add_trace(
      type = "box", boxpoints = "all",
      x = "",
      y = vcg_sample %>% filter(ANESTHETICS == "isoflurane") %>% pull(LBORRES),
      pointpos = 0,
      jitter = 1,
      marker = iso_markers,
      fillcolor = "rgba(255,255,255,0)",
      line = list(color = "rgba(255,255,255,0)"),
      name = "isoflurane-subset",
      legendgroup = "iso_subset",
      showlegend = FALSE,
      visible = scatter_visibility_iso
    ) %>%
    # #Add count of VCG sample population animals within range as annotation
    # add_annotations(
    #     text = n_vcg_sample,
    #     x = 0.95, y = 0.95,
    #     xref = "paper", yref = "paper",
    #     showarrow = F,
    #     xanchor = "right",
    #     yanchor = "top"
    # ) %>%
    layout(
      plot_bgcolor="#e5ecf6",
      # font = list(size = 22),
      #Add dashed lines showing the ranges from which you derive your VCGs
      shapes = list(
        list(type = "line",
             y0 = filterrange_upper, y1 = filterrange_upper,
             x0 = 0, x1 = 2.09, xref = "paper",
             line = list(color = "black", dash = "dot")),
        list(type = "line",
             y0 = filterrange_lower, y1 = filterrange_lower,
             x0 = 0, x1 = 2.09, xref = "paper",
             line = list(color = "black", dash = "dot"))
      ),
      xaxis = list(title = "VCG sample population",
                   zerolinecolor = "#ffff", 
                   zerolinewidth = 2, 
                   gridcolor = "ffff"),
      yaxis = list(title = "Ca<sup>2+</sup>-concentration in serum [mmol/L]",
                   zerolinecolor = "#ffff", 
                   zerolinewidth = 2, 
                   gridcolor = "ffff",
                   range = c(yaxislowerrange, yaxisupperrange))
    )
  # plot_sample_population
  
  #Make a dot-plot showing CCG animals colored by whether they were removed
  #or kept within the study
  plot_ccg_and_sentinels <- plot_ly() %>%
    #Make these dummy-traces in order to have a visible legend
    add_trace(
      type = "scatter",
      mode = "markers",
      x = "", y = -5,
      marker = removed_animals_markers,
      name = "removed CCG animals",
      showlegend = general_legend_on_switch
      ) %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = "", y = -5,
      marker = sentinel_animals_markers,
      name = "sentinel animals",
      showlegend = (scatter_visibility_sentinel == general_legend_on_switch),
      visible = scatter_visibility_sentinel
    ) %>%
    #Add the CCG animals which were removed
    #add points from isoflurane subset
    add_trace(
      type = "box", boxpoints = "all",
      x = "", y = removed_CCG_animals,
      pointpos = 0,
      jitter = 1,
      marker = removed_animals_markers,
      fillcolor = "rgba(255,255,255,0)",
      line = list(color = "rgba(255,255,255,0)"),
      name = "removed CCG animals",
      showlegend = FALSE
    ) %>%
    #Add the CCG animals which were kept as sentinel animals
    add_trace(
      type = "box", boxpoints = "all",
      x = "", y = sentinel_animals,
      pointpos = 0,
      jitter = 0.5,
      marker = sentinel_animals_markers,
      fillcolor = "rgba(255,255,255,0)",
      line = list(color = "rgba(255,255,255,0)"),
      name = "sentinel animals",
      visible = scatter_visibility_sentinel,
      showlegend = FALSE
    ) %>%
    # #Add count of removed animals
    # add_annotations(
    #   text = n_removed_animals,
    #   x = 0.95, y = 0.95,
    #   xref = "paper", yref = "paper",
    #   showarrow = F,
    #   xanchor = "right",
    #   yanchor = "top"
    # ) %>%
    # #Add count of sentinel animals
    # add_annotations(
    #   text = n_sentinel,
    #   x = 0.95, y = 0.9,
    #   xref = "paper", yref = "paper",
    #   showarrow = F,
    #   xanchor = "right",
    #   yanchor = "top",
    #   visible = scatter_visibility_sentinel
    # ) %>%
    layout(
      plot_bgcolor="#e5ecf6",
      # font = list(size = 22),
      xaxis = list(title = "CCG animals",
                   zerolinecolor = "#ffff", 
                   zerolinewidth = 2, 
                   gridcolor = "ffff"),
      yaxis = list(title = "",
                   zerolinecolor = "#ffff", 
                   zerolinewidth = 2, 
                   gridcolor = "ffff",
                   range = c(yaxislowerrange, yaxisupperrange))
    )
  # plot_CCG_and_sentinels
  
  #Combine both plots into one
  VCG_recruitment_plot <- subplot(
    plot_sample_population, plot_ccg_and_sentinels,
    titleX = T, titleY = T
    ) #%>%
    # #Add the y-axis title as an annotation
    # add_annotations(
    #   x = -0.065, y = 0.5, text = "Ca-concentration in serum [mmol/L]",
    #   font = list(size = 24), textangle = 270, showarrow = F,
    #   xref= "paper", yref= "paper"
    # )
  VCG_recruitment_plot
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
