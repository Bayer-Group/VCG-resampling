#*This function creates plots with respect to the selected study
#*1) Read the data generated in K1_get_data
#*2) Read the function which generates the plots using the plotly-package
#*   You can choose:
#*   - studydata (i.e., data you want to visualize)
#*   - sex (select the sex)
#*   - electrolyte (K or CA in our case)
#*   - whichplot (choose the plot in which way you want to visualize the values)
#*     - "hist_all" gives an uncolored histogram
#*     - "hist_narcosis" shows a histogram colored by the used anesthetics
#*     - "box_all" shows uncolored box plot with respect to the study year
#*     - "box_year_narcosis" shows a box plot with respect to the study year and
#*       colored by the used anesthetics
#*     - "box_narcosis" shows the box plots with respect to the used anesthetics
#*3) Execute the function for K and Ca values for each sex
#*4) Plotly graphs can be exported manually afterwards

#load packages
require(data.table)
require(tidyverse)
require(plotly)
require(webshot)


#read studydata table from data/Derived folder
studydata_to_test <- fread(paste0(der, "/electrolyte_parameters_4wk_rat.csv"))

#define function for making plots
batch_plots <- function(studydata_to_test,
                        sex,
                        electrolyte,
                        whichplot,
                        anesthetic1 = "isoflurane",
                        anesthetic2 = "CO2")
  {
  #Combine CO2/O2 and CO2/air into one group.
  #Do this only if "anesthetic1" is isoflurane
  studydata_harmonized_anesthetics <- if(anesthetic1 == "isoflurane") {
    studydata_to_test %>%
      #before 2017 and to "isoflurance" for studies after 2017
      mutate(
        ANESTHETICS = case_when(
          ANESTHETICS == "unknown" & START_YEAR < 2017 ~ "CO2",
          ANESTHETICS == "unknown" & START_YEAR >= 2017 ~ "isoflurane",
          grepl("CO2", ANESTHETICS) ~ "CO2",
          TRUE ~ ANESTHETICS
          )
        )
  } else {
    studydata_to_test %>%
      filter(ANESTHETICS %in% c(anesthetic1, anesthetic2))
  }

  
  #filter down studydata by which electrolyte needs to be visulized
  studydata_to_plot <- studydata_harmonized_anesthetics %>% filter(
    LBTESTCD == electrolyte,
    SEX == sex
  )
  
  #Calculate the population of values in each year for the attachments in the
  #box plots
  population_per_year <- studydata_to_plot %>%
    group_by(START_YEAR) %>%
    summarize(pop_per_year = n()) %>%
    mutate(pop_per_year_str = paste0("<i>n</i> = ", pop_per_year))
  
  #define words for the axis title
  electrolyte_word <- if(electrolyte == "K"){
    "K<sup>+</sup>"
  } else if(electrolyte == "CA"){
    "Ca<sup>2+</sup>"
  } else if(electrolyte == "SODIUM"){
    "Na<sup>+</sup>"
  } else if(electrolyte == "PHOS"){
    "PO<sub>4</sub><sup>3-</sup>"
  }
  #write axis title
  axistitle <- paste0(electrolyte_word, "-concentration in serum [mmol/L]")
  titleelectrolyte <- ifelse(electrolyte == "K", "Potassium", "Calcium")
  titlesex <- ifelse(sex == "M", "<b>males</b>", "<b>females</b>")
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
  #Customize lower range of y-axis
  axisrange_l <- if(electrolyte == "K") {
    2.5
  } else if(electrolyte == "CA") {
    2.1
  } else if(electrolyte == "SODIUM") {
    135
  } else if(electrolyte == "PHOS") {
    .5
  }
  #Customize upper range of y-axis
  axisrange_u <- if(electrolyte == "K") {
    8.5
  } else if(electrolyte == "CA") {
    3.5
  } else if(electrolyte == "SODIUM") {
    155
  } else if(electrolyte == "PHOS") {
    5
  }
  #*****************************************************************************
  #*****************************************************************************
  #*Make the plots
  #plot histogram of electrolyte-values
  plot_hist_all <- plot_ly(
    type = "histogram",
    ybins = list(size = xbinsize, start = xbinstart),
    opacity = 0.5,
    data = studydata_to_plot,
    y = ~LBORRES,
    marker = list(color = "#a5a5a5", line = list(color = "#000000", width = 2))
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
      font = list(family = "times new roman", size = 22),
      yaxis = list(
        title = axistitle,
        showline = T,
        showgrid = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T,
        dtick = xticks,
        range = c(axisrange_l, axisrange_u)
        ),
      xaxis = list(
        title = "Frequency",
        showline = T,
        showgrid = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T,
        autorange = "reversed"
        ),
      showlegend = F
    )
  #*****************************************************************************
  #plot histogram of electrolyte-values with respect to anesthetics
  plot_hist_narcosis <- plot_ly(opacity = 0.5, y = ~LBORRES) %>%
    add_histogram(
      ybins = list(size = xbinsize, start = xbinstart),
      data = studydata_to_plot %>% filter(ANESTHETICS == anesthetic2),
      marker = list(color = "#a5a5a5", line = list(color = "#000000", width = 2)),
      name = paste0(anesthetic2),
      legendgroup = "anesthetic2",
      showlegend = F#ifelse(sex == "M", T, F)
    ) %>%
    add_histogram(
      ybins = list(size = xbinsize, start = xbinstart),
      data = studydata_to_plot %>% filter(ANESTHETICS == anesthetic1),
      marker = list(color = "#6a3d9a", line = list(color = "#000000", width = 2)),
      name = paste0(anesthetic1),
      legendgroup = "anesthetic1",
      showlegend = F#ifelse(sex == "M", T, F)
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
      font = list(family = "times new roman", size = 22),
      yaxis = list(
        title = axistitle,#ifelse(sex == "M", "", paste0(axistitle)),
        title = "Frequency",
        showline = T,
        showgrid = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T,
        range = c(axisrange_l, axisrange_u),
        dtick = xticks
                   ),
      xaxis = list(
        title = "Frequency",
        showline = T,
        showgrid = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T,
        autorange = "reversed"
        ),
      barmode = "overlay"
    )
  #*****************************************************************************  
  #plot electrolyte with respect to study-start-year
  plot_box_all <- plot_ly(
    type = "box",
    marker = list(
      color = "#a5a5a5",
      size = 7, opacity = 0.5,
      line = list(color = "#000000", width = 2)
      ),
    line = list(color = "#000000", width = 2),
    fillcolor = "#a5a5a5",
    boxpoints = "all", jitter = "0.7", pointpos = "0",
    data = studydata_to_plot,
    x = ~START_YEAR,
    y = ~LBORRES
  ) %>%
    #Add the count of data points
    add_annotations(
      text = ~paste0("<i>n</i><sub>total</sub>: ", studydata_to_plot %>% nrow()),
      font = list(family = "times new roman", size = 22),
      x = 0.95,
      y = 0.95,
      xref = "paper",
      yref = "paper",
      xanchor = "right",
      yanchor = "top",
      align = "right",
      showarrow = F
    ) %>%
    #Add a "B" above the figure to show the panel description
    add_annotations(
      text = "<b>B</b>",
      x = 0.05,
      y = 1,
      xref = "paper",
      yref = "paper",
      showarrow = F,
      xanchor = "middle",
      yanchor = "bottom"
    )  %>%
    #Add the population from each year under each box
    add_annotations(
      data = population_per_year,
      text = ~paste0(pop_per_year_str),
      font = list(family = "times new roman", size = 16),
      x = ~START_YEAR,
      y = 0.01,
      yref = "paper",
      xanchor = "middle",
      yanchor = "bottom",
      showarrow = F
    ) %>%
    layout(
      font = list(family = "times new roman", size = 22),
      xaxis = list(
        title = "Year of the study begin",
        showline = T,
        showgrid = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T,
        tickmode = "linear"
        ),
      yaxis = list(
        title = "",
        showline = T,
        showgrid = T,
        showticklabels = F,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T,
        range = c(axisrange_l, axisrange_u)
        ),
      showlegend = F
    )
  #*****************************************************************************  
  #plot electrolyte with respect to study-start-year
  plot_box_by_year_by_narcosis <- plot_ly(
    x = ~START_YEAR,
    y = ~LBORRES,
    opacity = 0.8) %>%
    #add values from CO2 subset
    add_boxplot(
      data = studydata_to_plot %>% filter(ANESTHETICS == anesthetic2),
      marker = list(color = "#a5a5a5",
                    size = 7, opacity = 0.5,
                    symbol = "triangle-up",
                    line = list(color = "#000000", width = 2)),
      line = list(color = "#000000", width = 2),
      fillcolor = "#a5a5a5",
      boxpoints = "all", jitter = "0.7", pointpos = "0",
      name = paste0(anesthetic2),
      legendgroup = "anesthetic2",
      showlegend = ifelse(sex == "M", T, F)
      ) %>%
    add_boxplot(
      data = studydata_to_plot %>% filter(ANESTHETICS == anesthetic1),
      marker = list(color = "#6a3d9a",
                    size = 7, opacity = 0.5,
                    symbol = "pentagon",
                    line = list(color = "#000000", width = 2)),
      line = list(color = "#000000", width = 2),
      fillcolor = "#6a3d9a",
      boxpoints = "all", jitter = "0.7", pointpos = "0",
      name = paste0(anesthetic1),
      legendgroup = "anesthetic1",
      showlegend = ifelse(sex == "M", T, F)
  ) %>%
    #Add the count of each group
    add_annotations(
      text = ~paste0(
        "<i>n</i><sub>", anesthetic2,"-group</sub>: ", studydata_to_plot %>% filter(ANESTHETICS == anesthetic2) %>% nrow(),
        "\n<i>n</i><sub>", anesthetic1,"-group</sub>: ", studydata_to_plot %>% filter(ANESTHETICS == anesthetic1) %>% nrow()),
      size = 18,
      x = 0.95,
      y = 0.95,
      xref = "paper",
      yref = "paper",
      xanchor = "right",
      yanchor = "top",
      align = "right",
      showarrow = FALSE
    ) %>%
    #Add a "B" above the figure to show the panel description
    add_annotations(
      text = "<b>B</b>",
      x = 0.05,
      y = 1,
      xref = "paper",
      yref = "paper",
      showarrow = F,
      xanchor = "middle",
      yanchor = "bottom"
    )  %>%
    #Add the population from each year under each box
    add_annotations(
      data = population_per_year,
      text = ~paste0(pop_per_year_str),
      font = list(family = "times new roman", size = 16),
      x = ~START_YEAR,
      y = 0.01,
      yref = "paper",
      xanchor = "middle",
      yanchor = "bottom",
      showarrow = F
    ) %>%
    layout(
      font = list(family = "times new roman", size = 22),
      xaxis = list(
        title = "Year of the study begin",
        tickmode = "linear",
        showline = T,
        showgrid = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T
        ),
      yaxis = list(
        title = "",#paste0(axistitle),
        showline = T,
        showgrid = T,
        showticklabels = F,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T,
        dtick = xticks,
        range = c(axisrange_l, axisrange_u)
        )
    )
  #*****************************************************************************
  #plot electrolyte values with respect to narcosis
  plot_box_by_narcosis <- plot_ly(
    x = ~ANESTHETICS,
    y = ~LBORRES,
    opacity = 0.8) %>%
    #Add the CO2 group as a box
    add_boxplot(
      data = studydata_to_plot %>% filter(ANESTHETICS == anesthetic2),
      marker = list(color = "#a5a5a5",
                    size = 7, opacity = 0.5,
                    symbol = "triangle-up",
                    line = list(color = "#000000", width = 2)),
      line = list(color = "#000000", width = 2),
      fillcolor = "#a5a5a5",
      boxpoints = "all", jitter = "0.7", pointpos = "0",
      name = paste0(anesthetic2),
      legendgroup = "anesthetic2"
    ) %>%
    add_boxplot(
      data = studydata_to_plot %>% filter(ANESTHETICS == anesthetic1),
      marker = list(color = "#6a3d9a",
                    size = 7, opacity = 0.5,
                    symbol = "pentagon",
                    line = list(color = "#000000", width = 2)),
      line = list(color = "#000000", width = 2),
      fillcolor = "#6a3d9a",
      boxpoints = "all", jitter = "0.7", pointpos = "0",
      name = paste0(anesthetic1),
      legendgroup = "anesthetic1",
      showlegend = ifelse(sex == "M", T, F)
    ) %>%
    #Add the count of each group
    add_annotations(
      text = ~paste0(
        "<i>n</i><sub>", anesthetic2,"-group</sub>: ", studydata_to_plot %>% filter(ANESTHETICS == anesthetic2) %>% nrow(),
        "\n<i>n</i><sub>", anesthetic1, "-group</sub>: ", studydata_to_plot %>% filter(ANESTHETICS == anesthetic1) %>% nrow()),
      size = 18,
      x = 0.95,
      y = 0.95,
      xref = "paper",
      yref = "paper",
      xanchor = "right",
      yanchor = "top",
      align = "right",
      showarrow = FALSE
    ) %>%
    #Add a "B" above the figure to show the panel description
    add_annotations(
      text = "<b>B</b>",
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
      xaxis = list(
        title = "Anesthetics",
        showline = T,
        showgrid = T,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T
        ),
      yaxis = list(
        title = ifelse(anesthetic1 == "isoflurane", paste0(axistitle), ""),
        showline = T,
        showgrid = T,
        showticklabels = F,
        linewidth = 2,
        ticks = "outside",
        tickwidth = 2,
        ticklen = 10,
        mirror = T,
        dtick = xticks,
        range = c(axisrange_l, axisrange_u)
        ),
      showlegend = T
    )
  #*****************************************************************************
  #return requested plot
  if (whichplot == "hist_all") {return(plot_hist_all)}
  else if(whichplot == "hist_narcosis") {return(plot_hist_narcosis)}
  else if(whichplot == "box_all") {return(plot_box_all)}
  else if(whichplot == "box_year_narcosis") {return(plot_box_by_year_by_narcosis)}
  else if(whichplot == "box_narcosis") {return(plot_box_by_narcosis)}
}