# ViCoG-resampling
This code visualizes the electrolyte-values of serum control-group animals followed by a resampling experiment.
As an input, the electrolyte values of the ViCoG data set are read as CSV and visualized
A SEND-formatted legacy study needs to be placed into the Data/Derived-subfolder in order to excecute the resampling experiment.

## Dependencies
In order to run the R-scripts the following R-packages were used:
- [`data.table`](https://CRAN.R-project.org/package=data.table)
- [`tidyverse`](https://doi.org/10.21105/joss.01686)
- [`DescTools`](https://cran.r-project.org/package=DescTools) 
- [`plotly`](https://plotly-r.com) 
- [`webshot`](https://CRAN.R-project.org/package=webshot) 

## Data
The data for visualizing the electrolyte values of the control-group animals used in this manuscript are located in the subfolder `~/Data/Derived`.
This data is composed from 32 rat studies stored in the SEND format. The anesthetics used for sedation of these animals during blood withdrawal is stored in the subfolder `~/Data/Original/narcosis.csv`.
Data for visualizing the results of the legacy study are is the property of Bayer pharmaceuticals AG and are subject to confidentiality. They can be made available upon request. User would need to set up a CSV table called `legacy_calcium_study.csv`.

## This repository composes the following R-scripts:
### `00_master.R`
Excecutes all remaining R-scripts and stores the results
### `01_visualization_master.R`
Excecutes the specified visualization steps of the electrolyte-values
### `02_make_dunnett_test.R`
Excecutes the resampling approach and (re)calculate the Dunnett's test.
This is done by the following procedure:
1) Calculate a Dunnett's test using the individual values of the CSV input file from `~/Data/Derived`
2) Replaces the original control group values with a random sample (without replacement) of values from rats which were anesthetized with either isoflurane or CO2 or both and recalculates the Dunnett's test
3) Repeats the previous step _n_ times where _n_ is a user-specified number
4) Summarizes results in a list giving the percentage of how often the original result was reproduced (per dose group) as well as the mean values of each sample of the VCGs.
  These results are further split into following categories:
    - Consistent results (ViCoGs led to a result consistent with the results of the original study).
    - Inconsistently significant results (ViCoGs led to a Dunnett-significant result while there was non in the original result).
    - Inconsistently non-significant results (ViCoGs led to no signficant result while there was one in the original result).
    - Inverted significant result (ViCoGs led to a significant result and in the original study was one too, but the direction of the significance is inverted).

### `03_apply_dunnett_test_to_legacy_study.R`
Applies the Dunnett's test to the legacy study of "02_make_dunnett_test.R"  using a resampling approach with respect to the selected scenarios.
The figures and tables in the manuscript can be reproduced using the following R markdown file. The generated figures are saved in inst/manuscript_2022

## The results are then visualized by the following scripts for the figures used in the manuscript:
- `04_visualize_electrolytes.R` to plot the distributions of the electrolyte values.
- `05_visualize_scenarios.R` to plot the concurrent control animals and show which ones are kept As sentinel animals.
- `06_visualize_picked_iterations.R` to plot the results of a single handpicked iteration of the resampling experiment.
- `07_visualize_resampling_results.R` to plot all results of all iterations of the resampling experiment.

## Figures
The resulting figures are stored in the `~/Results/HTML`-subfolder as HTML files. The images as JPEG format can be found under `~/Results/JPEG`.

## Author
Alexander Gurjanov - [Alexander Gurjanov](mailto:alexander.gurjanov@bayer.com)
