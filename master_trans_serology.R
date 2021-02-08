## Master script for pipelining analysis of Covid-19 Translation Serology Study
## Contributors: Mun-Gwan Hong, Tea Dodig-Crnkovic, Cecilia Engel Thomas
## Initialized: 2020/04/27

## Description:
# Sources/Renders other scripts needed for analysis
    # source(rscript.R)
    # render(rmarkdown_report.html, rmarkdown_script.Rmd)

#------------ Options ------------------


#clean up workspace
rm(list=ls())

# set working directory
setwd("/home/common/storage/projects/covid19/translational_serology/data_analysis/scripts/")

#never make factors automatically
options(stringsAsFactors = FALSE)

#packages
library(rmarkdown)

#--- Load global options

# load global options to get information about which data set to run
source("options_master_trans_serology.R")

#- Data set

#if there is a global data set defined, use that
if(global_data_set != "no_global_data_set_defined") {
  data_set = global_data_set
  exp_method = get_data_set_path(data_set)[[2]]
  
#otherwise use the following data set, this option is primarily used for individual testing of the analysis scripts 
} else {
  data_set = "batch1"
  exp_method = "luminex"
}

#- Detection antibody

#if there was a global setting, use that
if(global_data_set != "no_global_detection_ab_defined") {
  detection_ab = global_detection_ab
  
#otherwise use the following detection antibody, this option is primarily used for individual testing of the analysis scripts 
} else {
  detection_ab = "IgG_IgM"
}

#------------ Distributions of reactivity signals -----------


#------------ Pairwise antigen correlation plots ------------
# Raw MFI
# log2 MFI

source("pairwise_antigen_correlations.R")

#------------ Beeswarm plots ------------
# Beeswarm plots per antigen using MFI

select_batch <- "batch2" # "batch1" "batch2"

render(input = "beeswarm_plots_per_antigen.Rmd",
       output_file = paste0("../results/beeswarms_per_ag/",
                            select_batch, "/luminex/",
                            format(Sys.time(),"%Y-%m-%d_%H%M%S"),
                            "_beeswarm_plots_per_antigen.html"))





#------------ Heatmaps ------------------
# Proteins vs donors
source('heatmaps_protein_vs_donor.R')

# Proteins vs proteins
source('heatmaps_protein_vs_protein.R')



#------------ Dimensionality reuction -------------
# PCA/tSNE/UMAP

render(input = "/home/common/storage/projects/covid19/translational_serology/data_analysis/scripts/dimensionality_reduction_plots.Rmd",
        output_file = paste0("/home/common/storage/projects/covid19/translational_serology/data_analysis/results/dim_reduction_plots/",
                             data_set, "/",
                             exp_method, "/",
                             Sys.Date(), "_",
                             data_set, "_",
                             detection_ab,
                             "_dimensionality_reduction_plots.html")
      )



#------------ Frequency tables -------------

#  By questions in questionnaires
rmarkdown::render(
  input = "table-questionnaires-freq.Rmd",
  output_file = "../results/sero_positive_by_sd/table-questionnaires-freq.html"
)

rmarkdown::render(
  input = "table-questionnaires-freq-by_reactive.Rmd",
  output_file = "../results/sero_positive_by_sd/table-questionnaires-freq-by_reactive.html"
)

# #  Scripts that produced the similar results during exploration, to those by the scripts above.
# #------------ CBC-01 batch 2 - Seropos/neg table  -------------
# source("gen-dbs_covid19.batch2-seropos_neg.R")
# source("table-cbc_covid19-seropos_neg.R")
# #------------ CBC-01 batch 1 - Seropos/neg table  -------------
# source("gen-dbs_covid19-seropositive.R")



#------------ CBC-03 Elisa vs Luminex data ------------
if(select_batch == "cbc_03"){

  source(elisa_vs_luminex_beeswarms.R)
  source(elisa_vs_luminex_heatmaps_protein_vs_protein.R)
  source(elisa_vs_luminex_antigen_correlations.R)
  source(elisa_vs_luminex_antigen_correlations_sample_types.R)

}



#------------ CBC-01 sero-positive proportion -------------

rmarkdown::render(
  input = "anal-cbc01-seropos_prop.Rmd",
  output_file = "../results/sero_positive_by_sd/anal-cbc01-seropos_prop.html"
)




#------------ CBC-01 Olink sample - CBA MFI value distribution  -------------

source("plot-cbc_01_olink-distribution_of_MFI.R")


#------------ CBC-01 Olink sample - Association btw Olink NPX and questionnaire  -------------

source("anal-cbc_01_olink-assoc_qnaire.R")



#------------ Analysis of overlapping samples in assay runs ------------

rmarkdown::render(
  input = "anal-overlapping samples.Rmd",
  output_file = "../results/anal-overlapping samples.html"
)

#------------ Analysis of correlation between Olink and antigen profiles ------------

rmarkdown::render(
  input = "anal-seroposneg-olink_antigen.Rmd",
  output_file = "../results/cbc_01-olink/anal-seroposneg-olink_antigen.html"
)


