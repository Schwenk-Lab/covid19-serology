## options_master_trans_serology.R

## Global options to be read in to all data analysis sub-scripts of Covid-19 Translation Serology Study
## Contributors: Mun-Gwan Hong, Tea Dodig-Crnkovic, Cecilia Engel Thomas
## Initialized: 2020/05/05


#------------ Data input options ------------------

#--- Working directory
setwd("/home/common/storage/projects/covid19/translational_serology/data_analysis/scripts/")

#--- Data set
# Options:
#    To allow each analysis script to use their internally defined data set:
#         "no_global_data_set_defined"
#    To force all analysis scripts (that allow for it) to use a specified data set:
#         "batch1"
#         "batch2"
#         "cbc03"

global_data_set = "batch2"

#--- Detection antibody
# Options:
#    To allow each analysis script to use their internally defined setting:
#         "no_global_detection_ab_defined"
#    To force all analysis scripts (that allow for it) to use a specified detection antibody or set of antibodies:
#         "all"
#         "IgG"
#         "IgM"
#         "IgA"
#         "IgG_IgM"
#         "IgG_IgA"
#         "IgM_IgA"

global_detection_ab = "IgG_IgM" 


#--- Data file path (full path in case working directory is changed)
# Should be the newest version of each data set

data_file_path_batch1 = normalizePath("../data/dbs_covid19_manuscript_batch1_cba_norm_v1.RData")
data_file_path_batch2 = normalizePath("../data/dbs_covid19_manuscript_batch2_cba_norm_v1.RData")
data_file_path_cbc03 = normalizePath("../data/dbs_covid19_manuscript_cbc03_cba_v1.RData")

#--- Function for getting path to selected data set
# Function is used in the individual scripts to define the path to the selected data set
get_data_set_path <- function(data_set) {
  
  if(data_set == "batch1"){
    data_file_path = data_file_path_batch1
    exp_method = "luminex"
  } else if(data_set == "batch2"){
    data_file_path = data_file_path_batch2
    exp_method = "luminex"
  } else if(data_set == "cbc03"){
    data_file_path = data_file_path_cbc03
    exp_method = "luminex"
  }
  
  return(list(data_file_path, exp_method))
  
}



#------------- Color options ------------------

# Defining global color options
global_color_options = matrix(c(
                            "negatives", "#E93434",
                            "positives", "#3458E9",
                            "neutral", "#606060",
                            "population_study", "#606060"
                          ),
                          nrow=2, ncol=4) %>%
                          t() %>%
                          as.data.frame()

names(global_color_options) = c("group","color")
rownames(global_color_options) = global_color_options$group





