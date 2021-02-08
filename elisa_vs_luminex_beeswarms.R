# elisa_vs_luminex_beeswarms.R
#
# description:  Covid-19 ELISA data
#               S1 and NCP antigen beeswarm plots
#
#
# required_packages: tidyverse, reshape2, scales, ggforce
#
# created: 2020-06-11
# creator: Tea Dodig-Crnkovic
# contributors: Mun-Gwan Hong, Cecilia Engel Thomas, Tea Dodig-Crnkovic
######################################################################
# empty global environment
rm(list = ls(all.names = TRUE))

# load packages
library(tidyverse)
library(reshape2)
library(scales)
library(ggforce)

options(stringsAsFactors = FALSE)


#### Read in data ####
# set global options
source("options_master_trans_serology.R")

global_data_set = "no_global_data_set_defined"

#if there was a global data set defined, use that 
if(global_data_set != "no_global_data_set_defined") { 
  data_set = global_data_set 
  
  #otherwise use a localy defined data set, this option is primarily used for individual testing of the analysis scripts  
} else { 
  data_set = "cbc03" 
} 


# get path for chosen data set
data_file_path = get_data_set_path(data_set)[[1]]
exp_method = "elisa_vs_luminex"

# load data
load(data_file_path)

# output directory
out_dir <- paste0("../results/beeswarms_per_ag/", data_set, "/", exp_method, "/")

sinfo <- sinfo %>% rename("sample_id" = "id")

# ELISA data
elisa <- read.delim("../data/cbc_03_edsberg.elisa.v1.tsv", stringsAsFactors = F)

# set plot day (today)
plot_date <- format(Sys.time(),"%Y-%m-%d")



#### Find overlapping samples ####
# plot S1 (S1 protein level) and NCP (nucleocapsid protein level)

# select overlapping samples between ELISA and Luminex
select_samp <- intersect(elisa$sample_id, sinfo$sample_id) %>% unique()

df <- 
  elisa[elisa$sample_id %in% select_samp, ] %>% 
  left_join(sinfo, by="sample_id")


# exclude bindeders according to list from 2020-06-10
excl <- c(grep("Peptide", binder$id),
          grep("neutravidin", binder$id),
          grep("IgM",  binder$id),
          grep("IgA",  binder$id),
          grep("ACE2",  binder$id),
          grep("_S1",  binder$id))

binder <- binder[-excl, ]
mfi <- mfi[, intersect(colnames(mfi), binder$id)]

# make uniqe Name + detection ab (manuscript names)
binder$Name_detect_Ab <- paste0(binder$Name, "_", binder$detect_Ab)

# add luminex data
luminex_mfi <- as.data.frame(mfi)
colnames(luminex_mfi) <- binder$Name_detect_Ab

luminex_mfi$sample_id <- rownames(luminex_mfi)


df <- left_join(df, luminex_mfi, by = "sample_id")

colnames(df)[colnames(df) %in% c("S1", "NCP")] <- paste0(c("S1", "NCP"), "_ELISA")

selected_binders <- c("S1_ELISA", "NCP_ELISA", binder$Name_detect_Ab)

df_melt <- df[ , colnames(df) %in% c(selected_binders,"elisa.S1", "sample_type", "sample_id")] %>% 
  melt(c("sample_id", "sample_type", "elisa.S1"))

df_melt <- 
  df_melt %>% rename("protein.id"="variable")

df_melt$protein.id_sample_type <- paste0(df_melt$protein.id, "_",
                                         df_melt$sample_type)
df_melt$elisa.S1 <- factor(df_melt$elisa.S1)


# make beeswarm plots divided by antigen and sample type,
# and color by S1 ELISA cut off

# Color by sample type
plot_list <- list(1:length(unique(df_melt$protein.id_sample_type)))

# Specify how many plot to do per page
ncol_n <- 4
nrow_n <- 3

n_pages_needed <-
  ceiling(length(unique(df_melt$protein.id_sample_type))/(ncol_n*nrow_n))


for (i in seq_len(n_pages_needed)) {

  protein <- sort(unique(df_melt$protein.id_sample_type))
  df_plot <- df_melt[df_melt$protein.id_sample_type %in% protein,]

  # Exclude NaN
  df_plot <- df_plot[complete.cases(df_plot$value), ]

  # Save panel of plots to list per page
  plot_list[[i]] <-
    ggplot(transform(df_plot,
                     protein.id=levels(sort(unique(df_plot$protein.id)))),
           aes(x = 0, y = value, color= elisa.S1)) +
    geom_beeswarm(cex=4, alpha=0.4, dodge.width = 3,  #dodge.width = 3,
                  priority = "none") + #cex=0.5
    xlab("") +
    ylab("log2") +
    theme_bw() +
    scale_y_continuous(trans = 'log2', breaks = log_breaks()) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank()) +
    labs(caption = paste0("Plot date: ", plot_date,
                          "\nInput data: ", data_file_path)) +
    facet_wrap_paginate(protein.id~sample_type, ncol = ncol_n, nrow = nrow_n, page = i,
                        strip.position="top", scales="free_y") +
    scale_color_manual(values=c("black", "red"))

}

rm(df_plot)



# Beeswarms per antigen, MFI data
pdf(paste0(out_dir,
           format(Sys.time(),"%Y-%m-%d_%H%M%S"),"_beeswarm_lmx_vs_elisa_panel.pdf"),
    width=12,height=8, useDingbats=F)

for (i in 1:length(plot_list)) {
  print(plot_list[[i]])
}

dev.off()

