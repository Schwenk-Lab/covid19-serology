# elisa_vs_luminex_antigen_correlations.R
#
# description:  Covid-19 data 
#               Pairwise correlations between each antigen
#               ELISA vs Luminex data.
#
#
# required_packages: tidyverse, scales
#
# created: 2020-06-02
# creator: Tea Dodig-Crnkovic
# contributors: Mun-Gwan Hong, Cecilia Engel Thomas, Tea Dodig-Crnkovic
######################################################################
# empty global environment
rm(list = ls(all.names = TRUE))

# Load packages
library(tidyverse)
library(scales)

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
out_dir <- paste0("../results/pairwise_antigen_correlations/", data_set, "/", exp_method, "/")

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



#### Generate plots ####

scatter_fun = function(dat, x, y, title_txt, logscale) {
  
  cor_s <- round(cor(log2(dat[, x]), log2(dat[, y]), method="spearman"), 2)
  cor_p <- round(cor(log2(dat[, x]), log2(dat[, y]), method="pearson")^2, 2)
  
  
  # set x and y axis limits
  #ax_min <- min(dat[, x], dat[, y], na.rm=T)*1.1
  #ax_max <- max(dat[, x], dat[, y], na.rm=T)*0.9
  
  
  p <- ggplot(dat, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point(size=2) +
    theme_bw() +
    labs(x = x,
         y = y,
         caption = paste0("Plot date: ", plot_date,
                          "\nInput data: ", data_file_path),
         title = title_txt,
         subtitle = paste0("Rho=", cor_s, ", R^2= ", cor_p)) +
    #geom_hline(yintercept=1.1, linetype="dashed", color = "red") +
    #geom_hline(yintercept=0.8, linetype="dashed", color = "red") +
    #coord_cartesian(ylim=c(ax_min, ax_max), xlim=c(ax_min, ax_max)) +
    scale_y_continuous(trans = 'log2', breaks = log_breaks()) +
    scale_x_continuous(trans = 'log2', breaks = log_breaks()) +
    labs(title = paste0(title_txt, " log2"),
           subtitle = paste0("Rho=", cor_s, ", R^2= ", cor_p)) +
    geom_smooth(method=lm, linetype=1, color="red", size=0.5,
                fill="gray75", se=TRUE) + # add CI
    geom_point(size=2)
  
  return(p)
  
}


# generate plots for all antigen combos

# get all pair combinations
paired_ag <- expand.grid(c("S1", "NCP", binder$Name_detect_Ab),
                         c("S1", "NCP", binder$Name_detect_Ab), stringsAsFactors = F)

# exclude self pair
ag_excl <- sapply(1:dim(paired_ag)[1], function(i) identical(paired_ag[i, "Var1"],
                                                             paired_ag[i, "Var2"]))
paired_ag <- paired_ag[!ag_excl,]


plot_list_log2_DBS <- list(1:dim(paired_ag)[1])
plot_list_log2_plasma <- list(1:dim(paired_ag)[1])

for(i in 1:dim(paired_ag)[1]){

  plot_list_log2_DBS[[i]] <-
    scatter_fun(df[df$sample_type == "DBS", ],
                paired_ag$Var1[i], paired_ag$Var2[i],
                title_txt = "MFI data (DBS)", logscale =T)
  
  plot_list_log2_plasma[[i]] <-
    scatter_fun(df[df$sample_type == "Plasma", ],
                paired_ag$Var1[i], paired_ag$Var2[i],
                title_txt = "MFI data (Plasma)", logscale =T)
}

#### Generate plots ####
pdf_list <- list(plot_list_log2_DBS,
                 plot_list_log2_plasma)

txt <- c("log2_DBS", "log2_plasma")


for(s in 1:length(pdf_list)){
  
  plot_dat <- pdf_list[[s]]
  
  
  pdf(paste0(out_dir,
             format(Sys.time(),"%Y-%m-%d_%H%M%S"),"_pairwise_ag_cors_",
             txt[s], ".pdf"),
      width=6,height=6, useDingbats=F)
  
  for(p in 1:dim(paired_ag)[1]){
    print(plot_dat[[p]])
  }
  
  dev.off()
  
}




# extract correlation values

for(i in 1:dim(paired_ag)[1]){


  paired_ag$rho_DBS[i] <-
    round(cor(log2(df[df$sample_type == "DBS", paired_ag$Var1[i]]), 
            log2(df[df$sample_type == "DBS", paired_ag$Var2[i]]), 
            use="complete.obs",
            method="spearman"), 2)
  
  
  
  paired_ag$r2_DBS[i] <-
    round(cor(log2(df[df$sample_type == "DBS", paired_ag$Var1[i]]), 
              log2(df[df$sample_type == "DBS", paired_ag$Var2[i]]), 
              use="complete.obs",
              method="pearson")^2, 2)
  
  
  paired_ag$rho_Plasma[i] <-
    round(cor(log2(df[df$sample_type == "Plasma", paired_ag$Var1[i]]), 
            log2(df[df$sample_type == "Plasma", paired_ag$Var2[i]]), 
            use="complete.obs",
            method="spearman"), 2)
  
  
  paired_ag$r2_Plasma[i] <-
    round(cor(log2(df[df$sample_type == "Plasma", paired_ag$Var1[i]]), 
              log2(df[df$sample_type == "Plasma", paired_ag$Var2[i]]), 
              use="complete.obs",
              method="pearson")^2, 2)
  
  
}


write.table(paired_ag, paste0(out_dir,
                                   format(Sys.time(),"%Y-%m-%d_%H%M%S"),
                                   "_pairwise_ag_cors_log2_data",
                                   ".txt"),
            sep="\t", quote = F, row.names = F)
