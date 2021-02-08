# elisa_vs_luminex_antigen_correlations_sample_types.R
#
# description:  Covid-19 data 
#               Pairwise correlations between samples
#               ELISA vs Luminex data.
#
#
# required_packages: tidyverse, reshape2, scales
#
# created: 2020-06-02
# creator: Tea Dodig-Crnkovic
# contributors: Mun-Gwan Hong, Cecilia Engel Thomas, Tea Dodig-Crnkovic
######################################################################
# empty global environment
rm(list = ls(all.names = TRUE))

# load packages
library(tidyverse)
library(reshape2)
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

colnames(df)[colnames(df) %in% c("S1", "NCP")] <- paste0(c("S1", "NCP"), "_ELISA")

selected_binders <- c("S1_ELISA", "NCP_ELISA", binder$Name_detect_Ab)



# one scatter plot for ELISA and one for LUMINEX (per antigen)

# elisa data
plot_data <- 
  df %>% 
  select(selected_binders, "subj.id", "sample_type")


plot_data <- reshape(plot_data, idvar = "subj.id",
        timevar = c("sample_type"), direction = "wide")




scatter_fun = function(dat, x, y, title_txt, logscale, cutoff) {
  
  cor_s <- round(cor(log2(dat[, x]), 
                     log2(dat[, y]), method="spearman", use="complete.obs"), 2)
  cor_p <- round(cor(log2(dat[, x]),
                     log2(dat[, y]), method="pearson", use="complete.obs")^2, 2)
  
  # set x and y axis limits
  ax_min <- min(dat[, x], dat[, y], na.rm=T)*1.1
  ax_max <- max(dat[, x], dat[, y], na.rm=T)*0.9
  
  p <- ggplot(dat, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point(size=2, na.rm=T) +
    theme_bw() +
    labs(x = x,
         y = y,
         caption = paste0("Plot date: ", plot_date,
                          "\nInput data: ", data_file_path),
         title = title_txt,
         subtitle = paste0("Rho=", cor_s, ", R^2= ", cor_p)) +
    coord_cartesian(ylim=c(ax_min, ax_max), xlim=c(ax_min, ax_max)) +
    scale_y_continuous(trans = 'log2', breaks = log_breaks()) +
      scale_x_continuous(trans = 'log2', breaks = log_breaks()) +
      labs(title = paste0(title_txt, " log2"),
           subtitle = paste0("Rho=", cor_s, ", R^2= ", cor_p)) +
    geom_smooth(method=lm, linetype=1, color="red", size=0.5,
                fill="gray80", se=TRUE, na.rm=T) + # add CI
    geom_point(size=2, na.rm=T)
  
  return(p)
  
}


plot_list_log2 <- list(1:length(selected_binders))


# if ELISA, add dotted lines

for(i in selected_binders){
  
  #i <- selected_binders[1]
  
  plot_list_log2[[i]] <- 
    scatter_fun(plot_data,
                paste0(i, ".DBS"), paste0(i, ".Plasma"),
                title_txt = "Raw data",  logscale =T)
   
  
    if(i %in% c("S1_ELISA", "NCP_ELISA") == T){
      
      plot_list_log2[[i]] <- 
        scatter_fun(plot_data,
                    paste0(i, ".DBS"), paste0(i, ".Plasma"),
                    title_txt = "Raw data",  logscale =T) +
        geom_hline(yintercept=1.1, linetype="dashed", color = "gray") +
        geom_vline(xintercept=1.1, linetype="dashed", color = "gray")
        
    }

}




pdf_list <- list(plot_list_log2)

txt <- c("log2_data")


for(s in 1:length(pdf_list)){
  
  plot_dat <- pdf_list[[s]]
  
  
  pdf(paste0(out_dir,
             format(Sys.time(),"%Y-%m-%d_%H%M%S"),"_pairwise_method_cors_dbs_plasma_",
             txt[s], ".pdf"),
      width=6,height=6, useDingbats=F)
  
  
  for(p in 1:length(selected_binders)){
    print(plot_dat[[p]])
  }
  

  dev.off()
  
}



# extract correlation values
correlation_df <- data.frame("binder_id" = selected_binders)

for(ab in selected_binders){
  
  
  correlation_df$rho[correlation_df$binder_id %in% ab] <- 
    round(cor(log2(plot_data[, paste0(ab, ".Plasma")]), 
                     log2(plot_data[, paste0(ab, ".DBS")]), 
                     method="spearman", use="complete.obs"), 2)
  
  correlation_df$r2[correlation_df$binder_id %in% ab] <- 
    round(cor(log2(plot_data[, paste0(ab, ".Plasma")]),
                     log2(plot_data[, paste0(ab, ".DBS")]),
                     method="pearson", use="complete.obs")^2, 2)
  
}

correlation_df$comment <- "Plasma vs DBS"

write.table(correlation_df, paste0(out_dir,
                   format(Sys.time(),"%Y-%m-%d_%H%M%S"),
                   "_pairwise_method_cors_dbs_plasma_log2_data",
                              ".txt"),
            sep="\t", quote = F, row.names = F)

