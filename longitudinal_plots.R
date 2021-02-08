# longitudinal_plots.R
#
# description:  Covid-19 longitudinal plots
#               Make two plots: IgG and IgM
#
# required_packages: tidyverse, reshape2, ggforce, scales
#
# created: 2020-06-15
# creator: Tea Dodig-Crnkovic
# contributors: Mun-Gwan Hong, Cecilia Engel Thomas, Tea Dodig-Crnkovic
######################################################################
# empty global environment
rm(list = ls(all.names = TRUE))

# load packages
library(tidyverse)
library(reshape2)
library(ggforce)
library(scales)

# load data
df_table <- read.delim("../data/samples/cbc_04/longitudinal/covid19_longitudinal.txt",
                 stringsAsFactors = F)

# set output directory
out_dir <- "../results/longitudinal_data/"

# set plot day (today)
plot_date <- format(Sys.time(),"%Y-%m-%d")

# exclude first sample that is from another person
df_table <- df_table[-1, ]

# IgG columns to use
igg <- c(29,30,31,32,33,34,35,38,48,49,50,51)
# IgM columns to use
igm <- c(55,56,57,58,59,60,61,64,74,75,76,77)

# "Comment" = days since onset of symptoms, which you can use a x-axis 
df <- df_table[, c(which(colnames(df_table) %in% c("id", "Comment")), igg, igm)]

df <- 
  df %>% 
  rename("days_since_symptoms_onset"="Comment")

# # exclude controls
# df <- df[, !colnames(df) %in% c("IGG_01_IGM", "IGM_02_IGM")]


# melt
df_melt <- melt(df, c("id", "days_since_symptoms_onset"))

# add IgG/IgM info
df_melt$detection <- NA
df_melt$detection[which(df_melt$variable %in% colnames(df_table)[igg])] <- "IgG"
df_melt$detection[which(df_melt$variable %in% colnames(df_table)[igm])] <- "IgM"

# values are not numeric -> fix
# remove leading and trailing white spaces
df_melt$value <- trimws(df_melt$value, which = c("both"))

# replace "," with "."
df_melt$value <- as.numeric(gsub(",", "", gsub("\\.", "", df_melt$value)))


# randomize factors for variables to get mixed color palette

# set color scheme
# cbp1 <- rep(c("#999999", "#E69F00", "#56B4E9", "#009E73",
          # "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 3)

# make one plot per Ig-type
p <- ggplot(data = df_melt, aes(x = days_since_symptoms_onset, 
                           y = value, group = variable,
                           color=variable, linetype=variable,
                           shape=variable)) +
  xlab("Days since onset of symptoms") +
  ylab("log2") +
  theme_bw() +
  geom_line() +
  geom_point(cex=1.7) +
  facet_grid(. ~ detection) +
  labs(caption = paste0("Plot date: ", plot_date)) +
  scale_y_continuous(trans = 'log2', breaks = log_breaks()) +
  scale_linetype_manual(values=rep(c("solid", "dashed"), 
                                   length(c(igg, igm)))) +
  scale_shape_manual(values=rep(c(15:25), 
                                length(c(igg, igm))))

p

# # make pdf
# pdf(paste0(out_dir,
#            format(Sys.time(),"%Y-%m-%d_%H%M%S"),"_igg_igm_panel.pdf"),
#     width=10,height=6, useDingbats=F)
# 
#   print(p)
# 
# dev.off()

