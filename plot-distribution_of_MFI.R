# ------------------------------------------------------------------------------
# Plot distribution of signals
# ------------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)


#----- File names --------------------------------------------------------------
source("options_master_trans_serology.R")

fn <- list(
  i = list(                               #  input
    c19 = data_file_path
    # c19 = '../data/dbs_covid19.v2.RData'
  ),
  o = list(                               #  output
    ut = "../results/distribution of MFIs of each binder.pdf"
  )
)

#  Check all exists
stopifnot(all(file.exists(c('.', unlist(fn$i)))),
          all(file.exists(dirname(c('.', unlist(fn$o))))))


#----- MAIN --------------------------------------------------------------------

load(fn$i$c19)

stopifnot(identical(rownames(mfi), sinfo$id))

# dummy binder IDs to minimize any errors
colnames(mfi) <- binder$id <- paste0("b", seq(1, ncol(mfi)))


si_mfi <- mfi %>%
  as_tibble() %>%
  bind_cols(sinfo, .) %>%
  filter(grepl("CBC\\+01", id)) %>%
  filter(Nr_OK_spots > 0)


ctrl_mfi <- mfi %>%
  as_tibble() %>%
  bind_cols(sinfo, .) %>%
  filter(grepl("Pos|Neg", Control))



##  Plot

pdf(fn$o$ut)

for(ii in binder$id) {
  is_IgA <- binder$detect_Ab[binder$id == ii] == "IgA"

  p <- ggplot() +
    aes_string(x = ii) +
    scale_x_log10() +
    xlab(
      paste(
        binder$detect_Ab[binder$id == ii],
        "-",
        binder$Binder[binder$id == ii]
      )
    ) +
    ylab("Density") +
    geom_histogram(
      aes(y= stat(density)/10, fill = Control),
      data = ctrl_mfi,
      bins = 30,
      alpha = 0.4
    ) +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    geom_density(
      aes(color= if(is_IgA) plate_iga else plate),
      data = si_mfi,
      na.rm= T,
      lwd = 2
    ) +
    guides(
      color = guide_legend(order = 1),
      fill = guide_legend(
        order = 2,
        override.aes = list(
          linetype = "blank"
        )
      )
    ) +
    labs(
      color= "Plate"
    )

  print(p)
}

dev.off()
