# -----------------------------------------------------------------------------#
# Create `table-cbc01-seropos_by_6sd`
# -----------------------------------------------------------------------------#
# created  : 2020-09-15 by Mun-Gwan
# modified :
# -----------------------------------------------------------------------------#
rm(list = ls())

# library(dplyr)
library(tidyverse)
# library(Useful2me) # remotes::install_github("Rundmus/Useful2me-R_package", upgrade= "never")

#----- File names --------------------------------------------------------------
source("options_master_trans_serology.R")


fn <- list(
	i = list(                               #  input
		b1 = data_file_path_batch1,
		b2 = data_file_path_batch2,
		binder = '../data/assay_information/bead_id_information/CBA5_information_js_matching.csv',
		umap = list(
			set1 = '../results/dim_reduction_plots/batch1/luminex/2020-06-22_dim_red_dbs_covid19_manuscript_batch1_cba_norm_v1_detectionAb_IgG_IgM_seropositivity.txt',
			set2 = '../results/dim_reduction_plots/batch2/luminex/2020-06-22_dim_red_dbs_covid19_manuscript_batch2_cba_norm_v1_detectionAb_IgG_IgM_seropositivity.txt'
		)
	),
  o = list(                               #  output
  	ut = list(
  		set1 = '../results/sero_positive_by_sd/table-cbc01.batch1-seropos_by_6sd.tsv',
  		set2 = '../results/sero_positive_by_sd/table-cbc01.batch2-seropos_by_6sd.tsv'
  	)
  )
)

#  Check all exists
stopifnot(all(file.exists(c('.', unlist(fn$i)))),
          all(file.exists(dirname(c('.', unlist(fn$o))))))


##  CONSTANTS

SERONEG_PROP = 0.8    # assumed sero-negative proportion
CUTOFF_SD = 6      # Cutoff standard deviation

#----- MAIN --------------------------------------------------------------------


###  Load data

load(fn$i$b1)
bBs <- list(
	set1 = list(
		sinfo = sinfo,
		mfi = mfi,
		binder = binder
	)
)

tmp <- load(fn$i$b2)
bBs$set2 <- list(
	sinfo = sinfo,
	mfi = mfi,
	binder = binder
)

rm(list = tmp)


cbas <- read_csv(
	fn$i$binder,
	col_types = cols_only(
		# CBA_nr = col_character(),
		# Binder = col_character(),
		# Comment = col_character(),
		Bead_ID = col_character(),
		# Coupling_chemistry = col_character(),
		Name = col_character(),
		# Provider = col_character(),
		# `Edsberg Analysis` = col_character(),
		`In batch 1` = col_character(),
		`Batch 2 Analysis` = col_character(),
		Match_Batch_1 = col_character()
	)
)
stopifnot(anyDuplicated(cbas$Name) == 0)


##  Only the binders in common

bBs$set1$binder <- cbas %>%
	separate_rows(Match_Batch_1, sep = ", ") %>%
	distinct(Match_Batch_1, .keep_all = T) %>%
	drop_na(Match_Batch_1) %>%
	dplyr::select(Name = Match_Batch_1, `Batch 2 Analysis`) %>%
	left_join(bBs$set1$binder, ., by= "Name")

common_in_set1 <- bBs$set1$binder %>%
	filter(`Batch 2 Analysis` == "Include") %>%
	pull(id)

bBs$set1$binder <- bBs$set1$binder %>%
	filter(id %in% common_in_set1)

bBs$set1$mfi <- bBs$set1$mfi[, colnames(bBs$set1$mfi) %in% common_in_set1]
stopifnot(identical(colnames(bBs$set1$mfi), bBs$set1$binder$id))


bBs$set2$binder <- bBs$set2$binder %>%
	left_join(cbas %>% dplyr::select(Name, `In batch 1`, `Batch 2 Analysis`), by= "Name")

# bBs$set2$mfi <- bBs$set2$mfi[, bBs$set2$binder$`In batch 1` == "Yes"]
# bBs$set2$binder <- bBs$set2$binder %>%
# 	filter(`In batch 1` == "Yes")
stopifnot(identical(colnames(bBs$set2$mfi), bBs$set2$binder$id))


##  Population, Positive and Negative only

bBs <- lapply(
	bBs,
	function(ea) {
		ea$sinfo <- ea$sinfo %>%
			mutate(
				grp = case_when(
					grepl("CBC\\+", subj.id) ~ "Population",
					grepl("Positive", subj.id) ~ "Positive",
					grepl("Negative", subj.id) ~ "Negative",
					TRUE ~ NA_character_
				)
			)
		ea$mfi <- ea$mfi[!is.na(ea$sinfo$grp), ]
		stopifnot(identical(colnames(ea$mfi), ea$binder$id))
		colnames(ea$mfi) <- paste(ea$binder$Name, ea$binder$detect_Ab, sep = " - ")

		ea$sinfo <- ea$sinfo %>%
			drop_na(grp)
		return(ea)
	}
)

stopifnot(
	identical(rownames(bBs$set1$mfi), bBs$set1$sinfo$id),
	identical(rownames(bBs$set2$mfi), bBs$set2$sinfo$id)
)


###   UMAP

umap <- lapply(
	names(fn$i$umap) %>% `names<-`(., .),
	function(iset) {
		read.delim(
			fn$i$umap[[iset]],
			sep= " ",
			stringsAsFactors = F
		) %>%
			as_tibble()
	}
)



##  Sero-positive or not (T or F) matrix per set and cutoff
posneg <- lapply(c("set1", "set2") %>% `names<-`(., .), function(iset) {
	# iset = "set1"

	#  No duplicated CBC+01 samples
	stopifnot(
		anyDuplicated(
			bBs[[iset]]$sinfo$subj.id[grepl("CBC\\+", bBs[[iset]]$sinfo$subj.id)]
		) == 0
	)

	subj.id <- bBs[[iset]]$sinfo$subj.id
	id <- bBs[[iset]]$sinfo$id
	grp <- bBs[[iset]]$sinfo$grp

	#  All population samples
	mfi_pop <- bBs[[iset]]$mfi[grp == "Population", ]

	cat("\n> ", iset, "\n")
	for(igrp in unique(grp)) {
		cat("\n* Dimension of", igrp, "samples\n")
		print(dim(bBs[[iset]]$mfi[grp == igrp, ]))

		if(igrp %in% c("Positive", "Negative")) {
			#  Known positive/negative control samples
			bBs[[iset]]$sinfo %>%
				filter(grp == igrp) %>%
				pull(id) %>%
				sub("\\*[[:digit:]]*", "", .) %>%
				table() %>%
				print()
		}
	}

	#  Finding cutoff ---------------------------------------------------------

	mfi_pop <- mfi_pop %>%
		as_tibble()

	##  Based on peak value
	den <- mfi_pop %>%
		lapply(density)

	#  peak
	m <- sapply(den, function(x) x$x[which.max(x$y)])

	#  std.dev. of (SERONEG_PROP) around the peak
	s <- sapply(seq(m), function(ii) {
		df <- abs(mfi_pop[[ii]] - m[ii])
		qtl <- quantile(df, probs = c(SERONEG_PROP))
		sd(mfi_pop[[ii]][df <= qtl])
	})


	#  Cutoff = peak + 'CUTOFF_SD' * std.dev. of (SERONEG_PROP) around the peak
	cutoff <- m + CUTOFF_SD * s

	#  Positve = TRUE, Negative = FALSE
	truefalse <- function(coff, mat) {
		rep(coff, each= nrow(mat)) %>%
			matrix(., nrow= nrow(mat)) %>%
			{mat > .} %>%
			as_tibble()
	}

	truefalse(cutoff, bBs[[iset]]$mfi) %>%
		mutate(grp = grp, id = id, subj.id = subj.id) %>%
		left_join(umap[[iset]] %>% dplyr::select(id, umap_seropositive_IgG_IgM), by= "id") %>%
		mutate_if(is.logical, . %>% if_else(., 1L, 0L)) %>%
		dplyr::select(sample_id = id, subj.id, grp, everything())
})

for(iset in names(posneg)) {
	posneg[[iset]] %>%
		write_tsv(path = fn$o$ut[[iset]])
}

