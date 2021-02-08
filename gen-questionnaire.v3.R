# -----------------------------------------------------------------------------#
# Create the version 3
#   - Include the data of batch 2
# -----------------------------------------------------------------------------#
rm(list = ls())

library(tidyverse)
library(readxl)
# library(Useful2me) # remotes::install_github("Rundmus/Useful2me-R_package", upgrade= "never")

#----- File names --------------------------------------------------------------

fn <- list(
  i = list(                               #  input
  	qnaires = '../data/samples/cbc_01/Covid19_Batch2_backupJune1.xls',
  	onemore = '../data/samples/cbc_01/Covid19_Batch2_1098.xls',
  	pre = '../data/questionnaire.v2.RData'
  ),
  o = list(                               #  output
  	ut = '../data/questionnaire.v3.RData'
  )
)

#  Check all exists
stopifnot(all(file.exists(c('.', unlist(fn$i)))),
          all(file.exists(dirname(c('.', unlist(fn$o))))))

#  function: show_selected_comments
source('utils/util-show_selected_comments.R')

#----- MAIN --------------------------------------------------------------------



##  Read the table created from questionnaires by manual typing

qnaires0 <- read_xls(
	fn$i$qnaires,
	sheet= "Sheet1",
	skip = 3
) %>%
	filter_at(vars(`20-29`:...25), any_vars(!is.na(.)))   # remove NA only rows

qnaires1 <- read_xls(
	fn$i$onemore,
	sheet= "Sheet1",
	skip = 3
) %>%
	filter_at(vars(`20-29`:...25), any_vars(!is.na(.)))   # remove NA only rows

qnaires0 <- bind_rows(qnaires0, qnaires1)
rm(qnaires1)
stopifnot(anyDuplicated(qnaires0$ID) == 0)
qnaires <- qnaires0


qnaires <- qnaires %>%
	#  Comments
	rename(Comment = ...23, `Comment 2` = ...24) %>%
	#  not used columns
	select(-c(...25:...32))


#  Sex
show_selected_comments("sex", qnaires)

tmp <- qnaires %>%
	pivot_longer(cols= Female:Male, names_to = "Sex", values_drop_na = T) %>%
	select(ID, Sex) %>%
	#  Other sex
	add_row(ID = "CBC+01+0901", Sex = "Other")
stopifnot(anyDuplicated(tmp$ID) == 0)   #no duplicated ID

qnaires <- left_join(qnaires %>% select(-c(Female:Male)), tmp, by= "ID")
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow
# qnaires0[qnaires0$ID %in% qnaires$ID[duplicated(qnaires$ID)], c("ID", "Female", "Male")]

# Age group
qnaires <- qnaires %>%
	pivot_longer(cols= `20-29`:`70-74`, names_to = "Age_grp", values_drop_na = T) %>%
	select(ID, Age_grp) %>%
	left_join(qnaires %>% select(-c(`20-29`:`70-74`)), ., by= "ID")
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

# Symptom
qnaires <- qnaires %>%
	pivot_longer(cols= No...11:`Yes, severe`, names_to = "Symptom", values_drop_na = T) %>%
	select(ID, Symptom) %>%
	left_join(qnaires %>% select(-c(No...11:`Yes, severe`)), ., by= "ID") %>%
	mutate(
		Symptom = sub("No...11", "No", Symptom) %>%
			factor(levels= c("No", "Yes, mild", "Yes, fever", "Yes, severe"), ordered = T)
	)
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

# Breath
qnaires <- qnaires %>%
	pivot_longer(cols= No...15:Both, names_to = "Breath", values_drop_na = T) %>%
	select(ID, Breath) %>%
	left_join(qnaires %>% select(-c(No...15:Both)), ., by= "ID") %>%
	mutate(Breath = sub("No...15", "No", Breath))
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow


# Positive (Covid-19)
qnaires <- qnaires %>%
	mutate(Positive = NA)
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

# Number of OK spots
tmp <- qnaires %>%
	pivot_longer(cols= Two:Zero, names_to = "Nr_OK_spots", values_drop_na = T)
tmp[tmp$ID %in% tmp$ID[duplicated(tmp$ID)], ] %>%
	print.data.frame()

#  manually fix double ticks
tmp <- tmp %>%
	filter(ID != "CBC+01+0959" | Nr_OK_spots == "One")

qnaires <- tmp %>%
	select(ID, Nr_OK_spots) %>%
	left_join(qnaires %>% select(-c(Two:Zero)), ., by= "ID") %>%
	mutate(Nr_OK_spots = if_else(
		Nr_OK_spots == "Zero", 0L,
		if_else(
			Nr_OK_spots == "One", 1L,
			if_else(
				Nr_OK_spots == "Two", 2L, NA_integer_
			)
		)
	))
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow


#  Double lancett
qnaires <- qnaires %>%
	rename("Double_lancett" = "Yes") %>%
	mutate(Double_lancett = if_else(Double_lancett == 1, "Yes", "No"))


#  reorder the columns
qnaires <- qnaires %>%
	select(
		ID, Sex:Nr_OK_spots,
		Double_lancett,
		Consent_date = `Consent form`,
		everything()
	)


show_selected_comments("include", qnaires)

show_selected_comments("positive", qnaires)


#  Combine with previous version ------------------------------------------

qnaires_b2 <- qnaires %>%
	mutate(batch = "2")
rm(qnaires)

load(fn$i$pre)

qnaires <- qnaires %>%
	mutate(batch = "1") %>%

	bind_rows(., qnaires_b2) %>%
	select(ID, batch, everything())


save(qnaires, file= fn$o$ut)

