# -----------------------------------------------------------------------------#
# Create the version 2
#   - Check if the contents of batch 1 samples are identical to those in the
#     table shared before
#   - Due to the confirmation, the data in `questionnaire.v1.RData` is not used
#     any longer. Instead, the table for all batch 1 samples including later
#     delivery was created using the updated table only
#     'Covid19_punching_final.xls'.
#   - Include the data of the samples collected later than the batch 1 run
# -----------------------------------------------------------------------------#
rm(list = ls())

library(tidyverse)
# library(Useful2me) # remotes::install_github("Rundmus/Useful2me-R_package", upgrade= "never")

#----- File names --------------------------------------------------------------

fn <- list(
  i = list(                               #  input
  	qnaires = '../data/samples/cbc_01/Covid19_punching_final.xls',
  	pre = '../data/questionnaire.v1.RData'
  ),
  o = list(                               #  output
  	ut = '../data/questionnaire.v2.RData'
  )
)

#  Check all exists
stopifnot(all(file.exists(c('.', unlist(fn$i)))),
          all(file.exists(dirname(c('.', unlist(fn$o))))))

#  function: show_selected_comments
source('utils/util-show_selected_comments.R')

#----- MAIN --------------------------------------------------------------------


#  load the older version (v1) of `qnaire`
tmp <- load(fn$i$pre)
old_qnaires <- qnaires
rm(list = tmp)


#  Read the xls table of questionnaires by manual typing

qnaires0 <- read_xls(
	fn$i$qnaires,
	sheet= "Sheet1",
	skip = 3
) %>%
	filter_at(vars(Postage:...24), any_vars(!is.na(.)))   # remove NA only rows

stopifnot(anyDuplicated(qnaires0$ID) == 0)  # no dupl. of sample `ID`s

#  initilize output table, which is in the long format
qnaires <- qnaires0


#  Sex
show_selected_comments("sex", qnaires, c("...23", "...24"))

qnaires <- qnaires %>%
	pivot_longer(cols= Female:Male, names_to = "Sex", values_drop_na = T) %>%
	select(ID, Sex) %>%
	#  Other sex
	filter(ID != "CBC+01+0186") %>%   # two entries
	add_row(ID = "CBC+01+0186", Sex = "Other") %>%
	add_row(ID = "CBC+01+0454", Sex = "Other") %>%

	left_join(qnaires %>% select(-c(Female:Male)), ., by= "ID")
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

#  Age group
qnaires <- qnaires %>%
	pivot_longer(cols= `20-29`:`70-74`, names_to = "Age_grp", values_drop_na = T) %>%
	select(ID, Age_grp) %>%
	left_join(qnaires %>% select(-c(`20-29`:`70-74`)), ., by= "ID")
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

#  Symptom
qnaires <- qnaires %>%
	pivot_longer(cols= No...12:`Yes, severe`, names_to = "Symptom", values_drop_na = T) %>%
	select(ID, Symptom) %>%
	left_join(qnaires %>% select(-c(No...12:`Yes, severe`)), ., by= "ID") %>%
	mutate(
		Symptom = sub("No...12", "No", Symptom) %>%
			factor(levels= c("No", "Yes, mild", "Yes, fever", "Yes, severe"), ordered = T)
	)
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

#  Breath
qnaires <- qnaires %>%
	pivot_longer(cols= No...16:Yes...17, names_to = "Breath", values_drop_na = T) %>%
	select(ID, Breath) %>%
	left_join(qnaires %>% select(-c(No...16:Yes...17)), ., by= "ID") %>%
	mutate(Breath = sub("\\.\\.\\.1[67]", "", Breath))
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow


#  Positive (Covid-19)
qnaires <- qnaires %>%
	pivot_longer(cols= No...18:Yes...19, names_to = "Positive", values_drop_na = T) %>%
	select(ID, Positive) %>%
	left_join(qnaires %>% select(-c(No...18:Yes...19)), ., by= "ID") %>%
	mutate(Positive = sub("\\.\\.\\.1[89]", "", Positive))
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

#  Number of OK spots
qnaires <- qnaires %>%
	pivot_longer(cols= Two:Zero, names_to = "Nr_OK_spots", values_drop_na = T) %>%
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

#  reorder the columns
qnaires <- qnaires %>%
	select(
		ID, Sex:Nr_OK_spots,
		Postage,
		Consent_date = `Consent form`,
		"Comment" = "...23",
		"Comment 2" = "...24"
	)


#  check UK samples
show_selected_comments("include", qnaires, c("Comment", "Comment 2"))

#  confirm the identical contents

print(names(qnaires)[1:9])
print(names(qnaires)[10:ncol(qnaires)])

#  all except Comment and Comment 2 columns are identical
stopifnot(identical(old_qnaires[, 1:9], qnaires[1:nrow(old_qnaires), 1:9]))
rm(old_qnaires)


save(qnaires, file= fn$o$ut)








