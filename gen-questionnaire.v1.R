# ------------------------------------------------------------------------------
# Generate a table of questionnaires
# ------------------------------------------------------------------------------
# created  : 2020-04-27 by Mun-Gwan
# modified :
# ------------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(readxl)


#----- File names --------------------------------------------------------------

fn <- list(
  i = list(                               #  input
    qnaires = '../data/samples/cbc_01/Covid19.xls'
  ),
  o = list(                               #  output
    ut = '../data/questionnaire.v1.RData'
  )
)

#  Check all exists
stopifnot(all(file.exists(c('.', unlist(fn$i)))),
          all(file.exists(dirname(c('.', unlist(fn$o))))))


#----- MAIN --------------------------------------------------------------------


##  Read the table created from questionnaires by manual typing

qnaires0 <- read_xls(
  fn$i$qnaires,
  sheet= "Sheet1",
  skip = 3
) %>%
  filter_at(vars(Postage:...24), any_vars(!is.na(.)))   # remove NA only rows

stopifnot(anyDuplicated(qnaires0$ID) == 0)
qnaires <- qnaires0

# Sex
qnaires <- qnaires %>%
  pivot_longer(cols= Female:Male, names_to = "Sex", values_drop_na = T) %>%
  select(ID, Sex) %>%
  #  Other sex
  filter(ID != "CBC+01+0186") %>%   # two entries
  add_row(ID = "CBC+01+0186", Sex = "Other") %>%
  add_row(ID = "CBC+01+0454", Sex = "Other") %>%

  left_join(qnaires %>% select(-c(Female:Male)), ., by= "ID")
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
  pivot_longer(cols= No...12:`Yes, severe`, names_to = "Symptom", values_drop_na = T) %>%
  select(ID, Symptom) %>%
  left_join(qnaires %>% select(-c(No...12:`Yes, severe`)), ., by= "ID") %>%
  mutate(
    Symptom = sub("No...12", "No", Symptom) %>%
      factor(levels= c("No", "Yes, mild", "Yes, fever", "Yes, severe"), ordered = T)
  )
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

# Breath
qnaires <- qnaires %>%
  pivot_longer(cols= No...16:Yes...17, names_to = "Breath", values_drop_na = T) %>%
  select(ID, Breath) %>%
  left_join(qnaires %>% select(-c(No...16:Yes...17)), ., by= "ID") %>%
  mutate(Breath = sub("\\.\\.\\.1[67]", "", Breath))
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow


# Positive (Covid-19)
qnaires <- qnaires %>%
  pivot_longer(cols= No...18:Yes...19, names_to = "Positive", values_drop_na = T) %>%
  select(ID, Positive) %>%
  left_join(qnaires %>% select(-c(No...18:Yes...19)), ., by= "ID") %>%
  mutate(Positive = sub("\\.\\.\\.1[89]", "", Positive))
stopifnot(nrow(qnaires) == nrow(qnaires0)) # confirm no change of nrow

# Number of OK spots
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


save(qnaires, file= fn$o$ut)
