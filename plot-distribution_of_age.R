# ------------------------------------------------------------------------------
# Plot distribution of age comparing with Stockholmares
# ------------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)


#----- File names --------------------------------------------------------------

fn <- list(
  i = list(                               #  input
    qn1 = '../data/dbs_covid19_manuscript_batch1_cba_v1.RData',
    qn2 = '../data/dbs_covid19_manuscript_batch2_cba_v1.RData',
    sthm = "../data/external_src/BE0101A6.tsv"
  ),
  o = list(                               #  output
    ut = "../results/distribution of age.pdf"
  )
)

#  Check all exists
stopifnot(all(file.exists(c('.', unlist(fn$i)))),
          all(file.exists(dirname(c('.', unlist(fn$o))))))


#----- MAIN --------------------------------------------------------------------

tmp <- load(fn$i$qn1)
sinfo1 <- sinfo %>%
  filter(grepl("CBC\\+01", id)) %>%
  select(Sex, Age_grp, batch)

load(fn$i$qn2)
sinfo2 <- sinfo %>%
  filter(grepl("CBC\\+01", id)) %>%
  select(Sex, Age_grp, batch)

rm(tmp)

qn_stat <- bind_rows(sinfo1, sinfo2) %>%
  drop_na(Sex) %>%
  filter(Sex != "Other") %>%
  mutate(batch = paste("Batch", batch)) %>%
  group_by(Sex, Age_grp, batch) %>%
  summarise(n = n())


###   Stockholm people

sthm0 <- read_tsv(
  fn$i$sthm,
  skip= 2,
  col_types = cols(
    region = col_skip(),
    age = col_character(),
    sex = col_character(),
    `2019` = col_double()
  )
)

sthm1 <- sthm0 %>%
  mutate(age = sub(" years", "", age)) %>%
  separate(age, c("age_from", "age_to"), sep = "-") %>%
  mutate_at(vars(starts_with("age_")), as.integer)

# confirm age range
stopifnot(
  max(sthm1$age_to) == 74,
  min(sthm1$age_from) == 20
)


#  age group change to fit to the groups in questionnaires
sthm <- sthm1 %>%
  mutate(
    age_grp = floor(age_from / 10) * 10,
    age_grp = paste(age_grp, age_grp + if_else(age_grp == 70, 4, 9), sep = "-")
  ) %>%
  group_by(age_grp, sex) %>%
  summarise(pop_2019 = sum(`2019`)) %>%
  mutate(
    Sex = case_when(
      sex == "men" ~ "Male",
      sex == "women" ~ "Female"
    )
  ) %>%
  select(Sex, Age_grp = age_grp, pop_2019)



###  Compare

ratio <- sum(qn_stat$n) / sum(sthm$pop_2019) / 2


pdf(fn$o$ut, width = 10)

p <- full_join(qn_stat, sthm, by= c("Sex", "Age_grp")) %>%
  ggplot() +
  aes(x = Age_grp, color = Sex, fill = "Stockholm\n(2019)") +
  facet_grid(Sex ~ batch) +
  geom_bar(aes(y = pop_2019 * ratio), stat= "identity") +
  geom_line(aes(y = n, group = 1), size = 3) +
  scale_y_continuous(
    name = "Participant",
    sec.axis = sec_axis(~./ratio, name = "Stockholm population (2019)")
  ) +
  scale_fill_manual(
    name = "", values = c("Stockholm\n(2019)" = "grey")
  ) +
  xlab("Age group")

print(p)
dev.off()
