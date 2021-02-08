# -----------------------------------------------------------------------------#
# Create version 4
#  - NOTE : This script is independent from previous versions. It reads all
#          original data files and generate output.
#  - Identity is confirmed by a seperate script.
#
# Parse MFI files from Luminex output files and combine it with sample and
# binder information. Because all analytes (or binders) were read by the
# instrument, despite only a small proportion of them were used, the columns
# with extremely low bead counts were removed.
# - Samples :
#     DBS-01 ~500 samples from Stockholm collected at early April
#     DBS-01 another ~500 samples collected at late April
# -----------------------------------------------------------------------------#
rm(list = ls())

library(tidyverse)
library(readxl)
library(BAf)

# library(Useful2me) # remotes::install_github("Rundmus/Useful2me-R_package", upgrade= "never")

#----- File names --------------------------------------------------------------

fn <- list(
   i = list(                               #  input
      cba = list(
         '../data/assay_information/bead_id_information/CBA_information.csv',
         '../data/assay_information/bead_id_information/CBA4_information.csv',
         '../data/assay_information/bead_id_information/CBA5_information_js_matching.csv'
      ),
      cba_cont = '../data/assay_information/bead_id_information/CBA_content_information.csv',
      qnaires = '../data/questionnaire.v3.RData',
      platemap = list(
         run1=   '../data/samples/cbc_01/CovPlateMap.xls',
         run2=   '../data/samples/cbc_01/CovPlateMap_Batch2.xls',
         run2.1= '../data/samples/cbc_01/CovPlateMap_Batch2_plate7_plate6mixup.xlsx'
      ),
      slayout = list(
         run1 = list(
            p1= '../data/assay_information/layout_information/covid_DBS_assay_plate1_layout.csv',
            p2= '../data/assay_information/layout_information/covid_DBS_assay_plate2_layout.csv',
            p3= '../data/assay_information/layout_information/covid_DBS_assay_plate3_layout.csv',
            p4= '../data/assay_information/layout_information/covid_DBS_assay_plate4_layout.csv',
            p5= '../data/assay_information/layout_information/covid_DBS_assay_plate5_layout.csv'
         ),
         run2 = list(
            p1= '../data/assay_information/layout_information/covid_DBS_batch2_assay_plate1_layout.csv',
            p2= '../data/assay_information/layout_information/covid_DBS_batch2_assay_plate2_layout.csv',
            p3= '../data/assay_information/layout_information/covid_DBS_batch2_assay_plate3_layout.csv',
            p4= '../data/assay_information/layout_information/covid_DBS_batch2_assay_plate4_layout.csv'
         )
      ),
      lmx = list(
         run1 = list(
            p1= '../data/luminex_data/DBS_covid19_assay_plate1_AnB_MD_Clyde_20200425_225239.csv',
            p2= '../data/luminex_data/DBS_covid19_assay_plate2_AnB_MD_20200426_171054.csv',
            p3= '../data/luminex_data/DBS_covid19_assay_plate_3_AnB_MD_Leia_20200426_213949.csv',
            p4= '../data/luminex_data/DBS_covid19_IgA_assay_plate4_AnB_MD_20200512_130319.csv',
            p5= '../data/luminex_data/DBS_covid19_IgA_assay_plate5_AnB_MD_20200512_200704.csv'
         ),
         run2 = list(
            p1= '../data/luminex_data/DBS_covid19_batch2_assay_plate1_AnB_MD_20200602_192541.csv',
            p2= '../data/luminex_data/DBS_covid19_batch2_assay_plate2_AnB_MD_20200603_163502.csv',
            p3= '../data/luminex_data/DBS_covid19_batch2_assay_plate3_AnB_MD_20200603_195901.csv',
            p4= '../data/luminex_data/DBS_Covid19_batch2_assay_plate4_AnB_MD_20200609_204831.csv'
         )
      )
   ),
   o = list(                               #  output
      ut = '../data/dbs_covid19.v4.RData'
   )
)

#  Check all exists
stopifnot(all(file.exists(c('.', unlist(fn$i)))),
          all(file.exists(dirname(c('.', unlist(fn$o))))))

#----- MAIN --------------------------------------------------------------------


#  Read files -------------------------------------------------------------

#  Binder `Name`

cba_cont <- read_csv(
   fn$i$cba_cont,
   col_types = cols_only(
      Coupling = col_integer(),
      CBA_nr = col_character(),
      Binder = col_character(),
      Name = col_character(),
      Bead_ID = col_character(),
      Coupling_chemistry = col_character(),
      Type = col_character(),
      Puprose = col_character(),
      Source = col_character(),
      # Date = col_date(),
      Comment = col_character()
   )
)

##  binders

cba0 <- lapply(
   fn$i$cba,
   read_csv,
   col_types = cols(
      CBA_nr = col_character(),
      Binder = col_character(),
      Comment = col_character(),
      Bead_ID = col_character(),
      Coupling_chemistry = col_character()
   )
)

cba <- bind_rows(cba0) %>%
   drop_na(Bead_ID) %>%
   select(CBA_nr:Name, Name_in_batch_1 = Match_Batch_1) %>%
   mutate(
      Binder = sub("\xb5l", "", Binder),  # remove weird characters
      detect_Ab = case_when(
         CBA_nr == 2 ~ "IgG",
         CBA_nr == 3 ~ "IgM",
         CBA_nr == 4 ~ "IgA",
         CBA_nr == 5 ~ "IgG",
         TRUE ~ NA_character_
      )
   ) %>%
   # CBA had both IgG and IgM
   bind_rows(., filter(., CBA_nr == 5) %>% mutate(detect_Ab = "IgM")) %>%

   #  Add `Name`
   left_join(cba_cont %>% select(CBA_nr, Bead_ID, Name),
             by= c("CBA_nr", "Bead_ID"),
             suffix = c("", ".y")) %>%
   mutate(
      Name = if_else(is.na(Name), Name.y, Name)
   ) %>%
   select(-Name.y)



# ##  Duplicated binders?
# cba %>%
#    group_by(Binder, Coupling_chemistry, detect_Ab) %>%
#    nest() %>%
#    mutate(
#       data = map(data, . %>% distinct(CBA_nr)),
#       n = map_int(data, ~ nrow(.x))
#    ) %>%
#    filter(n > 1) %>%
#    unnest(data) %>%
#    print.data.frame()

#  Confirm no duplicated bead IDs by mistake
stopifnot(anyDuplicated(cba[, c("CBA_nr", "Bead_ID", "detect_Ab")]) == 0)



##  sample information

#  The table from questionnaires
load(fn$i$qnaires)


#  Sheet names in the given Excel files
sheet_names <- list(
   run1= paste0("Plate P", 1:6) %>% `names<-`(as.character(1:6)),
   run2= paste0("batch2_plate", 1:6) %>% `names<-`(as.character(1:6)),
   run2.1= c("batch2_plate7", "batch2_plate6mixup") %>% `names<-`(c("7", "6_mixup"))
)

##  Plate Map

platemap0 <- lapply(
   c(paste0("run", 1:2), "run2.1"),
   function(irun) {
      lapply(
         seq_along(sheet_names[[irun]]),
         function(ii) {
            read_excel(
               fn$i$platemap[[irun]],
               sheet= sheet_names[[irun]][ii],
               col_names = c("RC", "ID", "punch_note_1", "punch_note_2"),
               range = "A1:D96"
            ) %>%
               mutate(
                  Plate = names(sheet_names[[irun]])[ii],
                  Run = if_else(irun == "run2.1", "run2", irun)
               )
         }
      )
   }
)

platemap <- platemap0
#  fix the mismatch by inconsistancy in IDs
platemap[[2]][[1]] <- platemap[[2]][[1]] %>%
   mutate(ID = sub("-01-", "+01+", ID))
#  manually fix missing sample ID
platemap[[3]][[2]] <- platemap[[3]][[2]] %>%
   mutate(ID = if_else(is.na(ID), "Unknown", ID))

#  Merge tables into one
platemap <- platemap %>%
   lapply(bind_rows) %>%
   bind_rows() %>%
   select(ID, everything()) %>%
   drop_na(ID) %>%
   left_join(qnaires, by= "ID")


#  Confirm no duplicated CBC-01 sample
stopifnot(anyDuplicated(platemap$ID[grep("CBC+01", platemap$ID)]) == 0)

#  Show the samples missing Questionnaire reply
platemap %>% filter(is.na(Nr_OK_spots)) %>% pull(ID)


##  Sample layout

slayout0 <- lapply(
   fn$i$slayout,
   lapply,
   function(x) {             # x <- sample(fn$i$slayout, 1)[[1]]
      read_csv(
         x,
         col_types= cols_only(
            `Plate number` = col_integer(),
            `Position (1-96)` = col_double(),
            `96_plate_well` = col_character(),
            sample_name = col_character(),
            tube_label = col_logical(),
            class = col_character(),
            subtype = col_character(),
            sample_type = col_character(),
            Sample_plate = col_character(),
            Sample_plate_well = col_character(),
            CBA_nr = col_character(),
            Detection_antibody = col_character(),
            `384-plate position` = col_character(),
            `Pos# in 384 well` = col_integer()
         )
      )
   }
)


#  Clean up tables and find IDs
slayout <- lapply(
   names(slayout0) %>% `names<-`(., .),
   function(irun) {
      lapply(
         slayout0[[irun]],
         function(eSL) {
            out <- eSL %>%
               rename(
                  "w96_plate" = "Plate number",
                  "w96_plate_well" = "96_plate_well",
                  det_ab = "Detection_antibody"
               ) %>%
               #  Simplify `Detection_antibody`
               mutate(det_ab = sub("anti-human ", "", det_ab)) %>%
               mutate(Run = irun) %>%
               left_join(platemap, by= c("Sample_plate" = "Plate", "Sample_plate_well" = "RC", "Run")) %>%
               select(id = "ID", everything())

            #  no missing nor overlapping ID
            stopifnot(all(xor(is.na(out$sample_name), is.na(out$id))))

            out %>%
               mutate(
                  id = if_else(is.na(id), sample_name, id)
               ) %>%
               select(-sample_name)
         }
      )
   }
)


##  Parse Luminex files

baf0 <- lapply(
   names(fn$i$lmx) %>% `names<-`(., .),
   function(irun) {
      lapply(
         names(fn$i$lmx[[irun]]) %>% `names<-`(., .),
         FUN= function(iP) {
            read_FlexMAP3D_csv(
               fn$i$lmx[[irun]][[iP]],
               assayid = iP,
               sinfo = slayout[[irun]][[iP]] %>%
                  mutate(plate = iP)
            )
         }
      )
   }
)


#  Find used analytes -----------------------------------------------------


baf <- lapply(
   names(baf0) %>% `names<-`(., .),
   function(irun) {
      lapply(
         names(baf0[[irun]]) %>% `names<-`(., .),
         FUN= function(iP) {
            eP <- baf0[[irun]][[iP]]

            # print(with(sI(eP), table(CBA_nr, det_ab)))
            #
            #        det_ab
            # CBA_nr IgG IgM
            #      2 192   0
            #      3   0 192

            bead_count <- parse_FlexMAP3D_csv_2_list(fn$i$lmx[[irun]][[iP]])$count %>%
               as_tibble()

            eP_cba <- lapply(sort(unique(eP@sinfo$det_ab)), function(iDA) {

               # Each CBA
               ea_cba <- eP %>%
                  BAf::filter(det_ab == iDA)
               ea_bead_count <- bead_count[eP@sinfo$det_ab == iDA, ]

               #  binder info for this CBA
               binder_this_cba <- cba %>%
                  filter(CBA_nr == ea_cba@sinfo$CBA_nr[1] & detect_Ab == iDA) %>%
                  # select(-CBA_nr) %>%
                  mutate(id = sub("Analyte ", "Analyte.", Bead_ID))

               #  Remove columns which becomes meaningless
               sI(ea_cba) <- sI(ea_cba) %>%
                  select(-det_ab, -CBA_nr)

               #  Exclude all binders that were not used
               ea_bead_count <- ea_bead_count[, bid(ea_cba) %in% binder_this_cba$id]
               ea_cba <- ea_cba %>%
                  BAf::select(bid(ea_cba) %in% binder_this_cba$id)

               #  Update binder info
               binder_this_cba <- binder_this_cba %>%
                  mutate(Bead_ID = sub("Analyte ", "", Bead_ID))
               sB(ea_cba) <- left_join(sB(ea_cba), binder_this_cba, by= "id")

               batch(ea_cba, "binder") <- paste0(batch(ea_cba, "binder"), '-', iDA)

               #  Update bead counts after excluding un-used beads
               ea_cba@assy_b$median_bead_count[, 1] <- apply(ea_bead_count, 2, median)
               ea_cba@assy_b$min_bead_count <- ea_cba@assy_b$median_bead_count
               ea_cba@assy_b$min_bead_count[, 1] <- apply(ea_bead_count, 2, min)

               ea_cba@assy_b$fail_flag[, 1] <-
                  if_else(ea_cba@assy_b$median_bead_count[, 1] < 35, "Too low bead count", "OK")

               ea_cba@assy_s$median_bead_count[, 1] <- apply(ea_bead_count, 1, median)
               ea_cba@assy_s$min_bead_count <- ea_cba@assy_s$median_bead_count
               ea_cba@assy_s$min_bead_count[, 1] <- apply(ea_bead_count, 1, min)
               # ea_cba@assy_s$fail_flag[, 1] <-
               #   if_else(ea_cba@assy_s$median_bead_count[, 1] < 35, "Too low bead count", "OK")

               bid(ea_cba) <-
                  make.names(paste0(sB(ea_cba)$Binder, "_", sB(ea_cba)$detect_Ab), unique = T)
               return(ea_cba)
            })


            #  remove not-used assay info
            eP_cba <- lapply(
               eP_cba,
               function(ea) {
                  ea@assy_s$reading_order <- NULL
                  ea@assy_s$fail_flag <- NULL
                  ea
               }
            )

            if(length(eP_cba) == 1) {
               out <- eP_cba[[1]]
            } else {  # when two CBAs were analysed on the same plate
               stopifnot(length(eP_cba) == 2)

               #  confirm identical sample IDs in IgG and IgM
               stopifnot(identical(eP_cba[[1]]@sinfo$id, eP_cba[[2]]@sinfo$id))

               #  remove variable columns to make the two sample information table same
               var_cols <- c("w96_plate", "w96_plate_well", "384-plate position", "Pos# in 384 well")
               eP_cba[[1]]@sinfo[, var_cols] <- eP_cba[[2]]@sinfo[, var_cols]

               out <- cbind_BAf(eP_cba[[1]], eP_cba[[2]])

            }

            stopifnot(anyDuplicated(out@binder$id) == 0)

            return(out)
         }
      )
   }
)

#  Combine and save the data ----------------------------------------------

baf_c <- list(
   run1 = list(
      IgG_IgM = do.call("rbind", baf$run1[1:3]),
      IgA = do.call("rbind", rev(baf$run1[4:5]))
   ),
   run2 = list(
      IgG_IgM = do.call("rbind", baf$run2[1:4])
   )
)


cbc01 <- lapply(
   baf_c,
   lapply,
   function(eA) {
      ##  extract various data set
      list(
         sinfo = eA@sinfo,
         mfi = eA@.Data,
         binder = eA@binder %>%
            select(-assay),
         median_bead_count = list(
            s = eA@assy_s$median_bead_count,
            b = eA@assy_b$median_bead_count
         ),
         min_bead_count = list(
            s = eA@assy_s$min_bead_count,
            b = eA@assy_b$min_bead_count
         )
      )
   }
)

save(cbc01, file= fn$o$ut)


# ##  Confirm identity of data contents
# load('../data/dbs_covid19.v3.RData')
# #  run1 & IgG,IgM
# stopifnot(
#    identical(cbc01$run1$IgG_IgM$sinfo$id, sinfo$id[!is.na(sinfo$w96_plate)]),
#    identical(cbc01$run1$IgG_IgM$binder$id, binder$id[binder$detect_Ab != "IgA"]),
#    identical(cbc01$run1$IgG_IgM$mfi, mfi[!is.na(sinfo$w96_plate), binder$detect_Ab != "IgA"]),
#    #  run1 & IgA
#    identical(cbc01$run1$IgA$sinfo$id, sinfo$id[match(cbc01$run1$IgA$sinfo$id, sinfo$id)]),
#    identical(cbc01$run1$IgA$binder$id, binder$id[binder$detect_Ab == "IgA"]),
#    identical(cbc01$run1$IgA$mfi, mfi[match(cbc01$run1$IgA$sinfo$id, sinfo$id), binder$detect_Ab == "IgA"]),
#    #  run2 p1-p3
#    identical(cbc01$run2$IgG_IgM$sinfo$id[cbc01$run2$IgG_IgM$sinfo$plate %in% c("p1", "p2", "p3")], sinfo2$id),
#    identical(cbc01$run2$IgG_IgM$binder$id, binder2$id),
#    identical(cbc01$run2$IgG_IgM$mfi[cbc01$run2$IgG_IgM$sinfo$plate %in% c("p1", "p2", "p3"), ], mfi2)
# )
