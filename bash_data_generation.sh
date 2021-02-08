#!/bin/bash

## Master bash script for generating data files of Covid-19 Translation
## Serology Study
## Contributors: Mun-Gwan Hong, Tea Dodig-Crnkovic, Cecilia Engel Thomas
## Initialized: 2020/05/04


## Table of Questionnaires answers

Rscript --vanilla gen-questionnaire.v1.R

## Table of Questionnaires answers - version 2 including late participant of batch 1

Rscript --vanilla gen-questionnaire.v2.R

## Table of Questionnaires answers - version 3 including batch 2

Rscript --vanilla gen-questionnaire.v3.R


# ## Tables of Covid-19 antigen array data for IgG and IgM
#
# # - Names of resulting data frames to use for downstream analysis:
# #    mfi = MFI data
# #    sinfo = Sample information
# #    binder = Probe/antigen information
#
# Rscript --vanilla gen-dbs_covid19.v1.R
#
#
#
# ## Tables of Covid-19 antigen array data including IgA data
#
# # - Names of resulting data frames to use for downstream analysis:
# #    mfi = MFI data
# #    sinfo = Sample information
# #    binder = Probe/antigen information
# #    low_bead_count_samples_iga = a table of samples that got too low bead
# #      counts for IgA assays (median < 35)
#
# Rscript --vanilla gen-dbs_covid19.v2.R
#
#
#
# ## Tables of Covid-19 antigen array data of batch 2 samples
#
# # - Names of resulting data frames to use for downstream analysis:
# #    mfi2 = MFI data
# #    sinfo2 = Sample information
# #    binder2 = Probe/antigen information
#
# Rscript --vanilla gen-dbs_covid19.v3.R


## Tables of Covid-19 antigen array data of batch 1 and batch 2 for all binders
#  This is an independent script from previous versions. It is confirmed that
#  The data in this version has identical data as previous version.

# cbc01 : a list of `run1` and `run2`,
#    which have 'IgG_IgM' and 'IgA',
#       which have
#         mfi = MFI data
#         sinfo = Sample information
#         binder = Probe/antigen information

Rscript --vanilla gen-dbs_covid19.v4.R


## Tables of Covid-19 antigen array data of batch 1 and 2, CBC-03 and CBC-004
#  One file was generated for each sample set. The sets were divided by the way
#  in our manuscript.
#
# - Names of data frames to use for downstream analysis: per sample set
#    mfi = MFI data
#    sinfo = Sample information
#    binder = Probe/antigen information

Rscript --vanilla gen-dbs_covid19_manuscript.cba.v1.R




## Tables of Covid-19 antigen array data obtained by CBA of DBS-03 Edsberg samples

# - Names of resulting data frames to use for downstream analysis:
#    mfi = MFI data
#    sinfo = Sample information
#    binder = Probe/antigen information

Rscript --vanilla gen-cbc_03_edsberg.cba.v1.R



## Tables of Covid-19 S1 and Nucleocapsid protein levels obtained by ELISA of DBS-03 Edsberg samples

Rscript --vanilla gen-cbc_03_edsberg.elisa.v1.R


## Table of Questionnaires answers of CBC-03 Edsberg participants

Rscript --vanilla gen-cbc_03_edsberg.questionnaire.v1.R



<<<<<<< HEAD


=======
## Tables of Covid-19 CBC-03 Olink protein data

# - Names of resulting data frames to use for downstream analysis:
#    npx = NPX data (data is already log2 transformed)
#    sinfo = Sample information
#    binder = Probe information

Rscript --vanilla gen_olink_cbc03.R
>>>>>>> feb47ccfbe65ba264346535acab29628ea2563be

