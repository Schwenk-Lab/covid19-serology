#!/bin/bash

## Master bash script for pipelining analysis of Covid-19 Translation Serology Study
## Contributors: Mun-Gwan Hong, Tea Dodig-Crnkovic, Cecilia Engel Thomas
## Initialized: 2020/05/04


## Data generation script

./bash_data_generation.sh


## Data visualization R script

R CMD BATCH master_trans_serology.R








