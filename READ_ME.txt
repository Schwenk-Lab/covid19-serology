Responsible: Mun-Gwan Hong, Tea Dodig, Cecilia E Thomas


###  Master scripts

`bash_pipeline.sh`
 - The main bash shell script to execute all following scripts. 

`bash_data_generation.sh`
 - Master script for data generation only. This should not be executed unless a serious error was found in generated data set.

`master_trans_serology.R`
 - Master script for running/sourcing/rendering other scripts in this folder.

`options_master_trans_serology.R`
 - Script defining global options for all scripts.



###  Scripts

`anal-cbc01-seropos_prop.Rmd`
  - CBC-01 Seropositive proportion. Compute the proportion per binder, but also 95% CI, Sens, Spec and PPV. UpSet plots. 


`anal-cbc03-seropos_prop.Rmd`
  - CBC-03 Seropositive proportion, UMAP seropositivity, compute the proportion per binder, tables and odds ratio.


`beeswarm_plots_per_antigen.Rmd`
 - Script for creating beeswarm plots per antigen using MFI and Z-scores.


`dimensionality_reduction_plots.Rmd`
 - Script for performing dimensionality reduction (PCA/tSNE/UMAP) and creating the corresponding plots.


`elisa_vs_luminex_antigen_correlations_sample_type.R`
 - Plots pairwise correlations between same antigens but different sample type (Plasma or DBS) in ELISA vs Luminex, using log2 scale.


`elisa_vs_luminex_antigen_correlations.R`
 - Plots pairwise correlations between antigens in ELISA vs Luminex, using MFI or log2 scale. This script is not part of the master script.


`elisa_vs_luminex_beeswarms.R`
 - Plots beeswarms colored by S-protein (ELISA) reactivity cut-off, log2 data, divided by sample type (Plasma or DBS).


`gen-dbs_covid19.v4.R`
  - Read MFI, sample and binder info of batch 1 and 2.


`gen-questionnaire.v1.R`
  - Script that reads the Excel file of questionnaire replies.


`gen-questionnaire.v2.R`
  - Script that reads the Excel file of questionnaire replies from batch 1 late participants.


`gen-questionnaire.v3.R`
  - Script that reads the Excel file of questionnaire replies from batch 2 and combines with batch 1.


`longitudinal_plots.R`
 - Longitudinal plots for one sample, divided by IgG and IgM antigen recognition.


`plot-distribution_of_age.R`
  - Make plots that show distribution of age in study set compared to Stockholm population.


`plot-distribution_of_MFI.R`
  - Make plots that show distribution of MFIs of each binder


`table-cbc01-seropos_by_6sd.R`
 - Calculate cut-offs per binder by 6xSD and export tables


`table-questionnaires-freq.Rmd`
  - Make the frequency table of questionnaire (Table1)

