# GagnonBartsch_ShemTov_2017_Replication

## Description 
This repository provides replication code and data to replicate the tables and figures that are presented in Gagnon-Bartsch and Shem-Tov (2017).  

All the code is written in the **R** statistical programming launguage. For each code file (.R) the associated log file (.Rout) is also included.

## Monte-Carlo simulations

### Data
* results100.rda, results200.rda and results500.rda: Simulations results data from three different sample size: 100,200 and 500. 
* sim3.rda contains the simultion results that are used to generate Figure 8.

### Code
##### **The Tables and Figures that each code (.R) file replicates**  
* Figure 1: *figures_sims_variable_b1.R* and *figures_sims_variable_b1.Rout*.
* Figure 2: *figures_fixed_rho_new.R* and *figures_fixed_rho_new.Rout*.
* Figure 8: *figures_in_vs_out_sample_roc.R* and *figures_in_vs_out_sample_roc.Rout*.


## Real data applications
### Data 
All the data sets that we use are included in the package *cpt.paper* which is included in this repository.

### Code
##### **The Tables and Figures that each code (.R) file replicates**  
* Table 1 and Figure 3: *layell_2009_analysis.R* and *layell_2009_analysis.Rout*. 
* Table 2, Figures 4 and Figure 11: *judges_analysis.R* and *judges_analysis.Rout*.
* Figures 7 and 12: *community_college_analysis.R* and *community_college_analysis.Rout*. 
* Figures 5, 6, 9 and 10, and Table 3: First execute *MPs.R* which will produce an R object *MPs.rda* that contains the saved results from *MPs.R*. Second, execute *mps_analysis.R* (*mps_analysis.Rout*) which constructs the tables and figures. 

