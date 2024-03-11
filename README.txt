The following describes the directory structure for analysis and details for each individual program.
Use RUN_SCRIPT.R in the top-level directory ("ROOT") to run all scripts from one location.

- DATA_SOURCE = Directory containing nodes file and edge file.

- ROOT = Main top-level directory containing sub-directories (below) for data processing, analysis, data storage, and output storage.

	o ./make_data = Contains programs for data processing that work toward a final analysis-ready dataset. 

	o ./analysis = Contains programs which conduct analyses, including fitting models for causal inference in the presence of 
	spillover, analysis of the community structure, and creating tables and figures.

	o ./store_data = Contains R objects such as datasets and network graph objects.

	o ./store_outputs = Contains tables and figures as .tex, .csv, and .png files.

	o ./sensitivity = Contains programs for sensitivity analysis - i.e., same outcome imputation for both community structures.
	Missing outcome is imputed based on generalized linear model with no fixed or random effect for the communities.

		> ./make_data = Contains program for creating a final analysis-ready dataset.

		> ./analysis = Contains programs which conduct analyses, including fitting models causal inference in the presence of 
		spillover, and creating tables and figures.

		> ./store_data = Contains R objects and datasets.

		> ./store_outputs = Contains tables and figures as .tex, .csv, and .png files.

	o ./subset = Contains programs for additional sensitivity analysis - i.e., communities with IPW close to zero from the 
	main analysis are removed.

		> ./analysis = Contains programs which remove communities with community-level IPW close to zero, refits models
		for causal effects, and creates tables and figures.

		> ./store_data = Contains models for causal effects.

		> ./store_outputs = Contains tables and figures as .tex and .png files.

NOTE: "CFG" refers to cluster-fast-greedy community detection.
      "LEC" refers to leading-eigenvector community detection.

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:          ROOT/load_packages.R
# PURPOSE:          Load all packages needed for analysis
#
###############################################################################################################################
###############################################################################################################################


###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/make_data/md_interim1.R
#
# PURPOSE:  Create first intermediate dataset with all variables needed for analysis, model selection, and derivations.
#
# INPUT:    DATA_SOURCE/TRIP_nodespublic_randomID_2022-11-14.csv
#
# OUTPUT:   ROOT/store_data/interim1
#
# NOTES:    None
#
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/make_data/md_interim2.R
#
# PURPOSE:  Create second intermediate dataset with communities. 
#           Create network objects - full network, and largest connected component.
#
# INPUT:    DATA_SOURCE/TRIP_edgespublic_randomID_2022-11-14.csv
#           ROOT//store_data/interim1
#
# OUTPUT:   ROOT/store_data/interim2
#           ROOT/store_data/net
#           ROOT/store_data/net_gc
#
# NOTES:    Libraries to load: igraph
#
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/make_data/md_interim3.R
#
# PURPOSE:  Create third intermediate dataset where missing values for covariates are imputed based on the missForest algorithm.
#
# INPUT:    ROOT/store_data/interim2
#
# OUTPUT:   ROOT/store_data/interim3
#
# NOTES:    Libraries to load: missForest
#
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/make_data/md_finaldata.R
#
# PURPOSE:  Create final dataset where missing values for the outcome are imputed based on three scenarios: 
#           1st based on predictive model,
#           2nd in favor of the null hypothesis, 
#           3rd in favor of the alternative hypothesis.
#
# INPUT:    ROOT/store_data/interim3
#
# OUTPUT:   ROOT/store_data/finaldata
#           ROOT/store_data/data_ymiss
#           ROOT/store_data/impute_y_formula_lec
#           ROOT/store_data/impute_y_formula_cfg
#
# NOTES:    Libraries to load: lme4
#
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/ms_propscores.R
#
# PURPOSE:  Fit propensity score models under CFG and LEC communities. 
#
# INPUT:    ROOT/store_data/finaldata
#
# OUTPUT:   ROOT/store_data/final_formula_cfg 
#           ROOT/store_data/imp_cfg
#           ROOT/store_data/final_formula_lec
#           ROOT/store_data/imp_lec
#
# NOTES:    Libraries to load: inferference
#           Community 213 has only 1 member and therefore not subject to dissemination, and therefore is removed from LEC models.# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/normality_test_cfg.R
#
# PURPOSE:  Test for a normal distribution of the random effects of CFG communities
#
# INPUT:    ROOT/store_data/finaldata
#           ROOT/store_data/final_formula_cfg
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: lme4, survival
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/normality_test_lec.R
#
# PURPOSE:  Test for a normal distribution of the random effects of LEC communities
#
# INPUT:    ROOT/store_data/finaldata
#           ROOT/store_data/final_formula_lec
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: lme4, survival.
#           Community 213 has only 1 member and therefore not subject to dissemination, and therefore is removed from LEC models.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/best_worst_models.R
#
# PURPOSE:  Fit models using "inferference" package for best and worst case scenarios.
#
# INPUT:    ROOT/store_data/finaldata
#           ROOT/store_data/final_formula_cfg
#           ROOT/store_data/final_formula_lec
#
# OUTPUT:   ROOT/store_data/nul_cfg, alt_cfg, nul_lec, alt_lec. 
#
# NOTES:    Libraries to load: inferference. 
#           Community 213 has only 1 member and therefore not subject to dissemination, and therefore is removed from LEC models.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/diffdiff.R
#
# PURPOSE:  Compare estimates from CFG models to estimates from LEC models. 
#
# INPUT:    ROOT/store_data/imp_cfg, imp_lec.
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: inferference.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/model_tables.R
#
# PURPOSE:  Output model results to table for LaTeX. 
#
# INPUT:    ROOT/store_data/imp_cfg, nul_cfg, alt_cfg, imp_lec, nul_lec, alt_lec.
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: inferference, stargazer. 
#           The function 'gaze2mods' uses the 'stargazer' package to create one table of results for 2 models 
#           placing results from both models side by side. Executing the function produces text to be copied
#           and pasted into LaTeX.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/ci_plot.R
#
# PURPOSE:  Plot estimates and confidence intervals for both community detection methods.
#
# INPUT:    ROOT/store_data/imp_lec, imp_cfg
#
# OUTPUT:   ROOT/store_outputs/diff_ci.png
#
# NOTES:    Libraries to load: inferference, plotrix.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/ci_plot_best.R
#
# PURPOSE:  Plot estimates and confidence intervals for both community detection methods under the best case scenario imputation.
#
# INPUT:    ROOT/store_data/alt_lec, alt_cfg
#
# OUTPUT:   ROOT/store_outputs/diff_ci_best.png
#
# NOTES:    Libraries to load: inferference, plotrix.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/ci_plot_worst.R
#
# PURPOSE:  Plot estimates and confidence intervals for both community detection methods under worst case scenario imputation.
#
# INPUT:    ROOT/store_data/nul_lec, nul_cfg
#
# OUTPUT:   ROOT/store_outputs/diff_ci_worst.png
#
# NOTES:    Libraries to load: inferference, plotrix.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/community_weights.R
#
# PURPOSE:  Create histograms of the community-level inverse probability weights 
#
# INPUT:    ROOT/store_data/imp_cfg, imp_lec
#
# OUTPUT:   ROOT//store_outputs/hist_comwt_cfg.png, hist_comwt_lec.png
#
# NOTES:    Libraries to load: inferference. 
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/covariate_summaries.R
#
# PURPOSE:  Create covariate summaries by treatment arm. 
#
# INPUT:    ROOT/store_data/finaldata, data_ymiss
#
# OUTPUT:   ROOT/store_outputs/agesum.csv, covsd.csv.
#
# NOTES:    None
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/perturbations.R
#
# PURPOSE:  Create VOI datasets for both community structures.
#
# INPUT:    ROOT/store_data/net_gc
#
# OUTPUT:   ROOT/store_data/LEC_voi, CFG_voi
#
# NOTES:    Libraries to load: perturbR, aricode, lpSolve, mcclust, igraph. 
#           Code is adapted from the 'perturbR' function to allow for
#           cluster-fast-greedy (CFG) and leading-eigenvector (LEC).
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/voi_CFG_at_10_20.R
#
# PURPOSE:  Determine what VOI is when 10% and 20% of nodes are randomly selected,
#           and randomly reassigned to different communities after having been determined by CFG. 
#
# INPUT:    ROOT/store_data/net_gc
#           ROOT/store_data/finaldata
#
# OUTPUT:   ROOT/store_data/VOI_CFG_10.R, VOI_CFG_20.R.
#
# NOTES:    Libraries to load: mcclust, igraph
#
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/voi_LEC_at_10_20.R
#
# PURPOSE:  Determine what VOI is when 10% and 20% of nodes are randomly selected,
#           and randomly reassigned to different communities after having been determined by LEC. 
#
# INPUT:    ROOT/store_data/net_gc
#           ROOT/store_data/finaldata
#
# OUTPUT:   ROOT/store_data/VOI_LEC_10.R, VOI_LEC_20.R.
#
# NOTES:    Libraries to load: mcclust, igraph
#
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/pert_plots.R
#
# PURPOSE:  Create perturbation plots.
#
# INPUT:    ROOT/store_data/CFG_voi, LEC_voi, VOI_CFG_10, VOI_CFG_20, VOI_LEC_10, VOI_LEC_20
#
# OUTPUT:   ROOT/store_outputs/pert_lec.png, pert_cfg.png
#
# NOTES:    None
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/gephi_inputs.R
#
# PURPOSE:  Create files needed for inputs to Gephi, a software for plotting networks.
#
# INPUT:    ROOT/store_data/finaldata
#           ROOT/store_data/net
#
# OUTPUT:   ROOT/store_outputs/inputs_for_gephi/gephi_nodes_network.csv,
#                                               gephi_edges_network.csv,
#                                               gephi_nodes_cfg.csv,
#                                               gephi_edges_cfg.csv,
#                                               gephi_nodes_lec.csv,
#                                               gephi_edges_lec.csv.
#
# NOTES:    Libraries to load: igraph, stringr. 
#           Community (LEC) with one participant is removed from outputs.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/sensitivity/make_data/sense_md_finaldata.R
#
# PURPOSE:  Create final dataset where missing values for the outcome are imputed based on predictive model. 
#
# INPUT:    ROOT/store_data/interim3
#           ROOT/sensitivity/store_data/impute_y_formula_cfg
#
# OUTPUT:   ROOT/sensitivity/store_data/sense_finaldata
#
# NOTES:    none
#
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/sensitivity/analysis/sense_finalmodels.R
#
# PURPOSE:  Fit models using "inferference" package.
#
# INPUT:    ROOT/sensitivity/store_data/sense_finaldata
#           ROOT/store_data/final_formula_cfg
#           ROOT/store_data/final_formula_lec
#
# OUTPUT:   ROOT/sensitivity/store_data/sense_imp_cfg, sense_imp_lec. 
#
# NOTES:    Libraries to load: inferference. 
#           Community 213 has only 1 member and therefore not subject to dissemination, and therefore is removed from LEC models.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/sensitivity/analysis/sense_diffdiff.R
#
# PURPOSE:  Compare estimates from cfg models to estimates from lec models. 
#
# INPUT:    ROOT/sensitivity/store_data/sense_imp_cfg, sense_imp_lec.
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: inferference.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/sensitivity/analysis/sense_model_tables.R
#
# PURPOSE:  Output model results to table for LaTeX. 
#
# INPUT:    ROOT/sensitivity/store_data/sense_imp_cfg, sense_imp_lec.
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: inferference, stargazer. 
#           The function 'gaze2mods' uses the 'stargazer' package to create one table of results for 2 models 
#           placing results from both models side by side. Executing the function produces text to be copied
#           and pasted into LaTeX.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/sensitivity/analysis/sense_ci_plot.R
#
# PURPOSE:  Plot confidence intervals for both community detection methods.
#
# INPUT:    ROOT/sensitivity/store_data/imp_lec, imp_cfg
#
# OUTPUT:   ROOT/sensitivity/store_outputs/sense_diff_ci.png
#
# NOTES:    Libraries to load: inferference, plotrix.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/subset/analysis/sub_finalmodels.R
#
# PURPOSE:  Fit models using "inferference" package after removing communites with IPW close to 0.
#
# INPUT:    ROOT/store_data/finaldata
#           ROOT/store_data/imp_cfg
#           ROOT/store_data/imp_lec
#           ROOT/store_data/final_formula_cfg
#           ROOT/store_data/final_formula_lec
#
# OUTPUT:   ROOT/subset/store_data/sub_imp_cfg, sub_imp_lec. 
#
# NOTES:    Libraries to load: inferference, lme4. 
#           Community 213 has only 1 member and therefore not subject to dissemination, and therefore is removed from LEC models.
#           The variable "nat" is removed from the propensity score model for CFG communites.
#
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/subset/analysis/sub_model_tables.R
#
# PURPOSE:  Output model results to table for LaTeX. 
#
# INPUT:    ROOT/subset/store_data/sub_imp_cfg, sub_imp_lec.
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: inferference, stargazer. 
#           The function 'gaze2mods' uses the 'stargazer' package to create one table of results for 2 models 
#           placing results from both models side by side. Executing the function produces text to be copied
#           and pasted into LaTeX.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/subset/analysis/sub_diffdiff.R
#
# PURPOSE:  Compare estimates from cfg models to estimates from lec models. 
#
# INPUT:    ROOT/subset/store_data/sub_imp_cfg, sub_imp_lec.
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: inferference.
# 
###############################################################################################################################
###############################################################################################################################

###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/subset/analysis/sub_ci_plot.R
#
# PURPOSE:  Plot confidence intervals for both community detection methods.
#
# INPUT:    ROOT/subset/store_data/sub_imp_cfg, sub_imp_lec
#
# OUTPUT:   ROOT/subset/store_outputs/sub_diff_ci.png
#
# NOTES:    Libraries to load: inferference, plotrix.
# 
###############################################################################################################################
###############################################################################################################################
