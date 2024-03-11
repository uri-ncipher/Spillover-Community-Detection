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

#############################
######### LOAD DATA #########
#############################

load(file = paste(ROOT, "/store_data/finaldata", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/final_formula_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/final_formula_lec", sep = ""), envir = .GlobalEnv)

library(inferference)

set.seed(seednum)

#############################
# CFG Models

# Convert to two separate formulas for best/worst case imputed outcomes.
newformula_cfg = gsub('y_imputed_cfg \\| x', '', final_formula_cfg)

newformula_nul_cfg = formula(paste("y_nul | x ~", newformula_cfg[[3]], collapse = "+"))
newformula_alt_cfg = formula(paste("y_alt | x ~", newformula_cfg[[3]], collapse = "+"))

# CFG Worst Case Scenario

nul_cfg <- interference(formula = newformula_nul_cfg,
                        allocations = c(0.20, 0.4, 0.60),
                        data = finaldata,
                        options = list(maxiter=10000),
                        randomization = 1, 
                        method = 'simple')

# CFG Best Case Scenario

alt_cfg <- interference(formula = newformula_alt_cfg,
                        allocations = c(0.20, 0.4, 0.60),
                        data = finaldata,
                        options = list(maxiter=10000),
                        randomization = 1, 
                        method = 'simple')


#############################
# LEC Models

# Data processing to remove row where 1 person is the only member of their community.
finaldata_l = finaldata
finaldata_l$clusterLEC[which(finaldata_l$clusterLEC == "213")]=NA
finaldata_l$clusterLEC2 = as.numeric(as.character(finaldata_l$clusterLEC))
finaldata_l$clusterLEC2[finaldata_l$clusterLEC2>213 & !is.na(finaldata_l$clusterLEC2)] = finaldata_l$clusterLEC2[finaldata_l$clusterLEC2>213 & !is.na(finaldata_l$clusterLEC2)]-1
finaldata_l$clusterLEC = NA
finaldata_l$clusterLEC = as.factor(finaldata_l$clusterLEC2)
finaldata_l = finaldata_l[which(!is.na(finaldata_l$clusterLEC)),]

# Convert separate formulas for best/worst case imputed outcomes.
newformula_lec = gsub('y_imputed_lec \\| x', '', final_formula_lec)

newformula_nul_lec = formula(paste("y_nul | x ~", newformula_lec[[3]], collapse = "+"))
newformula_alt_lec = formula(paste("y_alt | x ~", newformula_lec[[3]], collapse = "+"))

# LEC Worst Case Scenario

nul_lec <- interference(formula =newformula_nul_lec,
                        allocations = c(0.20, 0.4, 0.60),
                        data = finaldata_l,
                        options = list(maxiter=10000),
                        randomization = 1, 
                        method = 'simple')

# LEC Best Case Scenario

alt_lec <- interference(formula = newformula_alt_lec,
                        allocations = c(0.20, 0.4, 0.60),
                        data = finaldata_l,
                        options = list(maxiter=10000),
                        randomization = 1, 
                        method = 'simple')

# Save Models

save(nul_cfg, file = paste(ROOT, "/store_data/nul_cfg", sep = ""))
save(alt_cfg, file = paste(ROOT, "/store_data/alt_cfg", sep = ""))

save(nul_lec, file = paste(ROOT, "/store_data/nul_lec", sep = ""))
save(alt_lec, file = paste(ROOT, "/store_data/alt_lec", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))

