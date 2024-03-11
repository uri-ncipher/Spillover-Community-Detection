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

load(file = paste(ROOT, "/sensitivity/store_data/sense_finaldata", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/final_formula_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/final_formula_lec", sep = ""), envir = .GlobalEnv)

library(inferference)

set.seed(seednum)

# Rename outcome to work with previously saved formulas.
sense_finaldata$y_imputed_cfg = sense_finaldata$y_imputed
sense_finaldata$y_imputed_lec = sense_finaldata$y_imputed

#############################
# CFG Models

# Random Draw from Conditional Distribution

sense_imp_cfg <- interference(formula = final_formula_cfg,
                        allocations = c(0.20, 0.40, 0.60),
                        data = sense_finaldata,
                        options = list(maxiter=100000),
                        randomization = 1, 
                        method = 'simple')


#############################
# LEC Models

# Data processing to remove row where 1 person is the only member of their community.
sense_finaldata_l = sense_finaldata
sense_finaldata_l$clusterLEC[which(sense_finaldata_l$clusterLEC == "213")]=NA
sense_finaldata_l$clusterLEC2 = as.numeric(as.character(sense_finaldata_l$clusterLEC))
sense_finaldata_l$clusterLEC2[sense_finaldata_l$clusterLEC2>213 & !is.na(sense_finaldata_l$clusterLEC2)] = sense_finaldata_l$clusterLEC2[sense_finaldata_l$clusterLEC2>213 & !is.na(sense_finaldata_l$clusterLEC2)]-1
sense_finaldata_l$clusterLEC = NA
sense_finaldata_l$clusterLEC = as.factor(sense_finaldata_l$clusterLEC2)
sense_finaldata_l = sense_finaldata_l[which(!is.na(sense_finaldata_l$clusterLEC)),]

# Random Draw from Conditional Distribution

sense_imp_lec <- interference(formula = final_formula_lec,
                        allocations = c(0.20, 0.40, 0.60),
                        data = sense_finaldata_l,
                        options = list(maxiter=100000),
                        randomization = 1, 
                        method = 'simple')

# Save Models

save(sense_imp_cfg, file = paste(ROOT, "/sensitivity/store_data/sense_imp_cfg", sep = ""))

save(sense_imp_lec, file = paste(ROOT, "/sensitivity/store_data/sense_imp_lec", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))

