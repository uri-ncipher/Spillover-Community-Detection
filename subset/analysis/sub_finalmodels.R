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

library(inferference)
library(lme4)

set.seed(seednum)

load(file = paste(ROOT, "/store_data/finaldata", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/imp_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/imp_lec", sep = ""), envir = .GlobalEnv)

load(file = paste(ROOT, "/store_data/final_formula_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/final_formula_lec", sep = ""), envir = .GlobalEnv)
 
# Remove clusters with weights close to zero (i.e., <=0.001).
# Communities are renamed (i.e. minus one) every time a greater numbered community is removed,
# e.g., if 210 is removed, communities 211 is renamed to 210 but 209 is still 209.

# CFG

cfgw = as.data.frame(round(imp_cfg$weights[,c(1,2,3)],4))
colnames(cfgw) = c("a_20", "a_40", "a_60")
cfgw[which(cfgw$a_20<0.001 | cfgw$a_40<=0.001 | cfgw$a_60<=0.001),]
finaldata_c = finaldata

# < set clusterCFG to NA in finaldata_c for CFG analysis >
sub_finaldata_c = finaldata_c[which(!is.na(finaldata_c$clusterCFG)),]

# Number of participants removed - CFG.
nrow(finaldata)-nrow(sub_finaldata_c)

# LEC

lecw = as.data.frame(round(imp_lec$weights[,c(1,2,3)],4))
colnames(lecw) = c("a_20", "a_40", "a_60")
lecw[which(lecw$a_20<0.001 | lecw$a_40<0.001 | lecw$a_60<0.001),]
finaldata_l = finaldata

# First remove community with one participant (community 213) from final dataset.
# This was already removed prior to fitting the LEC model, so the updated 213 will
# be removed based on the weight close to zero.
finaldata_l$clusterLEC[which(finaldata_l$clusterLEC == "213")]=NA
finaldata_l$clusterLEC2 = as.numeric(as.character(finaldata_l$clusterLEC))
finaldata_l$clusterLEC2[finaldata_l$clusterLEC2>213 & !is.na(finaldata_l$clusterLEC2)] = finaldata_l$clusterLEC2[finaldata_l$clusterLEC2>213 & !is.na(finaldata_l$clusterLEC2)]-1
finaldata_l$clusterLEC = NA
finaldata_l$clusterLEC = as.factor(finaldata_l$clusterLEC2)
finaldata_l = finaldata_l[which(!is.na(finaldata_l$clusterLEC)),]

# < set clusterLEC to NA in finaldata_c for LEC analysis >
sub_finaldata_l = finaldata_l[which(!is.na(finaldata_l$clusterLEC)),]

# Number of participants removed - LEC (not including participant in own community)
(nrow(finaldata)-1)-nrow(sub_finaldata_l)

# Refit models.

sub_imp_cfg <- interference(formula = final_formula_cfg,
                            allocations = c(0.20, 0.40, 0.60),
                            data = sub_finaldata_c,
                            options = list(maxiter=100000),
                            randomization = 1, 
                            method = 'simple')

sub_imp_lec <- interference(formula = final_formula_lec,
                              allocations = c(0.20, 0.40, 0.60),
                              data = sub_finaldata_l,
                              options = list(maxiter=100000),
                              randomization = 1, 
                              method = 'simple')

# Check the weights.

cfgw = as.data.frame(round(sub_imp_cfg$weights[,c(1,2,3)],4))
colnames(cfgw) = c("a_20", "a_40", "a_60")
cfgw[which(cfgw$a_20<0.001 | cfgw$a_40<=0.001 | cfgw$a_60<=0.001),]

lecw = as.data.frame(round(sub_imp_lec$weights[,c(1,2,3)],4))
colnames(lecw) = c("a_20", "a_40", "a_60")
lecw[which(lecw$a_20<0.001 | lecw$a_40<=0.001 | lecw$a_60<=0.001),]

# Save models.

save(sub_imp_cfg, file = paste(ROOT, "/subset/store_data/sub_imp_cfg", sep = ""))
save(sub_imp_lec, file = paste(ROOT, "/subset/store_data/sub_imp_lec", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))

