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

library(inferference)

load(file = paste(ROOT, "/subset/store_data/sub_imp_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/subset/store_data/sub_imp_lec", sep = ""), envir = .GlobalEnv)


Estimates1 = cbind(round(c(cbind(
  -direct_effect(sub_imp_cfg)[1,5], -direct_effect(sub_imp_cfg)[2,5], -direct_effect(sub_imp_cfg)[3,5]),
  indirect_effect(sub_imp_cfg, .60, .20)[,5],
  indirect_effect(sub_imp_cfg, .4, .20)[,5],
  indirect_effect(sub_imp_cfg, .60, .4)[,5],
  -total_effect(sub_imp_cfg, .20, .60)[,5],
  -total_effect(sub_imp_cfg, .20, .4)[,5],
  -total_effect(sub_imp_cfg, .4, .60)[,5],
  overall_effect(sub_imp_cfg, .60, .20)[,5],
  overall_effect(sub_imp_cfg, .4, .20)[,5],
  overall_effect(sub_imp_cfg, .60, .4)[,5]), 3))


Estimates2 = cbind(round(c(cbind(
  -direct_effect(sub_imp_lec)[1,5], -direct_effect(sub_imp_lec)[2,5], -direct_effect(sub_imp_lec)[3,5]),
  indirect_effect(sub_imp_lec, .60, .20)[,5],
  indirect_effect(sub_imp_lec, .4, .20)[,5],
  indirect_effect(sub_imp_lec, .60, .4)[,5],
  -total_effect(sub_imp_lec, .20, .60)[,5],
  -total_effect(sub_imp_lec, .20, .4)[,5],
  -total_effect(sub_imp_lec, .4, .60)[,5],
  overall_effect(sub_imp_lec, .60, .20)[,5],
  overall_effect(sub_imp_lec, .4, .20)[,5],
  overall_effect(sub_imp_lec, .60, .4)[,5]), 3))

diffdiff = as.data.frame(cbind(Estimates1, Estimates2, c(Estimates1-Estimates2)))
colnames(diffdiff) = c("RD_cfg", "RD_lec", "Difference")
rownames(diffdiff) = cbind(c("Direct (.20, .20)", "Direct (.40, .40)", "Direct (.60, .60)", 
                             "Spillover (.60, .20)", "Spillover (.40, .20)", "Spillover (.60, .40)", 
                             "Total (.60, .20)", "Total (.40, .20)", "Total (.60, .40)", 
                             "Overall (.60, .20)", "Overall (.40, .20)", "Overall (.60, .40)"))

diffdiff

diffmax = diffdiff[which(abs(diffdiff$Difference)==max(abs(diffdiff$Difference))),]

diffmax

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))


