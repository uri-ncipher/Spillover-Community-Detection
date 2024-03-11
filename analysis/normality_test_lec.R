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

#############################
######### LOAD DATA #########
#############################

load(file = paste(ROOT, "/store_data/finaldata", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/final_formula_lec", sep = ""), envir = .GlobalEnv)

library(lme4)
library(survival)

set.seed(seednum)

# Data processing to remove row where 1 person is the only member of their community.
finaldata_l = finaldata
finaldata_l$clusterLEC[which(finaldata_l$clusterLEC == "213")]=NA
finaldata_l$clusterLEC2 = as.numeric(as.character(finaldata_l$clusterLEC))
finaldata_l$clusterLEC2[finaldata_l$clusterLEC2>213 & !is.na(finaldata_l$clusterLEC2)] = finaldata_l$clusterLEC2[finaldata_l$clusterLEC2>213 & !is.na(finaldata_l$clusterLEC2)]-1
finaldata_l$clusterLEC = NA
finaldata_l$clusterLEC = as.factor(finaldata_l$clusterLEC2)
finaldata_l = finaldata_l[which(!is.na(finaldata_l$clusterLEC)),]

# Remove the 'y|imputed_lec' 
glmr_form_lec = gsub('y_imputed_lec \\| x', 'x', final_formula_lec)
# Remove the '1 | clusterLEC' 
glmr_form_lec = gsub('\\(1 \\| clusterLEC\\) \\| clusterLEC', '\\(1 \\| clusterLEC\\)', glmr_form_lec)
glmr_form_lec = paste(glmr_form_lec[[2]], glmr_form_lec[[1]], glmr_form_lec[[3]])

### Fit the GLMM model

glmm.lec <- glmer(
  formula = glmr_form_lec,
  family = "binomial",
  data = finaldata_l
)

# Swap '(1|clusterLEC)' for 'strata(clusterLEC)' in the formula for the clogit model.
strat_form_lec = gsub('\\(1 \\| clusterLEC\\)', 'strata(clusterLEC)', glmr_form_lec)
strat_form_lec = formula(paste(strat_form_lec, collapse = "+"))

### Fit the conditional Logit model

clogit.lec <- clogit(
  formula = strat_form_lec,
  data = finaldata_l,
  method = "exact"
)

### Create a function

calc_TC_statistic = function(cl, gl, degrees_of_freedom){ idx=c(2:length(fixef(gl)))

if ("glmerMod" %in% class(gl)){
  message('glmer')
  
  foo_delta = cl$coefficients - lme4::getME(gl, "fixef")[idx]
  
  glmm_vcov = vcov(gl)[idx, idx]
}

if ("HLfit" %in% class(gl)){
  message('spaMM')
  
  foo_delta = cl$coefficients - gl$fixef[idx]
  
  glmm_vcov = vcov(gl)[idx, idx]
}

foo_sigma_M = cl$var - glmm_vcov

TC_stat = as.numeric(t(foo_delta) %*% solve(foo_sigma_M) %*% foo_delta)
p_value = pchisq(TC_stat, df = degrees_of_freedom, lower.tail = F)

output = list(
  results = data.frame(D = TC_stat, p=p_value, stringsAsFactors = FALSE)
  , mats = list(delta = foo_delta, sigma = foo_sigma_M)
)
output
}

calculated_tc_statistic = calc_TC_statistic(
  cl=clogit.lec
  , gl = glmm.lec
  , degrees_of_freedom = length(fixef(glmm.lec))
)

normality_pval_lec = calculated_tc_statistic$results

normality_pval_lec

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))


