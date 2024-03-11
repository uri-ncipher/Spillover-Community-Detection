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

#############################
######### LOAD DATA #########
#############################

load(file = paste(ROOT, "/store_data/interim3", sep = ""), envir = .GlobalEnv)

finaldata = interim3
data_ycom = finaldata[!is.na(finaldata$y),]

library(lme4)

set.seed(seednum)

#######################################################
#  IMPUTE MISSING Y BASED ON PREDICTIVE GLMER MODELS  #
#######################################################

# CFG

compmod_cfg = glmer(y ~ 
                      # <x1 + x2 + ...>
                      + (1|clusterCFG),
                      family = binomial(link="logit"), data=data_ycom)
y_prob_cfg = predict(compmod_cfg, type="response", newdata=finaldata)
y_gen_cfg = rbinom(nrow(finaldata), 1, y_prob_cfg)
finaldata$y_imputed_cfg = ifelse(is.na(finaldata$y), y_gen_cfg, finaldata$y)
impute_y_formula_cfg = formula(compmod_cfg)

# LEC

compmod_lec = glmer(y ~ 
                      # <x1 + x2 + ...>
                      + (1|clusterLEC), 
                      family = binomial(link="logit"), data=data_ycom)
y_prob_lec = predict(compmod_lec, type="response", newdata=finaldata)
y_gen_lec = rbinom(nrow(finaldata), 1, y_prob_lec)
finaldata$y_imputed_lec = ifelse(is.na(finaldata$y) | finaldata$clusterLEC == "213", y_gen_lec, finaldata$y)
impute_y_formula_lec = formula(compmod_lec)

#######################################################
# FAVORING THE NULL HYPOTHESIS OF NO TREATMENT EFFECT #
#######################################################

# x=0 => Ymiss=0
# x=1 => Ymiss=1

# Assign y_nul
finaldata$y_nul=NA
finaldata$y_nul[which(!is.na(finaldata$y))]=finaldata$y[which(!is.na(finaldata$y))]
finaldata$y_nul[which( is.na(finaldata$y) & finaldata$x==0)]=0
finaldata$y_nul[which( is.na(finaldata$y) & finaldata$x==1)]=1

# check y_nul
nul_check = as.matrix(cbind( cbind(finaldata$y[which(is.na(finaldata$y))]), 
                             cbind(finaldata$x[which(is.na(finaldata$y))]), 
                             cbind(finaldata$y_nul[which(is.na(finaldata$y))])),
                      nrow=51, ncol=3)

colnames(nul_check) = c("Ymiss", "x", "y_nul")

nul_check


########################################################################
# FAVORING THE ALTERNATIVE HYPOTHESIS OF A PROTECTIVE TREATMENT EFFECT #
########################################################################

# x=0 => Ymiss=1
# x=1 => Ymiss=0


# Assign y_alt
finaldata$y_alt=NA
finaldata$y_alt[which(!is.na(finaldata$y))]=finaldata$y[which(!is.na(finaldata$y))]
finaldata$y_alt[which( is.na(finaldata$y) & finaldata$x==0)]=1
finaldata$y_alt[which( is.na(finaldata$y) & finaldata$x==1)]=0

# check y_alt
alt_check = as.matrix(cbind( cbind(finaldata$y[which(is.na(finaldata$y))]), 
                             cbind(finaldata$x[which(is.na(finaldata$y))]), 
                             cbind(finaldata$y_alt[which(is.na(finaldata$y))])),
                      nrow=51, ncol=3)

colnames(alt_check) = c("Ymiss", "x", "y_alt")

alt_check


###########################################
# Save outputs

save(impute_y_formula_cfg, file = paste(ROOT, "/store_data/impute_y_formula_cfg", sep = ""))
save(impute_y_formula_lec, file = paste(ROOT, "/store_data/impute_y_formula_lec", sep = ""))

save(finaldata, file = paste(ROOT, "/store_data/finaldata", sep = ""))

# Also save dataset of just those with y_miss.
data_ymiss = finaldata[is.na(finaldata$y),]
save(data_ymiss, file = paste(ROOT, "/store_data/data_ymiss", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars", "vars0")))
