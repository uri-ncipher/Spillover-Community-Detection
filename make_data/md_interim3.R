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

library(missForest)

set.seed(seednum)

#############################
######### LOAD DATA #########
#############################

load(file = paste(ROOT, "/store_data/interim2", sep = ""), envir = .GlobalEnv)

# CHECK FOR MISSING COVARIATES

table(interim2$yb, useNA = "ifany") # 2 missing
table(interim2$homeless, useNA = "ifany") # 1 missing
table(interim2$married, useNA = "ifany") # 3 missing
table(interim2$care, useNA = "ifany") # 1 missing

# Subset to variables going into missForest, both with missing and non-missing values.
nodesdata <- subset(interim2, select = c(# non-missing variables
                                         # < "x1", "x2", "x3", ... ,>
                                         
                                         # variables with missing data
                                        "yb", "homeless", "married", "care"
                                            ))

# Impute data using missForest package

impute1 <- missForest(xmis = nodesdata, 
                      maxiter = 10, ntree = 100,
                      replace = TRUE,
                      cutoff = list(c(0,1),         # yb
                                    c(0,1),         # homeless
                                    c(0,1),         # married
                                    c(0,1),         # care
                                    # 1,            continuous x1
                                    #c(0,1),        2-level factor (binary) x2
                                    #c(0,1,2),      3-level factor x3
                                    #c(0,1,2,4),     4-level factor x4
                                    # ...           etc (no comma after last entry)
                                    ),
                      xtrue = NA)

# Expected values for each missing value
improb_yb       = impute1$ximp$yb
improb_homeless = impute1$ximp$homeless
improb_married  = impute1$ximp$married
improb_care     = impute1$ximp$care


#############################
##### Single Imputation #####
#############################

# Generate random draws from conditional binomial distribution
yb.gen = rbinom(nrow(nodesdata), 1, improb_yb)
homeless.gen = rbinom(nrow(nodesdata), 1, improb_homeless)
married.gen = rbinom(nrow(nodesdata), 1, improb_married)
care.gen = rbinom(nrow(nodesdata), 1, improb_care)

# Impute where missing
nodesdata$ybi = ifelse(is.na(nodesdata$yb), yb.gen, nodesdata$yb)
nodesdata$homelessi = ifelse(is.na(nodesdata$homeless), homeless.gen, nodesdata$homeless)
nodesdata$marriedi = ifelse(is.na(nodesdata$married), married.gen, nodesdata$married)
nodesdata$carei = ifelse(is.na(nodesdata$care), care.gen, nodesdata$care)

# Imputed covariate values
nodesdata[which(is.na(nodesdata$yb) | is.na(nodesdata$homeless) | is.na(nodesdata$married) | is.na(nodesdata$care)), c("ybi", "homelessi", "marriedi", "carei")]

# Define stage 2 dataset: has complete covariate data
interim3 = interim2

# Maintain original non-imputed covariates
interim3$yb_o       = nodesdata$yb
interim3$homeless_o = nodesdata$homeless
interim3$married_0  = nodesdata$married
interim3$care_0     = nodesdata$care

# Replace with complete/imputed covariates
interim3$yb       = nodesdata$ybi
interim3$homeless = nodesdata$homelessi
interim3$married  = nodesdata$marriedi
interim3$care     = nodesdata$carei


save(interim3, file = paste(ROOT, "/store_data/interim3", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))
