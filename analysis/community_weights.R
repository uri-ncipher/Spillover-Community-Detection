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

#############################
######### LOAD DATA #########
#############################

library(inferference)

load(file = paste(ROOT, "/store_data/finaldata", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/imp_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/imp_lec", sep = ""), envir = .GlobalEnv)

# Unpack the diagnose_weights function (below) 
# to change the xlab and ylab settings to blank.

diag = function (obj, allocations = NULL, ...) 
{
  obj_allocations <- as.numeric(dimnames(obj$weights)[[2]])
  if (!is.null(allocations)) {
    if (!all(allocations %in% obj_allocations)) {
      stop("Allocations argument must only include allocation levels used in interference call.")
    }
    else {
      which_cols <- which(obj_allocations %in% allocations)
      w <- obj$weights[, which_cols, drop = FALSE]
    }
  }
  else {
    n <- ncol(obj$weights)
    if (n > 5) {
      m <- ceiling(stats::median(1:n))
      m_1 <- ceiling(stats::median(1:(n/2)))
      m_2 <- ceiling(stats::median((n/2):n))
      w <- obj$weights[, c(1, m_1, m, m_2, n), drop = FALSE]
    }
    else {
      w <- obj$weights
    }
  }
  for (j in 1:ncol(w)) {
    graphics::hist(w[, j], main = "", xlab = "", ylab = "", # Set xlab and ylab to blank.
                   ...)
  }
}

# CFG Communities

png(filename = paste(ROOT, "/store_outputs/hist_comwt_cfg.png", sep = ""), width = 550, height = 250)

par(mfrow=c(1,3))

diag(obj = imp_cfg, allocations = c(0.20), nclass=30, ylim=c(0,15), xlim=c(0,3)
, axes = F)
axis(side = 2, at = round(seq(0, 15, 5),1), col = "black", cex.axis = 1.25)
axis(side = 1, at = round(seq(0, 3, 1),1), col = "black", cex.axis = 1.25)
mtext(expression(alpha~"=20%"), side=1, line=3, col="black", cex=1)

diag(obj = imp_cfg, allocations = c(0.40), nclass=30, ylim=c(0,15), xlim=c(0,5)
, axes = F)
axis(side = 2, at = round(seq(0, 15, 5),1), col = "black", cex.axis = 1.25)
axis(side = 1, at = round(seq(0, 5, 1),1), col = "black", cex.axis = 1.25)
mtext(expression(alpha~"=40%"), side=1, line=3, col="black", cex=1)

diag(obj = imp_cfg, allocations = c(0.60), nclass=30, ylim=c(0,15), xlim=c(0,8)
, axes = F)
axis(side = 2, at = round(seq(0, 15, 5),1), col = "black", cex.axis = 1.25)
axis(side = 1, at = round(seq(0, 8, 1),1), col = "black", cex.axis = 1.25)
mtext(expression(alpha~"=60%"), side=1, line=3, col="black", cex=1)

dev.off()

# LEC Communities

png(filename = paste(ROOT, "/store_outputs/hist_comwt_lec.png", sep = ""), width = 550, height = 250)

par(mfrow=c(1,3))

diag(obj = imp_lec, allocations = c(0.20), nclass=30, ylim=c(0,15), xlim=c(0,3)
, axes = F)
axis(side = 2, at = round(seq(0, 15, 5),1), col = "black", cex.axis = 1.25)
axis(side = 1, at = round(seq(0, 3, 1),1), col = "black", cex.axis = 1.25)
mtext(expression(alpha~"=20%"), side=1, line=3, col="black", cex=1)


diag(obj = imp_lec, allocations = c(0.40), nclass=30, ylim=c(0,15), xlim=c(0,4)
, axes = F)
axis(side = 2, at = round(seq(0, 15, 5),1), col = "black", cex.axis = 1.25)
axis(side = 1, at = round(seq(0, 4, 1),1), col = "black", cex.axis = 1.25)
mtext(expression(alpha~"=40%"), side=1, line=3, col="black", cex=1)


diag(obj = imp_lec, allocations = c(0.60), nclass=30, ylim=c(0,15), xlim=c(0,7)
, axes = F)
axis(side = 2, at = round(seq(0, 15, 5),1), col = "black", cex.axis = 1.25)
axis(side = 1, at = round(seq(0, 7, 1),1), col = "black", cex.axis = 1.25)
mtext(expression(alpha~"=60%"), side=1, line=3, col="black", cex=1)

dev.off()

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))

