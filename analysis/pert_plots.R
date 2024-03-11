###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/perturbations.R
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

############################################
#### Load datasets for making VOI plots ####
############################################

# Giant component of the network graph.
load(file = paste(ROOT, "/store_data/LEC_voi", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/CFG_voi", sep = ""), envir = .GlobalEnv)

# VOI values for 10% and 20% of nodes randomly reassigned to different communities
load(file = paste(ROOT, "/store_data/VOI_LEC_10", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/VOI_LEC_20", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/VOI_CFG_10", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/VOI_CFG_20", sep = ""), envir = .GlobalEnv)

############################################
################### Plots ################## 
############################################

##########################
########### CFG ##########
##########################
png(filename = paste(ROOT, "/store_outputs/pert_cfg.png", sep = ""), width = 550, height = 450)
plot(CFG_voi$VOI_con ~ rownames(CFG_voi), pch = 19, ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
     col = gray(level = .25, alpha = 0));par(new=T)
abline(h=seq(0,.8,.1), v=seq(0,1,.1), col=gray(level = .85, alpha = 1));par(new=T)
axis(at = seq(0,1,.1), side = 1, cex.axis=1.25)
axis(at = seq(0,1,.1), side = 2, cex.axis=1.25, line=-.5, las = 1)
mtext(expression(gamma), side=1, line=3, col="black", cex=2)
mtext(expression(V(gamma)), side=2, line=2.25, col="black", cex=1.75)
CFG_voi = CFG_voi[c(1:11,13,15,17,19,21),]

xCFG1=as.numeric(rownames(CFG_voi))
xCFG2=rev(xCFG1)
yCFG1=CFG_voi$LB_con
yCFG2=rev(CFG_voi$UB_con)
polygon(x=c(xCFG1,xCFG2),
        y=c(yCFG1,yCFG2), col = gray(.6, .5), border=NA)

xCFG11=as.numeric(rownames(CFG_voi))
xCFG22=rev(xCFG11)
yCFG11=CFG_voi$LB_real
yCFG22=rev(CFG_voi$UB_real)
polygon(x=c(xCFG11,xCFG22),
        y=c(yCFG11,yCFG22), col = gray(.1, .5), border=NA)
par(new=T)
plot(CFG_voi$VOI_con ~ rownames(CFG_voi), pch = 19, cex = 2, ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
     col = gray(level = .4, alpha = 1))
lines(x = as.numeric(rownames(CFG_voi)), y = CFG_voi$VOI_con, col = gray(level = .5, alpha = 1))
par(new=T)
plot(CFG_voi$VOI_real ~ rownames(CFG_voi), pch = 17, cex = 2, ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
     col = "black")
lines(x = as.numeric(rownames(CFG_voi)), y = CFG_voi$VOI_real, col = "black")
abline(a = VOI_CFG_10, b = 0, col="black", lwd=2)
abline(a = VOI_CFG_20, b = 0, col="black", lwd=2)

legend(.8, .5, cex = 1.2, pt.cex=1.75, legend = c("Null", "TRIP"), col = c(gray(level = .5, alpha = 1),"black"), pch = c(19, 17))

dev.off()

##########################
########### LEC ##########
##########################
png(filename = paste(ROOT, "/store_outputs/pert_lec.png", sep = ""), width = 550, height = 450)
plot(LEC_voi$VOI_con ~ rownames(LEC_voi), pch = 19, xlim=range(c(0,1)), ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
     col = gray(level = .25, alpha = 0));par(new=T)
abline(h=seq(0,.8,.1), v=seq(0,1,.1), col=gray(level = .85, alpha = 1));par(new=T)

axis(at = seq(0,1,.1), side = 1, cex.axis=1.25)
axis(at = seq(0,1,.1), side = 2, cex.axis=1.25, line=-.5, las = 1)
mtext(expression(gamma), side=1, line=3, col="black", cex=2)
mtext(expression(V(gamma)), side=2, line=2.25, col="black", cex=1.75)
LEC_voi = LEC_voi[c(1:11,13,15,17,19,21),]

xLEC1=as.numeric(rownames(LEC_voi))
xLEC2=rev(xLEC1)
yLEC1=LEC_voi$LB_con
yLEC2=rev(LEC_voi$UB_con)
polygon(x=c(xLEC1,xLEC2),
        y=c(yLEC1,yLEC2), col = gray(.6, .5), border=NA)

xLEC11=as.numeric(rownames(LEC_voi))
xLEC22=rev(xLEC11)
yLEC11=LEC_voi$LB_real
yLEC22=rev(LEC_voi$UB_real)
polygon(x=c(xLEC11,xLEC22),
        y=c(yLEC11,yLEC22), col = gray(.1, .5), border=NA)
par(new=T)
plot(LEC_voi$VOI_con ~ rownames(LEC_voi), pch = 19, cex = 2, xlim=range(c(0,1)), ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
     col = gray(level = .5, alpha = 1))
lines(x = as.numeric(rownames(LEC_voi)), y = LEC_voi$VOI_con, col = gray(level = .5, alpha = 1))
par(new=T)
plot(LEC_voi$VOI_real ~ rownames(LEC_voi), pch = 17, cex = 2, ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
     col = "black")
lines(x = as.numeric(rownames(LEC_voi)), y = LEC_voi$VOI_real, col = "black")
abline(a = VOI_LEC_10, b = 0, col="black", lwd=2)
abline(a = VOI_LEC_20, b = 0, col="black", lwd=2)

legend(.8, .5, cex = 1.2, pt.cex=1.75, legend = c("Null", "TRIP"), col = c(gray(level = .5, alpha = 1),"black"), pch = c(19, 17))

dev.off()

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))
