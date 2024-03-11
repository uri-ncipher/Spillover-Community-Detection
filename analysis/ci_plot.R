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

load(file = paste(ROOT, "/store_data/imp_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/imp_lec", sep = ""), envir = .GlobalEnv)

library(inferference)
library(plotrix)

alpha1 = cbind(c(cbind(
  direct_effect(imp_cfg)[1,1], direct_effect(imp_cfg)[2,1], direct_effect(imp_cfg)[3,1]),
  indirect_effect(imp_cfg, .60, .20)[,1],
  indirect_effect(imp_cfg, .4, .20)[,1],
  indirect_effect(imp_cfg, .60, .4)[,1],
  total_effect(imp_cfg, .60, .20)[,1],
  total_effect(imp_cfg, .4, .20)[,1],
  total_effect(imp_cfg, .60, .4)[,1],
  overall_effect(imp_cfg, .60, .20)[,1],
  overall_effect(imp_cfg, .4, .20)[,1],
  overall_effect(imp_cfg, .60, .4)[,1]))

alpha2 = cbind(c(cbind(
  direct_effect(imp_cfg)[1,3], direct_effect(imp_cfg)[2,3], direct_effect(imp_cfg)[3,3]),
  indirect_effect(imp_cfg, .60, .20)[,3],
  indirect_effect(imp_cfg, .4, .20)[,3],
  indirect_effect(imp_cfg, .60, .4)[,3],
  total_effect(imp_cfg, .60, .20)[,3],
  total_effect(imp_cfg, .4, .20)[,3],
  total_effect(imp_cfg, .60, .4)[,3],
  overall_effect(imp_cfg, .60, .20)[,3],
  overall_effect(imp_cfg, .4, .20)[,3],
  overall_effect(imp_cfg, .60, .4)[,3]))

Estimates1 = cbind(round(c(cbind(
  -direct_effect(imp_cfg)[1,5], -direct_effect(imp_cfg)[2,5], -direct_effect(imp_cfg)[3,5]),
  indirect_effect(imp_cfg, .60, .20)[,5],
  indirect_effect(imp_cfg, .4, .20)[,5],
  indirect_effect(imp_cfg, .60, .4)[,5],
  -total_effect(imp_cfg, .20, .60)[,5],
  -total_effect(imp_cfg, .20, .4)[,5],
  -total_effect(imp_cfg, .4, .60)[,5],
  overall_effect(imp_cfg, .60, .20)[,5],
  overall_effect(imp_cfg, .4, .20)[,5],
  overall_effect(imp_cfg, .60, .4)[,5]), 3))

Lbound01 = cbind(round(c(cbind(
  -direct_effect(imp_cfg)[1,8], -direct_effect(imp_cfg)[2,8], -direct_effect(imp_cfg)[3,8]),
  indirect_effect(imp_cfg, .60, .20)[,7],
  indirect_effect(imp_cfg, .4, .20)[,7],
  indirect_effect(imp_cfg, .60, .4)[,7],
  -total_effect(imp_cfg, .20, .60)[,8],
  -total_effect(imp_cfg, .20, .4)[,8],
  -total_effect(imp_cfg, .4, .60)[,8],
  overall_effect(imp_cfg, .60, .20)[,7],
  overall_effect(imp_cfg, .4, .20)[,7],
  overall_effect(imp_cfg, .60, .4)[,7]), 3))

Lbound1 = cbind(paste(rep("(", 12), Lbound01, sep = ""))

Ubound01 = cbind(round(c(cbind(
  -direct_effect(imp_cfg)[1,7], -direct_effect(imp_cfg)[2,7], -direct_effect(imp_cfg)[3,7]),
  indirect_effect(imp_cfg, .60, .20)[,8],
  indirect_effect(imp_cfg, .4, .20)[,8],
  indirect_effect(imp_cfg, .60, .4)[,8],
  -total_effect(imp_cfg, .20, .60)[,7],
  -total_effect(imp_cfg, .20, .4)[,7],
  -total_effect(imp_cfg, .4, .60)[,7],
  overall_effect(imp_cfg, .60, .20)[,8],
  overall_effect(imp_cfg, .4, .20)[,8],
  overall_effect(imp_cfg, .60, .4)[,8]), 3))

Ubound1 = cbind(paste(Ubound01, rep(")",12)))

Estimates2 = cbind(round(c(cbind(
  -direct_effect(imp_lec)[1,5], -direct_effect(imp_lec)[2,5], -direct_effect(imp_lec)[3,5]),
  indirect_effect(imp_lec, .60, .20)[,5],
  indirect_effect(imp_lec, .4, .20)[,5],
  indirect_effect(imp_lec, .60, .4)[,5],
  -total_effect(imp_lec, .20, .60)[,5],
  -total_effect(imp_lec, .20, .4)[,5],
  -total_effect(imp_lec, .4, .60)[,5],
  overall_effect(imp_lec, .60, .20)[,5],
  overall_effect(imp_lec, .4, .20)[,5],
  overall_effect(imp_lec, .60, .4)[,5]), 3))

Lbound02 = cbind(round(c(cbind(
  -direct_effect(imp_lec)[1,8], -direct_effect(imp_lec)[2,8], -direct_effect(imp_lec)[3,8]),
  indirect_effect(imp_lec, .60, .20)[,7],
  indirect_effect(imp_lec, .4, .20)[,7],
  indirect_effect(imp_lec, .60, .4)[,7],
  -total_effect(imp_lec, .20, .60)[,8],
  -total_effect(imp_lec, .20, .4)[,8],
  -total_effect(imp_lec, .4, .60)[,8],
  overall_effect(imp_lec, .60, .20)[,7],
  overall_effect(imp_lec, .4, .20)[,7],
  overall_effect(imp_lec, .60, .4)[,7]), 3))

Lbound2 = cbind(paste(rep("(", 12), Lbound02, sep = ""))

Ubound02 = cbind(round(c(cbind(
  -direct_effect(imp_lec)[1,7], -direct_effect(imp_lec)[2,7], -direct_effect(imp_lec)[3,7]),
  indirect_effect(imp_lec, .60, .20)[,8],
  indirect_effect(imp_lec, .4, .20)[,8],
  indirect_effect(imp_lec, .60, .4)[,8],
  -total_effect(imp_lec, .20, .60)[,7],
  -total_effect(imp_lec, .20, .4)[,7],
  -total_effect(imp_lec, .4, .60)[,7],
  overall_effect(imp_lec, .60, .20)[,8],
  overall_effect(imp_lec, .4, .20)[,8],
  overall_effect(imp_lec, .60, .4)[,8]), 3))

Ubound2 = cbind(paste(Ubound02, rep(")",12)))

bounds1 = paste(Lbound1,rep(", ",12), Ubound1)
bounds2 = paste(Lbound2,rep(", ",12), Ubound2)

rnames1 = cbind(c("Direct  ", "", "",
                  "Spillover", "", "",
                  "Total  ", "", "",
                  "Overall", "", ""))

rnames = c(
  "  (0.20, 0.20)", "  (0.40, 0.40)", "  (0.60, 0.60)", "  (0.60, 0.20)",
  "  (0.40, 0.20)", "  (0.60, 0.40)", "  (0.60, 0.20)", "  (0.40, 0.20)",
  "  (0.60, 0.40)", "  (0.60, 0.20)", "  (0.40, 0.20)", "  (0.60, 0.40)"
)

cnames = c("effect0","effect", "est_cfg", "lb_cfg", "ub_cfg", "est_lec", "lb_lec", "ub_lec")

output = as.data.frame(cbind(rnames1, rnames, Estimates1, Lbound01, Ubound01, Estimates2, Lbound02, Ubound02), nrow = 12, ncol = 5, byrow = F)

colnames(output) = cnames

output$est_cfg = as.numeric(output$est_cfg)
output$lb_cfg = as.numeric(output$lb_cfg)
output$ub_cfg = as.numeric(output$ub_cfg)
output$est_lec = as.numeric(output$est_lec)
output$lb_lec = as.numeric(output$lb_lec)
output$ub_lec = as.numeric(output$ub_lec)


xleft = -0.6

xright = 0.3

output$y1 = rev(c(10,20,30,
                  50,60,70,
                  90,100,110,
                  130,140,150))

output$y2 = output$y1+3

png(filename = paste(ROOT, "/store_outputs/diff_ci.png", sep = ""), width = 550, height = 550)

par(mar=c(5.1, 6.6, 4.1, 2.1))

plotCI(x = output$est_cfg,
       y = output$y1,
       li = output$lb_cfg,
       ui = output$ub_cfg,
       err = "x",
       col = gray(0,0),
       xlab = "",
       ylab = "",
       main = " ",
       xlim = c(xleft,xright),
       ylim = c(5,155),
       pch = 16,
       cex = 2, sfrac = 0.01, lwd=2,
       axes = F)

par(las=1)

axis(side = 2, at = output$y1+2, labels = output$effect, col = "black", cex.axis = 1.2)
axis(side = 2, at = output$y1+11, labels = output$effect0, col = "black", cex.axis = 1.4, tick = F)
axis(side = 1, at = round(seq(xleft, xright,.1),1), col = "black", cex.axis = 1.1)
abline(v = 0, col = gray(.15, 0.4), lty = 2, lwd = 2)

x1=c(xleft-0.025, xright+0.025)
x2=c(xright+0.025, xleft-0.025)

y1=c(157, 157)
y2=c(147,147)
polygon(x=c(x1,x2),
        y=c(y1,y2), col = gray(.6, 0.4), border=NA)

y1=c(137, 137)
y2=c(127,127)
polygon(x=c(x1,x2),
        y=c(y1,y2), col = gray(.6, 0.4), border=NA)

y1=c(117, 117)
y2=c(107,107)
polygon(x=c(x1,x2),
        y=c(y1,y2), col = gray(.6, 0.4), border=NA)

y1=c(97, 97)
y2=c(87,87)
polygon(x=c(x1,x2),
        y=c(y1,y2), col = gray(.6, 0.4), border=NA)

y1=c(77, 77)
y2=c(67,67)
polygon(x=c(x1,x2),
        y=c(y1,y2), col = gray(.6, 0.4), border=NA)

y1=c(57, 57)
y2=c(47,47)
polygon(x=c(x1,x2),
        y=c(y1,y2), col = gray(.6, 0.4), border=NA)

y1=c(37, 37)
y2=c(27,27)
polygon(x=c(x1,x2),
        y=c(y1,y2), col = gray(.6, 0.4), border=NA)

y1=c(17, 17)
y2=c(7,7)
polygon(x=c(x1,x2),
        y=c(y1,y2), col = gray(.6, 0.4), border=NA)

plotCI(x = output$est_cfg,
       y = output$y1,
       li = output$lb_cfg,
       ui = output$ub_cfg,
       err = "x",
       col = "black",
       xlab = "",
       ylab = "",
       main = " ",
       xlim = c(-0.4,0.2),
       ylim = c(5,155),
       pch = 16,
       cex = 2, sfrac = 0.01, lwd=2,
       axes = F,
       add=T)

plotCI(x = output$est_lec,
       y = output$y2,
       li = output$lb_lec,
       ui = output$ub_lec,
       err = "x",
       col = "red",
       ylim = c(10,38),
       pch = 17,
       cex = 2, sfrac = 0.01, lwd = 2,
       add=T)

legend(0.15, 46, cex = 1, legend = c("LE", "CFG"), col = c("red", "black"), pch = c(17, 16), pt.cex = 1.60)

dev.off()

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))

