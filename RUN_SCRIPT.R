
DATA_SOURCE = "<directory containing raw data>"
ROOT = "<top level directory containing all R script files"
vars = c("meth","agez","sex","homeless","edu","employ","married","nat","inject","cocaine","heroin","crack","alcohol","care","hiv", "type")

#seednum = <number> # uncomment and insert number

# Load Required Packages
# source(file = paste(ROOT, "/load_packages.R", sep = ""), echo=T)

# Make-data Programs
source(file = paste(ROOT, "/make_data/md_interim1.R", sep = ""), echo=T)
source(file = paste(ROOT, "/make_data/md_interim2.R", sep = ""), echo=T)
source(file = paste(ROOT, "/make_data/md_interim3.R", sep = ""), echo=T)
source(file = paste(ROOT, "/make_data/md_finaldata.R", sep = ""), echo=T)

# Analysis Programs

source(file = paste(ROOT, "/analysis/ms_propscores.R", sep = ""), echo=T)

source(file = paste(ROOT, "/analysis/normality_test_cfg.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/normality_test_lec.R", sep = ""), echo=T)

source(file = paste(ROOT, "/analysis/best_worst_models.R", sep = ""), echo=T)

source(file = paste(ROOT, "/analysis/diffdiff.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/model_tables.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/ci_plot.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/ci_plot_best.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/ci_plot_worst.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/community_weights.R", sep = ""), echo=T)

source(file = paste(ROOT, "/analysis/perturbations.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/voi_CFG_at_10_20.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/voi_LEC_at_10_20.R", sep = ""), echo=T)
source(file = paste(ROOT, "/analysis/pert_plots.R", sep = ""), echo=T)

# SENSITIVITY ANALYSIS 1 PROGRAMS

# Same outcome imputation for both LEC and CFG communities.

# Makedata Programs
source(file = paste(ROOT, "/sensitivity/make_data/sense_md_finaldata.R", sep = ""), echo=T)

# Analysis Programs
source(file = paste(ROOT, "/sensitivity/analysis/sense_finalmodels.R", sep = ""), echo=T)
source(file = paste(ROOT, "/sensitivity/analysis/sense_diffdiff.R", sep = ""), echo=T)
source(file = paste(ROOT, "/sensitivity/analysis/sense_model_tables.R", sep = ""), echo=T)
source(file = paste(ROOT, "/sensitivity/analysis/sense_ci_plot.R", sep = ""), echo=T)

# SENSITIVITY ANALYSIS 2 PROGRAMS

# Analysis Programs
source(file = paste(ROOT, "/subset/analysis/sub_finalmodels.R", sep = ""), echo=T)
source(file = paste(ROOT, "/subset/analysis/sub_diffdiff.R", sep = ""), echo=T)
source(file = paste(ROOT, "/subset/analysis/sub_model_tables.R", sep = ""), echo=T)
source(file = paste(ROOT, "/subset/analysis/sub_ci_plot.R", sep = ""), echo=T)


