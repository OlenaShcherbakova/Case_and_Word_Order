#Comparing predictive performance of three models with K-fold CV

#Note for running on cluster. For this script, the following memory specifications were used.
#SBATCH --cpus-per-task 20
#SBATCH --mem 122G

source("library.R")

if (!suppressPackageStartupMessages(require("pacman"))) {
  install.packages("pacman")
}

pacman::p_load(future)

load("output_models/model1_Vf.RData")
load("output_models/model2_Vf.RData")
load("output_models/model3_Vf.RData")

summary(model1)
summary(model2)
summary(model3)

#kfold comparison
plan(multicore) #from future package
options(future.globals.maxSize = 4000 * 1024 ^ 2)
kfold_comparison <-
  brms::kfold(
    model1,
    model2,
    model3,
    K = 10,
    chains = 1,
    save_fits = TRUE,
    compare = TRUE
  )

save(kfold_comparison, file = "output_models/kfold_comparison_Vf.RData")

# cat("comparisons of models based on kfold")
# kfold_comparison
# 
# cat("output structure")
# str(kfold_comparison)

filepath_kfold_comparison <-
  paste0("output_tables/", "kfold_comparison_Vf", ".csv", collapse = "")
write.csv(kfold_comparison$diffs, filepath_kfold_comparison)
