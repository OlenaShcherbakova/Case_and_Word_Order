#Comparing predictive performance of three models with LOO-CV (with and without moment matching)

#Note for running on cluster. For this script, the following memory specifications were used.
#SBATCH --cpus-per-task 20
#SBATCH --mem 80G

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

#LOO comparison with "moment_match=TRUE"
loo_comparison_mm <-
  brms::loo(model1, model2, model3, moment_match = TRUE)
cat("LOO comparison between the models with moment match set to TRUE")
loo_comparison_mm

save(loo_comparison_mm, file = "output_models/loo_comparison_mm_Vf.RData")

filepath_loo_comparison_mm <-
  paste0("output_tables/", "loo_comparison_mm_Vf", ".csv", collapse = "")
write.csv(loo_comparison_mm$diffs, filepath_loo_comparison_mm)

#LOO comparison
loo_comparison <- brms::loo(model1, model2, model3)
cat("LOO comparison (without moment_match set to TRUE")
loo_comparison
save(loo_comparison, file = "output_models/loo_comparison_Vf.RData")
filepath_loo_comparison <-
  paste0("output_tables/", "loo_comparison_Vf", ".csv", collapse = "")
write.csv(loo_comparison$diffs, filepath_loo_comparison)

pareto_k <- data.frame(
  ID = c(1:1705),
  model1 = loo_comparison$loos$model1$pointwise[, "influence_pareto_k"],
  model2 = loo_comparison$loos$model2$pointwise[, "influence_pareto_k"],
  model3 = loo_comparison$loos$model3$pointwise[, "influence_pareto_k"]
)

filepath_pareto_k <-
  paste0("output_tables/",
         "loo_comparison_pareto_k_Vf",
         ".csv",
         collapse = "")
write.csv(pareto_k, filepath_pareto_k)

sessionInfo()
