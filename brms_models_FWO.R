source('library.R')

set.seed(123)

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv"))

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <-
  grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label,]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)


# make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode))


grambank_phylopath_compl <- grambank_phylopath_compl %>%
  mutate(Glottocode2 = Glottocode) %>%
  mutate(Glottocode3 = Glottocode2) %>%
  remove_rownames() %>%
  column_to_rownames(var = "Glottocode3") %>%
  as.data.frame()

#rownames(grambank_phylopath_compl) <- grambank_phylopath_compl$taxon

assert_that(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode),
            msg = "The data and phylogeny taxa do not match")

A <- ape::vcv.phylo(tree)
#A <- A / max(A)

grambank_phylopath_compl = grambank_phylopath_compl[order(match(grambank_phylopath_compl$Glottocode, rownames(A))), ]

kappa = 1
phi_1 = c(1, 1.25) # "Local" version: (sigma, phi) First value is not used

#calculate geo_dists
lat_long_matrix <- grambank_phylopath_compl %>% 
  #column_to_rownames("Glottocode") %>% 
  dplyr::select(Longitude, Latitude) %>% 
  as.matrix()

rdist.earth_dists <- fields::rdist.earth(lat_long_matrix, miles = FALSE)

rdist.earth_dists[upper.tri(rdist.earth_dists, diag = TRUE)] <- NA

dists_vector <- as.vector(rdist.earth_dists) %>% na.omit()

spatial_covar_mat_local = varcov.spatial(dists.lowertri = dists_vector, 
                                         cov.pars = phi_1, kappa = kappa)

spatial_covar_mat_local <- spatial_covar_mat_local$varcov

dimnames(spatial_covar_mat_local) = list(grambank_phylopath_compl$Glottocode2,
                                         grambank_phylopath_compl$Glottocode2)
spatial_covar_mat_local <-
  spatial_covar_mat_local / max(spatial_covar_mat_local)

stopifnot(all(rownames(A) == rownames(spatial_covar_mat_local)))

#setting the same weakly informative prior for all fixed effects
weakly_informative <- c(
  set_prior("exponential(4)", class = "sd"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept")
)

#random effect of phylogenetic relationships
model1 <- brm(
  data = grambank_phylopath_compl,
  data2 = list(A = A),
  family = "bernoulli",
  formula = Free_word_order ~ 1 + (1 | gr(Glottocode, cov = A)),
  prior = weakly_informative,
  control = list(adapt_delta = 0.95),
  #default adapt_delta=0.8
  iter = 7000,
  cores = 4,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  seed = 12345
)

cat("Model summary: model 1 - phy")
summary(model1)

save(model1, file = "output_models/model1_FWO.RData")

fitted <-
  fitted(model1) %>% as.data.frame() %>% bind_cols(grambank_phylopath_compl)
filepath_fitted <-
  paste0("output_tables/", "fitted_set1_model", "1_FWO", ".csv", collapse = "")
write.csv(fitted, filepath_fitted)

residuals <-
  residuals(model1) %>% as.data.frame() %>% bind_cols(grambank_phylopath_compl)
filepath_residuals <-
  paste0("output_tables/",
         "residuals_set1_model",
         "1_FWO",
         ".csv",
         collapse = "")
write.csv(residuals, filepath_residuals)

ranef_1 <- ranef(model1)
save(ranef_1, file = "output_models/random_effects_1_FWO.RData")

fixef_1 <- fixef(model1)
save(fixef_1, file = "output_models/fixed_effects_1_FWO.RData")


#random effects of phylogenetic and spatial relationships (spatial matrix)
model2 <- brm(
  data = grambank_phylopath_compl,
  data2 = list(spatial_covar_mat_local = spatial_covar_mat_local),
  family = "bernoulli",
  formula = Free_word_order ~ 1 + (1 |
                                  gr(Glottocode2, cov = spatial_covar_mat_local)),
  prior = weakly_informative,
  control = list(adapt_delta = 0.95),
  #default adapt_delta=0.8
  iter = 7000,
  cores = 4,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  seed = 12345
)

cat("Model summary: model 2 - spa")
summary(model2)

save(model2, file = "output_models/model2_FWO.RData")

fitted <-
  fitted(model2) %>% as.data.frame() %>% bind_cols(grambank_phylopath_compl)
filepath_fitted <-
  paste0("output_tables/", "fitted_set1_model", "2_FWO", ".csv", collapse = "")
write.csv(fitted, filepath_fitted)

residuals <-
  residuals(model2) %>% as.data.frame() %>% bind_cols(grambank_phylopath_compl)
filepath_residuals <-
  paste0("output_tables/",
         "residuals_set1_model",
         "2_FWO",
         ".csv",
         collapse = "")
write.csv(residuals, filepath_residuals)

ranef_2 <- ranef(model2)
save(ranef_2, file = "output_models/random_effects_2_FWO.RData")

fixef_2 <- fixef(model2)
save(fixef_2, file = "output_models/fixed_effects_2_FWO.RData")


#random effects of phylogenetic and spatial relationships (spatial matrix)
model3 <- brm(
  data = grambank_phylopath_compl,
  data2 = list(A = A, spatial_covar_mat_local = spatial_covar_mat_local),
  family = "bernoulli",
  formula = Free_word_order ~ 1 + (1 |
                                  gr(Glottocode2, cov = spatial_covar_mat_local)) +
    (1 | gr(Glottocode, cov = A)),
  prior = weakly_informative,
  control = list(adapt_delta = 0.95),
  #default adapt_delta=0.8
  iter = 7000,
  cores = 4,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  seed = 12345
)

cat("Model summary: model3 - phy + spa")
summary(model3)

save(model3, file = "output_models/model3_FWO.RData")

fitted <-
  fitted(model3) %>% as.data.frame() %>% bind_cols(grambank_phylopath_compl)
filepath_fitted <-
  paste0("output_tables/", "fitted_set1_model", "3_FWO", ".csv", collapse = "")
write.csv(fitted, filepath_fitted)

residuals <-
  residuals(model3) %>% as.data.frame() %>% bind_cols(grambank_phylopath_compl)
filepath_residuals <-
  paste0("output_tables/",
         "residuals_set1_model",
         "3_FWO",
         ".csv",
         collapse = "")
write.csv(residuals, filepath_residuals)

ranef_3 <- ranef(model3)
save(ranef_3, file = "output_models/random_effects_3_FWO.RData")

fixef_3 <- fixef(model3)
save(fixef_3, file = "output_models/fixed_effects_3_FWO.RData")
