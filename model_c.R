source('library.R')

set.seed(123)

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv"))

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <-
  grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label, ]
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

grambank_phylopath_compl = grambank_phylopath_compl[order(match(grambank_phylopath_compl$Glottocode, rownames(A))),]

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
  set_prior("exponential(4)", class = "sd", resp="Freewordorder"),
  set_prior("exponential(4)", class = "sd", resp="Nominalcase"),
  set_prior("student_t(3, 0, 2.5)", class = "b", resp="Freewordorder"),
  set_prior("student_t(3, 0, 2.5)", class = "b", resp="Nominalcase"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept", resp="Freewordorder"),
  set_prior("student_t(3, 0, 2.5)", class = "Intercept", resp="Nominalcase")
)

fwo <-  brms::bf(Free_word_order ~ 1 + Nominal_case +
                       (1 | gr(Glottocode, cov = A)))

nc <- brms::bf(Nominal_case ~ 1 + Verb_final +
                 (1 | gr(Glottocode, cov = A)))

#model c where Free_word_order is predicted by Nominal_case and phylogenetic effects
model_c <- brm(
  data = grambank_phylopath_compl,
  data2 = list(A = A),
  family = "bernoulli",
  
  fwo + nc + set_rescor(FALSE),
  
  prior = weakly_informative,
  control = list(adapt_delta = 0.99),
  #default adapt_delta=0.8
  iter = 7000,
  cores = 4,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  seed = 12345
)

cat(
  "Model summary: model_c - 1) Nominal_case ~ Verb_final + phy, 2) Free_word_order ~ Nominal_case + phy + spa"
)
summary(model_c)

save(model_c, file = "output_models/model_c.RData")
