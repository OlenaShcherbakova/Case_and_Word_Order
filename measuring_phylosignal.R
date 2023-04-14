#measuring phylogenetic signal: calculating the D value as in Fritz & Purvis (2010) 
#(Fritz, S. A. and Purvis, A. (2010). Selectivity in mammalian extinction risk and threat types: a new measure of phylogenetic signal strength in binary traits. Conservation Biology, 24(4):1042-1051.)

source('library.R')

set.seed(123)

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv")) 

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <- grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label, ]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)


# make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode))

grambank_phylopath_compl<-grambank_phylopath_compl[match(tree$tip.label, grambank_phylopath_compl$Glottocode),]
rownames(grambank_phylopath_compl) <- grambank_phylopath_compl$Glottocode


#measuring phylogenetic signal:
data <- comparative.data(phy = tree, data = grambank_phylopath_compl, names.col = Glottocode, vcv = TRUE, na.omit = FALSE, warn.dropped = TRUE)

#from function description: "The value of D depends on phylogeny size - more sister clades yield higher sums - and so the means of the two sets of simulated data are used as calibrations to scale both observed and simulated values of D to set points of 0 (as phylogenetically conserved as expected under a Brownian threshold model) and 1 (random). The value of D can be both smaller than 0 (highly conserved) and greater than 1 (overdispersed) and the distributions of scaled D from the simulations are used to assess the significance of the observed scaled D."
#DEstimate: The estimated D value
#Pval1: A p value, giving the result of testing whether D is significantly different from one
#Pval0: A p value, giving the result of testing whether D is significantly different from zero

physig_Verb_final_D <- caper::phylo.d(data=data, binvar = Verb_final, permut = 1000)
physig_Verb_final_DEstimate <- physig_Verb_final_D$DEstimate[1][["Obs"]]
physig_Verb_final_Pval1 <- physig_Verb_final_D$Pval1 # D compared to 1
physig_Verb_final_Pval0 <- physig_Verb_final_D$Pval0 # D compared to 0
Verb_final_world <- c(physig_Verb_final_DEstimate, physig_Verb_final_Pval1, physig_Verb_final_Pval0)

physig_Free_word_order_D <- caper::phylo.d(data=data, binvar = Free_word_order, permut = 1000)
physig_Free_word_order_DEstimate <- physig_Free_word_order_D$DEstimate[1][["Obs"]]
physig_Free_word_order_Pval1 <- physig_Free_word_order_D$Pval1
physig_Free_word_order_Pval0 <- physig_Free_word_order_D$Pval0
Free_word_order_world <- c(physig_Free_word_order_DEstimate, physig_Free_word_order_Pval1, physig_Free_word_order_Pval0)

physig_Nominal_case_D <- caper::phylo.d(data=data, binvar = Nominal_case, permut = 1000)
physig_Nominal_case_DEstimate <- physig_Nominal_case_D$DEstimate[1][["Obs"]]
physig_Nominal_case_Pval1 <- physig_Nominal_case_D$Pval1
physig_Nominal_case_Pval0 <- physig_Nominal_case_D$Pval0
Nominal_case_world <- c(physig_Nominal_case_DEstimate, physig_Nominal_case_Pval1, physig_Nominal_case_Pval0)


#Making a table
physig <- as.data.frame(rbind(Nominal_case_world, Verb_final_world, Free_word_order_world))
physig_test <- physig
physig_test_1 <- physig_test

physig <- physig %>%
  as.data.frame() %>%
  rename(`D value` = V1) %>%
  rename(`p-value: departure from phylogenetic randomness` = V2) %>%
  rename(`p-value: departure from Brownian threshold model` = V3) %>%
  mutate(round(., digits=2)) %>%
  add_column(Feature=c("Nominal case", "Verb-final word order", "Free word order"), .before = 1)

write.csv(physig, "output_tables/Phylogenetic_signal_D_value.csv", row.names = FALSE)

physig <- physig %>%
  flextable() %>%
  autofit() %>%
  fix_border_issues()

save_as_docx(
  "Phylogenetic signal" = physig_test, 
  path = "output/Phylogenetic_signal_D_value.docx")
