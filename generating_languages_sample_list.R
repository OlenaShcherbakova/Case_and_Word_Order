#generate a full list of languages
grambank_phylopath_compl <- load_data_NAs() %>%
    left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv")) 

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <- grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label, ]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)

#make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode))

rownames(grambank_phylopath_compl) <- grambank_phylopath_compl$Glottocode
grambank_phylopath_compl$Verb_final <- as.factor(grambank_phylopath_compl$Verb_final)
grambank_phylopath_compl$Free_word_order <- as.factor(grambank_phylopath_compl$Free_word_order)
grambank_phylopath_compl$Nominal_case <- as.factor(grambank_phylopath_compl$Nominal_case)

grambank_phylopath_compl_spreadsheet <- grambank_phylopath_compl %>%
    dplyr::select(Name, Glottocode, Family_ID, AUTOTYP_area, Nominal_case, Verb_final, Free_word_order, Latitude, Longitude)

write.csv(grambank_phylopath_compl_spreadsheet, file = here("output", "Languages_Appendix.csv"), row.names = FALSE)
