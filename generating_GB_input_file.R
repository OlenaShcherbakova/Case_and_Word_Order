#DO NOT RUN: this script is intended for generating the Grambank input file from the entire database for the time when Grambank is not officially released yet. The final input file includes 5 relevant Gramabank features and the sample of 1705 languages for which values are either "0" or "1", i.e. with no missing values.

grambank_phylopath_compl <- read.csv("data/GB_wide_strict.tsv", header = TRUE, sep = '\t', stringsAsFactors=FALSE)

colnames(grambank_phylopath_compl)[colnames(grambank_phylopath_compl)=="Language_ID"] <- "Glottocode"
  
grambank_phylopath_compl <- grambank_phylopath_compl[!with(grambank_phylopath_compl, is.na(GB070) | is.na(GB131) | is.na(GB132) | is.na(GB133) | is.na(GB136)),]

grambank_phylopath_compl <- subset(x = grambank_phylopath_compl, select = c("Glottocode", "GB070", "GB131", "GB132", "GB133", "GB136"))
  
tree <- read.tree("data/wrangled.tree")
  
grambank_phylopath_compl <- grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label, ]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)
  
#make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode))
  
grambank_phylopath_compl %>%
    write_tsv(file="data/GB_input.tsv")
