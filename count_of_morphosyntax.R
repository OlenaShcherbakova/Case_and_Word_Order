#counting languages with at least one of the two strategies: case and fixed (rigid) word order

source('library.R')

set.seed(123)

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv"))

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <-
  grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label,]

grambank_phylopath_compl_count <- grambank_phylopath_compl %>%
  mutate(morphosyntax = ifelse((Free_word_order == 0) |
                                 (Nominal_case == 1),
                               1,
                               0
  )) %>%
  filter(!is.na(morphosyntax))

sum(grambank_phylopath_compl_count$morphosyntax == "1")

#counting languages with at least one of the following: fixed WO, case, and verbal indexing
extra <-
  read.csv(
    "data/GB_wide_strict.tsv",
    header = TRUE,
    sep = '\t',
    stringsAsFactors = FALSE
  ) %>%
  dplyr::select(Glottocode = Language_ID, GB091, GB092, GB093, GB094) %>%
  filter(!is.na(GB091),!is.na(GB092),!is.na(GB093),!is.na(GB094))

grambank_phylopath_compl <- grambank_phylopath_compl %>%
  left_join(extra)

grambank_phylopath_compl_count <- grambank_phylopath_compl %>%
  mutate(morphosyntax = ifelse((Free_word_order == 0) |
                                 (Nominal_case == 1) |
                                 (GB091 == 1 |
                                    GB092 == 1 |
                                    GB093 == 1 | GB094 == 1),
                               1,
                               0
  )) %>%
  filter(!is.na(morphosyntax))

sum(grambank_phylopath_compl_count$morphosyntax == "1")
