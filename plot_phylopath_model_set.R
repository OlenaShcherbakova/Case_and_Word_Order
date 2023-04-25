#creating a custom plot of the tested causal models in the phylogenetic path analysis on 1705 languages for three following variables: NC, Vf, FWO

source('library.R')
source('customizing_phylopath_plot_model_set_function.R')

set.seed(123)

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv"))

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <-
  grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label,]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)


# make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode))

rownames(grambank_phylopath_compl) <-
  grambank_phylopath_compl$Glottocode
grambank_phylopath_compl$Verb_final <-
  as.factor(grambank_phylopath_compl$Verb_final)
grambank_phylopath_compl$Free_word_order <-
  as.factor(grambank_phylopath_compl$Free_word_order)
grambank_phylopath_compl$Nominal_case <-
  as.factor(grambank_phylopath_compl$Nominal_case)

names(grambank_phylopath_compl)[names(grambank_phylopath_compl) == "Verb_final"] <-
  "Vf"
names(grambank_phylopath_compl)[names(grambank_phylopath_compl) == "Free_word_order"] <-
  "FWO"
names(grambank_phylopath_compl)[names(grambank_phylopath_compl) == "Nominal_case"] <-
  "NC"

#modeling 12 possible causal scenarios

m <- define_model_set(
  null = c(),
  a = c(NC ~ FWO + Vf),
  b = c(Vf ~ NC, FWO ~ NC),
  c = c(NC ~ Vf, FWO ~ NC),
  d = c(NC ~ FWO, Vf ~ NC),
  e = c(Vf ~ NC, FWO ~ Vf),
  f = c(FWO ~ NC, Vf ~ FWO),
  g = c(Vf ~ FWO, NC ~ Vf),
  h = c(FWO ~ Vf, NC ~ FWO),
  i = c(NC ~ Vf),
  j = c(NC ~ FWO),
  k = c(Vf ~ NC),
  l = c(FWO ~ NC)
)

positions <- data.frame(
  name = c('NC', 'FWO', 'Vf'),
  x = c(2, 1, 3),
  y = c(2, 1, 1)
)

names <- c('NC', 'FWO', 'Vf')
custom_names <- c("Case", "Flexible", "Verb-\nfinal")
named_vector <- setNames(custom_names, names)

#plot_model_set(m, manual_layout = positions)
model_sets <- plot_model_set(
  m,
  manual_layout = positions,
  edge_width = 3,
  curvature = 0,
  text_size = 18,
  labels = named_vector,
  box_x = 65,
  box_y = 65,
  arrow = grid::arrow(type = 'closed', 30, grid::unit(12, 'points'))
)
#model_sets

ggsave(
  file = "output/phylopath_model_sets_custom.svg",
  plot = model_sets,
  width = 35,
  height = 25,
  dpi = 300
)
ggsave(
  file = "output/phylopath_model_sets_custom.jpg",
  plot = model_sets,
  width = 35,
  height = 25,
  dpi = 300
)
ggsave(
  file = "output/phylopath_model_sets_custom.pdf",
  plot = model_sets,
  width = 35,
  height = 25,
  dpi = 300
)

