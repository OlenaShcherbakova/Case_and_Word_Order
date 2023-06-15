library(here)
source('library.R')

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv")) %>%
  filter(Family_ID == "indo1319")

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <-
  grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label,]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)

#replacing Glottocodes with informative names
tree$tip.label[match(grambank_phylopath_compl$Glottocode, tree$tip.label)] <-
  grambank_phylopath_compl$Name

# make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Name))

grambank_phylopath_compl <- grambank_phylopath_compl %>%
  mutate(Nominal_case_labelled = dplyr::recode(Nominal_case,
                                                      "1" = "present",
                                                      "0" = "absent")) %>%
  mutate(Verb_final_labelled = dplyr::recode(Verb_final,
                                             "1" = "present",
                                             "0" = "absent")) %>%
  mutate(Free_word_order_labelled = dplyr::recode(Free_word_order,
                                                  "1" = "present",
                                                  "0" = "absent"))

Trait_1 <-
  setNames(grambank_phylopath_compl[, "Nominal_case_labelled"], grambank_phylopath_compl[, "Name"])
Trait_2 <-
  setNames(grambank_phylopath_compl[, "Verb_final_labelled"], grambank_phylopath_compl[, "Name"])
Trait_3 <-
  setNames(grambank_phylopath_compl[, "Free_word_order_labelled"], grambank_phylopath_compl[, "Name"])

ylim <- nrow(grambank_phylopath_compl)

#Trait 1
#cols <- setNames(palette()[1:length(unique(Trait_1))],sort(unique(Trait_1)))
cols <-
  setNames(c("gray80", "#DC143C")[1:length(unique(Trait_1))], sort(unique(Trait_1)))
fitER <- ape::ace(Trait_1, tree, model = "ER", type = "discrete")
ancstats <- as.data.frame(fitER$lik.anc)
ancstats$node <- 1:tree$Nnode + Ntip(tree)

pies <- nodepie(ancstats, cols = 1:2) #, cols = 1:6)
pies <- lapply(pies, function(g)
  g + scale_fill_manual(values = cols))

tree2 <-
  full_join(tree, data.frame(label = names(Trait_1), stat = Trait_1), by = 'label')
p <-
  ggtree(tree2, size=0.8) + geom_tiplab(hjust = 0.5,
                              offset = 2.1,
                              size = 9.5) + #offset = lengths_vector) +
  geom_tippoint(aes(color = stat), size = 7) +
  scale_color_manual("Case", values = cols) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "vertical",
    text = element_text(size = 55)
  ) +
  xlim(NA, 10) + ylim(NA, ylim)

tree_nc <- p + geom_inset(pies, width = .06, height = .06)

#Trait 2
cols <-
  setNames(c("gray80", "#DC143C")[1:length(unique(Trait_2))], sort(unique(Trait_2)))
fitER <- ape::ace(Trait_2, tree, model = "ER", type = "discrete")
ancstats <- as.data.frame(fitER$lik.anc)
ancstats$node <- 1:tree$Nnode + Ntip(tree)

pies <- nodepie(ancstats, cols = 1:2) #, cols = 1:6)
pies <- lapply(pies, function(g)
  g + scale_fill_manual(values = cols))

tree2 <-
  full_join(tree, data.frame(label = names(Trait_2), stat = Trait_2), by = 'label')
p <-
  ggtree(tree2, size=0.8) + #geom_tiplab(hjust = 0.5, offset=0.7) + #offset = lengths_vector) +
  geom_tippoint(aes(color = stat), size = 7) +
  scale_color_manual("Verb-final word order", values = cols) +
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.direction = "vertical",
    text = element_text(size = 55)
  ) +
  xlim(NA, 10) + ylim(NA, ylim) +
  scale_x_reverse()

tree_vf <-
  p + geom_inset(pies,
                 width = .06,
                 height = .06,
                 reverse_x = TRUE)

#Trait 3
cols <-
  setNames(c("gray80", "#DC143C")[1:length(unique(Trait_3))], sort(unique(Trait_3)))
fitER <- ape::ace(Trait_3, tree, model = "ER", type = "discrete")
ancstats <- as.data.frame(fitER$lik.anc)
ancstats$node <- 1:tree$Nnode + Ntip(tree)

pies <- nodepie(ancstats, cols = 1:2) #, cols = 1:6)
pies <- lapply(pies, function(g)
  g + scale_fill_manual(values = cols))

tree2 <-
  full_join(tree, data.frame(label = names(Trait_3), stat = Trait_3), by = 'label')
p <-
  ggtree(tree2, size=0.8) + #geom_tiplab(hjust = 0.5, offset=0.7) + #offset = lengths_vector) +
  geom_tippoint(aes(color = stat), size = 7) +
  scale_color_manual("Flexible word order", values = cols) +
  theme(
    legend.position = "top",
    legend.direction = "vertical",
    legend.justification = "right",
    text = element_text(size = 55)
  ) +
  xlim(NA, 10) + ylim(NA, ylim) +
  scale_x_reverse()

tree_fwo <-
  p + geom_inset(pies,
                 width = .06,
                 height = .06,
                 reverse_x = TRUE)


try_1 <- tree_nc + tree_vf +
  plot_layout(widths = c(1, 0.7))
try_2 <- tree_nc + tree_fwo  +
  plot_layout(widths = c(1, 0.7))
try <- try_1 / try_2
ggsave(
  file = "output/plot_IE_mirror_trees.svg",
  plot = try,
  width = 25,
  height = 40,
  dpi = 300
)

ggsave(
  file = "output/plot_IE_mirror_trees.jpg",
  plot = try,
  width = 25,
  height = 40,
  dpi = 300
)

ggsave(
  file = "output/plot_IE_mirror_trees.pdf",
  plot = try,
  width = 25,
  height = 40,
  dpi = 300
)
