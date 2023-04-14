#phylogenetic path analysis on 1705 languages for three following variables: NC, Vf, FWO

source('library.R')

set.seed(123) 

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv")) 

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <- grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label, ]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)


# make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode))

rownames(grambank_phylopath_compl) <- grambank_phylopath_compl$Glottocode
grambank_phylopath_compl$Verb_final <- as.factor(grambank_phylopath_compl$Verb_final)
grambank_phylopath_compl$Free_word_order <- as.factor(grambank_phylopath_compl$Free_word_order)
grambank_phylopath_compl$Nominal_case <- as.factor(grambank_phylopath_compl$Nominal_case)

names(grambank_phylopath_compl)[names(grambank_phylopath_compl) == "Verb_final"] <- "Vf"
names(grambank_phylopath_compl)[names(grambank_phylopath_compl) == "Free_word_order"] <- "FWO"
names(grambank_phylopath_compl)[names(grambank_phylopath_compl) == "Nominal_case"] <- "NC"

#modeling 12 possible causal scenarios

m <- define_model_set(
  null = c(),
  a = c(NC~FWO+Vf),
  b = c(Vf~NC, FWO~NC),
  c = c(NC~Vf, FWO~NC),
  d = c(NC~FWO, Vf~NC),
  e = c(Vf~NC, FWO~Vf),
  f = c(FWO~NC, Vf~FWO),
  g = c(Vf~FWO, NC~Vf),
  h = c(FWO~Vf, NC~FWO),
  i = c(NC~Vf),
  j = c(NC~FWO),
  k = c(Vf~NC),
  l = c(FWO~NC)
)

positions <- data.frame(
  name = c('NC', 'FWO', 'Vf'),
  x = c(2, 1, 3),
  y = c(2, 1, 1)
)

plot_model_set(m, manual_layout = positions)
model_sets <- plot_model_set(m, manual_layout = positions, edge_width = 1)

p <- phylo_path(m, grambank_phylopath_compl, tree)

#"Specifically, it reports the model name, the number of independence claims made by the model (k), the number of parameters (q), the C statistic and the accompanying p-value. A significant p-value would indicate that the available evidence rejects the model. It also reports model selection information: the C-statistic information criterion corrected for small sample sizes (CICc), the difference in CICc with the top model (delta_CICc) and finally the associated relative likelihoods (l) and CICc weights (w)" (van der Bijl 2018)
s <- summary(p)
s
plot(s)
phylopath_summary <- plot(s)
ggsave(file="output/phylopath_nc_summary.svg", plot=phylopath_summary, width=10, height=8)

a <- average(p)
averaged_model <- plot(a, manual_layout = positions, text_size = 4.5)

a_ci <- average(p, boot = 500)
a_ci

coef_plot <- coef_plot(a_ci, error_bar = "ci", order_by = "strength") + ggplot2::coord_flip()
ggsave(file="output/phylopath_nc_coef_plot_averaged.svg", plot=coef_plot, width=10, height=8)

save(a_ci, file = "output_models/phylopath_CI_nc_averaged.RData")
load('output_models/phylopath_CI_nc_averaged.RData')


b <- best(p)
best_model <- plot(b, manual_layout = positions, text_size = 4.5)
ggsave(file="output/phylopath_nc_best_model.svg", plot=best_model, width=10, height=8)

b_ci <- best(p, boot = 500)
b_ci

coef_plot <- coef_plot(b_ci, error_bar = "ci", order_by = "strength") + ggplot2::coord_flip()
ggsave(file="output/phylopath_nc_coef_plot.svg", plot=coef_plot, width=10, height=8)

save(b_ci, file = "output_models/phylopath_CI_nc.RData")
load('output_models/phylopath_CI_nc.RData')


#custom plot of model comparisons
cut_off = 2

s$model <- factor(s$model, rev(s$model))
phylopath_models <-  ggplot2::ggplot(s, ggplot2::aes_(~w, ~model, fill = ~delta_CICc < cut_off, label = ~round(p, 3))) +
  ggplot2::geom_col(col = 'black', alpha = 0.6) +
  ggplot2::geom_text(hjust = "inward", text_size=15) +
  ggplot2::scale_fill_manual(
    values = c('TRUE' = 'firebrick', 'FALSE' = 'black'),
    labels = c('TRUE' = paste('within', cut_off, 'CICc')),
    breaks = c('TRUE')
  ) +
  ggplot2::scale_x_continuous(position = 'top') +
  ggplot2::coord_cartesian(expand = FALSE) +
  ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
  ggplot2::labs(y = "models", x = "model weight", caption = "bar labels are p-values, signficance indicates rejection") +
  ggplot2::theme(legend.position = 'bottom', text = element_text(size = 15))

ggsave(file="output/phylopath_nc_summary_custom.svg", plot=phylopath_models, width=10, height=8)

table <- cbind(s$model, s$CICc, s$delta_CICc, s$l, s$w, s$k, s$q, s$C, s$p)
colnames(table) <- c("model", "CICc", "delta_CICc", "relative likelihood", "CICc weight", "n of independence claims", "n of parameters", "C statistic", "p-value")
table <- table %>%
  as.data.frame() %>%
  mutate_at(vars("CICc", "delta_CICc", "relative likelihood", "CICc weight", "n of independence claims", "n of parameters", "C statistic", "p-value"), as.numeric) %>%
  mutate_if(is.numeric, round, 2)

write.csv(table, file="output/Phylopath_output_table.csv")
