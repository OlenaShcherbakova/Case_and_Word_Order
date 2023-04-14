#customizing the plotting function of phylopath to produce plots of phylopath output

box_x = 22; box_y = 18; edge_width = 3.5; curvature = 0.08
colors = c('firebrick', 'navy')

custom_layout <- matrix(c(
  2,2,
  1,1,
  3,1),
  ncol=2, byrow=TRUE)

custom_layout <- as.data.frame(custom_layout)
colnames(custom_layout) <- c("x", "y")
custom_layout$name <- c("Case", "Free", "Verb-final")

#best model
load('output_models/phylopath_CI_nc_averaged.RData')

colnames(a_ci[["coef"]]) <- c("Verb-final", "Case", "Free")
rownames(a_ci[["coef"]]) <- c("Verb-final", "Case", "Free")


g <- igraph::graph_from_adjacency_matrix(a_ci$coef, weighted = TRUE)
l <- ggraph::create_layout(g, 'igraph', algorithm = "sugiyama") #algorithm does not work
l$x <- custom_layout$x[match(l$name, custom_layout$name)]
l$y <- custom_layout$y[match(l$name, custom_layout$name)]

arrow = grid::arrow(type = 'open', 18, grid::unit(15, 'points'))

p <- ggplot2::ggplot(l) +
  ggraph::geom_edge_arc(
    ggplot2::aes_(width = ~abs(weight), color = ~weight < 0, 
                  label = ~round(weight, 2)),
    strength = 0.1, #completely straight lines strength = 0; > 0 - curved lines
    arrow = arrow, end_cap = ggraph::rectangle(box_x, box_y, 'mm'),
    start_cap = ggraph::rectangle(box_x, box_y, 'mm'), show.legend = FALSE,
    linejoin = c('bevel'), #lineend = c('round'), #"bevel" or "round" or "mitre"
    angle_calc = 'along', label_dodge = grid::unit(12, 'points'), label_size=7) +
  ggraph::geom_node_text(ggplot2::aes_(label = ~name), size = 7) +
  ggraph::scale_edge_width_continuous(limits = c(0, max(igraph::E(g)$weight)), range = c(0, 2),
                                      guide = 'none') +
  ggraph::scale_edge_color_manual(name = NULL,
                                  values = c('FALSE' = colors[1], 'TRUE' = colors[2]),
                                  labels = c('positive', 'negative')) + 
  ggraph::theme_graph(base_family = 'sans') + ggplot2::theme(legend.position = 'bottom', text = element_text(size = 20)) +
  scale_x_continuous(expand = c(0.2, 0.2)) + scale_y_continuous(expand = c(0.2, 0.2)) #+ ggtitle('best model')
p

ggsave(file="output/graph_phylopath_nc_averaged_model.svg", plot=p, width=10, height=8)


#best model coefficients
colnames(a_ci[["coef"]]) <- c("Verb-final", "Case", "Free")
rownames(a_ci[["coef"]]) <- c("Verb-final", "Case", "Free")

v <- colnames(a_ci$coef)
df <- as.data.frame(a_ci$coef)
df$from <- rownames(df)
df <- stats::reshape(df, varying = v, 'coef', direction = 'long')
df$to <- v[df$time]
df$lower <- c(a_ci$lower)
df$upper <- c(a_ci$upper)

df$path <- paste(df$from, df$to, sep = ' \U2192 ')

df <- df[order(df$coef), ]

df <- df[!df$coef == 0,]

#custom change of rows to match the other plot
df <- df %>%
  mutate(num = c(1, 4, 3, 2)) %>%
  arrange(num) %>%
  mutate(path = factor(path, path))

coef <- ggplot2::ggplot(df,
                        ggplot2::aes_(~path, ~coef, ymin = ~lower, ymax = ~upper)) +
  ggplot2::geom_hline(yintercept = 0, size = 1, lty = 2) +
  ggplot2::geom_pointrange(size = 0.75) +
  ggplot2::xlab('') +
  ggplot2::ylab('standardized regression\ncoefficient \U00B1 CI') + theme_classic() + theme(axis.text.x = element_text(angle = 15, hjust=1, size = 18), axis.text = element_text(size = 18), axis.title = element_text(size = 18))

coef

ggsave(file="output/graph_phylopath_nc_averaged_model_coefficients.svg", plot=coef, width=10, height=8)

joined <- p|coef
ggsave(file="output/graph_phylopath_averaged.svg", plot=joined, width=12, height=9)

#joined <- p/coef
joined <- p + coef +
  plot_layout(heights = c(0.6, 0.4))
ggsave(file="output/graph_phylopath_averaged.svg", plot=joined, width=6, height=8)


