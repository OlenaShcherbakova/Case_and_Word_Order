#modified version of the phylopath() function to make a custom plot with required text size by adding strip_text_size=50

adjust_layout <- function(l, rotation, flip_x, flip_y) {
  rotation <- rotation * (2 * pi / 360)
  R <- matrix(c(cos(rotation), sin(rotation), -sin(rotation), cos(rotation)), nrow = 2)
  l[c('x', 'y')] <- as.matrix(l[c('x', 'y')]) %*% R
  if (flip_x) {
    l$x <- -l$x
  }
  if (flip_y) {
    l$y <- -l$y
  }
  return(l)
}

combine_with_labels <- function(l, labels) {
  incoming_class <- class(l)
  if (is.null(labels)) {
    return(l)
  }
  if (is.null(names(labels))) {
    stop('labels must be a named vector.', call. = FALSE)
  }
  if (length(setdiff(l$name, names(labels))) > 0) {
    stop('Some nodes are missing from labels.', call. = FALSE)
  }
  l$name <- factor(l$name, names(labels), labels)
  class(l) <- incoming_class
  return(l)
}

plot_model_set <- function(
    model_set, labels = NULL, algorithm = 'kk', manual_layout = NULL,
    text_size = 5, box_x = 12, box_y = 10, edge_width = 1, curvature = 0.05,
    rotation = 0, flip_x = FALSE, flip_y = FALSE, nrow = NULL,
    arrow = grid::arrow(type = 'closed', 15, grid::unit(10, 'points'))) {
  # Input checks
  if (!is.list(model_set) | !all(purrr::map_lgl(model_set, ~inherits(., 'DAG')))) {
    stop('model_set should be a list of DAG objects.')
  }
  if (is.null(names(model_set))) {
    names(model_set) <- LETTERS[seq_along(model_set)]
  }
  var_names <- lapply(model_set, colnames)
  if (length(model_set) > 1 &
      (stats::var(lengths(model_set)) != 0 |
       any(lengths(sapply(var_names[-1], setdiff, var_names[[1]])) != 0))) {
    stop('All causal models need to include the same variables. Combined, your
         models include the following variables:\n',
         paste(sort(unique(unlist(var_names))), collapse = '\n'),
         call. = FALSE)
  }
  
  # Build  single complete graph
  result <- igraph::make_empty_graph() + igraph::vertices(row.names(model_set[[1]]))
  for (i in seq_along(model_set)) {
    m <- model_set[[i]]
    ind  <- which(m == 1)
    from <- ind %% nrow(model_set[[i]])
    to   <- (ind - from) / nrow(model_set[[i]]) + 1
    result <- igraph::add_edges(result, c(rbind(rownames(m)[from], colnames(m)[to])),
                                attr = list(model = names(model_set)[[i]]))
  }
  igraph::edge.attributes(result)$model <- factor(igraph::E(result)$model,
                                                  names(model_set), names(model_set))
  
  l <- ggraph::create_layout(result, 'igraph', algorithm = algorithm)
  if (!is.null(manual_layout)) {
    l$x <- manual_layout$x[match(l$name, manual_layout$name)]
    l$y <- manual_layout$y[match(l$name, manual_layout$name)]
  }
  l <- adjust_layout(l, rotation, flip_x, flip_y)
  l <- combine_with_labels(l, labels)
  
  # Build plot.
  ggraph::ggraph(l) +
    ggraph::geom_edge_arc(strength = curvature, arrow = arrow, edge_width = edge_width,
                          end_cap = ggraph::rectangle(box_x, box_y, 'mm'),
                          start_cap = ggraph::rectangle(box_x, box_y, 'mm')) +
    ggraph::geom_node_text(ggplot2::aes_(label = ~name), size = text_size) +
    ggraph::facet_edges(~model, nrow = nrow) +
    ggplot2::scale_x_continuous(expand = c(0.2, 0)) +
    ggplot2::scale_y_continuous(expand = c(0.2, 0)) +
    ggraph::theme_graph(foreground = 'grey80', base_family = 'sans', strip_text_size=50) 
}