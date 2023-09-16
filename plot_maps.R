source('library.R')

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv"))

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <-
  grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label,]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)


# make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode))

#Nominal case under word order condition (Free word order)

combination <- subset(
  x = grambank_phylopath_compl,
  select = c(
    "Glottocode",
    "Free_word_order",
    "Nominal_case",
    "Verb_final",
    "Longitude",
    "Latitude"
  )
)


#four conditions
world <-
  map_data(
    'world',
    wrap = c(-25, 335),
    ylim = c(-56, 80),
    margin = T
  )

lakes <-
  map_data(
    "lakes",
    wrap = c(-25, 335),
    col = "white",
    border = "gray",
    ylim = c(-55, 65),
    margin = T
  )

#shifting the longlat of the dataframe to match the pacific centered map
combination <- combination %>%
  mutate(Longitude = if_else(Longitude <= -25, Longitude + 360, Longitude))

combination_new <- combination %>%
  mutate(
    NC_Vf_details = case_when(
      Nominal_case == "1" & Verb_final == "1" ~ "Case + Verb-final",
      Nominal_case == "1" & Verb_final == "0" ~ "Case",
      Nominal_case == "0" & Verb_final == "0" ~ "None",
      Nominal_case == "0" & Verb_final == "1" ~ "Verb-final"
    )
  ) %>%
  mutate(
    NC_FWO_details = case_when(
      Nominal_case == "1" &
        Free_word_order == "1" ~ "Case + Flexible",
      Nominal_case == "1" & Free_word_order == "0" ~ "Case",
      Nominal_case == "0" & Free_word_order == "0" ~ "None",
      Nominal_case == "0" &
        Free_word_order == "1" ~ "Flexible"
    )
  ) %>%
  mutate(NC_FWO_details = as.factor(NC_FWO_details)) %>%
  mutate(NC_Vf_details = as.factor(NC_Vf_details)) %>%
  mutate(NC_Vf_details = factor(
    NC_Vf_details,
    levels = c("Case", "Verb-final", "Case + Verb-final", "None")
  )) %>%
  mutate(NC_FWO_details = factor(
    NC_FWO_details,
    levels = c("Case", "Flexible", "Case + Flexible", "None")
  ))

combination_new %>%
  group_by(NC_Vf_details) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

combination_new %>%
  group_by(NC_FWO_details) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

#Basemap
basemap <- ggplot(combination_new) +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    colour = "gray87",
    fill = "gray87",
    size = 0.5
  ) +
  geom_polygon(
    data = lakes,
    aes(x = long, y = lat, group = group),
    colour = "gray87",
    fill = "white",
    size = 0.3
  )  +
  theme(
    panel.grid.major = element_blank(),
    #all of these lines are just removing default things like grid lines, axises etc
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )   +
  coord_map(projection = "vandergrinten", ylim = c(-56, 67))


m1 <- basemap + geom_point(
  aes(x = Longitude, y = Latitude, color = NC_Vf_details),
  alpha = 0.4,
  stat = "identity",
  size = 2
)

m1 <- m1 + scale_color_manual(
  values = c(
    "Case" = "#DDCC77",
    "Verb-final" = "#88CCEE",
    "Case + Verb-final" = "#117733",
    "None" = "gray50"
  )
) +
  labs(color = "") +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = c(.5, .17),
    legend.background = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))


m2 <- basemap + geom_point(
  aes(x = Longitude, y = Latitude, color = NC_FWO_details),
  alpha = 0.4,
  stat = "identity",
  size = 2
)

m2 <- m2 + scale_color_manual(
  values = c(
    "Case" = "#DDCC77",
    "Flexible" = "#88CCEE",
    "Case + Flexible" = "#117733",
    "None" = "gray50"
  )
) +
  labs(color = "") +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.position = c(.5, .17),
    legend.background = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))

two_maps <- m1 / m2

ggsave(
  file = "output/maps_both.jpg",
  plot = two_maps,
  width = 10,
  height = 12,
  dpi = 300
)

ggsave(
  file = "output/maps_both.pdf",
  plot = two_maps,
  width = 10,
  height = 12,
  dpi = 300
)

