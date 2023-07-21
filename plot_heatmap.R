#heat map with a tree plot: 3 grammatical features - 2 feature combinations

source('library.R')

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv"))

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <-
  grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label,]
tree <- keep.tip(tree, grambank_phylopath_compl$Glottocode)


# make sure we have a complete match between the tree and the data
stopifnot(all(tree$tip.label %in% grambank_phylopath_compl$Glottocode))

grambank_phylopath_compl <- grambank_phylopath_compl %>%
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
      Nominal_case == "1" & Free_word_order == "1" ~ "Case + Flexible",
      Nominal_case == "1" & Free_word_order == "0" ~ "Case",
      Nominal_case == "0" & Free_word_order == "0" ~ "None",
      Nominal_case == "0" & Free_word_order == "1" ~ "Flexible"
    )
  )

grambank_phylopath_compl$NC_Vf_details <-
  factor(
    grambank_phylopath_compl$NC_Vf_details,
    levels = c("Case", "Verb-final", "Case + Verb-final", "None")
  )
grambank_phylopath_compl$NC_FWO_details <-
  factor(
    grambank_phylopath_compl$NC_FWO_details,
    levels = c("Case", "Flexible", "Case + Flexible", "None")
  )

#plot(ladderize(tree), cex = 0.5, align.tip.label = TRUE, label.offset=0.1)
#tiplabels(pch=22, bg=grambank_phylopath_compl[tree$tip.label, 'grambank_phylopath_compl'], adj = 0)

row.names(grambank_phylopath_compl) <-
  grambank_phylopath_compl$Glottocode

df1 <-
  grambank_phylopath_compl %>% dplyr::select(NC_Vf_details, Glottocode)


#changing the names in the column FWO-related column in df2 so that we have later one uniform palette and label for two word orders
df2 <-
  grambank_phylopath_compl %>% dplyr::select(NC_FWO_details, Glottocode) %>%
  mutate(
    NC_FWO_details =
      dplyr::recode(
        NC_FWO_details,
        "Case + Flexible" = "Case + word order",
        "Flexible" = "Word order"
      )
  )


row.names(df1) <- df1$Glottocode
row.names(df2) <- df2$Glottocode

df1 <- df1 %>%
  dplyr::select(NC_Vf_details) %>% dplyr::rename(`Verb-final` = `NC_Vf_details`)

df2 <- df2 %>%
  dplyr::select(NC_FWO_details) %>% rename(`Flexible` = `NC_FWO_details`)

### Adding colored branches of the biggest families in the dataset
grambank_phylopath_compl %>%
  group_by(Family_ID) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(n)) %>%
  filter(!Family_ID == "") %>%
  top_n(13, freq) -> table

biggest_families <- table$Family_ID
grambank_phylopath_compl$family_status <- NA
grambank_phylopath_compl$family_status <-
  ifelse(
    grambank_phylopath_compl$Family_ID %in% biggest_families,
    grambank_phylopath_compl$Family_ID,
    "other"
  )

#double-checking if all families indeed converted to names and none is left with "NA"
unique(grambank_phylopath_compl$family_status)

grambank_phylopath_compl <- grambank_phylopath_compl %>%
  mutate(
    family = dplyr::recode(
      family_status,
      "aust1307"  = "Austronesian",
      "aust1305"  = "Austroasiatic",
      "indo1319"  = "Indo-European",
      "atla1278" = "Atlantic-Congo",
      "pama1250" = "Pama-Nyungan",
      "sino1245" = "Sino-Tibetan",
      "afro1255"  = "Afro-Asiatic",
      "nucl1709" = "Trans New Guinea",
      "ural1272" = "Uralic",
      "araw1281" = "Arawakan",
      "utoa1244" = "Uto-Aztecan",
      "otom1299" = "Otomanguean",
      "cent2225" = "Central Sudanic",
      "other" = "other"
    )
  )

#double-checking if all families indeed converted to names and none is left with "NA"
unique(grambank_phylopath_compl$family)

#ordering the families in the desired way
grambank_phylopath_compl$family <-
  factor(
    grambank_phylopath_compl$family,
    order = TRUE,
    levels = c(
      "other",
      "Austronesian",
      "Austroasiatic",
      "Sino-Tibetan",
      "Indo-European",
      "Atlantic-Congo",
      "Afro-Asiatic",
      "Pama-Nyungan",
      "Trans New Guinea",
      "Uralic",
      "Arawakan",
      "Uto-Aztecan",
      "Otomanguean",
      "Central Sudanic"
    )
  )

tips_lists <- vector(mode = "list", length = 13)

for (f in 1:length(biggest_families)) {
  tips_lists[[f]] <-
    na.omit(grambank_phylopath_compl[grambank_phylopath_compl$Family_ID == biggest_families[f], ]$Glottocode)
}

#the correct order within biggest families is preserved and the Glottocodes are replaced with suitable family name labels
biggest_families_verbose <- dplyr::recode(
  biggest_families,
  "aust1307"  = "Austronesian",
  "aust1305"  = "Austroasiatic",
  "indo1319"  = "Indo-European",
  "atla1278" = "Atlantic-Congo",
  "pama1250" = "Pama-Nyungan",
  "sino1245" = "Sino-Tibetan",
  "afro1255"  = "Afro-Asiatic",
  "nucl1709" = "Trans New Guinea",
  "ural1272" = "Uralic",
  "araw1281" = "Arawakan",
  "utoa1244" = "Uto-Aztecan",
  "otom1299" = "Otomanguean",
  "cent2225" = "Central Sudanic",
  "other" = "other"
)

names(tips_lists) <- biggest_families_verbose

nodes <- vector(mode = "character", length = length(biggest_families))

for (tips in 1:length(tips_lists)) {
  nodes[tips] <- getMRCA(tree, tips_lists[[tips]])
}

#test
#nodes <- vector(mode="character", length=1)
#nodes[1] <- getMRCA(tree, tips_lists[[4]])

nodes <- as.numeric(nodes)

coloured_branches <- groupClade(tree, nodes)
coloured_branches <-
  ggtree(
    coloured_branches,
    layout = 'rect',
    branch.length = 'none',
    size = 0.5
  )#, aes(color=group)) +
#  scale_color_manual(values = cols, labels = c("other", biggest_families_verbose)) #tips lists length


p1 <-
  gheatmap(
    coloured_branches,
    df1,
    offset = -3,
    width = .3,
    colnames_angle = 0,
    colnames_offset_y = 25,
    colnames_position = "top",
    font.size = 20,
    hjust = 0.5,
    color = NULL
  ) + ylim(-5, 1750) + #ylim(-5, 1450)
  scale_fill_manual(
    "df1",
    values = c(
      "Case" = "#DCE319FF",
      "Verb-final" = "#404688FF",
      "Case + Verb-final" = "#31B57BFF",
      "None" = "gray50"
    )
  ) +
  geom_cladelabel(
    node = nodes[1],
    label = biggest_families_verbose[1],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[2],
    label = biggest_families_verbose[2],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[3],
    label = biggest_families_verbose[3],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[4],
    label = biggest_families_verbose[4],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[5],
    label = biggest_families_verbose[5],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[6],
    label = biggest_families_verbose[6],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[7],
    label = biggest_families_verbose[7],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[8],
    label = biggest_families_verbose[8],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[9],
    label = biggest_families_verbose[9],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[10],
    label = biggest_families_verbose[10],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[11],
    label = biggest_families_verbose[11],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[12],
    label = biggest_families_verbose[12],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  geom_cladelabel(
    node = nodes[13],
    label = biggest_families_verbose[13],
    offset = 22,
    align = TRUE,
    fontsize = 20
  ) +
  guides(fill = "none")


p2 <- p1 + new_scale_fill()

p3 <- gheatmap(
  p2,
  df2,
  offset = 7,
  width = .3,
  colnames_angle = 0,
  colnames_offset_y = 25,
  colnames_position = "top",
  font.size = 20,
  hjust = 0.5,
  color = NULL
) + ylim(-5, 1750) + xlim(0, 70) +
  scale_fill_manual(
    "",
    values = c(
      "Case" = "#DCE319FF",
      "Word order" = "#404688FF",
      "Case + word order" = "#31B57BFF",
      "None" = "gray50"
    )
  ) +
  labs(fill = "") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    text = element_text(size = 70)
  )#,
#legend.key.size = unit(0.7, 'cm'))
p3

ggsave(
  file = "output/plot_heatmap.svg",
  plot = p3,
  width = 30,
  height = 30, 
  dpi = 300
)

ggsave(
  file = "output/plot_heatmap.jpg",
  plot = p3,
  width = 30,
  height = 30, 
  dpi = 300
)

ggsave(
  file = "output/plot_heatmap.png",
  plot = p3,
  width = 30,
  height = 30, 
  dpi = 300
)

ggsave(
  file = "output/plot_heatmap.pdf",
  plot = p3,
  width = 30,
  height = 30, 
  dpi = 300
)
