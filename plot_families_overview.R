#generating input data and
#preparing a plot for describing tendencies in selected language families

#Script written by Simon Greenhill and modified by Olena Shcherbakova

source('library.R')

set.seed(123)

grambank_phylopath_compl <- load_data_NAs() %>%
  left_join(read_tsv("data/glottolog_AUTOTYPE_areas.tsv")) %>%
  dplyr::select(-c(Latitude, Longitude, AUTOTYP_area, Name))

tree <- read.tree("data/wrangled.tree")

grambank_phylopath_compl <-
  grambank_phylopath_compl[grambank_phylopath_compl$Glottocode %in% tree$tip.label,]

grambank_phylopath_compl %>%
  write_tsv(file = "data/GB_input_families.tsv")

grambank_phylopath_compl <- read_tsv("data/GB_input_families.tsv")

grambank_phylopath_compl_selected <- grambank_phylopath_compl %>%
  filter(
    Family_ID == "aust1307" | Family_ID == "aust1305" |
      Family_ID == "taik1256" | Family_ID == "cent2225" |
      Family_ID == "maya1287" | Family_ID == "atla1278" |
      Family_ID == "drav1251" | Family_ID == "ural1272" |
      Family_ID == "nakh1245" | Family_ID == "gong1255" |
      Family_ID == "turk1311"
  ) %>%
  mutate(Family_ID = as.factor(Family_ID)) %>%
  mutate(
    Family_ID = dplyr::recode(
      Family_ID,
      "aust1307" = "Austronesian",
      "aust1305" = "Austroasiatic",
      "taik1256" = "Tai-Kadai",
      "cent2225" = "Central Sudanic",
      "maya1287" = "Mayan",
      "atla1278" = "Atlantic-Congo",
      "drav1251" = "Dravidian",
      "ural1272" = "Uralic",
      "nakh1245" = "Nakh-Daghestanian",
      "gong1255" = "Ta-Ne-Omotic",
      "turk1311" = "Turkic"
    )
  )

# grambank_phylopath_compl_selected$Family_ID <-
#   fct_relevel(grambank_phylopath_compl_selected$Family_ID,
#               "Ta-Ne-Omotic",
#               "Turkic",
#               "Austronesian")


grambank_phylopath_compl_selected %>%
  write_tsv(file = "data/GB_input_families_selected.tsv")

devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE)
library(ggradar)

df <-
  read_tsv("data/GB_input_families_selected.tsv")

get_proportions <- function(df, var) {
  var <- sym(var)
  df %>%
    dplyr::select(Family_ID, !!var) %>%
    group_by(Family_ID) %>%
    summarise(var = sum(!!var) / n()) %>%
    rename(!!var := var)
}

df.prop <- get_proportions(df, 'Verb_final') %>%
  left_join(get_proportions(df, 'Free_word_order'), by = "Family_ID") %>%
  left_join(get_proportions(df, 'Nominal_case'), by = "Family_ID")

df.prop <-
  df.prop %>% rename(`Verb-final` = Verb_final,
                     `Flexible` = Free_word_order,
                     `Case` = Nominal_case)

# remove unwanted families for the second plot
df.prop_2 <-
  df.prop %>% filter(
    Family_ID %in% c(
      'Uralic',
      'Dravidian',
      'Nakh-Daghestanian',
      'Ta-Ne-Omotic',
      'Turkic'
    ) == FALSE
  )

df.prop_1 <-
  df.prop %>% filter(
    Family_ID %in% c(
      'Uralic',
      'Dravidian',
      'Nakh-Daghestanian',
      'Ta-Ne-Omotic',
      'Turkic'
    ) == TRUE
  )

prop_plot_1 <-
  ggradar(df.prop_2,
          grid.label.size = 8,
          axis.label.size = 7) +
  facet_wrap(~ Family_ID, ncol = 2) +
  guides(color = "none") +
  theme(text = element_text(size = 27)) 

prop_plot_2 <-
  ggradar(df.prop_1,
          grid.label.size = 8,
          axis.label.size = 7) + 
  facet_wrap(~ Family_ID, ncol = 2) + 
  guides(color = "none") +
  theme(text = element_text(size = 27)) 
  

ggsave(
  file = "output/plot_proportions_1.jpg",
  plot = prop_plot_1,
  width = 11,
  height = 14
)

ggsave(
  file = "output/plot_proportions_1.pdf",
  plot = prop_plot_1,
  width = 11,
  height = 14,
  dpi=300
)

ggsave(
  file = "output/plot_proportions_2.jpg",
  plot = prop_plot_2,
  width = 11,
  height = 14
)

ggsave(
  file = "output/plot_proportions_2.pdf",
  plot = prop_plot_2,
  width = 11,
  height = 14,
  dpi=300
)
