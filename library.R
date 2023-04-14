#Run this script first. The input data file is based Grambank v. 1.0 and Glottolog v.4.4.

#necessary only for running on cluster
#.libPaths(c("rlib/", .libPaths()))

if (!suppressPackageStartupMessages(require("pacman"))) {
  install.packages("pacman")
}

pacman::p_load(
  #Matrix, #loading this package to avoid problems with tidyverse download in R.4.2 as described here https://github.com/tidymodels/tune/issues/362
  tidyverse,
  reshape2,
  stringr,
  readr,
  tidyr,
  dplyr,
  flextable,
  
  #brms
  brms,
  posterior,
  tidybayes,
  bayesplot,
  
  #plotting
  viridis,
  ggtree,
  RColorBrewer,
  ggplot2,
  ggraph,
  patchwork,
  
  # phylogenetic packages
  ape,
  phytools,
  caper,
  bayestraitr,
  phylopath,
  geiger,
  
  # testing
  assertthat,
  
  # misc
  here,
  ggnewscale
)

OUTPUTDIR_output <- here("output")
# create output dir if it does not exist.
if (!dir.exists(OUTPUTDIR_output)) {
  dir.create(OUTPUTDIR_output)
}

OUTPUTDIR_output_tables <- here("output_tables")
# create output dir if it does not exist.
if (!dir.exists(OUTPUTDIR_output_tables)) {
  dir.create(OUTPUTDIR_output_tables)
}

OUTPUTDIR_output_models <- here("output_models")
# create output dir if it does not exist.
if (!dir.exists(OUTPUTDIR_output_models)) {
  dir.create(OUTPUTDIR_output_models)
}


# quiet down, tidyverse:
options(tidyverse.quiet = TRUE)
options(warn.conflicts = FALSE)
options(stringsAsFactors = FALSE)

#The following line is commented out because it leads to the script generating the Grambank input file for the analysis and glottolog file from Glottolog 4.4 version based on grambank-analysed submodule, which is not currently publically available.
#source("obtaining_files_from_grambank-analysed.R")

# helper function to convert and load trait data into a named vector for
# plotting.
get_trait_vector <- function(tree, data, variable) {
  x <- data[tree$tip.label, variable]
  x[is.na(x)] <- 0  # set NA's to zero to enable plotting.
  names(x) <- tree$tip.label
  x
}

if (!dir.exists("grambank-analysed")) {
  source("get_external_data.R")
}
source("obtaining_files_from_grambank-analysed.R")
source("generating_GB_input_file.R")

load_data_NAs <- function(filename = "data/GB_input.tsv") {
  grambank <-
    read.csv(
      filename,
      header = TRUE,
      sep = '\t',
      stringsAsFactors = FALSE
    )
  colnames(grambank)[colnames(grambank) == "Language_ID"] <-
    "Glottocode"
  grambank <-
    grambank[!with(grambank,
                   is.na(GB070) |
                     is.na(GB131) | is.na(GB132) | is.na(GB133) | is.na(GB136)), ]
  grambank_phylopath_compl <-
    subset(
      x = grambank,
      select = c("Glottocode", "GB070", "GB131", "GB132", "GB133", "GB136")
    )
  
  #preparing the file for analysis
  for (i in 1:nrow(grambank_phylopath_compl)) {
    if ((grambank_phylopath_compl$GB131[i] == "0") &
        (grambank_phylopath_compl$GB132[i] == "0") &
        (grambank_phylopath_compl$GB133[i] == "1")) {
      grambank_phylopath_compl$Verb_final[i] <- "1"
    }
    else {
      grambank_phylopath_compl$Verb_final[i] <- "0"
    }
  }
  
  for (i in 1:nrow(grambank_phylopath_compl)) {
    if (grambank_phylopath_compl$GB070[i] == "1") {
      grambank_phylopath_compl$Nominal_case[i] <- "1"
    }
    else {
      grambank_phylopath_compl$Nominal_case[i] <- "0"
    }
  }
  
  for (i in 1:nrow(grambank_phylopath_compl)) {
    if (grambank_phylopath_compl$GB136[i] == "0") {
      grambank_phylopath_compl$Free_word_order[i] <- "1"
    }
    else {
      grambank_phylopath_compl$Free_word_order[i] <- "0"
    }
  }
  
  grambank_phylopath_compl <-
    subset(
      x = grambank_phylopath_compl,
      select = c(
        "Glottocode",
        "Verb_final",
        "Free_word_order",
        "Nominal_case"
      )
    )
  
  # make sure we have factors here.
  for (col in colnames(grambank_phylopath_compl)) {
    grambank_phylopath_compl[[col]] <-
      as.factor(grambank_phylopath_compl[[col]])
  }
  rownames(grambank_phylopath_compl) <-
    grambank_phylopath_compl$Glottocode
  grambank_phylopath_compl
}

source('varcovar.spatial_function.R')
source('wrangling_tree.R')
source('assigning_AUTOTYP_areas.R')
source('generating_languages_sample_list.R')
