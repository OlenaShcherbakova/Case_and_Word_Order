#running the scripts in the correct order

source("library.R")

#phylopath analysis
source("phylopath_3_variables.R")

#plots
source("plot_ggraph_causal_models_averaged.R")
source("plot_phylopath_model_set.R")
source("plot_maps.R")
source("plot_heatmap.R")
source("plot_mirror_trees_IE.R")
source("plot_mirror_trees_UA.R")
source("plot_mirror_trees_ST.R")

#measure phylogenetic signal of three binary grammatical features
source("measuring_phylosignal.R")

#generating the table of the language sample
source("generating_languages_sample_list.R")

#the scripts for brms analyses and model comparisons below are time-consuming, so the output information is available in output_models and output_tables folders
source("brms_models_NC.R")
source("brms_LOO.R")
source("brms_kfold.R")

source("brms_models_Vf.R")
source("brms_LOO_Vf.R")
source("brms_kfold_Vf.R")

source("brms_models_FWO.R")
source("brms_LOO_FWO.R")
source("brms_kfold_FWO.R")

#generating the table of outputs from these scripts:
source("brms_models_comparison_tables_summary.R")



