# Code accompanying the paper *The evolutionary dynamics of how languages signal “who does what to whom”* by Olena Shcherbakova, Damián E. Blasi, Volker Gast, Hedvig Skirgård, Russell D. Gray, and Simon J. Greenhill


## Running scripts 

Consult ```all_scripts.R``` for the correct order of running scripts. 

1. Run ```library.R``` for all packages, custom functions, and generating/transforming input files. This will also generate an Appendix with the full list of languages and relevant feature values. `get_external_data.R` inside ```library.R``` fetches input data from Grambank, AUTOTYP, and Glottolog.
2. Run phylgenetic path analysis in phylopath in ```phylopath_3_variables.R``` and obtain the output. Output file ```phylopath_CI_nc_averaged.RData``` was also saved for convenience in ```output_models``` folder, but phylogenetic path analysis takes very little time to run.
3. Plot figures from phylogenetic path analysis with ```plot_ggraph_causal_models_averaged.R``` and ```plot_phylopath_model_set.R```.
4. Plot geographic maps in ```plot_maps.R``` and trees for the global tree in ```plot_heatmap.R``` and the subset of Indo-European, Uto-Aztecan, and Sino-Tibetan families in ```plot_mirror_trees_IE.R```, ```plot_mirror_trees_UA.R```, and ```plot_mirror_trees_ST.R```.
5. Run extra analysis: ```measuring_phylosignal.R``` for measuring the phylogenetic signal in three features. 
6. Run analyses in brms in ```brms_models_NC.R``` (for (Nominal) Case), ```brms_models_Vf.R``` (for Verb-final word order), and ```brms_models_FWO.R``` (for Flexible word order). 
7. Compare the predictive performance of brms models using three approaches (the leave-out-out cross-validation (LOO-CV), LOO-CV with the moment matching correction for importance sampling, and K-fold cross-validation) in ```brms_LOO.R``` and ```brms_kfold.R``` (for Case), ```brms_LOO_Vf.R``` and ```brms_kfold_Vf.R``` (for Verb-final word order), and ```brms_LOO_FWO.R``` and ```brms_kfold_FWO.R``` (for Flexible word order). To generate the table of outputs from these scripts, run ```brms_models_comparison_tables_summary.R```.
Steps 2 and 3 are time-consuming, so all relevant output files have already been saved in in ```output_tables``` folder (```brms_models_comparison.csv```, ```fitted_set1_model1/2/3.csv```, ```kfold_comparison.csv```, ```loo_comparison.csv```, ```loo_comparison_mm.csv```, ```loo_comparison_pareto_k.csv```, ```residuals_set1_model1/2/3.csv``` for Case, but also for Verb-final and Flexible word order, with file names followed "Vf" and "FWO" suffixes respectively). Additionally, 1) the log files from cluster running ```brms``` analysis and model comparison scripts and 2) jobs for running brms analyses on cluster can be found in ```brms_cluster``` folder.
8. Rerun the best models according to Phylogenetic Path Analysis in brms (```model_b.R```, ```model_c.R```, and ```model_d.R```) and obtain the coefficients of fixed effects from this models for comparison (````brms_coefficients_extraction.R```).

## Data sources

The data that serves as the input for the analysis comes from Grambank, (v1.0, Skirgård et al (in prep)), AUTOTYP (v1.01, Bickel et al (2022)), Glottolog (v4.5), and EDGE-tree (v1.0.0, Bouckaert et al (2023)).

All data is available openly via the science archive Zenodo and/or public GitHub repositories. 

Zenodo locations:

*   Grambank (v.1.0) <https://doi.org/10.5281/zenodo.7740140>
*   Grambank-analysed (v1.0) <https://doi.org/10.5281/zenodo.7740822>
*   Glottolog-cldf (v4.5) <https://doi.org/10.5281/zenodo.5772649>
*   AUTOTYP (v1.0.1) <https://doi.org/10.5281/zenodo.6255206>

GitHub locations:

* EDGE-tree (v1.0.0) <https://github.com/rbouckaert/global-language-tree-pipeline/tree/v1.0.0>
* Grambank (v1.0) <https://github.com/grambank/grambank/tree/v1.0>
* Grambank-analysed (v1.0) <https://github.com/grambank/grambank-analysed/tree/v1.0>
* Glottolog-cldf (v4.5) <https://github.com/glottolog/glottolog-cldf/tree/v4.5>
* AUTOTYP (v1.01) <https://github.com/autotyp/autotyp-data/tree/v1.0.1>

In this project, we fetch the data from the Zenodo locations by downloading a zip file and expanding it. We have also made tables and files available derived from these sources in this repos so that users may run the analysis without engaging with fetching from Zenodo. The scripts that generate these files are also found in this repository and can be run by users if they would like.
