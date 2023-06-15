#combining tables from brms analysis outputs

#NC model comparison
loo <- read.csv("output_tables/loo_comparison.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "LOO-CV") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

loo_mm <- read.csv("output_tables/loo_comparison_mm.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "LOO with moment matching") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

kfold <- read.csv("output_tables/kfold_comparison.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "K-fold CV") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

comparison_NC <- as.data.frame(rbind(loo, loo_mm, kfold)) %>% 
  mutate(`Response variable` = "Case") %>% 
  relocate(`Response variable`, .after = "Model")

#Vf model comparison
loo <- read.csv("output_tables/loo_comparison_Vf.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "LOO-CV") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

loo_mm <- read.csv("output_tables/loo_comparison_mm_Vf.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "LOO with moment matching") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

kfold <- read.csv("output_tables/kfold_comparison_Vf.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "K-fold CV") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

comparison_Vf <- as.data.frame(rbind(loo, loo_mm, kfold)) %>% 
  mutate(`Response variable` = "Verb-final word order") %>% 
  relocate(`Response variable`, .after = "Model")

#FWO model comparison
loo <- read.csv("output_tables/loo_comparison_FWO.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "LOO-CV") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

loo_mm <- read.csv("output_tables/loo_comparison_mm_FWO.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "LOO with moment matching") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

kfold <- read.csv("output_tables/kfold_comparison_FWO.csv") %>%
  rename(Model = "X") %>%
  mutate(`Cross-validation type` = "K-fold CV") %>%
  mutate_if(is.numeric, round, digits=2) %>%
  relocate(`Cross-validation type`, .after=se_diff) %>%
  dplyr::select(1:4)

comparison_FWO <- as.data.frame(rbind(loo, loo_mm, kfold)) %>% 
  mutate(`Response variable` = "Flexible word order") %>% 
  relocate(`Response variable`, .after = "Model")

comparison <- as.data.frame(rbind(comparison_NC, comparison_Vf, comparison_FWO))

comparison <- comparison %>%
  mutate(Model = 
           recode(Model,
                "model1" = "phylogenetic",
                "model2" = "spatial",
                "model3" = "spatiophylogenetic"))

write.csv(comparison, "output_tables/brms_models_comparison.csv")

comparison_doc <- comparison %>%
  as.data.frame() %>%
  flextable() %>%
  autofit() %>%
  hline(part="all") %>%
  vline(part="all") %>%
  merge_v(j=c("Cross-validation type", "Response variable")) %>%
  fix_border_issues()

save_as_docx(
  "Comparison of brms models" = comparison_doc, 
  path = "output/Table_brms_models_comparison.docx")
