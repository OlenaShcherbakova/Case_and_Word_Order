load("output_models/model_b.RData")
load("output_models/model_c.RData")
load("output_models/model_d.RData")

summary(model_b)
summary(model_c)
summary(model_d)

fixed_b <- fixef(model_b) %>% 
  as.data.frame() %>% 
  dplyr::mutate(model = "model b") %>% 
  dplyr::select(model, everything()) %>% 
  rownames_to_column(var = "Parameters") %>% 
  filter(!str_detect(Parameters, "_Intercept")) %>% 
  dplyr::mutate(across(where(is.numeric), ~round(., 2)))

fixed_c <- fixef(model_c) %>% 
  as.data.frame() %>% 
  dplyr::mutate(model = "model c") %>% 
  dplyr::select(model, everything()) %>% 
  rownames_to_column(var = "Parameters") %>% 
  filter(!str_detect(Parameters, "_Intercept")) %>% 
  dplyr::mutate(across(where(is.numeric), ~round(., 2)))

fixed_d <- fixef(model_d) %>% 
  as.data.frame() %>% 
  dplyr::mutate(model = "model d") %>% 
  dplyr::select(model, everything()) %>% 
  rownames_to_column(var = "Parameters") %>% 
  filter(!str_detect(Parameters, "_Intercept")) %>% 
  dplyr::mutate(across(where(is.numeric), ~round(., 2)))

fixed_effs <- rbind(fixed_b, fixed_c, fixed_d) 

fixed_effs <- fixed_effs %>% 
  rename(Model = model) %>% 
  separate(Parameters, into = c("Response", "Predictor"), 
           sep = "_", remove = FALSE, fill = "right") %>%
  dplyr::select(-Parameters) %>% 
  dplyr::mutate(Response = case_when(
    Response == "Freewordorder" ~ "Flexible word order",
    Response == "Verbfinal" ~ "Verb-final",
    Response == "Nominalcase" ~ "Case",
    TRUE ~ Response
  )) %>% 
  dplyr::mutate(Predictor = case_when(
    Predictor == "Free" ~ "Flexible word order",
    Predictor == "Nominal" ~ "Case",
    Predictor == "Verb" ~ "Verb-final",
    TRUE ~ Response
  )) 

write.csv(fixed_effs, "output_tables/brms_models_fixed_effects.csv")


fixed_effs_doc <- fixed_effs %>%
  as.data.frame() %>%
  flextable() %>%
  autofit() %>%
  hline(part="all") %>%
  vline(part="all") %>%
  merge_v(j=c("Model")) %>%
  fix_border_issues()

save_as_docx(
  "Fixed effects" = fixed_effs_doc, 
  path = "output_tables/brms_models_fixed_effects.docx")


