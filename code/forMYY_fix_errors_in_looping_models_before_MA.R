
f <- "./data/0301-MA-input/exp_sub_nature_df_MYY.rds"
exp_sub_nature_df <- readRDS(file = f)
cols_keep <- c('Indicator', 'Tools', 
               'Tool', ## you do not have `Tools` in your data, so I added this
               'nature_type',  'nature_quantity', 'exposure_type')


exp_sub_nature_mod_l <- data.frame()

for (i in 1:20) {
  # print(i)
  nature_mod_id <- paste0("Model.", i,".")
  exp_sub_nature_mod <- exp_sub_nature_df %>%
    dplyr::select(id,
                  any_of(cols_keep),
                  effect_size_indices,`Health.outcome.direction`,
                  starts_with(nature_mod_id) & !contains("measurement", ignore.case = TRUE)) %>% # loop all 20 models but no measurement table(i.e., table 3)
    dplyr::mutate(model_id = i) %>% #Where's the "model_id"? In the exp_sub_nature_mod
    dplyr::select(id, effect_size_indices, model_id, `Health.outcome.direction`, everything()) #Why select twice? To make it in order
  
  ##' remove model id so that they can be rbind (require the same column names)
  colnames(exp_sub_nature_mod) <- sub(nature_mod_id, "", colnames(exp_sub_nature_mod))
  ##' remove model id so that they can be rbind (require the same column names)
  colnames(exp_sub_nature_mod) <- sub(mod_id, "", colnames(exp_sub_nature_mod))
  ##' Remove part of string after "."
  colnames(exp_sub_nature_mod) <- gsub("\\..*","",colnames(exp_sub_nature_mod))
  ##' Repair duplicate names - specify your own name repair function
  exp_sub_nature_mod <- exp_sub_nature_mod %>%
    as_tibble(., .name_repair = make.unique) %>%
    as.data.frame()
  
  ##' bind dataframe for each model
  exp_sub_nature_mod_l <- rbind(exp_sub_nature_mod_l, exp_sub_nature_mod)#Why I don't have the complete variables(809)?
}

rm(exp_sub_nature_mod)
