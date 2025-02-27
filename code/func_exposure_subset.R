


###' subset papers that examined `exposure_type_i`
###' 

func_exp_subset <- function(df, exposure_type_i, mh_tool) {
  
  ##' Filter 1. get paper id based on selected exposure type; studies will be filtered out 
  ##'     if the `exposure_type` column is NA
  exp_sub <- df_exp_l %>%
    dplyr::filter(exposure_type %in% exposure_type_i)
  exp_sub_id <- unique(exp_sub$id) %>% sort()
  
  ##' Filter 2. further subset based on selected tools
  exp_sub_df <- df %>%
    dplyr::filter(id %in% exp_sub_id) %>%
    dplyr::filter(stringr::str_detect(Tools, paste(mh_tool, collapse = "|"))) %>%
    
    ## subset cols -----------------------------------------------------------------------
    dplyr::select(id:`Effect size indices`, effect_size_indices, 
                everything()) %>%
    dplyr::select(id:Country, 
                  any_of(cols_keep), 
                  n_participants, buffers, buffers_unit,
                  effect_size_indices:ncol(.)) %>%
    dplyr::select(-c('Title', 'Reviewer_id', 'meet_criteria')) %>% ## keep "Reviewer" to match data from gsheet
    dplyr::select(-contains(c('Please specify the tables', 'Additional comments'))) %>%
    as.data.frame()
  
  return(exp_sub_df)
  
}
