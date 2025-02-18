
source('./code/func_remove_duplicates.R')

func_clean_design <- function(data, column_name, aggregate = F) {
  d <- data %>%
    dplyr::mutate(
      !!column_name := gsub("Other: ", "", !!sym(column_name)),
      ## remove text within parenthesis 
      !!column_name := gsub("\\s*\\([^\\)]+\\)", "", !!sym(column_name)), 
      

      !!column_name := gsub(",+", ",", !!sym(column_name)), ## Removing repeated punctuation character from a string
      !!column_name := gsub(";+", ";", !!sym(column_name)), ## Removing repeated punctuation character from a string
      !!column_name := trimws(!!sym(column_name)),
      !!column_name := str_squish(!!sym(column_name))
      
    ) %>% 
    # dplyr::filter(!!sym(column_name) != 'NA') %>%
    as.data.frame()
  

  if (aggregate == F) {
    d <- d

    ## to aggregate the study design
  } else {
    d <- d %>%
      dplyr::mutate(

        # !!column_name := gsub("\\- Green roof/wall|\\- Shrub\\/scrub", "", !!sym(column_name)),
        !!column_name := case_when(
          !!sym(column_name) %in% c('Cross sectional study', 'Longitudinal cohort study', 'Longitudinal panel study') ~ "Observational",
          # !!sym(column_name) %in% c('Randomised controlled trial', 
          #                           'Randomised controlled trial (RCT) - parallel',
          #                           'RCT - crossover',
          #                           'Non-randomised experimental study',
          #                           'Uncontrolled before-after study', 
          #                           'experiment', ## ?? these inputs should be corrected 
          #                           'Quasi-experimental study') ~ "Experimental",
          str_detect(!!sym(column_name), regex("controlled trial|RCT|Uncontrolled before|experiment|experimental", ignore_case = TRUE)) ~ "Experimental",
          TRUE ~ !!sym(column_name)),

        !!column_name := trimws(!!sym(column_name)),
        !!column_name := str_squish(!!sym(column_name)),

        ## to remove duplicates within each cell (e.g., multiple "Greenspace" in a cell)
        !!column_name := sapply(!!sym(column_name), remove_duplicates)
      ) %>%
      as.data.frame()
  }
  
  
  return(d)
}

