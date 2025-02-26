
#' Clean messy data in `Country` and `City`
#' @description Clean messy data
#'
#' @usage func_clean_country(data, column_name = "Country")
#' 
#' @param data          Data frame as the input
#' @param column_name   The target column for cleaning, in a string format
#' 

library(dplyr)
library(stringr)

func_clean_country <- function(data, column_name = "Country") {
  d <- data %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      ## clean and correct country names
      !!column_name := gsub('The United Kingdom', 'United Kingdom', !!sym(column_name)), 
      !!column_name := case_when(
        str_detect(string = !!sym(column_name), pattern = 'Scotland|UK|England|United kingdom') ~ 'United Kingdom',
        str_detect(string = !!sym(column_name), pattern = 'Indonasia') ~ 'Indonesia',
        str_detect(string = !!sym(column_name), pattern = 'Malasia') ~ 'Malaysia',
        str_detect(string = !!sym(column_name), pattern = 'The Netherlands') ~ 'Netherlands',
        str_detect(string = !!sym(column_name), pattern = 'Danmark') ~ 'Denmark',
        str_detect(string = !!sym(column_name), pattern = 'Beigium') ~ 'Belgium',
        str_detect(string = !!sym(column_name), pattern = 'Brasil') ~ 'Brazil',
        str_detect(string = !!sym(column_name), pattern = 'Chili') ~ 'Chile',
        str_detect(string = !!sym(column_name), pattern = 'Czech') ~ 'Czechia',
        str_detect(string = !!sym(column_name), pattern = 'Virginia|United States') ~ 'USA',
        # 
        T ~ !!sym(column_name))
    ) %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := trimws(!!sym(column_name))) %>%
    as.data.frame()
  return(d)
}



func_clean_city <- function(data, column_name = "City") {
  d <- data %>%
    dplyr::mutate(!!column_name := case_when(
      !!sym(column_name) %in% c('m', NA) ~ NA,
      T ~ !!sym(column_name))
      ) %>%
    dplyr::mutate(
      !!column_name := str_squish(!!sym(column_name)),
      !!column_name := trimws(!!sym(column_name))) %>%
    as.data.frame()
  return(d)
}



## add region ----------------------------------------------------------------------------
library(SDGdetector)
packageVersion('SDGdetector')

data(countryRegions,envir=environment(),package="rworldmap")

# library(countrycode) 
# # names(codelist)
# nation.list <- codelist %>%
#   select(country.name.en, iso.name.en, un.name.en, cldr.name.en, iso2c, iso3c)


## 1. World region data 1
codelist.supp <- data.frame(
  country.name.en = c('USA'),
  iso3c = c('USA'),
  region = c('North America')
)
codelist <- SDGdetector::codelist_panel %>%
  dplyr::distinct(country.name.en, iso3c, region) %>%
  rbind(., codelist.supp) %>%
  dplyr::mutate(
    country.name.en = case_when(
      str_detect(string = iso3c, pattern = 'HKG') ~ 'Hong Kong',
      T ~ country.name.en)
  ) %>%
  dplyr::distinct(country.name.en, iso3c, region)



## 2. World region data 2
getwd()

f = list.files(path = paste0(here::here(), '/data/'), pattern = '*our_world_in_data.xlsx', full.names = T, include.dirs = T); f
# region_owid <- read_csv(file = f) #%>%
region_owid <- readxl::read_excel(path = f) %>%
  dplyr::select(-Year) %>%
  dplyr::mutate(Code = gsub('OWID_', '', Code)) %>%
  dplyr::rename('name_owid' = 'Entity',
                'ISO3' = 'Code') %>%
  as.data.frame()


## 3. World region data combined 
countryCode_region <- countryRegions %>%
  dplyr::mutate(REGION = ifelse(REGION=='Australia', 'Oceania', REGION)) %>%
  left_join(., region_owid, by = 'ISO3') %>%
  dplyr::select(1:REGION, Continent, continent, everything())


func_add_region <- function(data) {
  d <- data %>%
    merge(x = ., 
          y = codelist, 
          by.x = 'Country', 
          by.y = 'country.name.en', all.x = T) %>%
    ## add more detailed region name info
    merge(x = ., 
          y = countryCode_region %>%
            dplyr::select(ISO3, REGION, continent), 
          by.x = 'iso3c', 
          by.y = 'ISO3', all.x = T) %>%
    ## remove `region`, and rename `REGION` to `Region`
    dplyr::select(-region) %>%
    dplyr::rename('Region' = 'REGION') %>%
    arrange(!is.na(iso3c))
  
  return(d)
}
