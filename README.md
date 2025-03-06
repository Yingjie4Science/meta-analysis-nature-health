![Visitor Badge](https://visitor-badge.laobi.icu/badge?page_id=yingjie4science.meta-analysis-nature-health)

# meta-analysis

## System requirements

The R scripts were prepared using R version 4.3.2 on Windows 11 Pro.

The following publicly available R packages were used in this analysis:

*This project does not involve any non-standard hardware.*

```         
##' Installation
install.packages("readr")
install.packages("sf")
install.packages("meta")
install.packages("ggplot2")
install.packages("ggpubr")


##' Load R packages
library(readr)
library(sf)
library(meta)     # v7.0.0
library(ggplot2)
library(ggpubr)
```

## Data

All the data that support the findings of this study are publicly available. The complete list of studies included in the systematic review is saved as a CSV file in the `data` folder.

```         
 data
  |-- included_papers_for_systematic_review.csv  ## for systematic review, 466 papers
  |-- included_papers_for_MA.csv                 ## papers for meta-analysis, 78 papers
  |-- 
  |-- ...
  
```

## Code

All analysis code are deposited in the `code` folder.

*Users can refer to the detailed annotations in the main scripts below to execute the code.*

```         
000_load_Extraction.Rmd               # Load raw data, save paper list for sys review
001_mini-review.Rmd                   # Systematic review analysis
009_Quality_Assessment.RMD            # Quality assessment
010_MA_data.Rmd                       # Format data for meta-analysis
012_MA_MD.Rmd                         # Perform meta-analysis
012a_publication_bias.R               # Publication_bias analysis
013b_ma_sensitivity.Rmd               # Sensitivity analysis
020_viz_effect_size_overall.Rmd       # Visualize results

```

## Contact

yingjieli DOT edu AT gmail DOT com
