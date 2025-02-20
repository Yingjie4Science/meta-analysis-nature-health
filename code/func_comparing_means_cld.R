library(agricolae)
library(ggplot2)


##' The use of letters to indicate significant differences in pairwise comparisons is called 
##'   `compact letter display` (CLD), and can simplify the visualization and discussion of 
##'   significant differences among means. We are going to use the `multcompLetters4` function 
##'   from the `multcompView` package. 
##'   The arguments are the object from an aov function and the object from the `TukeyHSD` function.
##'   
##'   
## Example non-parametric data
# data <- data.frame(
#   Group = factor(rep(c("Control", "Experiment 1", "Experiment 2"), each = 10)),
#   Value = c(runif(10, min = 4, max = 6), runif(10, min = 5, max = 7), runif(10, min = 6, max = 8))
# )





func_test_dif <- function(df, 
                          value = 'value', 
                          group = 'group',
                          facet_by = 'ind_sub') {
  # df <- ma_result_each_i
  unique(df$ind_sub)
  
  data_comb <- data.frame()
  
  for (ind in unique(df$ind_sub)) {
    data <- df %>%
      dplyr::rename('Value' = value, 
                    'Group' = group) %>%
      dplyr::filter(ind_sub == ind)
    
    # Perform Kruskal-Wallis test and obtain CLD
    kruskal_result <- kruskal(data$Value, data$Group, group = TRUE)
    cld <- kruskal_result$groups
    
    # Merge CLD with original data
    data$Group <- factor(data$Group, levels = rownames(cld))
    cld$Group <- rownames(cld)
    
    # Combine the original data with CLD information
    data_merged <- merge(data, cld, by = "Group")
    
    data_comb <- rbind(data_comb, data_merged)
  }
  
  
  # Calculate upper quantile (75th percentile) for each category
  data_comb_quantiles <- data_comb %>%
    group_by(ind_sub, Group, groups) %>%
    summarize(Q3 = quantile(Value, 0.75, na.rm = T))  # Extract 75th percentile
  
  
  # Create the plot with ggplot2
  p <- ggplot(data_comb, aes(x = Group, y = Value, fill = Group)) +
    geom_boxplot(show.legend = F) +
    
    geom_text(data = data_comb_quantiles, aes(x = Group, y = Q3, fill = Group, label = groups), 
              vjust=-0.3, hjust = -0.5,  # Adjust hjust to move text slightly right
              color = "gray20") +  
    
    labs(
      # title = "Boxplot with Compact Letter Display (Non-Parametric)", 
      # caption = '*Means not sharing any letter are significantly different',
      x = "", y = "Means") +
    # theme_minimal() +
    # coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  ##' For figure legend: 
  ##' Kruskal-Wallis test was used to detect if there is a significant difference among groups. 
  ##' Post-hoc Dunn’s test with Bonferroni correction was used for pairwise comparisons. 
  ##' Groups that share the same letter (above each boxplot) are not significantly different, while different letters indicate significant differences. 
  ##' The horizontal line inside each box represents the median, and the boxes span the interquartile range (IQR, 25th to 75th percentile). 
  ##' Whiskers extend to 1.5× IQR, and points outside this range are outliers. 
  
  
  if (!is.null(facet_by)) {
    p <- p +
      facet_wrap(~ get(facet_by), 
                 scales = 'free_y',
                 ncol = 5)
  }
  
  return(p)
}