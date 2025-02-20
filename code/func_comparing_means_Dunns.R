library(rstatix)
library(ggpubr)


func_test_dif_dunn1 <- function(df, 
                          value = 'value', 
                          group = 'group',
                          facet_by = 'ind_sub') {
  # df <- ma_result_each_i
  unique(df$ind_sub)
  
  data_comb <- data.frame()
  test_comb <- data.frame()
  
  for (ind in unique(df$ind_sub)) {
    data <- df %>%
      dplyr::rename('Value' = value, 
                    'Group' = group) %>%
      dplyr::filter(ind_sub == ind)
    
    # Perform Kruskal-Wallis test and obtain CLD
    pwc <- data %>%
      dunn_test(Value ~ Group, p.adjust.method = "bonferroni")
    pwc$ind_sub = ind
    pwc <- pwc %>% 
      rstatix::add_xy_position(x = "group") %>%
      mutate(xmin = group1, xmax = group2)  # Manually assign x positions
    
    res.kruskal <- data %>% 
      kruskal_test(Value ~ Group) %>%
      mutate(ind_sub = ind)
    
    # Merge CLD with original data
    data$Group <- factor(data$Group, levels = rownames(cld))
    
    data_comb <- rbind(data_comb, data)
    test_comb <- rbind(test_comb, pwc)
  }
  

  # Create the plot with ggplot2
  # Visualization: box plots with p-values

  if (!is.null(facet_by)) {
    
    p <- 
      ggboxplot(data_comb, x = "Group", y = "Value", fill = 'Group', 
                ggtheme = theme_bw()) +
      stat_pvalue_manual(test_comb, hide.ns = TRUE) +
      # labs(subtitle = get_test_label(res.kruskal, detailed = TRUE),
      #      caption = get_pwc_label(test_comb)) +
      facet_wrap(~ get(facet_by), scales = 'free_y', ncol = 5) + 
      theme(legend.position="none")
      
  } else {
    p <- 
      ggboxplot(data_comb, x = "Group", y = "Value", fill = 'Group', 
                ggtheme = theme_bw()) +
      stat_pvalue_manual(test_comb, hide.ns = TRUE) +
      theme(legend.position="none")
  }
  
  return(p)
}




## ---------------------------------------------------------------------------------------
library(dplyr)
library(ggpubr)
library(rstatix)
library(rlang)



func_test_dif_dunn <- function(
    df, 
    value = 'value', 
    group = 'group',
    facet_by = 'ind_sub') {
  
  # Step 1: Perform Dunn's test for each group (facet)
  df <- df %>%
    dplyr::rename('Value' = value,
                  'Group' = group)
  
  # Step 1: Perform Dunn's test for each facet
  dunn_results <- df %>%
    group_by(ind_sub) %>%
    dunn_test(Value ~ Group, p.adjust.method = "bonferroni")
  
  # Step 2: Ensure `xmin` and `xmax` are correctly assigned
  dunn_results <- dunn_results %>%
    add_xy_position(x = "group")  %>% # Add positioning for plotting
    mutate(xmin = group1, xmax = group2)  # Manually assign x positions
  
  
  
  # Step 3: Create a faceted boxplot with Dunn’s test results
  p <- ggboxplot(df, x = "Group", y = "Value", fill = "Group") +
    stat_pvalue_manual(dunn_results, hide.ns = TRUE) +  # Add p-values
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +  # Add 10% space at the top
    facet_wrap(~ ~ get(facet_by), scales = "free_y", ncol = 5) +  # Facet by group
    theme_bw() +
    labs(x = "", y = "Means") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(legend.position = "none")
  return(p)
}



## test code -----------------------------------------------------------------------------
# ## alternatively, this also produce the same results
# ## https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# # library(rstatix)
# # library(ggpubr)
# # 
# data <- ma_result_each_i %>%
#       dplyr::rename('Value' = 'SMD',
#                     'Group' = 'subgroup') %>%
#       dplyr::filter(ind_sub == 'Vitality')
# 
# # Pairwise comparisons
# pwc <- data %>%
#   dunn_test(Value ~ Group, p.adjust.method = "bonferroni")
# pwc
# 
# pwc2 <- data %>%
#   wilcox_test(Value ~ Group, p.adjust.method = "bonferroni")
# pwc2
# 
# 
# # Visualization: box plots with p-values
# pwc <- pwc %>% add_xy_position(x = "Group")
# res.kruskal <- data %>% kruskal_test(Value ~ Group)
# 
# ggboxplot(data, x = "Group", y = "Value", fill = 'Group', 
#           ggtheme = theme_bw()) +
#   stat_pvalue_manual(pwc, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(res.kruskal, detailed = TRUE),
#     caption = get_pwc_label(pwc)
#     ) +
#   theme(legend.position="none")