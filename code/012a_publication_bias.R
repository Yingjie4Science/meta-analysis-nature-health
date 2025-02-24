

### Funnel plot for publication bias

# Given our assumptions, and in the case when there is no publication bias, all studies would lie symmetrically around our pooled effect size (the vertical line in the middle), within the form of the funnel. 
# 
# When *publication bias* is present, we would assume that the funnel would look asymmetrical, because only the small studies with a large effect size very published, while small studies without a significant, large effect would be missing.
# 
# We can see in the plot that while some studies have statistically significant effect sizes (the gray areas), others do not (white background). 
# 
# - https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/smallstudyeffects.html
# 
# - https://wviechtb.github.io/metafor/reference/funnel.html


# # Install and load required packages (if not already installed)
# install.packages(c("meta", "metafor", "ggplot2", "puniform"))
library(meta)
library(metafor)
library(ggplot2)
library(puniform)

# Load sample dataset (example effect sizes and standard errors)
# data <- data.frame(
#   study = c("Study1", "Study2", "Study3", "Study4", "Study5", 
#             "Study6", "Study7", "Study8", "Study9", "Study10"),
#   hedges_g = c(0.5, 0.8, 0.3, 0.9, 0.7, 0.6, 0.4, 1.0, 0.2, 0.75), # Effect sizes
#   se = c(0.1, 0.15, 0.12, 0.2, 0.18, 0.14, 0.13, 0.22, 0.11, 0.17)  # Standard errors
# )
# 
# # Conduct a random-effects meta-analysis
# ma_smd <- rma(yi = hedges_g, sei = se, data = data, method = "REML")
# summary(ma_smd)
# 
# exp_sub_mods_md <- read_csv("./data/0301-MA-input/sub_POMS_cleaned.csv", show_col_types = F)
  

# Define fill colors for contour
col.contour = c("gray75", "gray85", "gray95")





## 1. load data
mh_tool <- 'POMS'
fs <- list.files(path = './data/0302-MA-output/', 
                 pattern = paste('^ma_smd', paste(mh_tool, collapse = "_"), '.*.rds', sep = '_'), 
                 full.names = T); 
fs

for (f in fs) {
  
  ind <- basename(f) %>% gsub('ma_smd_|_.rds|_', '', .) %>% gsub(mh_tool, '', .); ind
  
  ma_smd <- readRDS(f)
  
  
  # ========================
  # 1. Funnel Plot (Visual Assessment)
  # ========================
  # Generate Original Funnel Plot
  funnel_original <- funnel(ma_smd, 
                            # contour = c(0.9, 0.95, 0.99),
                            # col.contour = col.contour,
                            main = "Original Funnel Plot", col = "blue")
  
  
  # ========================
  # 2. Egger’s Test (Statistical Test for Funnel Plot Asymmetry)
  # ========================
  
  # egger_test <- regtest(ma_smd, model = "lm")
  # Since meta::metacont() does not directly support Egger’s Test, we extract Hedges' g effect sizes and standard errors:
  # Convert meta::metacont() output to metafor format
  metafor_data <- data.frame(
    yi = ma_smd$TE,  # Extract effect sizes (Hedges' g)
    sei = ma_smd$seTE  # Extract standard errors
  )
  egger_test <- regtest(metafor_data$yi, metafor_data$sei, model = "lm")
  egger_test
  egger_pval <- round(egger_test$pval, 3)  # Extract p-value for annotation
  egger_pval <- ifelse(egger_pval < 0.05, 
                       'p < 0.05 (Sig. publication bias)', 
                       'p > 0.05 (No sig. publication bias)')
  
  # Interpretation:
  # - If p < 0.05 → Significant bias detected
  # - If p > 0.05 → No significant bias
  

  # ========================
  # 3. Trim-and-Fill Method (Correction for Missing Studies)
  # ========================
  trimfill_result <- trimfill(ma_smd)
  # summary(trimfill_result)
  
  # Funnel Plot with Adjusted Effect Sizes
  # Generate Adjusted Funnel Plot (Trim-and-Fill Method)
  funnel_adjusted <- funnel(trimfill_result, main = "Trim-and-Fill Adjusted Funnel Plot", col = "red")
  
  
  # Extract the original and adjusted effect sizes
  original_effect_size <- round(ma_smd$TE.random, 2)
  adjusted_effect_size <- round(trimfill_result$TE.random, 2)
  
  # Extract Key Values for Annotations
  y_upper <- max(ma_smd$seTE, na.rm = T)    # Highest standard error
  y_lower <- min(ma_smd$seTE, na.rm = T)    # Lowest standard error
  
  
  
  # ========================
  # Combine both plots into one image
  # ========================
  f <- paste0(dir.fig, 'publication_bias/Funnel_', mh_tool, '_', ind,  '.png'); f
  png(f, width = 1200, height = 600)  # Save as PNG
  par(mfrow = c(1, 2))  # Arrange plots side by side
  
  funnel(ma_smd, 
         main = "Original Funnel Plot", 
         # contour = c(0.9, 0.95, 0.99),
         # col.contour = col.contour, legend=TRUE,
         col = "blue", cex = 1.5)
  # # Add a legend
  # legend(x = quantile(ma_smd$TE, 0.97, na.rm = T), y = 0.01, legend = c("p < 0.1", "p < 0.05", "p < 0.01"), fill = col.contour)
  mtext(paste(mh_tool, ind, sep = ' - '), side = 3, line = 0.5, adj = 0.01, cex = 1.5)
  mtext(paste("Original Hedges' g =", original_effect_size), side = 3, line = -2, adj = 0.01, cex = 1.2)
  mtext(paste("Egger’s test:", egger_pval), side = 3, line = -4, adj = 0.02, cex = 1.2)
  
  # Add Annotations to the Funnel Lines
  text(original_effect_size, y_upper, labels = "Mean Effect Size", pos = 3, cex = 1.1, col = "black")
  text(original_effect_size - 1, y_upper - 0.05, labels = "95% CI", pos = 3, cex = 1.1, col = "black")
  text(original_effect_size + 1, y_upper - 0.05, labels = "95% CI", pos = 3, cex = 1.1, col = "black")
  
  
  funnel(trimfill_result, 
         main = "Trim-and-Fill Adjusted Funnel Plot",
         col = "red", cex = 1.5)
  legend("topright", legend = c("Original Studies", "Trim-and-Fill Adjusted"), 
         col = "red", 
         pt.bg = c("gray50", 'white'), pch = 21, pt.cex = 1.5, bty = "n", cex = 1.2)
  mtext(paste(mh_tool, ind, sep = ' - '), side = 3, line = 0.5, adj = 0.01, cex = 1.5)
  mtext(paste("Adjusted Hedges' g =", adjusted_effect_size), side = 3, line = -2, adj = 0.01, cex = 1.2)
  
  dev.off()  # Close PNG device
  
  
  
  
  # ========================
  # 6. Report Summary of Publication Bias
  # ========================
  # cat("\n--- Publication Bias Assessment Summary ---\n")
  # cat("Egger’s Test p-value:", round(egger_test$pval, 3), "\n")
  # cat("The Original Effect Size:", round(ma_smd$TE.random, 3), "\n")
  # cat("Trim-and-Fill Adjusted Effect Size:", round(trimfill_result$TE.random, 3), "\n")
}


  








###' Compare the new results to the original ones and include a statement on sensitivity analyses:
###' "To assess publication bias, we first conducted a funnel plot analysis, which showed slight asymmetry (Fig. X). 
###' Egger’s regression test confirmed this observation (p = 0.03), indicating a significant small-study effect. 
###' To adjust for missing studies, we applied Duval and Tweedie’s trim-and-fill method, 
###' which estimated X missing studies and adjusted the pooled effect size from Hedges' g = 0.65 (95% CI: 0.50–0.80) to 0.60 (95% CI: 0.45–0.75). 
###' Overall, while some publication bias was detected, sensitivity analyses suggest our conclusions remain robust."
###' 
###' 
###' 


# funnel(res, ylim=c(0,.08), las=1, digits=list(2L,2), legend=TRUE)
# 
# ## trim and fill method
# funnel(trimfill(res), # , side = 'left'
#        las=1, ylim=c(0,.08), digits=list(2L,2), 
#        # cex=1.2,
#        legend=TRUE)
# 
# 
# f <- paste0('./figures/', 'funnel_cor_', paste(MH_tool_o1_list, collapse = "_"), '_', today ,'.png'); f
# png(file=f, 
#     width = 1000, height = 1000, units = "px", pointsize = 22) 
# funnel(
#   # trimfill(res, side = 'right'),
#   res,
#   las=1, ylim=c(0,.08), digits=list(2L,2),
#   level=c(.10, .05, .01),
#   shade=c("white", "gray50", "gray65"), ## pink -- not significant
#   legend=TRUE,
#   back="grey90",
#   hlines=NULL)
# dev.off() 
