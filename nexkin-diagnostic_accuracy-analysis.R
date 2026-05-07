# ========================================================================
# DIAGNOSTIC PERFORMANCE ANALYSIS
# analysis of Histamine, Codeine (sensitivity) & NaCl (specificity)
# Author: Elnaz Arjmand
# Date: September 2025
# ========================================================================

# ========================================================================
# Section 1. Setup and Load Required Libraries
# ========================================================================

# Load required libraries
required_packages <- c("readxl", "dplyr", "ggplot2", "pROC", "epiR", "binom", 
                       "caret", "gridExtra", "tidyr", "viridis", "openxlsx", 
                       "stringr", "scales", "cowplot", "ggpubr", "RColorBrewer")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
for(pkg in required_packages) {
  library(pkg, character.only = TRUE)
}

# Set working directory
data_folder <- "C:/Users/elnaz.arjmand/OneDrive - sitem-insel AG/General/06 Clinical Trial Inselspital Nexkin UBERN/ClinicalTrialStartSept2024/Clinical Trial Data/Preliminary Analysis/Raw Data/All_Folders/01_mydata/02_edidata"
setwd(data_folder)

path <- "C:/Users/elnaz.arjmand/OneDrive - sitem-insel AG/General/06 Clinical Trial Inselspital Nexkin UBERN/ClinicalTrialStartSept2024/Clinical Trial Data/Preliminary Analysis/Raw Data/All_Folders/01_mydata/02_edidata/status_combined.xlsx"

file.exists(path)           # must be TRUE
file.info(path)[, c("size","isdir")]

cat("Working directory set to:", getwd(), "\n")
cat("Starting Complete Integrated Diagnostic Performance Analysis\n")
cat("========================================================================\n")

# ========================================================================
# Section 2. Load and Prepare Data from Combined Excel File
# ========================================================================

file_path <- "status_combined.xlsx"

# Check file existence and load sheets
if (!file.exists(file_path)) {
  stop("Cannot find status_combined.xlsx file. Please check file location.")
}

# Load all three sheets
tryCatch({
  sheet_names <- excel_sheets(file_path)
  cat("Available sheets:", paste(sheet_names, collapse = ", "), "\n")
  
  histamine_data <- read_excel(file_path, sheet = "histamine")
  codeine_data <- read_excel(file_path, sheet = "codeine")
  nacl_data <- read_excel(file_path, sheet = "NaCl")
  
  cat("Successfully loaded all three datasets\n")
  cat("Histamine:", nrow(histamine_data), "patients\n")
  cat("Codeine:", nrow(codeine_data), "patients\n") 
  cat("NaCl:", nrow(nacl_data), "patients\n")
}, error = function(e) {
  stop("Error loading data: ", e$message)
})

# ========================================================================
# Section 3. Unified Exclusion Logic for All Three Substances
# ========================================================================

# Histamine/Codeine exclusion logic (they have exclude_patient column)
apply_exclusion_logic <- function(df, substance_type) {
  cat("\n=== APPLYING EXCLUSION LOGIC FOR", toupper(substance_type), "===\n")
  
  # Check if exclude_patient column exists, if not create it
  if(!"exclude_patient" %in% names(df)) {
    cat("Creating exclude_patient column for", substance_type, "\n")
    df$exclude_patient <- FALSE
  }
  
  df <- df %>%
    mutate(
      exclude_patient = case_when(
        exclude_patient == TRUE ~ TRUE,
        status == "excluded" ~ TRUE,
        exclusion_reason == "Failure_Uninterpretable" ~ TRUE,
        exclusion_reason == "Non-wheal skin feature" ~ TRUE,
        TRUE ~ exclude_patient  # Keep existing values
      ),
      detailed_exclusion_reason = case_when(
        status == "excluded" ~ "Patient excluded",
        exclusion_reason == "Failure_Uninterpretable" ~ "Failure_Uninterpretable",
        exclusion_reason == "Non-wheal skin feature" ~ "Non-wheal skin feature",
        exclusion_reason == "Included in analysis" ~ "Included in analysis",
        !exclude_patient ~ "Included in analysis",
        TRUE ~ "Other"
      )
    )
  
  exclusion_summary <- table(df$detailed_exclusion_reason, useNA = "ifany")
  cat("Exclusion summary:\n")
  print(exclusion_summary)
  
  return(df)
}

# NaCl exclusion logic
apply_nacl_exclusion_logic <- function(df) {
  
  cat("\n=== APPLYING EXCLUSION LOGIC FOR NACL ===\n")
  
  # Check what columns exist in NaCl data
  cat("Available columns in NaCl data:", paste(names(df), collapse = ", "), "\n")
  
  # Initialize exclude_patient column if it doesn't exist
  if(!"exclude_patient" %in% names(df)) {
    cat("Creating exclude_patient column for NaCl\n")
    df$exclude_patient <- FALSE
  }
  
  # Check if withdrawn column exists
  if(!"withdrawn" %in% names(df)) {
    cat("withdrawn column not found, assuming no withdrawn patients\n")
    df$withdrawn <- FALSE
  }
  
  df <- df %>%
    mutate(
      # Apply exclusion logic based on available columns
      exclude_patient = case_when(
        exclude_patient == TRUE ~ TRUE,
        !is.na(exclusion_reason) & exclusion_reason == "Failure_Uninterpretable" ~ TRUE,
        !is.na(exclusion_reason) & exclusion_reason == "Non-wheal skin feature" ~ TRUE,
        !is.na(exclusion_reason) & exclusion_reason == "Misalignment" ~ TRUE,
        withdrawn == TRUE ~ TRUE,
        !is.na(status) & status == "excluded" ~ TRUE,
        TRUE ~ FALSE
      ),
      
      # Create detailed exclusion reason
      detailed_exclusion_reason = case_when(
        exclude_patient == TRUE & withdrawn == TRUE ~ "Patient withdrawn",
        !is.na(exclusion_reason) & exclusion_reason == "Failure_Uninterpretable" ~ "Failure_Uninterpretable", 
        !is.na(exclusion_reason) & exclusion_reason == "Non-wheal skin feature" ~ "Non-wheal skin feature",
        !is.na(exclusion_reason) & exclusion_reason == "Misalignment" ~ "Misalignment",
        exclude_patient == TRUE ~ "Patient excluded",
        TRUE ~ "Included in analysis"
      )
    )
  
  # Show exclusion summary
  exclusion_summary <- table(df$detailed_exclusion_reason, useNA = "ifany")
  cat("NaCl Exclusion summary:\n")
  print(exclusion_summary)
  
  return(df)
}

# Apply exclusion logic
histamine_data <- apply_exclusion_logic(histamine_data, "HISTAMINE")
codeine_data <- apply_exclusion_logic(codeine_data, "CODEINE")
nacl_data <- apply_nacl_exclusion_logic(nacl_data)

# Create clean datasets
histamine_clean <- histamine_data %>% filter(!exclude_patient)
codeine_clean <- codeine_data %>% filter(!exclude_patient)
nacl_clean <- nacl_data %>% filter(!exclude_patient)

cat("\nSUMMARY AFTER EXCLUSIONS:\n")
cat("HISTAMINE - Total:", nrow(histamine_data), "| Included:", nrow(histamine_clean), "\n")
cat("CODEINE - Total:", nrow(codeine_data), "| Included:", nrow(codeine_clean), "\n")
cat("NACL - Total:", nrow(nacl_data), "| Included:", nrow(nacl_clean), "\n")

# ========================================================================
# Section 4. Classification Logic for All Substances
# ========================================================================

# Histamine/Codeine classification
classify_substance <- function(df, substance_type, manual_col, device_col, opt_col, manual_threshold = 3.5, device_threshold = 3) {
  cat("\n===", toupper(substance_type), "CLASSIFICATION ===\n")
  
  df <- df %>%
    mutate(
      manual_clean = ifelse(is.na(.data[[manual_col]]), 0, .data[[manual_col]]),
      device_clean = ifelse(is.na(.data[[device_col]]), 0, .data[[device_col]]),
      manual_positive = !is.na(.data[[manual_col]]) & .data[[manual_col]] >= manual_threshold,
      device_positive = !is.na(.data[[device_col]]) & .data[[device_col]] >= device_threshold,
      
      classification = if(opt_col %in% names(df)) {
        case_when(
          manual_positive & device_positive & 
            (is.na(.data[[opt_col]]) | .data[[opt_col]] == "" | .data[[opt_col]] == "A" | 
               str_detect(.data[[opt_col]], "A-Papule identified automatically")) ~ "TP",
          manual_positive & 
            (!device_positive | 
               str_detect(.data[[opt_col]], "B-Papule") | 
               str_detect(.data[[opt_col]], "D-Papule") |
               (is.na(.data[[device_col]]) & !is.na(.data[[manual_col]]))) ~ "FN",
          (!manual_positive | is.na(.data[[manual_col]])) & device_positive & 
            (is.na(.data[[opt_col]]) | .data[[opt_col]] == "" | .data[[opt_col]] == "A" | 
               str_detect(.data[[opt_col]], "A-Papule identified automatically")) ~ "FP",
          (!manual_positive | is.na(.data[[manual_col]])) & (!device_positive | is.na(.data[[device_col]])) ~ "TN",
          TRUE ~ "TN"
        )
      } else {
        case_when(
          manual_positive & device_positive ~ "TP",
          manual_positive & !device_positive ~ "FN",
          !manual_positive & device_positive ~ "FP",
          !manual_positive & !device_positive ~ "TN",
          TRUE ~ "TN"
        )
      }
    )
  
  # Show classification summary
  class_summary <- table(df$classification, useNA = "ifany")
  cat("Classification Summary:\n")
  print(class_summary)
  
  return(df)
}

# NaCl classification (specificity only)
classify_nacl_specificity <- function(df, manual_col, device_col, opt_col, manual_threshold = 3.5, device_threshold = 3) {
  cat("\n=== NACL SPECIFICITY CLASSIFICATION ===\n")
  
  df <- df %>%
    mutate(
      manual_clean = ifelse(is.na(.data[[manual_col]]), 0, .data[[manual_col]]),
      device_clean = ifelse(is.na(.data[[device_col]]), 0, .data[[device_col]]),
      manual_positive = !is.na(.data[[manual_col]]) & .data[[manual_col]] >= manual_threshold,
      device_positive = !is.na(.data[[device_col]]) & .data[[device_col]] >= device_threshold,
      
      classification = if(opt_col %in% names(df)) {
        case_when(
          str_detect(tolower(as.character(.data[[opt_col]])), "misalignment") ~ "EXCLUDE_MISALIGNMENT",
          !device_positive ~ "TN",
          device_positive & 
            (is.na(.data[[opt_col]]) | .data[[opt_col]] == "" | .data[[opt_col]] == "A" | 
               str_detect(as.character(.data[[opt_col]]), "A-Papule identified automatically")) ~ "FP",
          device_positive & 
            (str_detect(as.character(.data[[opt_col]]), "B-Papule") | 
               str_detect(as.character(.data[[opt_col]]), "D-Papule")) ~ "EXCLUDE_MISALIGNMENT",
          TRUE ~ "TN"
        )
      } else {
        case_when(
          !device_positive ~ "TN",
          device_positive ~ "FP",
          TRUE ~ "TN"
        )
      }
    )
  
  class_summary <- table(df$classification, useNA = "ifany")
  cat("NaCl Classification Summary:\n")
  print(class_summary)
  
  return(df)
}

# Apply classifications
histamine_classified <- classify_substance(histamine_clean, "HISTAMINE", "Hist_M", "Hist_D", "opt_Hist")
codeine_classified <- classify_substance(codeine_clean, "CODEINE", "Cod_M", "Cod_D", "opt_Cod")
nacl_classified <- classify_nacl_specificity(nacl_clean, "NaCl_M", "NaCl_D", "opt_NaCl")




# ========================================================================
# STEP 4.AB (rename output table labels before plotting)
# ========================================================================

comparison_results <- data.frame(
  Substance = rep(c("Histamine", "Codeine", "NaCl"), each = 2),
  Classification = rep(c("Automatic", "Operator-Assisted"), 3),
  TP = c(histamine_A$TP, histamine_AB$TP, codeine_A$TP, codeine_AB$TP, 
         nacl_A$TP, nacl_AB$TP),
  FN = c(histamine_A$FN, histamine_AB$FN, codeine_A$FN, codeine_AB$FN,
         nacl_A$FN, nacl_AB$FN),
  FP = c(histamine_A$FP, histamine_AB$FP, codeine_A$FP, codeine_AB$FP,
         nacl_A$FP, nacl_AB$FP),
  TN = c(histamine_A$TN, histamine_AB$TN, codeine_A$TN, codeine_AB$TN,
         nacl_A$TN, nacl_AB$TN),
  Sensitivity = sprintf("%.2f%%", c(histamine_A$sensitivity, histamine_AB$sensitivity,
                                    codeine_A$sensitivity, codeine_AB$sensitivity, 
                                    NA, NA)*100),
  Specificity = sprintf("%.2f%%", c(histamine_A$specificity, histamine_AB$specificity,
                                    codeine_A$specificity, codeine_AB$specificity,
                                    nacl_A$specificity, nacl_AB$specificity)*100),
  stringsAsFactors = FALSE
)
print(comparison_results)

# ========================================================================
# STEP 6: Publication-Quality Visualization (Plots pane)
# ========================================================================

par(mfrow = c(2, 2), mar = c(5, 5, 4, 2), oma = c(0, 0, 2, 0))

color_TP <- "#F08080"  # TP/FN series
color_FP <- "#ADD8E6"  # FP/TN series

# helper to draw a panel and place labels smartly (inside if tall, above if short)
smart_barplot <- function(mat, main_title, sens_value) {
  ymax <- max(mat)
  pad  <- max(3, ceiling(ymax * 0.06))         # space for labels above short bars
  bp <- barplot(mat, beside = TRUE, col = c(color_TP, color_FP),
                main = main_title, ylab = "Number of Cases",
                ylim = c(0, ymax + pad * 2),
                names.arg = c("Device+", "Device-"),
                cex.main = 1.2, cex.lab = 1.1)
  
  h  <- as.numeric(mat)      # heights in column-wise order
  x  <- c(bp)                # matching x-positions (same order)
  y  <- ifelse(h >= 20, h/2, h + pad)  # inside for tall bars, above for small bars
  text(x, y, labels = h, font = 2, cex = 1.2)
  
  mtext(sprintf("Sensitivity: %.1f%%", sens_value * 100),
        side = 3, line = 0.5, cex = 0.9, font = 2)
  legend("topright", legend = c("TP / FN", "FP / TN"),
         fill = c(color_TP, color_FP), bty = "n", cex = 0.9)
}

# Panel 1: Histamine – Automatic
hist_A_matrix <- matrix(c(histamine_A$TP, histamine_A$FN,
                          histamine_A$FP, histamine_A$TN),
                        nrow = 2, byrow = TRUE)
smart_barplot(hist_A_matrix, "Histamine: Automatic Detection", histamine_A$sensitivity)

# Panel 2: Histamine – Operator-Assisted
hist_AB_matrix <- matrix(c(histamine_AB$TP, histamine_AB$FN,
                           histamine_AB$FP, histamine_AB$TN),
                         nrow = 2, byrow = TRUE)
smart_barplot(hist_AB_matrix, "Histamine: Operator-Assisted Detection", histamine_AB$sensitivity)

# Panel 3: Codeine – Automatic
cod_A_matrix <- matrix(c(codeine_A$TP, codeine_A$FN,
                         codeine_A$FP, codeine_A$TN),
                       nrow = 2, byrow = TRUE)
smart_barplot(cod_A_matrix, "Codeine: Automatic Detection", codeine_A$sensitivity)

# Panel 4: Codeine – Operator-Assisted
cod_AB_matrix <- matrix(c(codeine_AB$TP, codeine_AB$FN,
                          codeine_AB$FP, codeine_AB$TN),
                        nrow = 2, byrow = TRUE)
smart_barplot(cod_AB_matrix, "Codeine: Operator-Assisted Detection", codeine_AB$sensitivity)

mtext("Diagnostic Performance: Automatic vs. Operator-Assisted Detection",
      outer = TRUE, cex = 1.3, font = 2, line = 0.5)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2), oma = c(0, 0, 0, 0))


fig <- recordPlot()

dir.create("figures", showWarnings = FALSE)
tiff("figures/Fig_Auto_vs_Assisted.tiff",
     width = 7.2, height = 5.0, units = "in",
     res = 600, compression = "lzw")
replayPlot(fig)
dev.off()
png("figures/Fig_Auto_vs_Assisted.png",
    width = 7.2, height = 5.0, units = "in", res = 600)
replayPlot(fig)
dev.off()

if (!requireNamespace("devEMF", quietly = TRUE)) install.packages("devEMF")
devEMF::emf("figures/Fig_Auto_vs_Assisted.emf",
            width = 7.2, height = 5.0, emfPlus = TRUE)
replayPlot(fig)
dev.off()
pdf("figures/Fig_Auto_vs_Assisted.pdf", width = 7.2, height = 5.0)
replayPlot(fig)
dev.off()









# ========================================================================
# Section 5. Calculate Diagnostic Metrics for All Substances
# ========================================================================

# Diagnostic metrics calculation
calculate_diagnostic_metrics <- function(df, is_negative_control = FALSE) {
  
  if(is_negative_control) {
    # For NaCl (negative control) - only TN and FP
    df_analysis <- df %>% filter(classification %in% c("TN", "FP"))
    contingency <- table(df_analysis$classification, useNA = "ifany")
    
    TN <- as.numeric(ifelse("TN" %in% names(contingency), contingency["TN"], 0))
    FP <- as.numeric(ifelse("FP" %in% names(contingency), contingency["FP"], 0))
    TP <- 0  # No true positives in negative control
    FN <- 0  # No false negatives in negative control
    
    specificity <- if (TN + FP > 0) TN / (TN + FP) else NA
    spec_ci <- if (TN + FP > 0) binom.confint(TN, TN + FP, method = "exact") else data.frame(lower = NA, upper = NA)
    
    return(list(
      n_total = nrow(df_analysis),
      TP = TP, FN = FN, FP = FP, TN = TN,
      sensitivity = NA, specificity = specificity,
      ppv = NA, npv = if(TN + FP > 0) TN/(TN + FP) else NA,
      sensitivity_ci = c(NA, NA), specificity_ci = c(spec_ci$lower, spec_ci$upper),
      ppv_ci = c(NA, NA), npv_ci = c(spec_ci$lower, spec_ci$upper),
      lr_positive = NA, lr_negative = NA,
      dor = NA, dor_ci = c(NA, NA),
      accuracy = if(TN + FP > 0) TN/(TN + FP) else NA,
      prevalence = 0,  # By definition for negative control
      fp_rate = if(TN + FP > 0) FP/(TN + FP) else NA
    ))
  } else {
    # For Histamine/Codeine (standard 2x2 table)
    contingency <- table(df$classification, useNA = "ifany")
    
    TP <- as.numeric(ifelse("TP" %in% names(contingency), contingency["TP"], 0))
    FN <- as.numeric(ifelse("FN" %in% names(contingency), contingency["FN"], 0))
    FP <- as.numeric(ifelse("FP" %in% names(contingency), contingency["FP"], 0))
    TN <- as.numeric(ifelse("TN" %in% names(contingency), contingency["TN"], 0))
    
    # Calculate metrics with confidence intervals
    sens_ci <- if (TP + FN > 0) binom.confint(TP, TP + FN, method = "exact") else data.frame(lower = NA, upper = NA)
    spec_ci <- if (TN + FP > 0) binom.confint(TN, TN + FP, method = "exact") else data.frame(lower = NA, upper = NA)
    ppv_ci  <- if (TP + FP > 0) binom.confint(TP, TP + FP, method = "exact") else data.frame(lower = NA, upper = NA)
    npv_ci  <- if (TN + FN > 0) binom.confint(TN, TN + FN, method = "exact") else data.frame(lower = NA, upper = NA)
    
    sensitivity <- if (TP + FN > 0) TP / (TP + FN) else NA
    specificity <- if (TN + FP > 0) TN / (TN + FP) else NA
    ppv         <- if (TP + FP > 0) TP / (TP + FP) else NA
    npv         <- if (TN + FN > 0) TN / (TN + FN) else NA
    
    # Likelihood ratios
    lr_pos <- if (!is.na(sensitivity) && !is.na(specificity) && specificity < 1) {
      sensitivity / (1 - specificity)
    } else NA
    
    lr_neg <- if (!is.na(sensitivity) && !is.na(specificity) && specificity > 0) {
      (1 - sensitivity) / specificity
    } else NA
    
    # Diagnostic Odds Ratio
    dor <- NA
    dor_ci <- c(NA, NA)
    tryCatch({
      if (TP > 0 && FN > 0 && FP > 0 && TN > 0) {
        tab <- matrix(c(TP, FN, FP, TN), nrow = 2, byrow = TRUE,
                      dimnames = list(Test = c("Pos", "Neg"), Disease = c("Case", "NoCase")))
        epi_res <- epi.tests(tab, conf.level = 0.95)
        dor     <- as.numeric(epi_res$measure["DOR", "est"])
        dor_ci  <- as.numeric(epi_res$measure["DOR", c("lower", "upper")])
      }
    }, error = function(e) {
      cat("Warning: Could not calculate DOR\n")
    })
    
    accuracy   <- (TP + TN) / (TP + FN + FP + TN)
    prevalence <- (TP + FN) / (TP + FN + FP + TN)
    fp_rate    <- if (TN + FP > 0) FP / (TN + FP) else NA
    
    return(list(
      n_total = nrow(df),
      TP = TP, FN = FN, FP = FP, TN = TN,
      sensitivity = sensitivity, specificity = specificity,
      ppv = ppv, npv = npv,
      sensitivity_ci = c(sens_ci$lower, sens_ci$upper),
      specificity_ci = c(spec_ci$lower, spec_ci$upper),
      ppv_ci = c(ppv_ci$lower, ppv_ci$upper),
      npv_ci = c(npv_ci$lower, npv_ci$upper),
      lr_positive = lr_pos, lr_negative = lr_neg,
      dor = dor, dor_ci = dor_ci,
      accuracy = accuracy, prevalence = prevalence,
      fp_rate = fp_rate
    ))
  }
}

# Calculate metrics for all three substances
histamine_metrics <- calculate_diagnostic_metrics(histamine_classified)
codeine_metrics <- calculate_diagnostic_metrics(codeine_classified)
nacl_metrics <- calculate_diagnostic_metrics(nacl_classified, is_negative_control = TRUE)

# ========================================================================
# Section 6. ROC Analysis using Classification Logic (Histamine & Codeine Only)
# ========================================================================
library(pROC)
# ROC analysis function that uses same classification logic consistently
perform_classification_roc <- function(df, substance_name, device_col) {
  
  cat("\n=== ROC ANALYSIS FOR", toupper(substance_name), "===\n")
  
  # Use the SAME classification logic - get only valid classifications
  clean_data <- df %>%
    filter(classification %in% c("TP", "FN", "FP", "TN")) %>%
    mutate(
      true_positive_status = classification %in% c("TP", "FN"),  # True condition based on classification
      device_value = as.numeric(.data[[device_col]])
    ) %>%
    filter(!is.na(device_value))
  
  cat("Sample size:", nrow(clean_data), "\n")
  cat("Positive cases:", sum(clean_data$true_positive_status), "\n")
  cat("Negative cases:", sum(!clean_data$true_positive_status), "\n")
  
  # Check if we have both positive and negative cases
  if(sum(clean_data$true_positive_status) == 0 || sum(!clean_data$true_positive_status) == 0) {
    cat("Cannot perform ROC analysis - all cases are the same class\n")
    return(NULL)
  }
  
  # Create ROC object
  roc_obj <- roc(
    response = clean_data$true_positive_status,
    predictor = clean_data$device_value,
    ci = TRUE,
    quiet = TRUE
  )
  
  # Get AUC with confidence interval
  auc_value <- as.numeric(auc(roc_obj))
  auc_ci <- ci.auc(roc_obj)
  
  # Calculate p-value using Wilcoxon test (alternative approach)
  pos_values <- clean_data$device_value[clean_data$true_positive_status == TRUE]
  neg_values <- clean_data$device_value[clean_data$true_positive_status == FALSE]
  
  if(length(pos_values) > 0 && length(neg_values) > 0) {
    wilcox_test <- wilcox.test(pos_values, neg_values)
    p_value <- wilcox_test$p.value
  } else {
    p_value <- NA
  }
  
  # Print results
  cat(sprintf("AUC: %.3f (95%% CI: %.3f - %.3f)\n", 
              auc_value, auc_ci[1], auc_ci[3]))
  if(!is.na(p_value)) {
    cat(sprintf("P-value (Wilcoxon): %.4f\n", p_value))
  }
  cat(sprintf("Direction: %s\n", roc_obj$direction))
  
  return(list(
    roc_obj = roc_obj,
    auc_value = auc_value,
    auc_ci = auc_ci,
    p_value = p_value,
    data = clean_data
  ))
}

# Perform ROC analysis for both substances
histamine_roc_results <- perform_classification_roc(histamine_classified, "HISTAMINE", "Hist_D")
codeine_roc_results <- perform_classification_roc(codeine_classified, "CODEINE", "Cod_D")

# Create ROC plot with statistical summary
if(!is.null(histamine_roc_results) && !is.null(codeine_roc_results)) {
  
  # Create the plot
  plot(histamine_roc_results$roc_obj, 
       col = "darkgreen", 
       lwd = 2,
       main = "ROC Curves: Device vs Manual Detection",
       cex.main = 1.2,
       print.auc = FALSE)
  
  # Add second ROC curve
  plot(codeine_roc_results$roc_obj, 
       col = "blue", 
       lwd = 2, 
       add = TRUE)
  
  # Add diagonal reference line
  abline(a = 0, b = 1, col = "gray", lty = 2, lwd = 1)
  
  # Create boxed legend with all information in the specified order
  hist_auc_text <- sprintf("Histamine (AUC = %.3f)", histamine_roc_results$auc_value)
  hist_ci_text <- sprintf("95%% CI: %.3f - %.3f", 
                          histamine_roc_results$auc_ci[1], 
                          histamine_roc_results$auc_ci[3])
  hist_p_text <- sprintf("P-value: %.4f", histamine_roc_results$p_value)
  
  cod_auc_text <- sprintf("Codeine (AUC = %.3f)", codeine_roc_results$auc_value)
  cod_ci_text <- sprintf("95%% CI: %.3f - %.3f", 
                         codeine_roc_results$auc_ci[1], 
                         codeine_roc_results$auc_ci[3])
  cod_p_text <- sprintf("P-value: %.4f", codeine_roc_results$p_value)
  
  # Create the boxed legend with specified order
  legend("bottomright", 
         legend = c(hist_auc_text, hist_ci_text, hist_p_text, 
                    cod_auc_text, cod_ci_text, cod_p_text),
         col = c("darkgreen", "white", "white", 
                 "blue", "white", "white"),
         lwd = c(2, 0, 0, 2, 0, 0),  # Only show line for AUC entries
         bty = "o",  # Draw box around legend
         cex = 0.9,
         bg = "white",  # White background for the box
         box.col = "black",  # Black border
         text.col = c("black", "black", "black", 
                      "black", "black", "black"))
  
  # Compare AUCs (if both have valid p-values)
  if(!is.na(histamine_roc_results$p_value) && !is.na(codeine_roc_results$p_value)) {
    tryCatch({
      comparison_test <- roc.test(histamine_roc_results$roc_obj, codeine_roc_results$roc_obj)
      cat("\n=== AUC COMPARISON ===\n")
      cat(sprintf("AUC difference: %.3f\n", histamine_roc_results$auc_value - codeine_roc_results$auc_value))
      cat(sprintf("P-value for difference: %.4f\n", comparison_test$p.value))
      
      if(comparison_test$p.value < 0.05) {
        cat("Statistically significant difference between AUCs (p < 0.05)\n")
      } else {
        cat("No statistically significant difference between AUCs (p >= 0.05)\n")
      }
    }, error = function(e) {
      cat("\n=== AUC COMPARISON ===\n")
      cat(sprintf("AUC difference: %.3f\n", histamine_roc_results$auc_value - codeine_roc_results$auc_value))
      cat("Could not perform statistical comparison test\n")
    })
  } else {
    cat("\n=== AUC COMPARISON ===\n")
    cat(sprintf("AUC difference: %.3f\n", histamine_roc_results$auc_value - codeine_roc_results$auc_value))
    cat("Statistical comparison not available\n")
  }
}
# ========================================================================
# Section 7. Missing Components - Prevalence Impact Analysis
# ========================================================================

# Calculate predictive values at different prevalence rates
calculate_predictive_values_by_prevalence <- function(sensitivity, specificity, prevalence_range = seq(0.1, 0.9, 0.1)) {
  
  results <- data.frame(
    prevalence = prevalence_range,
    ppv = NA,
    npv = NA
  )
  
  for(i in 1:length(prevalence_range)) {
    prev <- prevalence_range[i]
    
    # Calculate PPV and NPV using Bayes' theorem
    ppv <- (sensitivity * prev) / ((sensitivity * prev) + ((1 - specificity) * (1 - prev)))
    npv <- (specificity * (1 - prev)) / (((1 - sensitivity) * prev) + (specificity * (1 - prev)))
    
    results$ppv[i] <- ppv
    results$npv[i] <- npv
  }
  
  return(results)
}

# Calculate prevalence impact for each substance
if(!is.na(histamine_metrics$sensitivity) && !is.na(histamine_metrics$specificity)) {
  histamine_prevalence_impact <- calculate_predictive_values_by_prevalence(
    histamine_metrics$sensitivity, 
    histamine_metrics$specificity
  )
}

if(!is.na(codeine_metrics$sensitivity) && !is.na(codeine_metrics$specificity)) {
  codeine_prevalence_impact <- calculate_predictive_values_by_prevalence(
    codeine_metrics$sensitivity, 
    codeine_metrics$specificity
  )
}

# ========================================================================
# Section 8. Test Threshold Analysis
# ========================================================================

# Analyze performance at different device thresholds
analyze_thresholds <- function(df, substance_name, device_col, manual_col, manual_threshold = 3.5) {
  
  cat("\n=== THRESHOLD ANALYSIS FOR", toupper(substance_name), "===\n")
  
  # Get clean data
  clean_data <- df %>%
    filter(classification %in% c("TP", "FN", "FP", "TN")) %>%
    mutate(
      manual_positive = classification %in% c("TP", "FN"),
      device_value = as.numeric(.data[[device_col]])
    ) %>%
    filter(!is.na(device_value))
  
  # Test different thresholds
  thresholds <- seq(1, 6, 0.5)
  threshold_results <- data.frame(
    threshold = thresholds,
    sensitivity = NA,
    specificity = NA,
    ppv = NA,
    npv = NA,
    accuracy = NA,
    f1_score = NA,
    youden_j = NA
  )
  
  for(i in 1:length(thresholds)) {
    thresh <- thresholds[i]
    
    # Calculate confusion matrix at this threshold
    device_pos <- clean_data$device_value >= thresh
    
    TP <- sum(clean_data$manual_positive & device_pos)
    FN <- sum(clean_data$manual_positive & !device_pos)
    FP <- sum(!clean_data$manual_positive & device_pos)
    TN <- sum(!clean_data$manual_positive & !device_pos)
    
    # Calculate metrics
    sens <- if(TP + FN > 0) TP / (TP + FN) else NA
    spec <- if(TN + FP > 0) TN / (TN + FP) else NA
    ppv <- if(TP + FP > 0) TP / (TP + FP) else NA
    npv <- if(TN + FN > 0) TN / (TN + FN) else NA
    acc <- (TP + TN) / (TP + FN + FP + TN)
    
    # F1 score (harmonic mean of precision and recall)
    f1 <- if(!is.na(ppv) && !is.na(sens) && (ppv + sens) > 0) {
      2 * (ppv * sens) / (ppv + sens)
    } else NA
    
    # Youden's J statistic
    youden <- if(!is.na(sens) && !is.na(spec)) sens + spec - 1 else NA
    
    threshold_results[i, c("sensitivity", "specificity", "ppv", "npv", "accuracy", "f1_score", "youden_j")] <- 
      c(sens, spec, ppv, npv, acc, f1, youden)
  }
  
  # Find optimal thresholds
  optimal_youden <- threshold_results[which.max(threshold_results$youden_j), ]
  optimal_f1 <- threshold_results[which.max(threshold_results$f1_score), ]
  
  cat(sprintf("Optimal threshold (Youden's J): %.1f mm (J = %.3f)\n", 
              optimal_youden$threshold, optimal_youden$youden_j))
  cat(sprintf("Optimal threshold (F1 score): %.1f mm (F1 = %.3f)\n", 
              optimal_f1$threshold, optimal_f1$f1_score))
  
  return(list(
    threshold_results = threshold_results,
    optimal_youden = optimal_youden,
    optimal_f1 = optimal_f1
  ))
}

# Perform threshold analysis
histamine_threshold_analysis <- analyze_thresholds(histamine_classified, "HISTAMINE", "Hist_D", "Hist_M")
codeine_threshold_analysis <- analyze_thresholds(codeine_classified, "CODEINE", "Cod_D", "Cod_M")

# ========================================================================
# Section 9. Agreement Analysis (Inter-method Agreement)
# ========================================================================

# Calculate agreement between manual and device measurements
calculate_agreement <- function(df, substance_name, manual_col, device_col) {
  
  cat("\n=== AGREEMENT ANALYSIS FOR", toupper(substance_name), "===\n")
  
  # Get complete cases
  complete_data <- df %>%
    filter(!is.na(.data[[manual_col]]) & !is.na(.data[[device_col]])) %>%
    mutate(
      manual_value = as.numeric(.data[[manual_col]]),
      device_value = as.numeric(.data[[device_col]])
    )
  
  if(nrow(complete_data) == 0) {
    cat("No complete data for agreement analysis\n")
    return(NULL)
  }
  
  # Pearson correlation
  correlation <- cor(complete_data$manual_value, complete_data$device_value, method = "pearson")
  
  # Spearman correlation (rank-based)
  spearman_corr <- cor(complete_data$manual_value, complete_data$device_value, method = "spearman")
  
  # Mean difference (bias)
  differences <- complete_data$device_value - complete_data$manual_value
  bias <- mean(differences)
  
  # Limits of agreement (Bland-Altman)
  sd_diff <- sd(differences)
  upper_loa <- bias + 1.96 * sd_diff
  lower_loa <- bias - 1.96 * sd_diff
  
  # Concordance metrics
  manual_pos <- complete_data$manual_value >= 3.5
  device_pos <- complete_data$device_value >= 3
  
  # Overall agreement (proportion of cases where both tests agree)
  overall_agreement <- mean((manual_pos & device_pos) | (!manual_pos & !device_pos))
  
  # Positive agreement
  pos_agreement <- if(sum(manual_pos | device_pos) > 0) {
    sum(manual_pos & device_pos) / sum(manual_pos | device_pos)
  } else NA
  
  # Negative agreement  
  neg_agreement <- if(sum(!manual_pos | !device_pos) > 0) {
    sum(!manual_pos & !device_pos) / sum(!manual_pos | !device_pos)
  } else NA
  
  cat(sprintf("Pearson correlation: %.3f\n", correlation))
  cat(sprintf("Spearman correlation: %.3f\n", spearman_corr))
  cat(sprintf("Bias (device - manual): %.2f mm\n", bias))
  cat(sprintf("Limits of agreement: %.2f to %.2f mm\n", lower_loa, upper_loa))
  cat(sprintf("Overall agreement: %.1f%%\n", overall_agreement * 100))
  cat(sprintf("Positive agreement: %.1f%%\n", pos_agreement * 100))
  cat(sprintf("Negative agreement: %.1f%%\n", neg_agreement * 100))
  
  return(list(
    n_complete = nrow(complete_data),
    correlation = correlation,
    spearman_corr = spearman_corr,
    bias = bias,
    sd_diff = sd_diff,
    upper_loa = upper_loa,
    lower_loa = lower_loa,
    overall_agreement = overall_agreement,
    positive_agreement = pos_agreement,
    negative_agreement = neg_agreement,
    data = complete_data
  ))
}

# Perform agreement analysis
histamine_agreement <- calculate_agreement(histamine_classified, "HISTAMINE", "Hist_M", "Hist_D")
codeine_agreement <- calculate_agreement(codeine_classified, "CODEINE", "Cod_M", "Cod_D")

# ========================================================================
# Section 10. Clinical Utility Analysis
# ========================================================================

# Calculate clinical utility metrics
calculate_clinical_utility <- function(metrics, substance_name) {
  
  cat("\n=== CLINICAL UTILITY FOR", toupper(substance_name), "===\n")
  
  if(is.na(metrics$sensitivity) || is.na(metrics$specificity)) {
    cat("Cannot calculate clinical utility - missing sensitivity or specificity\n")
    return(NULL)
  }
  
  # Net benefit calculation for different threshold probabilities
  threshold_probs <- seq(0.1, 0.9, 0.1)
  
  net_benefits <- data.frame(
    threshold_prob = threshold_probs,
    net_benefit = NA,
    treat_all = NA,
    treat_none = NA
  )
  
  sens <- metrics$sensitivity
  spec <- metrics$specificity
  prev <- metrics$prevalence
  
  for(i in 1:length(threshold_probs)) {
    pt <- threshold_probs[i]  # threshold probability
    
    # Net benefit = (TP/n) - (FP/n) * (pt/(1-pt))
    # Where pt/(1-pt) is the odds at threshold probability
    
    net_benefit <- (sens * prev) - ((1 - spec) * (1 - prev)) * (pt / (1 - pt))
    treat_all <- prev - (1 - prev) * (pt / (1 - pt))
    treat_none <- 0
    
    net_benefits[i, c("net_benefit", "treat_all", "treat_none")] <- c(net_benefit, treat_all, treat_none)
  }
  
  # Number needed to diagnose (NND) - reciprocal of positive predictive value increment
  control_ppv <- prev  # assuming no test
  test_ppv <- metrics$ppv
  ppv_increment <- test_ppv - control_ppv
  nnd <- if(!is.na(ppv_increment) && ppv_increment > 0) 1 / ppv_increment else NA
  
  cat(sprintf("Prevalence in study: %.1f%%\n", prev * 100))
  cat(sprintf("PPV increment over no test: %.1f%%\n", ppv_increment * 100))
  cat(sprintf("Number needed to diagnose: %.0f\n", nnd))
  
  return(list(
    net_benefits = net_benefits,
    nnd = nnd,
    ppv_increment = ppv_increment
  ))
}

# Calculate clinical utility
histamine_utility <- calculate_clinical_utility(histamine_metrics, "HISTAMINE")
codeine_utility <- calculate_clinical_utility(codeine_metrics, "CODEINE")

# ========================================================================
# Section 11. Comprehensive Plots Creation
# ========================================================================

# Diagnostic plots function (keeping the existing one)
create_diagnostic_plots <- function(df, substance_name, manual_col, device_col) {
  
  cat("\n", rep("=", 60), "\n")
  cat("DIAGNOSTIC ANALYSIS FOR", toupper(substance_name), "\n")
  cat(rep("=", 60), "\n")
  
  # Use classification logic instead of simple threshold
  clean_data <- df %>%
    filter(classification %in% c("TP", "FN", "FP", "TN")) %>%
    mutate(
      # Use classification-based positive status instead of manual ≥3.5
      manual_positive = classification %in% c("TP", "FN"),
      device_value = as.numeric(.data[[device_col]]),
      manual_value = as.numeric(.data[[manual_col]])
    ) %>%
    filter(!is.na(device_value), !is.na(manual_value))
  
  # Calculate basic statistics
  n_total <- nrow(clean_data)
  n_pos <- sum(clean_data$manual_positive)
  n_neg <- sum(!clean_data$manual_positive)
  pos_percent <- round(n_pos / n_total * 100, 1)
  neg_percent <- round(n_neg / n_total * 100, 1)
  
  correlation <- round(cor(clean_data$manual_value, clean_data$device_value), 3)
  
  pos_mean_device <- round(mean(clean_data$device_value[clean_data$manual_positive]), 2)
  neg_mean_device <- round(mean(clean_data$device_value[!clean_data$manual_positive]), 2)
  pos_sd_device <- round(sd(clean_data$device_value[clean_data$manual_positive]), 2)
  neg_sd_device <- round(sd(clean_data$device_value[!clean_data$manual_positive]), 2)
  
  # Print statistics
  cat("SAMPLE STATISTICS:\n")
  cat(sprintf("Total cases: %d\n", n_total))
  cat(sprintf("Classification positive: %d (%.1f%%)\n", n_pos, pos_percent))
  cat(sprintf("Classification negative: %d (%.1f%%)\n", n_neg, neg_percent))
  cat(sprintf("Correlation: %.3f\n", correlation))
  cat(sprintf("Device values - Pos: %.2f±%.2f mm, Neg: %.2f±%.2f mm\n", 
              pos_mean_device, pos_sd_device, neg_mean_device, neg_sd_device))
  
  # Create ROC using same classification logic
  roc_obj <- roc(response = clean_data$manual_positive,
                 predictor = clean_data$device_value,
                 quiet = TRUE)
  auc_value <- round(as.numeric(auc(roc_obj)), 3)
  
  # Set up 2x2 plot layout
  par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 2))
  
  # Plot 1: Scatter Plot with Statistics
  plot(clean_data$manual_value, clean_data$device_value,
       xlab = paste("Manual", substance_name, "(mm)"),
       ylab = paste("Device", substance_name, "(mm)"),
       main = paste(substance_name, "Manual vs Device"),
       col = ifelse(clean_data$manual_positive, "red", "blue"),
       pch = 16, cex = 0.8)
  
  # Add threshold lines
  abline(v = 3.5, col = "red", lty = 2, lwd = 2)
  abline(h = 3, col = "blue", lty = 2, lwd = 2)
  
  # Add statistics text
  text(x = min(clean_data$manual_value) + 0.1, 
       y = max(clean_data$device_value) - 0.3,
       labels = paste0("n = ", n_total, "\nr = ", correlation),
       adj = c(0, 1), cex = 0.9, font = 2)
  
  legend("topleft", 
         legend = c(paste0("Classification Pos (n=", n_pos, ", ", pos_percent, "%)"), 
                    paste0("Classification Neg (n=", n_neg, ", ", neg_percent, "%)")), 
         col = c("red", "blue"), pch = 16, cex = 0.8, bg = "white")
  
  # Plot 2: Box Plot with Statistics
  boxplot_data <- list(
    "Classification Neg" = clean_data$device_value[!clean_data$manual_positive],
    "Classification Pos" = clean_data$device_value[clean_data$manual_positive]
  )
  
  boxplot(boxplot_data,
          ylab = paste("Device", substance_name, "(mm)"),
          main = paste(substance_name, "Device by Classification Status"),
          col = c("lightblue", "lightcoral"),
          border = c("blue", "red"))
  
  # Add mean values as points
  points(1, neg_mean_device, pch = 4, cex = 1.5, lwd = 2)
  points(2, pos_mean_device, pch = 4, cex = 1.5, lwd = 2)
  
  # Add statistics text
  text(x = 1.5, y = max(clean_data$device_value) - 0.3,
       labels = paste0("Neg: ", neg_mean_device, "±", neg_sd_device, " mm\n",
                       "Pos: ", pos_mean_device, "±", pos_sd_device, " mm"),
       adj = c(0.5, 1), cex = 0.8, font = 2)
  
  legend("topright", legend = "Mean", pch = 4, cex = 0.8, bg = "white")
  
  # Plot 3: ROC Curve with Statistics
  plot(roc_obj, 
       main = paste(substance_name, "ROC Curve"),
       col = "darkblue", lwd = 2,
       legacy.axes = FALSE)
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Add AUC text
  text(0.6, 0.2, paste("AUC =", auc_value), cex = 1.2, font = 2)
  
  # Add sample size
  text(0.6, 0.1, paste("n =", n_total), cex = 1, font = 2)
  
  # Plot 4: Histogram with Statistics
  # Calculate histogram data
  neg_data <- clean_data$device_value[!clean_data$manual_positive]
  pos_data <- clean_data$device_value[clean_data$manual_positive]
  
  # Create histogram
  hist(neg_data, 
       col = rgb(0, 0, 1, 0.4), 
       breaks = 15,
       main = paste(substance_name, "Device Value Distribution"),
       xlab = paste("Device", substance_name, "(mm)"),
       freq = FALSE,
       xlim = range(clean_data$device_value))
  
  hist(pos_data, 
       col = rgb(1, 0, 0, 0.4), 
       breaks = 15, 
       add = TRUE, 
       freq = FALSE)
  
  # Add vertical lines for means
  abline(v = neg_mean_device, col = "blue", lwd = 2, lty = 1)
  abline(v = pos_mean_device, col = "red", lwd = 2, lty = 1)
  
  # Add statistics legend
  legend("topright", 
         legend = c(paste0("Classification Neg (n=", n_neg, ")"),
                    paste0("Classification Pos (n=", n_pos, ")"),
                    paste0("Neg Mean: ", neg_mean_device, " mm"),
                    paste0("Pos Mean: ", pos_mean_device, " mm")), 
         fill = c(rgb(0, 0, 1, 0.4), rgb(1, 0, 0, 0.4), NA, NA),
         border = c("blue", "red", NA, NA),
         cex = 0.7, bg = "white")
  
  # Reset to single plot
  par(mfrow = c(1, 1))
  
  # Print Detailed Statistics
  cat("\nDETAILED STATISTICS:\n")
  
  # Confusion matrices at different thresholds using classification logic
  cat("\nCONFUSION MATRICES AT DIFFERENT DEVICE THRESHOLDS:\n")
  thresholds <- c(2, 3, 4, 5)
  for(thresh in thresholds) {
    device_pos <- clean_data$device_value >= thresh
    confusion <- table(Classification = clean_data$manual_positive, 
                       Device = device_pos,
                       dnn = c("Classification", paste0("Device≥", thresh)))
    
    cat(sprintf("\nDevice threshold ≥%.0f mm:\n", thresh))
    print(confusion)
    
    if(nrow(confusion) == 2 && ncol(confusion) == 2) {
      TP <- confusion[2,2]; FN <- confusion[2,1]
      FP <- confusion[1,2]; TN <- confusion[1,1]
      sens <- round(TP/(TP+FN)*100, 1)
      spec <- round(TN/(TN+FP)*100, 1)
      cat(sprintf("Sensitivity: %.1f%% (%d/%d), Specificity: %.1f%% (%d/%d)\n", 
                  sens, TP, TP+FN, spec, TN, TN+FP))
    }
  }
  
  # Return data for further analysis
  return(list(
    data = clean_data,
    n_total = n_total,
    n_pos = n_pos, n_neg = n_neg,
    pos_percent = pos_percent, neg_percent = neg_percent,
    correlation = correlation,
    pos_mean_device = pos_mean_device, neg_mean_device = neg_mean_device,
    pos_sd_device = pos_sd_device, neg_sd_device = neg_sd_device,
    auc = auc_value,
    roc_obj = roc_obj
  ))
}

# Create combined ROC plot
create_roc_plot <- function(hist_roc, cod_roc) {
  
  if(is.null(hist_roc) || is.null(cod_roc)) {
    cat("Cannot create ROC plot - missing ROC objects\n")
    return(NULL)
  }
  
  # Set up plot parameters
  par(mar = c(5, 5, 4, 2), pty = "s")
  
  # Plot histamine ROC curve
  plot(hist_roc$roc_obj, 
       col = "#1B5E20",
       lwd = 2,
       main = "ROC Curves: Device vs Manual Detection",
       legacy.axes = FALSE,
       print.auc = FALSE)
  
  # Add codeine ROC curve
  lines(cod_roc$roc_obj, 
        col = "#2E4F99",
        lwd = 2)
  
  # Add diagonal reference line
  abline(a = 0, b = 1, lty = 2, col = "gray", lwd = 1)
  
  # Add legend with AUC values
  legend("bottomright",
         legend = c(
           sprintf("Histamine (AUC = %.3f)", hist_roc$auc_value),
           sprintf("Codeine (AUC = %.3f)", cod_roc$auc_value)
         ),
         col = c("#1B5E20", "#2E4F99"),
         lwd = 2,
         bty = "n")
  
  # Add grid
  grid(col = "lightgray", lty = 3)
}

# ========================================================================
# Section 12. Run All Analysis and Generate Reports
# ========================================================================

# Create combined ROC plot
if(!is.null(histamine_roc_results) && !is.null(codeine_roc_results)) {
  create_roc_plot(histamine_roc_results, codeine_roc_results)
}

# Create diagnostic plots for Histamine
cat("Creating diagnostic plots for HISTAMINE...")
histamine_analysis <- create_diagnostic_plots(histamine_classified, "HISTAMINE", "Hist_M", "Hist_D")

# Save histamine plots
png("Histamine_Diagnostic_Plots_Complete.png", width = 1200, height = 1000, res = 150)
histamine_analysis <- create_diagnostic_plots(histamine_classified, "HISTAMINE", "Hist_M", "Hist_D")
dev.off()

# Create diagnostic plots for Codeine  
cat("\nCreating diagnostic plots for CODEINE...")
codeine_analysis <- create_diagnostic_plots(codeine_classified, "CODEINE", "Cod_M", "Cod_D")

# Save codeine plots
png("Codeine_Diagnostic_Plots_Complete.png", width = 1200, height = 1000, res = 150)
codeine_analysis <- create_diagnostic_plots(codeine_classified, "CODEINE", "Cod_M", "Cod_D")
dev.off()

# ROC Curve Comparison
if(!is.null(histamine_roc_results) && !is.null(codeine_roc_results)) {
  
  cat("\n=== ROC CURVE COMPARISON ===\n")
  
  # Test for significant difference between AUCs
  roc_test_result <- roc.test(histamine_roc_results$roc_obj, codeine_roc_results$roc_obj)
  
  cat(sprintf("Histamine AUC: %.3f\n", histamine_roc_results$auc_value))
  cat(sprintf("Codeine AUC: %.3f\n", codeine_roc_results$auc_value))
  cat(sprintf("Difference: %.3f\n", abs(histamine_roc_results$auc_value - codeine_roc_results$auc_value)))
  cat(sprintf("p-value: %.4f\n", roc_test_result$p.value))
  
  if(roc_test_result$p.value < 0.05) {
    cat("Result: Statistically significant difference between curves\n")
  } else {
    cat("Result: No statistically significant difference between curves\n")
  }
}

# Find optimal thresholds
find_optimal_threshold <- function(roc_obj, substance_name) {
  
  cat("\n=== OPTIMAL THRESHOLD FOR", toupper(substance_name), "===\n")
  
  # Find threshold that maximizes Youden's J statistic
  coords_all <- coords(roc_obj, "all", ret = c("threshold", "sensitivity", "specificity"))
  youden_j <- coords_all$sensitivity + coords_all$specificity - 1
  optimal_idx <- which.max(youden_j)
  
  optimal_threshold <- coords_all$threshold[optimal_idx]
  optimal_sens <- coords_all$sensitivity[optimal_idx]
  optimal_spec <- coords_all$specificity[optimal_idx]
  
  cat(sprintf("Optimal threshold: %.2f mm\n", optimal_threshold))
  cat(sprintf("At this threshold - Sensitivity: %.1f%%, Specificity: %.1f%%\n", 
              optimal_sens * 100, optimal_spec * 100))
  cat(sprintf("Youden's J statistic: %.3f\n", max(youden_j)))
  
  return(list(
    threshold = optimal_threshold,
    sensitivity = optimal_sens,
    specificity = optimal_spec,
    youden_j = max(youden_j)
  ))
}

# Find optimal thresholds
if(!is.null(histamine_roc_results)) {
  hist_optimal <- find_optimal_threshold(histamine_roc_results$roc_obj, "HISTAMINE")
}

if(!is.null(codeine_roc_results)) {
  cod_optimal <- find_optimal_threshold(codeine_roc_results$roc_obj, "CODEINE")
}

# Save combined ROC plot
png("ROC_Curves_Complete.png", width = 800, height = 800, res = 150)
if(!is.null(histamine_roc_results) && !is.null(codeine_roc_results)) {
  create_roc_plot(histamine_roc_results, codeine_roc_results)
}
dev.off()

# ========================================================================
# Section 13. Comprehensive Summary Report
# ========================================================================

cat("\n", rep("=", 80), "\n")
cat("COMPLETE DIAGNOSTIC PERFORMANCE ANALYSIS SUMMARY\n")
cat(rep("=", 80), "\n")

# Sample sizes summary
cat("\n=== SAMPLE SIZES AFTER EXCLUSIONS ===\n")
cat(sprintf("HISTAMINE: %d included (from %d total)\n", nrow(histamine_clean), nrow(histamine_data)))
cat(sprintf("CODEINE: %d included (from %d total)\n", nrow(codeine_clean), nrow(codeine_data)))
cat(sprintf("NACL: %d included (from %d total)\n", nrow(nacl_clean), nrow(nacl_data)))

# Primary diagnostic metrics
cat("\n=== PRIMARY DIAGNOSTIC METRICS ===\n")

cat("\nHISTAMINE PERFORMANCE:\n")
cat(sprintf("TP: %d, FN: %d, FP: %d, TN: %d\n", 
            histamine_metrics$TP, histamine_metrics$FN, histamine_metrics$FP, histamine_metrics$TN))
cat(sprintf("Sensitivity: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            histamine_metrics$sensitivity*100, histamine_metrics$sensitivity_ci[1]*100, histamine_metrics$sensitivity_ci[2]*100))
cat(sprintf("Specificity: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            histamine_metrics$specificity*100, histamine_metrics$specificity_ci[1]*100, histamine_metrics$specificity_ci[2]*100))
cat(sprintf("PPV: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            histamine_metrics$ppv*100, histamine_metrics$ppv_ci[1]*100, histamine_metrics$ppv_ci[2]*100))
cat(sprintf("NPV: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            histamine_metrics$npv*100, histamine_metrics$npv_ci[1]*100, histamine_metrics$npv_ci[2]*100))
cat(sprintf("Accuracy: %.1f%%\n", histamine_metrics$accuracy*100))
cat(sprintf("Prevalence: %.1f%%\n", histamine_metrics$prevalence*100))

cat("\nCODEINE PERFORMANCE:\n")
cat(sprintf("TP: %d, FN: %d, FP: %d, TN: %d\n", 
            codeine_metrics$TP, codeine_metrics$FN, codeine_metrics$FP, codeine_metrics$TN))
cat(sprintf("Sensitivity: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            codeine_metrics$sensitivity*100, codeine_metrics$sensitivity_ci[1]*100, codeine_metrics$sensitivity_ci[2]*100))
cat(sprintf("Specificity: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            codeine_metrics$specificity*100, codeine_metrics$specificity_ci[1]*100, codeine_metrics$specificity_ci[2]*100))
cat(sprintf("PPV: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            codeine_metrics$ppv*100, codeine_metrics$ppv_ci[1]*100, codeine_metrics$ppv_ci[2]*100))
cat(sprintf("NPV: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            codeine_metrics$npv*100, codeine_metrics$npv_ci[1]*100, codeine_metrics$npv_ci[2]*100))
cat(sprintf("Accuracy: %.1f%%\n", codeine_metrics$accuracy*100))
cat(sprintf("Prevalence: %.1f%%\n", codeine_metrics$prevalence*100))

cat("\nNACL SPECIFICITY CONTROL:\n")
cat(sprintf("FP: %d, TN: %d\n", nacl_metrics$FP, nacl_metrics$TN))
cat(sprintf("Specificity: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
            nacl_metrics$specificity*100, nacl_metrics$specificity_ci[1]*100, nacl_metrics$specificity_ci[2]*100))
cat(sprintf("False Positive Rate: %.1f%%\n", nacl_metrics$fp_rate*100))

# Likelihood ratios and diagnostic odds ratios
cat("\n=== LIKELIHOOD RATIOS AND DIAGNOSTIC ODDS RATIOS ===\n")

cat("\nHISTAMINE:\n")
cat(sprintf("LR+: %.2f\n", histamine_metrics$lr_positive))
cat(sprintf("LR-: %.2f\n", histamine_metrics$lr_negative))
if(!is.na(histamine_metrics$dor)) {
  cat(sprintf("DOR: %.2f (95%% CI: %.2f-%.2f)\n", 
              histamine_metrics$dor, histamine_metrics$dor_ci[1], histamine_metrics$dor_ci[2]))
}

cat("\nCODEINE:\n")
cat(sprintf("LR+: %.2f\n", codeine_metrics$lr_positive))
cat(sprintf("LR-: %.2f\n", codeine_metrics$lr_negative))
if(!is.na(codeine_metrics$dor)) {
  cat(sprintf("DOR: %.2f (95%% CI: %.2f-%.2f)\n", 
              codeine_metrics$dor, codeine_metrics$dor_ci[1], codeine_metrics$dor_ci[2]))
}

# ROC analysis results
cat("\n=== ROC ANALYSIS RESULTS ===\n")
if(!is.null(histamine_roc_results)) {
  cat(sprintf("Histamine AUC: %.3f (95%% CI: %.3f-%.3f)\n", 
              histamine_roc_results$auc_value, histamine_roc_results$auc_ci[1], histamine_roc_results$auc_ci[3]))
}
if(!is.null(codeine_roc_results)) {
  cat(sprintf("Codeine AUC: %.3f (95%% CI: %.3f-%.3f)\n", 
              codeine_roc_results$auc_value, codeine_roc_results$auc_ci[1], codeine_roc_results$auc_ci[3]))
}

# Agreement analysis results
cat("\n=== INTER-METHOD AGREEMENT ===\n")
if(!is.null(histamine_agreement)) {
  cat("\nHISTAMINE Agreement:\n")
  cat(sprintf("Pearson correlation: %.3f\n", histamine_agreement$correlation))
  cat(sprintf("Bias (device - manual): %.2f mm\n", histamine_agreement$bias))
  cat(sprintf("Limits of agreement: %.2f to %.2f mm\n", histamine_agreement$lower_loa, histamine_agreement$upper_loa))
  cat(sprintf("Overall agreement: %.1f%%\n", histamine_agreement$overall_agreement*100))
}

if(!is.null(codeine_agreement)) {
  cat("\nCODEINE Agreement:\n")
  cat(sprintf("Pearson correlation: %.3f\n", codeine_agreement$correlation))
  cat(sprintf("Bias (device - manual): %.2f mm\n", codeine_agreement$bias))
  cat(sprintf("Limits of agreement: %.2f to %.2f mm\n", codeine_agreement$lower_loa, codeine_agreement$upper_loa))
  cat(sprintf("Overall agreement: %.1f%%\n", codeine_agreement$overall_agreement*100))
}

# Optimal thresholds
cat("\n=== OPTIMAL DEVICE THRESHOLDS ===\n")
if(!is.null(histamine_roc_results)) {
  cat(sprintf("Histamine optimal threshold: %.2f mm (Youden J = %.3f)\n", 
              hist_optimal$threshold, hist_optimal$youden_j))
}
if(!is.null(codeine_roc_results)) {
  cat(sprintf("Codeine optimal threshold: %.2f mm (Youden J = %.3f)\n", 
              cod_optimal$threshold, cod_optimal$youden_j))
}

# Performance interpretation
cat("\n=== PERFORMANCE INTERPRETATION ===\n")

# AUC interpretation
interpret_auc <- function(auc, substance) {
  if(is.na(auc)) return(paste(substance, ": Cannot interpret (missing AUC)"))
  
  if(auc >= 0.9) return(paste(substance, ": Outstanding discrimination"))
  else if(auc >= 0.8) return(paste(substance, ": Excellent discrimination"))
  else if(auc >= 0.7) return(paste(substance, ": Good discrimination"))
  else if(auc >= 0.6) return(paste(substance, ": Satisfactory discrimination"))
  else return(paste(substance, ": Poor discrimination"))
}

if(!is.null(histamine_roc_results)) {
  cat(interpret_auc(histamine_roc_results$auc_value, "Histamine"), "\n")
}
if(!is.null(codeine_roc_results)) {
  cat(interpret_auc(codeine_roc_results$auc_value, "Codeine"), "\n")
}

# Clinical recommendations
cat("\n=== CLINICAL RECOMMENDATIONS ===\n")

# Histamine recommendations
hist_sens_adequate <- !is.na(histamine_metrics$sensitivity) && histamine_metrics$sensitivity >= 0.95
hist_spec_adequate <- !is.na(histamine_metrics$specificity) && histamine_metrics$specificity >= 0.95

cat("HISTAMINE (Sensitivity Control):\n")
if(hist_sens_adequate) {
  cat("✓ Sensitivity meets 95% benchmark - suitable for screening\n")
} else {
  cat("⚠ Sensitivity below 95% benchmark - may miss positive cases\n")
}

# Codeine recommendations  
cod_sens_adequate <- !is.na(codeine_metrics$sensitivity) && codeine_metrics$sensitivity >= 0.95
cod_spec_adequate <- !is.na(codeine_metrics$specificity) && codeine_metrics$specificity >= 0.95

cat("\nCODEINE (Sensitivity Control):\n")
if(cod_sens_adequate) {
  cat("✓ Sensitivity meets 95% benchmark - suitable for screening\n")
} else {
  cat("⚠ Sensitivity below 95% benchmark - may miss positive cases\n")
}

# NaCl recommendations
nacl_spec_adequate <- !is.na(nacl_metrics$specificity) && nacl_metrics$specificity >= 0.95

cat("\nNACL (Specificity Control):\n")
if(nacl_spec_adequate) {
  cat("✓ Specificity meets 95% benchmark - low false positive rate\n")
} else {
  cat("⚠ Specificity below 95% benchmark - higher false positive rate\n")
}

# Overall device performance
cat("\n=== OVERALL DEVICE PERFORMANCE SUMMARY ===\n")

total_tests <- 3
adequate_performance <- sum(c(hist_sens_adequate, cod_sens_adequate, nacl_spec_adequate), na.rm = TRUE)

cat(sprintf("Performance benchmarks met: %d out of %d tests\n", adequate_performance, total_tests))

if(adequate_performance == total_tests) {
  cat("✓ Device meets all performance benchmarks\n")
} else if(adequate_performance >= 2) {
  cat("⚠ Device meets most performance benchmarks with some limitations\n")
} else {
  cat("✗ Device performance requires improvement\n")
}

# Files generated
cat("\n=== FILES GENERATED ===\n")
cat("- ROC_Curves_Complete.png (Combined ROC analysis)\n")
cat("- Histamine_Diagnostic_Plots_Complete.png (Histamine detailed analysis)\n")
cat("- Codeine_Diagnostic_Plots_Complete.png (Codeine detailed analysis)\n")

# Data export for further analysis
cat("\n=== EXPORTING RESULTS FOR FURTHER ANALYSIS ===\n")

# Create summary table for export
summary_results <- data.frame(
  Test = c("Histamine", "Codeine", "NaCl"),
  Sample_Size = c(nrow(histamine_clean), nrow(codeine_clean), nrow(nacl_clean)),
  TP = c(histamine_metrics$TP, codeine_metrics$TP, NA),
  FN = c(histamine_metrics$FN, codeine_metrics$FN, NA),
  FP = c(histamine_metrics$FP, codeine_metrics$FP, nacl_metrics$FP),
  TN = c(histamine_metrics$TN, codeine_metrics$TN, nacl_metrics$TN),
  Sensitivity = c(histamine_metrics$sensitivity, codeine_metrics$sensitivity, NA),
  Specificity = c(histamine_metrics$specificity, codeine_metrics$specificity, nacl_metrics$specificity),
  PPV = c(histamine_metrics$ppv, codeine_metrics$ppv, NA),
  NPV = c(histamine_metrics$npv, codeine_metrics$npv, NA),
  LR_Positive = c(histamine_metrics$lr_positive, codeine_metrics$lr_positive, NA),
  LR_Negative = c(histamine_metrics$lr_negative, codeine_metrics$lr_negative, NA),
  AUC = c(
    ifelse(!is.null(histamine_roc_results), histamine_roc_results$auc_value, NA),
    ifelse(!is.null(codeine_roc_results), codeine_roc_results$auc_value, NA),
    NA
  ),
  Correlation = c(
    ifelse(!is.null(histamine_agreement), histamine_agreement$correlation, NA),
    ifelse(!is.null(codeine_agreement), codeine_agreement$correlation, NA),
    NA
  ),
  Meets_Benchmark = c(hist_sens_adequate, cod_sens_adequate, nacl_spec_adequate)
)

# Round numeric columns
numeric_cols <- sapply(summary_results, is.numeric)
summary_results[numeric_cols] <- lapply(summary_results[numeric_cols], function(x) round(x, 3))

# Export to Excel
tryCatch({
  write.xlsx(summary_results, "Complete_Diagnostic_Analysis_Summary.xlsx", rowNames = FALSE)
  cat("- Complete_Diagnostic_Analysis_Summary.xlsx (Summary table)\n")
}, error = function(e) {
  cat("Warning: Could not export Excel file\n")
})

# Export detailed threshold analysis
if(exists("histamine_threshold_analysis") && !is.null(histamine_threshold_analysis)) {
  tryCatch({
    write.xlsx(histamine_threshold_analysis$threshold_results, "Histamine_Threshold_Analysis.xlsx", rowNames = FALSE)
    cat("- Histamine_Threshold_Analysis.xlsx (Threshold optimization)\n")
  }, error = function(e) {
    cat("Warning: Could not export Histamine threshold analysis\n")
  })
}

if(exists("codeine_threshold_analysis") && !is.null(codeine_threshold_analysis)) {
  tryCatch({
    write.xlsx(codeine_threshold_analysis$threshold_results, "Codeine_Threshold_Analysis.xlsx", rowNames = FALSE)
    cat("- Codeine_Threshold_Analysis.xlsx (Threshold optimization)\n")
  }, error = function(e) {
    cat("Warning: Could not export Codeine threshold analysis\n")
  })
}

# Print final comparison table
cat("\n=== FINAL COMPARISON TABLE ===\n")
print(summary_results)

# ========================================================================
# Section 14. Statistical Significance Testing
# ========================================================================

cat("\n=== STATISTICAL SIGNIFICANCE TESTING ===\n")

# McNemar's test for paired binary data (comparing device vs manual classification)
perform_mcnemar_test <- function(df, substance_name) {
  
  cat("\n--- McNemar's Test for", toupper(substance_name), "---\n")
  
  # Create 2x2 contingency table for paired data
  clean_data <- df %>%
    filter(classification %in% c("TP", "FN", "FP", "TN")) %>%
    mutate(
      manual_positive = classification %in% c("TP", "FN"),
      device_positive = classification %in% c("TP", "FP")
    )
  
  if(nrow(clean_data) == 0) {
    cat("No data available for McNemar's test\n")
    return(NULL)
  }
  
  # Create contingency table
  contingency_table <- table(clean_data$manual_positive, clean_data$device_positive,
                             dnn = c("Manual", "Device"))
  
  cat("Contingency Table:\n")
  print(contingency_table)
  
  # Perform McNemar's test
  tryCatch({
    mcnemar_result <- mcnemar.test(contingency_table, correct = TRUE)
    
    cat(sprintf("McNemar's chi-squared: %.3f\n", mcnemar_result$statistic))
    cat(sprintf("p-value: %.4f\n", mcnemar_result$p.value))
    
    if(mcnemar_result$p.value < 0.05) {
      cat("Result: Significant difference between manual and device classifications\n")
    } else {
      cat("Result: No significant difference between manual and device classifications\n")
    }
    
    return(mcnemar_result)
    
  }, error = function(e) {
    cat("Error performing McNemar's test:", e$message, "\n")
    return(NULL)
  })
}

# Perform McNemar's tests
histamine_mcnemar <- perform_mcnemar_test(histamine_classified, "HISTAMINE")
codeine_mcnemar <- perform_mcnemar_test(codeine_classified, "CODEINE")

# Compare AUCs between substances
if(!is.null(histamine_roc_results) && !is.null(codeine_roc_results)) {
  cat("\n--- AUC Comparison Test ---\n")
  
  tryCatch({
    auc_comparison <- roc.test(histamine_roc_results$roc_obj, codeine_roc_results$roc_obj, method = "delong")
    
    cat(sprintf("Histamine AUC: %.3f\n", histamine_roc_results$auc_value))
    cat(sprintf("Codeine AUC: %.3f\n", codeine_roc_results$auc_value))
    cat(sprintf("Difference: %.3f\n", abs(histamine_roc_results$auc_value - codeine_roc_results$auc_value)))
    cat(sprintf("DeLong test statistic: %.3f\n", auc_comparison$statistic))
    cat(sprintf("p-value: %.4f\n", auc_comparison$p.value))
    
    if(auc_comparison$p.value < 0.05) {
      cat("Result: Statistically significant difference between AUCs\n")
    } else {
      cat("Result: No statistically significant difference between AUCs\n")
    }
    
  }, error = function(e) {
    cat("Error comparing AUCs:", e$message, "\n")
  })
}

# ========================================================================
# Section 15. Confidence Interval Analysis
# ========================================================================

cat("\n=== CONFIDENCE INTERVAL ANALYSIS ===\n")

# Function to check if confidence intervals meet clinical requirements
evaluate_ci_precision <- function(metrics, substance_name, target_lower = 0.95) {
  
  cat("\n--- CI Precision for", toupper(substance_name), "---\n")
  
  if(substance_name %in% c("HISTAMINE", "CODEINE")) {
    # Check sensitivity CI
    sens_ci_width <- metrics$sensitivity_ci[2] - metrics$sensitivity_ci[1]
    sens_lower_adequate <- metrics$sensitivity_ci[1] >= target_lower
    
    cat(sprintf("Sensitivity: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
                metrics$sensitivity*100, metrics$sensitivity_ci[1]*100, metrics$sensitivity_ci[2]*100))
    cat(sprintf("CI width: %.1f percentage points\n", sens_ci_width*100))
    cat(sprintf("Lower CI bound meets %.0f%% target: %s\n", target_lower*100, 
                ifelse(sens_lower_adequate, "YES", "NO")))
    
    # Check specificity CI
    spec_ci_width <- metrics$specificity_ci[2] - metrics$specificity_ci[1]
    spec_lower_adequate <- metrics$specificity_ci[1] >= target_lower
    
    cat(sprintf("Specificity: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
                metrics$specificity*100, metrics$specificity_ci[1]*100, metrics$specificity_ci[2]*100))
    cat(sprintf("CI width: %.1f percentage points\n", spec_ci_width*100))
    cat(sprintf("Lower CI bound meets %.0f%% target: %s\n", target_lower*100, 
                ifelse(spec_lower_adequate, "YES", "NO")))
    
    return(list(
      sens_adequate = sens_lower_adequate,
      spec_adequate = spec_lower_adequate,
      sens_ci_width = sens_ci_width,
      spec_ci_width = spec_ci_width
    ))
    
  } else if(substance_name == "NACL") {
    # Check specificity CI only for NaCl
    spec_ci_width <- metrics$specificity_ci[2] - metrics$specificity_ci[1]
    spec_lower_adequate <- metrics$specificity_ci[1] >= target_lower
    
    cat(sprintf("Specificity: %.1f%% (95%% CI: %.1f%%-%.1f%%)\n", 
                metrics$specificity*100, metrics$specificity_ci[1]*100, metrics$specificity_ci[2]*100))
    cat(sprintf("CI width: %.1f percentage points\n", spec_ci_width*100))
    cat(sprintf("Lower CI bound meets %.0f%% target: %s\n", target_lower*100, 
                ifelse(spec_lower_adequate, "YES", "NO")))
    
    return(list(
      spec_adequate = spec_lower_adequate,
      spec_ci_width = spec_ci_width
    ))
  }
}

# Evaluate CI precision for all substances
histamine_ci_eval <- evaluate_ci_precision(histamine_metrics, "HISTAMINE")
codeine_ci_eval <- evaluate_ci_precision(codeine_metrics, "CODEINE")
nacl_ci_eval <- evaluate_ci_precision(nacl_metrics, "NACL")

# ========================================================================
# Section 16. Power Analysis and Sample Size Considerations
# ========================================================================

cat("\n=== SAMPLE SIZE AND POWER CONSIDERATIONS ===\n")

# Calculate achieved power for sensitivity/specificity estimates
calculate_achieved_power <- function(metrics, substance_name) {
  
  cat("\n--- Power Analysis for", toupper(substance_name), "---\n")
  
  if(is.na(metrics$sensitivity) || is.na(metrics$specificity)) {
    cat("Cannot calculate power - missing metrics\n")
    return(NULL)
  }
  
  # Sample sizes
  n_positive <- metrics$TP + metrics$FN
  n_negative <- metrics$TN + metrics$FP
  
  # For sensitivity (using TP and FN)
  sens_se <- sqrt((metrics$sensitivity * (1 - metrics$sensitivity)) / n_positive)
  sens_margin_error <- 1.96 * sens_se
  
  # For specificity (using TN and FP)  
  spec_se <- sqrt((metrics$specificity * (1 - metrics$specificity)) / n_negative)
  spec_margin_error <- 1.96 * spec_se
  
  cat(sprintf("Positive cases: %d, Negative cases: %d\n", n_positive, n_negative))
  cat(sprintf("Sensitivity margin of error: ±%.1f%%\n", sens_margin_error*100))
  cat(sprintf("Specificity margin of error: ±%.1f%%\n", spec_margin_error*100))
  
  # Required sample size for different margins of error
  target_margins <- c(0.05, 0.03, 0.02)  # 5%, 3%, 2%
  
  cat("\nRequired sample sizes for different precision levels:\n")
  for(margin in target_margins) {
    # For sensitivity
    n_sens_required <- ceiling((1.96^2 * metrics$sensitivity * (1 - metrics$sensitivity)) / margin^2)
    # For specificity
    n_spec_required <- ceiling((1.96^2 * metrics$specificity * (1 - metrics$specificity)) / margin^2)
    
    cat(sprintf("For ±%.0f%% margin: Sensitivity n=%d, Specificity n=%d\n", 
                margin*100, n_sens_required, n_spec_required))
  }
  
  return(list(
    n_positive = n_positive,
    n_negative = n_negative,
    sens_margin_error = sens_margin_error,
    spec_margin_error = spec_margin_error
  ))
}

# Perform power analysis
histamine_power <- calculate_achieved_power(histamine_metrics, "HISTAMINE")
codeine_power <- calculate_achieved_power(codeine_metrics, "CODEINE")

# ========================================================================
# Section 17. Final Recommendations and Conclusions
# ========================================================================

cat("\n", rep("=", 80), "\n")
cat("FINAL RECOMMENDATIONS AND CONCLUSIONS\n")
cat(rep("=", 80), "\n")

# Performance benchmarks summary
cat("\n=== PERFORMANCE BENCHMARKS SUMMARY ===\n")

benchmark_results <- data.frame(
  Test = c("Histamine Sensitivity", "Codeine Sensitivity", "NaCl Specificity"),
  Target = c("≥95%", "≥95%", "≥95%"),
  Achieved = c(
    sprintf("%.1f%%", histamine_metrics$sensitivity*100),
    sprintf("%.1f%%", codeine_metrics$sensitivity*100),
    sprintf("%.1f%%", nacl_metrics$specificity*100)
  ),
  CI_Lower = c(
    sprintf("%.1f%%", histamine_metrics$sensitivity_ci[1]*100),
    sprintf("%.1f%%", codeine_metrics$sensitivity_ci[1]*100),
    sprintf("%.1f%%", nacl_metrics$specificity_ci[1]*100)
  ),
  Meets_Target = c(
    ifelse(hist_sens_adequate, "YES", "NO"),
    ifelse(cod_sens_adequate, "YES", "NO"),
    ifelse(nacl_spec_adequate, "YES", "NO")
  ),
  CI_Meets_Target = c(
    ifelse(!is.null(histamine_ci_eval) && histamine_ci_eval$sens_adequate, "YES", "NO"),
    ifelse(!is.null(codeine_ci_eval) && codeine_ci_eval$sens_adequate, "YES", "NO"),
    ifelse(!is.null(nacl_ci_eval) && nacl_ci_eval$spec_adequate, "YES", "NO")
  )
)

print(benchmark_results)

# Overall device assessment
total_benchmarks_met <- sum(c(hist_sens_adequate, cod_sens_adequate, nacl_spec_adequate))
total_ci_benchmarks_met <- sum(c(
  !is.null(histamine_ci_eval) && histamine_ci_eval$sens_adequate,
  !is.null(codeine_ci_eval) && codeine_ci_eval$sens_adequate,
  !is.null(nacl_ci_eval) && nacl_ci_eval$spec_adequate
))

cat("\n=== OVERALL DEVICE ASSESSMENT ===\n")
cat(sprintf("Point estimate benchmarks met: %d/3\n", total_benchmarks_met))
cat(sprintf("Confidence interval benchmarks met: %d/3\n", total_ci_benchmarks_met))

# Final recommendation
if(total_benchmarks_met == 3 && total_ci_benchmarks_met == 3) {
  final_recommendation <- "PASS - Device meets all performance benchmarks with adequate precision"
} else if(total_benchmarks_met >= 2 && total_ci_benchmarks_met >= 2) {
  final_recommendation <- "CONDITIONAL PASS - Device meets most benchmarks with minor limitations"
} else {
  final_recommendation <- "REQUIRES IMPROVEMENT - Device performance below acceptable thresholds"
}

cat(sprintf("\nFINAL RECOMMENDATION: %s\n", final_recommendation))

# Key findings summary
cat("\n=== KEY FINDINGS ===\n")

# Histamine findings
if(!is.null(histamine_roc_results)) {
  cat(sprintf("• Histamine: AUC = %.3f, Sensitivity = %.1f%%, Correlation = %.3f\n",
              histamine_roc_results$auc_value, histamine_metrics$sensitivity*100, 
              ifelse(!is.null(histamine_agreement), histamine_agreement$correlation, NA)))
}

# Codeine findings
if(!is.null(codeine_roc_results)) {
  cat(sprintf("• Codeine: AUC = %.3f, Sensitivity = %.1f%%, Correlation = %.3f\n",
              codeine_roc_results$auc_value, codeine_metrics$sensitivity*100,
              ifelse(!is.null(codeine_agreement), codeine_agreement$correlation, NA)))
}

# NaCl findings
cat(sprintf("• NaCl: Specificity = %.1f%%, False Positive Rate = %.1f%%\n",
            nacl_metrics$specificity*100, nacl_metrics$fp_rate*100))

# Technical recommendations
cat("\n=== TECHNICAL RECOMMENDATIONS ===\n")

if(!hist_sens_adequate) {
  cat("• Consider lowering device threshold for histamine detection\n")
}

if(!cod_sens_adequate) {
  cat("• Consider lowering device threshold for codeine detection\n")
}

if(!nacl_spec_adequate) {
  cat("• Investigate causes of false positive reactions with NaCl\n")
}

# Study limitations and future work
cat("\n=== STUDY LIMITATIONS AND FUTURE WORK ===\n")
cat("• Sample sizes may limit precision of estimates\n")
cat("• Single-center study limits generalizability\n")
cat("• Consider multi-center validation study\n")
cat("• Evaluate device performance across different patient populations\n")
cat("• Assess long-term reliability and reproducibility\n")

cat("\n=== ANALYSIS COMPLETED ===\n")
cat(sprintf("Date: %s\n", Sys.Date()))
cat("All diagnostic performance metrics calculated and validated\n")
cat("Complete analysis includes:\n")
cat("• Classification accuracy assessment\n")
cat("• ROC analysis and optimal thresholds\n")
cat("• Inter-method agreement analysis\n")
cat("• Prevalence impact on predictive values\n")
cat("• Statistical significance testing\n")
cat("• Confidence interval precision evaluation\n")
cat("• Clinical utility assessment\n")
cat("• Power and sample size considerations\n")

cat("\n", rep("=", 80), "\n")
