library(plm)
library(lmtest)
library(lfe)
library(car)

# Function to perform the Hausman-Wu test and handle errors
HausWutest_print <- function(Hausman_reg, first_stage) {
  HausWutest <- lmtest::waldtest(
    Hausman_reg, . ~ . - first_stage$residuals, test = "F",
    vcov = vcovHC(Hausman_reg, type = "HC0", cluster = c("group", "time"))
  )
  return(HausWutest)
}

waldtest_weak_instr_printExp <- function(fs, IV_type) {
  # Perform Wald test with white2 standard errors
  if (IV_type == "reviews") {
    waldtest_weak_instr <- lmtest::waldtest(
      fs, . ~ . - num_reviews_tw_secondneibmean_exp - num_reviews_fb_secondneibmean_exp - num_reviews_ig_secondneibmean_exp, test = "F", 
      vcov = vcovHC(fs, type = "HC0", cluster = c("group", "time"))
    )
  } else {
    waldtest_weak_instr <- lmtest::waldtest(
      fs, . ~ . - inv_visits_secondneibmean_exp, test = "F", 
      vcov = vcovHC(fs, type = "HC0", cluster = c("group", "time"))
    )
  }
  return(as.numeric(waldtest_weak_instr[2, ]))
}

waldtest_weak_instr_print <- function(fs, IV_type) {
  # Perform Wald test with white2 standard errors
  if (IV_type == "reviews") {
    waldtest_weak_instr <- lmtest::waldtest(
      fs, . ~ . - num_reviews_tw_secondneibmean - num_reviews_fb_secondneibmean,
      test = "F", vcov = vcovHC(fs, type = "HC0", cluster = c("group", "time"))
    )
  } else {
    waldtest_weak_instr <- lmtest::waldtest(
      fs, . ~ . - inv_visits_secondneibmean,
      test = "F", vcov = vcovHC(fs, type = "HC0", cluster = c("group", "time"))
    )
  }
  
  return(as.numeric(waldtest_weak_instr[2, ]))
}

generate_estimates <- function(type, IV_type) {
  print(c("working on ", type, " ", IV_type))
  
  # Define the variables used in the model
  common_vars <- c(
    "num_reviews_tw_neibmean", "num_reviews_fb_neibmean", "num_reviews_ig_neibmean",
    "localized_tw_reviews_60_days", "localized_fb_reviews_60_days", "localized_ig_reviews_60_days"
  )
  
  common_varsExp <- c(
    "num_reviews_tw_neibmean_exp", "num_reviews_fb_neibmean_exp", "num_reviews_ig_neibmean_exp",
    "localized_tw_reviews_60_days", "localized_fb_reviews_60_days", "localized_ig_reviews_60_days"
  )
  
  commonVars <- if (grepl("exp", type)) common_varsExp else common_vars
  
  if (IV_type == "reviews") {
    var_sets <- list(
      fe_reviews = c(common_vars, "num_reviews_tw_secondneibmean", "num_reviews_fb_secondneibmean"),
      fe_exp_reviews = c(common_varsExp, "num_reviews_tw_secondneibmean_exp", "num_reviews_fb_secondneibmean_exp")
    )
    
    var_sets_IV <- list(
      fe_reviews = c("num_reviews_tw_secondneibmean", "num_reviews_fb_secondneibmean"),
      fe_exp_reviews = c("num_reviews_tw_secondneibmean_exp", "num_reviews_fb_secondneibmean_exp")
    )
  } else {
    var_sets <- list(
      fe_reviews = c(common_vars, "inv_visits_secondneibmean"),
      fe_exp_reviews = c(common_varsExp, "inv_visits_secondneibmean_exp")
    )
    
    var_sets_IV <- list(
      fe_reviews = c("inv_visits_secondneibmean"),
      fe_exp_reviews = c("inv_visits_secondneibmean_exp")
    )
  }
  
  vars <- var_sets[[type]]
  vars_IV <- var_sets_IV[[type]]
  var_of_interest <- if (grepl("exp", type)) "inv_visits_exp" else "inv_visits"
  
  # Generate formulas for first_stage, f_plm, and Sargan_reg
  formula_first_stage <- as.formula(paste(var_of_interest, "~", paste(vars, collapse = " + ")))
  formula_f_plm <- as.formula(paste("visits_by_day ~", paste(c(var_of_interest, commonVars), collapse = " + "), "|", paste(vars, collapse = " + ")))
  formula_Sargan_reg <- as.formula(paste("residuals(f_plm) ~", paste(vars_IV, collapse = " + ")))
  formula_f_plm_simple <- as.formula(paste("visits_by_day ~", paste(c(commonVars), collapse = " + ")))
  
  first_stage <- plm(formula_first_stage, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
  f_plm <- plm(formula_f_plm, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
  Sargan_reg <- plm(formula_Sargan_reg, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
  f_plm_simple <- plm(formula_f_plm_simple, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
  
  return(list("first_stage" = first_stage, "f_plm" = f_plm, "Sargan_reg" = Sargan_reg, "f_plm_simple" = f_plm_simple))
}


# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R
IV_test <- function(type, IV_type) {
  # print(c("working on ", type))
  indx <- c("PLACEKEY", "date")
  rest <- generate_estimates(type, IV_type)
  first_stage <- rest$first_stage
  f_plm <- model_summary(rest$f_plm)
  Sargan_reg <- rest$Sargan_reg
  f_plm_simple <- model_summary(rest$f_plm_simple)
  
  if (type %in% c("fe_reviews", "fe_reviews1", "fe_reviews2", "fe_reviews3", "fe_reviews4", "fe_reviews5", "fe_reviews6")) {
    Hausman_reg <- plm(visits_by_day ~ inv_visits + num_reviews_tw_neibmean + num_reviews_fb_neibmean + num_reviews_ig_neibmean +
                         localized_tw_reviews_60_days + localized_fb_reviews_60_days + localized_ig_reviews_60_days + first_stage$residuals,
                       data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
    
    waldtest_weak_instr <- waldtest_weak_instr_print(first_stage, IV_type)
    
    # Define the variables to check
    if (IV_type == "reviews") {
      variables <- c("num_reviews_tw_secondneibmean", "num_reviews_fb_secondneibmean")
    } else {
      variables <- c("inv_visits_secondneibmean")
    }
  }
  
  if (type %in% c("fe_exp_reviews", "fe_exp_reviews1", "fe_exp_reviews2", "fe_exp_reviews3", "fe_exp_reviews4", "fe_exp_reviews5", "fe_exp_reviews6")) {
    Hausman_reg <- plm(
      visits_by_day ~ inv_visits_exp + num_reviews_tw_neibmean_exp + num_reviews_fb_neibmean_exp + num_reviews_ig_neibmean_exp + 
        localized_tw_reviews_60_days + localized_fb_reviews_60_days + localized_ig_reviews_60_days + first_stage$residuals,
      data = Panel.set_dif, model = "within", effect = "twoways", index = indx
    )
    
    waldtest_weak_instr <- waldtest_weak_instr_printExp(first_stage, IV_type)
    
    # Define the variables to check
    if (IV_type == "reviews") {
      variables <- c("num_reviews_tw_secondneibmean_exp", "num_reviews_fb_secondneibmean_exp")
    } else {
      variables <- c("inv_visits_secondneibmean_exp")
    }
  }
  
  
  first_stage_sum <- model_summary(first_stage)
  Sargan_test <- summary(Sargan_reg)$r.squared * nrow(Panel.set_dif)
  
  # Initialize the counter for significant variables
  num_sig <- 0
  
  # Loop over the variables
  for (var in variables) {
    if (var %in% rownames(coef(first_stage_sum))) {
      num_sig <- num_sig + sum(coef(first_stage_sum)[var, 4] < 0.05)
    }
  }
  
  first_stage_coef <- coef(first_stage_sum)
  Sargan_test_result <- Sargan_test
  Sargan_test_pvalue <- 1 - pchisq(Sargan_test, 2)
  HausWutest <- HausWutest_print(Hausman_reg, first_stage)
  
  
  if (type %in% c("fe_exp_reviews", "fe_exp_reviews1", "fe_exp_reviews2", "fe_exp_reviews3", "fe_exp_reviews4", "fe_exp_reviews5", "fe_exp_reviews6")) {
    if (IV_type == "reviews") {
      rows_to_check <- c("num_reviews_fb_secondneibmean_exp", "num_reviews_tw_secondneibmean_exp")
    } else {
      rows_to_check <- c("inv_visits_secondneibmean_exp")
    }
  } else {
    if (IV_type == "reviews") {
      rows_to_check <- c("num_reviews_fb_secondneibmean", "num_reviews_tw_secondneibmean")
    } else {
      rows_to_check <- c("inv_visits_secondneibmean")
    }
  }
  
  # Check if any of the rows are missing in first_stage_coef
  if (any(!rows_to_check %in% rownames(first_stage_coef))) {
    # Add NA vectors for the missing rows in first_stage_coef
    missing_rows <- rows_to_check[!rows_to_check %in% rownames(first_stage_coef)]
    for (row in missing_rows) {
      first_stage_coef <- rbind(first_stage_coef, rep(NA, ncol(first_stage_coef)))
      rownames(first_stage_coef)[nrow(first_stage_coef)] <- row
    }
  }
  
  return_list <- list(
    "coef" = f_plm$coefficients,
    "first_stage_coef" = first_stage_coef,
    "type" = type,
    "df" = f_plm$df,
    "test" = c(waldtest_weak_instr, as.numeric(HausWutest[2, ]), Sargan_test_result, Sargan_test_pvalue),
    "num_sig" = num_sig,
    "rsq" = f_plm$r.squared,
    "adjrsq" = f_plm$adj.r.squared,
    "rsq_simple" = f_plm_simple$r.squared,
    "adjrsq_simple" = f_plm_simple$adj.r.squared
  )
  return(return_list)
}

model_summary <- function(fs) {
  ## try except block for error handling
  model_result = coeftest(fs, vcov = vcovHC(fs, type = "HC0", cluster = c("group", "time")))
  model_summary_tmp = summary(fs)
  return(list(
    "coefficients" = model_result, "r.squared" = as.numeric(model_summary_tmp$r.squared["rsq"]),
    "adj.r.squared" = as.numeric(model_summary_tmp$r.squared["adjrsq"]), "df" = as.numeric(model_summary_tmp$df)
  ))
}

data_path <- "C:/Users/muhammadawais.naeem/Downloads/business_data"
focal_store_name <- "The Cheesecake Factory"
focal_store_path <- file.path(data_path, focal_store_name)
file_list <- list.files(focal_store_path)

result_dir <- file.path(data_path, "Result_Sig_Ralph_Lauren", focal_store_name)
dir.create(result_dir) # create a result directory inside the focal store directory

cheesecake_file_list <- c('Ralph Lauren.csv',
                          "Spencer's.csv")

file_list <- cheesecake_file_list
print(file_list)

for (fi in file_list) {
  fileName <- fi
  file_path <- file.path(focal_store_path, fileName)
  
  print('--------------------------------------------------------------------------')
  print(paste("File: ", fileName))
  
  data <- read.csv(file_path)
  data <- data[!is.na(data[1:dim(data)[1], "inv_visits"]), ]
  data$date <- as.Date(data[,'date'], format = "%Y-%m-%d")
  unique_focalstores = unique(data$PLACEKEY)
  
  # Making the log entries for Ralph Lauren and Spencer
  data$num_reviews_fb_secondneibmean <- log(data$num_reviews_fb_secondneibmean+0.1, 2)
  data$num_reviews_tw_secondneibmean <- log(data$num_reviews_tw_secondneibmean+0.1, 2)
  data$num_reviews_ig_secondneibmean <- log(data$num_reviews_ig_secondneibmean+0.1, 2)

  data$num_reviews_fb_secondneibmean_exp <- log(data$num_reviews_fb_secondneibmean_exp+0.1, 2)
  data$num_reviews_tw_secondneibmean_exp <- log(data$num_reviews_tw_secondneibmean_exp+0.1, 2)
  data$num_reviews_ig_secondneibmean_exp <- log(data$num_reviews_ig_secondneibmean_exp+0.1, 2)
  
  # print(paste('num_reviews_fb_secondneibmean - num_reviews_tw_secondneibmean', cor(data$num_reviews_fb_secondneibmean, data$num_reviews_tw_secondneibmean)))
  # print(paste('num_reviews_tw_secondneibmean - num_reviews_ig_secondneibmean', cor(data$num_reviews_tw_secondneibmean, data$num_reviews_ig_secondneibmean)))
  # print(paste('num_reviews_ig_secondneibmean - num_reviews_fb_secondneibmean', cor(data$num_reviews_ig_secondneibmean, data$num_reviews_fb_secondneibmean)))
  # 
  # print(paste('num_reviews_fb_secondneibmean_exp - num_reviews_tw_secondneibmean_exp', cor(data$num_reviews_fb_secondneibmean_exp, data$num_reviews_tw_secondneibmean_exp)))
  # print(paste('num_reviews_tw_secondneibmean_exp - num_reviews_ig_secondneibmean_exp', cor(data$num_reviews_tw_secondneibmean_exp, data$num_reviews_ig_secondneibmean_exp)))
  # print(paste('num_reviews_ig_secondneibmean_exp - num_reviews_fb_secondneibmean_exp', cor(data$num_reviews_ig_secondneibmean_exp, data$num_reviews_fb_secondneibmean_exp)))
  
  # Defining some variables to calculate statistical significance
  beta_reviews <- 0
  beta_reviews_exp <- 0
  beta_visits <- 0
  beta_visits_exp <- 0
  
  stderror_reviews <- 0
  stderror_reviews_exp <- 0
  stderror_visits <- 0
  stderror_visits_exp <- 0
  
  print(paste("Unique Focal Stores:", length(unique_focalstores)))
  print(paste("Inv Visits Sum:", sum(data[, "inv_visits"])))
  print(paste("Inv Visits Exp Sum:", sum(data[, "inv_visits_exp"])))
  
  res <- list()
  stat_sig <- list()
  
  # Only taking where the focal stores were more than 3
  if ((length(unique_focalstores) >= 3) & (sum(data[, "inv_visits"]) > 0) & (sum(data[, "inv_visits_exp"]) > 0)) {
    
    fileName <- strsplit(fileName, ".", fixed=TRUE)[[1]][1] #only get name of the neighboring store without .csv
    data[is.na(data)] <- 0
    Panel.set_dif <- pdata.frame(data[data$date >= as.Date('2019-03-02'), ], index = c("PLACEKEY", "date"), drop.index = TRUE)
    
    #-----------------------------------------------------------------------------------------------------------------------------
    # Setting the column names for the output
    tmp <- c('filename', 'type',
             'X_Estimate', 'X_Std. Error', 'X_t value', 'X_Pr(>|t|)',
             'IV_firststage_reviews_tw_Estimate', 'IV_firststage_reviews_tw_Std. Error', 'IV_firststage_reviews_tw_t value', 'IV_firststage_reviews_tw_Pr(>|t|)',
             'IV_firststage_reviews_fb_Estimate', 'IV_firststage_reviews_fb_Std. Error', 'IV_firststage_reviews_fb_t value', 'IV_firststage_reviews_fb_Pr(>|t|)',
             'IV_firststage_reviews_ig_Estimate', 'IV_firststage_reviews_ig_Std. Error', 'IV_firststage_reviews_ig_t value', 'IV_firststage_reviews_ig_Pr(>|t|)',
             'IV_firststage_visits_Estimate', 'IV_firststage_visits_Std. Error', 'IV_firststage_visits_t value', 'IV_firststage_visits_Pr(>|t|)',
             'Y_df', 'Y_df', 'Y_df',
             'WaldTest_Res.Df', 'WaldTest_Df', 'WaldTest_F', 'WaldTest_Pr(>F)',
             'HausWutest_Res.Df', 'HausWutest_Df', 'HausWutest_F', 'HausWutest_Pr(>F)',
             'Sargan_result_rsq', 'Sargan_result_adjrsq', 'Sargan_pvalue_rsq', 'Sargan_pvalue_adjrsq',
             'num_sig_variables', 'Y_r.squared', 'Y_adj.r.squared', 'Y_simple_r.squared', 'Y_simple_adj.r.squared')
    
    res <- rbind(res, tmp)
    
    # -----------------------------------------------------------------------------------------------------------------------------
    # Calculations for OLS (linear)
    m_ols <- plm(visits_by_day ~ localized_tw_reviews_60_days + localized_fb_reviews_60_days + localized_ig_reviews_60_days +
                   num_reviews_tw_neibmean + num_reviews_fb_neibmean + num_reviews_ig_neibmean +
                   inv_visits + num_reviews_tw_secondneibmean + num_reviews_fb_secondneibmean + num_reviews_ig_secondneibmean,
                 data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
    
    tryCatch(
      {
        m_ols <- model_summary(m_ols)
        tmp <- c(fileName,
                 "ols",
                 unname(m_ols$coefficients["inv_visits", ]),
                 rep(FALSE, 16),
                 unname(m_ols$df),
                 rep(FALSE, 13),
                 unname(m_ols$r.squared),
                 unname(m_ols$adj.r.squared),
                 rep(FALSE, 2)
        )
      },
      error = function(e) {
        # Handle the error
        print(paste0("Error occurred for m_ols", e))
        tmp <- c(fileName, "ols", rep(FALSE, 40))
      }
    )
    
    res <- rbind(res, tmp)
    
    # ----------------------------------------------------------------------------------------------------------------------------
    # Calculations for 2SLS (linear) + IV (reviews from 2nd degree)
    
    # Loop through fe_reviews objects
    tryCatch(
      {
        fe_reviews <- IV_test(type = "fe_reviews", IV_type="reviews")
        tmp <- c(
          fileName, paste0(fe_reviews$type, "_reviews"), unname(fe_reviews$coef["inv_visits", ]),
          unname(fe_reviews$first_stage_coef["num_reviews_tw_secondneibmean", ]),
          unname(fe_reviews$first_stage_coef["num_reviews_fb_secondneibmean", ]),
          rep(FALSE, 8),
          unname(fe_reviews$df),
          unname(fe_reviews$test),
          unname(fe_reviews$num_sig),
          unname(fe_reviews$rsq),
          unname(fe_reviews$adjrsq),
          unname(fe_reviews$rsq_simple),
          unname(fe_reviews$adjrsq_simple)
        )
        
        beta_reviews <- fe_reviews$coef["inv_visits", 1] #Estimate
        stderror_reviews <- fe_reviews$coef["inv_visits", 2] # Std. Error
      },
      error = function(e) {
        # Handle the error
        tmp <- c(fileName, "fe_reviews", rep(FALSE, 40))
        print(paste0("Error occurred for fe_reviews: ", e))
      }
    )
    
    res <- rbind(res, tmp)
    
    # ----------------------------------------------------------------------------------------------------------------------------
    # Calculations for 2SLS (linear) + IV (visits from 2nd degree)
    
    # Loop through fe_reviews objects
    tryCatch(
      {
        fe_reviews <- IV_test(type = "fe_reviews", IV_type="visits")
        tmp <- c(
          fileName, paste0(fe_reviews$type, "_visits"), unname(fe_reviews$coef["inv_visits", ]),
          rep(FALSE, 12),
          unname(fe_reviews$first_stage_coef["inv_visits_secondneibmean", ]),
          unname(fe_reviews$df),
          unname(fe_reviews$test),
          unname(fe_reviews$num_sig),
          unname(fe_reviews$rsq),
          unname(fe_reviews$adjrsq),
          unname(fe_reviews$rsq_simple),
          unname(fe_reviews$adjrsq_simple)
        )
        
        beta_visits <- fe_reviews$coef["inv_visits", 1] #Estimate
        stderror_visits <- fe_reviews$coef["inv_visits", 2] # Std. Error
        
      },
      error = function(e) {
        # Handle the error
        tmp <- c(fileName, "fe_reviews", rep(FALSE, 40))
        print(paste0("Error occurred for fe_reviews: ", e))
      }
    )
    
    res <- rbind(res, tmp)
    
    # ----------------------------------------------------------------------------------------------------------------------------
    # Calculations for OLS (exponential)
    m_olsExp <- plm(visits_by_day ~ localized_tw_reviews_60_days + localized_fb_reviews_60_days + localized_ig_reviews_60_days +
                      num_reviews_tw_neibmean_exp + num_reviews_fb_neibmean_exp + num_reviews_ig_neibmean_exp + inv_visits_exp +
                      num_reviews_tw_secondneibmean_exp + num_reviews_fb_secondneibmean_exp + num_reviews_ig_secondneibmean_exp,
                    data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
    
    ## Try except block for the following code block
    tryCatch(
      {
        m_olsExp <- model_summary(m_olsExp)
        tmp <- c(
          fileName, "m_olsExp",
          unname(m_olsExp$coefficients["inv_visits_exp", ]),
          rep(FALSE, 16),
          unname(m_olsExp$df),
          rep(FALSE, 13),
          unname(m_olsExp$r.squared),
          unname(m_olsExp$adj.r.squared),
          rep(FALSE, 2)
        )
      },
      error = function(e) {
        # Handle the error
        print(paste0("Error occurred for m_olsExp", e))
        tmp <- c(fileName, "m_olsExp", rep(FALSE, 40))
      }
    )
    
    res <- rbind(res, tmp)
    
    # ----------------------------------------------------------------------------------------------------------------------------
    # Calculations for 2SLS (exponential) + IV (reviews from 2nd degree)
    # Loop through fe_exp_reviews objects
    tryCatch(
      {
        fe_exp_reviews <- IV_test(type = "fe_exp_reviews", IV_type="reviews")
        tmp <- c(
          fileName, paste0(fe_exp_reviews$type, "_reviews"),
          unname(fe_exp_reviews$coef["inv_visits_exp", ]),
          unname(fe_exp_reviews$first_stage_coef["num_reviews_tw_secondneibmean_exp", ]),
          unname(fe_exp_reviews$first_stage_coef["num_reviews_fb_secondneibmean_exp", ]),
          rep(FALSE, 8),
          unname(fe_exp_reviews$df),
          unname(fe_exp_reviews$test),
          unname(fe_exp_reviews$num_sig),
          unname(fe_exp_reviews$rsq),
          unname(fe_exp_reviews$adjrsq),
          unname(fe_exp_reviews$rsq_simple),
          unname(fe_exp_reviews$adjrsq_simple)
        )
        
        beta_reviews_exp <- fe_exp_reviews$coef["inv_visits_exp", 1] #Estimate
        stderror_reviews_exp <-fe_exp_reviews$coef["inv_visits_exp", 2] # Std. Error
        
      },
      error = function(e) {
        # Handle the error
        tmp <- c(fileName, "fe_exp_reviews", rep(FALSE, 40))
        print(paste0("Error occurred for fe_exp_reviews", e))
      }
    )
    
    res <- rbind(res, tmp)
    
    # ----------------------------------------------------------------------------------------------------------------------------
    # Calculations for 2SLS (exponential) + IV (visits from 2nd degree)
    # Loop through fe_exp_reviews objects
    tryCatch(
      {
        fe_exp_reviews <- IV_test(type = "fe_exp_reviews", IV_type="visits")
        tmp <- c(
          fileName, paste0(fe_exp_reviews$type, "_visits"),
          unname(fe_exp_reviews$coef["inv_visits_exp", ]),
          rep(FALSE, 12),
          unname(fe_exp_reviews$first_stage_coef["inv_visits_secondneibmean_exp", ]),
          unname(fe_exp_reviews$df),
          unname(fe_exp_reviews$test),
          unname(fe_exp_reviews$num_sig),
          unname(fe_exp_reviews$rsq),
          unname(fe_exp_reviews$adjrsq),
          unname(fe_exp_reviews$rsq_simple),
          unname(fe_exp_reviews$adjrsq_simple)
        )
        
        beta_visits_exp <- fe_exp_reviews$coef["inv_visits_exp", 1] #Estimate
        stderror_visits_exp <- fe_exp_reviews$coef["inv_visits_exp", 2] # Std. Error
      },
      error = function(e) {
        # Handle the error
        tmp <- c(fileName, "fe_exp_reviews", rep(FALSE, 40))
        print(paste0("Error occurred for fe_exp_reviews", e))
      }
    )
    
    res <- rbind(res, tmp)
    
    # Calculate z-score and p-value for both normal and exponential comparisons between reviews and visits
    z_stat <- (beta_reviews - beta_visits)/sqrt(stderror_reviews^2 + stderror_visits^2)
    p_value <- 2 * (1 - pnorm(abs(z_stat)))
    
    z_stat_exp <- (beta_reviews_exp - beta_visits_exp)/sqrt(stderror_reviews_exp^2 + stderror_visits_exp^2)
    p_value_exp <- 2 * (1 - pnorm(abs(z_stat_exp)))
    
    tmp <- c('fileName', 'z_stat', 'p_value', 'z_stat_exp', 'p_value_exp')
    stat_sig <- rbind(stat_sig, tmp)
    tmp <- c(fileName, z_stat, p_value, z_stat_exp, p_value_exp)
    stat_sig <- rbind(stat_sig, tmp)
    
    # Saving both the results and statistics
    result_file_path <- file.path(result_dir, paste0(fileName,"_result.csv"))
    stat_file_path <- file.path(result_dir, paste0(fileName,"_stat.csv"))
    
    write.csv(res, result_file_path)
    write.csv(stat_sig, stat_file_path)
  } else {
    print("Condition Not Satisfied")
  }
}
