library(plm)
library(lmtest)
library(lfe)
library(car)
# why not r square in iv: http://edrub.in/ARE212/Spring2017/Section11/section11.pdf
# nohup Rscript 3_sls_local_network.r processed/ma_target > ma_target.out &
# nohup Rscript 3_sls_local_network.r processed/ma_sephora > ma_sephora.out &
# nohup Rscript 3_sls_local_network.r processed/ma_the_cheesecake_factory > ma_the_cheesecake_factory.out &

# nohup Rscript ./code/3sls_local_network_only.r processed/ma_target > ma_target.out &
# nohup Rscript ./code/3sls_local_network_only.r processed/ma_sephora > ma_sephora.out &
# nohup Rscript ./code/3sls_local_network_only.r processed/ma_the_cheesecake_factory > ma_the_cheesecake_factory.out &
# scp -r yl36578@austin.utexas.edu@msb-iromsrv02.austin.utexas.edu:~/veraset/processed/*tt.csv /Users/yleng/Dropbox\ \(MIT\)/projects/veserat/logs


# Function to perform the Hausman-Wu test and handle errors
HausWutest_print <- function(Hausman_reg, first_stage) {
    HausWutest <- lmtest::waldtest(
        Hausman_reg, . ~ . - first_stage$residuals, test = "F",
        vcov = vcovHC(Hausman_reg, type = "HC0", cluster = c("group", "time"))
    )
    return(HausWutest)
}

# Helper function to generate the formula for the first stage summary
generate_formula <- function(response, excluded_vars) {
    formula <- as.formula(paste(response, paste("-", excluded_vars, collapse = " - "), sep = "~"))
    return(formula)
}

waldtest_weak_instr_printExp <- function(fs) {
    # Perform Wald test with white2 standard errors
    waldtest_weak_instr <- lmtest::waldtest(
        fs, . ~ . - num_reviews_dif_SecondNeibMeanExp - num_reviews_ig_dif_SecondNeibMeanExp - num_reviews_fb_dif_SecondNeibMeanExp, test = "F", 
        vcov = vcovHC(fs, type = "HC0", cluster = c("group", "time"))
    )
    return(as.numeric(waldtest_weak_instr[2, ]))
}

waldtest_weak_instr_print <- function(fs) {
    # Perform Wald test with white2 standard errors
    waldtest_weak_instr <- lmtest::waldtest(
        fs, . ~ . - num_reviews_dif_SecondNeibMean - num_reviews_ig_dif_SecondNeibMean - num_reviews_fb_dif_SecondNeibMean,
        test = "F", vcov = vcovHC(fs, type = "HC0", cluster = c("group", "time"))
    )
    return(as.numeric(waldtest_weak_instr[2, ]))
}

generate_estimates <- function(type) {
    print(c("working on ", type))

    # Define the variables used in the model
    common_vars <- c(
        "num_reviews_dif_NeibMean", "num_reviews_ig_dif_NeibMean", "num_reviews_fb_dif_NeibMean",
        "num_reviews_dif", "num_reviews_fb_dif", "num_reviews_ig_dif", "inv_visits_dif"
    )

    common_varsExp <- c(
        "num_reviews_dif_NeibMeanExp", "num_reviews_ig_dif_NeibMeanExp", "num_reviews_fb_dif_NeibMeanExp",
        "num_reviews_dif", "num_reviews_fb_dif", "num_reviews_ig_dif", "inv_exp_visits"
    )

    commonVars <- if (grepl("Exp", type)) common_varsExp else common_vars

    var_sets <- list(
        fe_dif = c(common_vars, "num_reviews_dif_SecondNeibMean", "num_reviews_ig_dif_SecondNeibMean", "num_reviews_fb_dif_SecondNeibMean"),
        fe_dif1 = c(common_vars, "num_reviews_ig_dif_SecondNeibMean", "num_reviews_fb_dif_SecondNeibMean"),
        fe_dif2 = c(common_vars, "num_reviews_dif_SecondNeibMean", "num_reviews_fb_dif_SecondNeibMean"),
        fe_dif3 = c(common_vars, "num_reviews_dif_SecondNeibMean", "num_reviews_ig_dif_SecondNeibMean"),
        fe_dif4 = c(common_vars, "num_reviews_dif_SecondNeibMean"),
        fe_dif5 = c(common_vars, "num_reviews_ig_dif_SecondNeibMean"),
        fe_dif6 = c(common_vars, "num_reviews_fb_dif_SecondNeibMean"),
        fe_difExp = c(common_varsExp, "num_reviews_dif_SecondNeibMeanExp", "num_reviews_ig_dif_SecondNeibMeanExp", "num_reviews_fb_dif_SecondNeibMeanExp"),
        fe_difExp1 = c(common_varsExp, "num_reviews_ig_dif_SecondNeibMeanExp", "num_reviews_fb_dif_SecondNeibMeanExp"),
        fe_difExp2 = c(common_varsExp, "num_reviews_dif_SecondNeibMeanExp", "num_reviews_fb_dif_SecondNeibMeanExp"),
        fe_difExp3 = c(common_varsExp, "num_reviews_dif_SecondNeibMeanExp", "num_reviews_ig_dif_SecondNeibMeanExp"),
        fe_difExp4 = c(common_varsExp, "num_reviews_dif_SecondNeibMeanExp"),
        fe_difExp5 = c(common_varsExp, "num_reviews_ig_dif_SecondNeibMeanExp"),
        fe_difExp6 = c(common_varsExp, "num_reviews_fb_dif_SecondNeibMeanExp")
    )

    print('test')
    vars <- var_sets[[type]]
    var_of_interest <- if (grepl("Exp", type)) "X.visits_dif_NeibMeanExp" else "X.visits_dif_NeibMean"

    # Generate formulas for first_stage, f_plm, and Sargan_reg
    formula_first_stage <- as.formula(paste(var_of_interest, "~", paste(vars, collapse = " + ")))
    print('test')

    formula_f_plm <- as.formula(paste("X.visits_dif ~", paste(c(var_of_interest, commonVars), collapse = " + "), "|", paste(vars, collapse = " + ")))
    print('test')

    formula_Sargan_reg <- as.formula(paste("residuals(first_stage) ~", paste(vars, collapse = " + ")))

    print('test')

    first_stage <- plm(formula_first_stage, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
    f_plm <- plm(formula_f_plm, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)
    Sargan_reg <- plm(formula_Sargan_reg, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)

    return(list("first_stage" = first_stage, "f_plm" = f_plm, "Sargan_reg" = Sargan_reg))
}


# http://eclr.humanities.manchester.ac.uk/index.php/IV_in_R
IV_test <- function(type) {
    # print(c("working on ", type))
    indx <- c("safegraph_place_id", "day")
    print('asdfasdfasdf')
    rest <- generate_estimates(type)
    first_stage <- rest$first_stage
    f_plm <- model_summary(rest$f_plm)
    Sargan_reg <- rest$Sargan_reg

    if (type %in% c("fe_dif", "fe_dif1", "fe_dif2", "fe_dif3", "fe_dif4", "fe_dif5", "fe_dif6")) {
        Hausman_reg <- plm(X.visits_dif ~ X.visits_dif_NeibMean + num_reviews_dif_NeibMean +
            num_reviews_fb_dif_NeibMean + num_reviews_ig_dif_NeibMean + num_reviews_dif +
            num_reviews_fb + num_reviews_ig + inv_visits_dif + first_stage$residuals, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)

        waldtest_weak_instr <- waldtest_weak_instr_print(first_stage)

        # Define the variables to check
        variables <- c("num_reviews_dif_SecondNeibMean", "num_reviews_fb_dif_SecondNeibMean", "num_reviews_ig_dif_SecondNeibMean")
    }
    if (type %in% c("fe_difExp", "fe_difExp1", "fe_difExp2", "fe_difExp3", "fe_difExp4", "fe_difExp5", "fe_difExp6")) {
        Hausman_reg <- plm(
            X.visits_dif ~ X.visits_dif_NeibMeanExp + num_reviews_dif_NeibMeanExp +
                num_reviews_fb_dif_NeibMeanExp + num_reviews_ig_dif_NeibMeanExp + num_reviews_dif +
                num_reviews_fb_dif + num_reviews_ig_dif + inv_exp_visits_dif + first_stage$residuals,
            data = Panel.set_dif, model = "within", effect = "twoways", index = indx
        )
        # Define the variables to check
        variables <- c("num_reviews_dif_SecondNeibMeanExp", "num_reviews_fb_dif_SecondNeibMeanExp", "num_reviews_ig_dif_SecondNeibMeanExp")
        waldtest_weak_instr <- waldtest_weak_instr_printExp(first_stage)

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


    if (type %in% c("fe_difExp", "fe_difExp1", "fe_difExp2", "fe_difExp3", "fe_difExp4", "fe_difExp5", "fe_difExp6")) {
        rows_to_check <- c("num_reviews_ig_dif_SecondNeibMeanExp", "num_reviews_fb_dif_SecondNeibMeanExp", "num_reviews_dif_SecondNeibMeanExp")
    } else {
        rows_to_check <- c("num_reviews_ig_dif_SecondNeibMean", "num_reviews_fb_dif_SecondNeibMean", "num_reviews_dif_SecondNeibMean")
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
        "adjrsq" = f_plm$adj.r.squared
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

# 20230605_sephora_nordstrom rack.csv"
# city <- "../processed/ma_sephora"




args <- commandArgs(trailingOnly = TRUE)
for (city in c(args[1])) {
    res <- list()
    file_list <- list.files(city)
    print(paste0('number of files in list', length(file_list)))
    for (fi in file_list) {
        data <- read.csv(paste(city, "/", fi, sep = ""))
        data <- data[!is.na(data[1:dim(data)[1], "X.visits_NeibMean"]), ]
        print(c("length of file", length(res)))
        if ((dim(data)[1] >= 181 * 3 & sum(data[, "X.visits_NeibMean"]) > 0)) {
            fileName <- gsub(",", "", gsub(".csv", "", gsub(city, "", fi)))
            data[is.na(data)] <- 0
            Panel.set_dif <- pdata.frame(data[data$day >= 4, ], index = c("safegraph_place_id", "day"), drop.index = TRUE)

            m_ols <- plm(X.visits ~ num_reviews_NeibMean + num_reviews_ig_NeibMean + num_reviews_fb_NeibMean + X.visits_NeibMean + num_reviews +
                num_reviews_ig + num_reviews_fb + inv_visits + num_reviews_SecondNeibMean + num_reviews_ig_SecondNeibMean + num_reviews_fb_SecondNeibMean, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)

            m_ols_dif <- plm(X.visits_dif ~ num_reviews_dif + num_reviews_fb_dif + num_reviews_ig_dif + num_reviews_dif_NeibMean + num_reviews_ig_dif_NeibMean +
                num_reviews_fb_dif_NeibMean + X.visits_dif_NeibMean + inv_visits_dif + num_reviews_dif_SecondNeibMean +
                num_reviews_ig_dif_SecondNeibMean + num_reviews_fb_dif_SecondNeibMean, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)

                        ## Try except block for the following code block
            tryCatch(
                {
                    m_ols <- model_summary(m_ols)
                    tmp <- c(fileName, "ols", m_ols$coefficients["X.visits_NeibMean", ], rep(FALSE, 12), m_ols$df, rep(FALSE, 13), m_ols$r.squared, m_ols$adj.r.squared)
                    res <- rbind(res, tmp)
                },
                error = function(e) {
                    # Handle the error
                    print(paste0("Error occurred for m_olsExp", e))
                    tmp <- c(fileName, "ols", rep(FALSE, 34))
                }
            )
            res <- rbind(res, tmp)

            tryCatch(
                {
                    m_ols_dif <- model_summary(m_ols_dif)

                    tmp <- c(
                        fileName, "ols_dif", m_ols_dif$coefficients["X.visits_dif_NeibMean", ],
                        rep(FALSE, 12), m_ols_dif$df, rep(FALSE, 13), m_ols_dif$r.squared, m_ols_dif$adj.r.squared
                    )
    
                },
                error = function(e) {
                    # Handle the error
                    print(paste0("Error occurred for ols_dif", e))
                    tmp <- c(fileName, "ols_dif", rep(FALSE, 34))
                }
            )
            res <- rbind(res, tmp)

            
            # Loop through fe_difExp objects
            for (i in c("", 1:6)) {
                tryCatch(
                    {   
                        print(paste0('asdfasdf', "fe_dif", i))
                        fe_dif <- IV_test(type = paste0("fe_dif", i))
                        tmp <- c(
                            fileName, fe_dif$type, fe_dif$coef["X.visits_dif_NeibMean", ],
                            fe_dif$first_stage_coef["num_reviews_dif_SecondNeibMean", ],
                            fe_dif$first_stage_coef["num_reviews_ig_dif_SecondNeibMean", ],
                            fe_dif$first_stage_coef["num_reviews_fb_dif_SecondNeibMean", ],
                            fe_dif$df,
                            fe_dif$test,
                            fe_dif$num_sig,
                            fe_dif$rsq, fe_dif$adjrsq
                        )
                        break
                    },
                    error = function(e) {
                        # Handle the error
                        tmp <- c(fileName, "fe_dif", rep(FALSE, 34))
                        print(paste0("Error occurred for fe_dif", i, e))
                    }
                )
            }


            res <- rbind(res, tmp)

            m_olsExp <- plm(X.visits ~ num_reviews + num_reviews_ig + num_reviews_fb + num_reviews_NeibMeanExp + num_reviews_ig_NeibMeanExp +
                num_reviews_fb_NeibMeanExp + X.visits_NeibMeanExp + inv_exp_visits + num_reviews_SecondNeibMeanExp + num_reviews_ig_SecondNeibMeanExp +
                num_reviews_fb_SecondNeibMeanExp, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)

            m_ols_difExp <- plm(X.visits_dif ~ num_reviews_dif + num_reviews_fb_dif + num_reviews_ig_dif + num_reviews_dif_NeibMeanExp +
                num_reviews_ig_dif_NeibMeanExp + num_reviews_fb_dif_NeibMeanExp + X.visits_dif_NeibMeanExp + inv_exp_visits_dif +
                num_reviews_dif_SecondNeibMeanExp + num_reviews_ig_dif_SecondNeibMeanExp +
                num_reviews_fb_dif_SecondNeibMeanExp, data = Panel.set_dif, model = "within", effect = "twoways", index = indx)


            ## Try except block for the following code block
            tryCatch(
                {
                    m_olsExp <- model_summary(m_olsExp)
                    tmp <- c(
                        fileName, "m_olsExp", m_olsExp$coefficients["X.visits_NeibMeanExp", ],
                        rep(FALSE, 12), m_olsExp$df, rep(FALSE, 13), m_olsExp$r.squared, m_olsExp$adj.r.squared
                    )
                },
                error = function(e) {
                    # Handle the error
                    print(paste0("Error occurred for m_olsExp", e))
                    tmp <- c(fileName, "m_olsExp", rep(FALSE, 34))
                }
            )
            res <- rbind(res, tmp)

            tryCatch(
                {
                    m_ols_difExp <- model_summary(m_ols_difExp)

                    tmp <- c(
                        fileName, "ols_difExp", m_ols_difExp$coefficients["X.visits_dif_NeibMeanExp", ],
                        rep(FALSE, 12), m_ols_difExp$df, rep(FALSE, 13), m_ols_difExp$r.squared, m_ols_difExp$adj.r.squared
                    )
                },
                error = function(e) {
                    # Handle the error
                    print(paste0("Error occurred for ols_difExp", e))
                    tmp <- c(fileName, "ols_difExp", rep(FALSE, 34))
                }
            )
            
            res <- rbind(res, tmp)

            # Loop through fe_difExp objects
            for (i in c("", 1:6)) {
                tryCatch(
                    {
                        fe_difExp <- IV_test(type = paste0("fe_difExp", i))

                        tmp <- c(
                            fileName, fe_difExp$type, fe_difExp$coef["X.visits_dif_NeibMeanExp", ],
                            fe_difExp$first_stage_coef["num_reviews_dif_SecondNeibMeanExp", ],
                            fe_difExp$first_stage_coef["num_reviews_ig_dif_SecondNeibMeanExp", ],
                            fe_difExp$first_stage_coef["num_reviews_fb_dif_SecondNeibMeanExp", ],
                            fe_difExp$df,
                            fe_difExp$test,
                            fe_difExp$num_sig,
                            fe_difExp$rsq, fe_difExp$adjrsq
                        )
                        
                        break
                    },
                    error = function(e) {
                        # Handle the error
                        tmp <- c(fileName, "fe_difExp", rep(FALSE, 34))
                        print(paste0("Error occurred for fe_difExp", i, e))
                    }
                )
            }

            res <- rbind(res, tmp)

            print(length(res))
            print(paste('export to', paste(args[1], 'resulttt.csv', sep = '')))
            write.csv(res, paste(args[1], 'resulttt.csv', sep = ''))
        }
    }
}
print("end of file")



cor_matrix <- cor(Panel.set_dif[,c('num_reviews', 'num_reviews_ig', 'num_reviews_fb', 'num_reviews_NeibMeanExp', 'num_reviews_ig_NeibMeanExp',
                 'num_reviews_fb_NeibMeanExp', 'X.visits_NeibMeanExp', 'inv_exp_visits', 'num_reviews_SecondNeibMeanExp', 'num_reviews_ig_SecondNeibMeanExp',
                 'num_reviews_fb_SecondNeibMeanExp')])
