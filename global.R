# ED50 ############################################################################################

# log_breaks_cstm <- function(x) {10^seq(floor(log10(min(x))), ceiling(log10(max(x))), by = 1)}

display_format <- function(val) {
  if (val != "/") {
    val <- as.numeric(val)
    return(ifelse(val < 0.01, formatC(val, format = "E", digits = 2), format(round(val, digits = 2), nsmall = 2)))
  } else {
    return(val)
  }
}

## Select the best model ---------------------------------------------------

m_select_new <- function(df_temp, fctList_monotnc, fctList_biphsc, const){
  #df_temp <- Data[3,] %>% unnest()
  ## Select the list of functions based the shape of the curves
  if (unique(df_temp$Biphasic) == "Y") {
    fctList <- c(fctList_biphsc)
  } else {
    fctList <- c(fctList_monotnc)
  }
  
  fctList_df <- data.frame(fctList = fctList) %>% 
    separate(fctList, into = c("Fct", "c", "d"), sep = "_") %>% 
    mutate(parms_n = as.numeric(gsub(".*\\.([0-9]+)", "\\1", Fct)),
           parm = pmap(list(c, d, parms_n), function (c,d,n) { if (!is.na(n)) {paste0("fixed = c(", paste(c("NA", c, d, rep("NA", n-3)), collapse = ", "), ")")} else {""}}),
           fctList_f = paste0(Fct, "(", parm, ")"))
  
  fctList_f <- fctList_df$fctList_f
  lenFL <- length(fctList_f)
  
  ## Pick the best fit model
  retMat <- matrix(0, lenFL, 5)
  for (i in 1:lenFL) {
    
    tempObj <- try(eval(parse(text = paste0("drm(Response ~ Conc, data = df_temp, fct = ", 
                                            fctList_df$fctList_f[i], ")"))), silent = FALSE)
    
    if (!inherits(tempObj, "try-error")){
      retMat[i, 1] <- logLik(tempObj)
      retMat[i, 2] <- AIC(tempObj)
      retMat[i, 3] <- BIC(tempObj)
      retMat[i, 4] <- modelFit(tempObj)[2, 5]
      retMat[i, 5] <- summary(tempObj)$"resVar"
    }else{
      retMat[i, 1] <- NA
      retMat[i, 2] <- NA
      retMat[i, 3] <- NA
      retMat[i, 4] <- NA
      retMat[i, 5] <- NA
    }
    
  }
  
  if (!all(is.na(retMat[, 1]))) {
    colnames(retMat) <- c("logLik", "AIC", "BIC", "Lac_of_fit", "resVar")
    
    retMat <- data.frame(retMat, fctList_f = fctList_f)
    retMat <- retMat %>% 
      mutate(rowNmbr = rownames(retMat)) %>% 
      left_join(fctList_df, by = "fctList_f")
    
    if(const == "Lack_of_fit") {
      eval(parse(text = paste0("retMat <- retMat %>% arrange(desc(", const, "))")))
    } else {
      eval(parse(text = paste0("retMat <- retMat %>% arrange(", const, ")")))
    }
    
    fct_select <- as.numeric(retMat$rowNmbr[1])
    df_ret <- fctList_df[fct_select, ]
  } else {
    ## If none of the models can be applied to the data
    df_ret <- matrix(NA, 1, 6) %>% as.data.frame()
    colnames(df_ret) <- c("Fct", "c", "d", "parms_n", "parm", "fctList_f")
    
  }
  ## Change the function name
  df_fct <- data.frame(
    Fct = c("LL.4", "LL.5", "W1.4", "W2.4", "BC.5", "DRC.beta", NA),
    Model = c("Log-logistic\n(4 paras)", "Log-logistic\n(5 paras)", "Weibull I", "Weibull II", "Brain-Cousens", "beta", NA)
  )
  df_ret <- df_ret %>% left_join(df_fct, by = "Fct")
  
  return(df_ret)
  
}

## SG method ---------------------------------------------------------------

# Monotonicity
monotonicity <- function(fittedModel, mindose){
  
  #fittedModel <- opt_mod
  
  dose <- fittedModel$data$Conc
  doseRange <- seq_log(mindose, max(dose), length.out = 1000)
  p <- stats::predict(fittedModel, newdata = data.frame(dose = as.vector(doseRange))) %>% as.data.frame() %>% 
    mutate(dose = doseRange)
  colnames(p) <- c("respns", "conc")
  d <- p$respns[2:length(p$respns)] - p$respns[1:(length(p$respns)-1)]
  
  # find the peak/trough point
  j <- 1
  while (d[j]==0 & (j < nrow(p)-1)) {j <- j+1} # in case some straight line is generated at the beginning of the curve
  i <- j
  while ((d[i]*d[i+1] >= 0) & (i < nrow(p)-1)) {i <- i+1}
  
  # determine whether it is increase/decrease
  f0 = predict(fittedModel, newdata = data.frame(dose = p$conc[j+1]))
  ff = predict(fittedModel, newdata = data.frame(dose = p$conc[i+1]))
  if(ff > f0) {
    return("Up")
  } else {
    if (ff < f0) { return("Down") } else { return("Flat") }
  }
  
}

# Equal variance 
control_variance <- function(model, constant_variance, training_dose){
  res = residuals(model)
  if(constant_variance){
    ctrl_variance = mean(res^2) # MSE - use residuals of all samples
  }else{
    ctrl_variance = mean(res[training_dose == 0]^2) # RMSE - use residuals of all controls samples (dose=0)
  }
  return(ctrl_variance)
}

# Curve df and ED50
compute_ed <- function(opt_mod, rl = 1.349, dataframe, 
                       constantVar, conf_interval, interval_type, bp, minidose, ed50_type) {
  # dataframe <- Data[1, ] %>% unnest()
  # opt_mod <- drm(Response ~ Conc, data = dataframe, fct = LL.4(fixed = c(NA, 0, NA, NA)))
  # plot(opt_mod)
  
  respns <- dataframe$Response
  dose <- dataframe$Conc
  fctName <- summary(opt_mod)[["fctName"]]
  
  if(is.na(fctName)) {
    bmd_val <- matrix(NA, 1, 12)
  } else {
    
    # starting point - adopted from plot.drc()
    if (min(dose) == 0) {
      if (bp == "default") {
        min_dose <- minidose
      } else {
        min_dose <- as.numeric(bp)
      }
    } else {
      min_dose <- min(dose)
    }
    
    # calculate the 50% max response for ed50 estimation
    doseRange <- seq_log(min_dose, max(dose), length.out = 1000)
    if(min(dose) == 0){doseRange <- c(0, doseRange)}
    
    # calculate the curve interval
    conf_interval_cv <- stats::predict(opt_mod, newdata = data.frame(dose = as.vector(doseRange)), interval = "confidence", level = conf_interval) %>% as.data.frame()
    
    # Export the nested data for generate plot
    curve_df <- conf_interval_cv %>% dplyr::select(1) %>% mutate(Conc = doseRange)
    if(min(dose) == 0) {curve_df <- curve_df %>% filter(Conc != 0)}
    colnames(curve_df) <- c("Prediction", "Conc")
    bmd_val <- nest(curve_df, data = everything())
    colnames(bmd_val) <- "Curve_BestFit_data"
    
    # dermine the monotonicity of the curve/the first part of curve
    monotonic_behaviour <- monotonicity(fittedModel = opt_mod, mindose = min_dose)
    
    # Export the info.
    bmd_val <- bmd_val %>% 
      mutate(FctName = gsub("(.*)\\()", "\\1", fctName),
             Monotonicity = monotonic_behaviour)
    
    if (monotonic_behaviour == "Flat") {
      
      temp_res <- matrix(NA, 1, 9) %>% as.data.frame()
      
    } else {
      temp_res <- tryCatch({
        
        # define the doseRange before & after peak/trough
        if(monotonic_behaviour == "Up"){
          doseRange_1st <- c(1: which.max(conf_interval_cv[, 1]))
          doseRange_2nd <- c(which.max(conf_interval_cv[, 1]):nrow(conf_interval_cv))
        }else if (monotonic_behaviour == "Down"){
          doseRange_1st <- c(1: which.min(conf_interval_cv[, 1]))
          doseRange_2nd <- c(which.min(conf_interval_cv[, 1]):nrow(conf_interval_cv))
        }
        
        # calculate the half max response in the first phase under the concs tested in the experiment
        if (ed50_type == "Absolute") {
          half_resp <- 50
        } else {
          half_resp <- max(conf_interval_cv[doseRange_1st, 1]) - (abs(max(conf_interval_cv[doseRange_1st, 1]) - min(conf_interval_cv[doseRange_1st, 1]))/2)
        }

        ### SG Method - approx()
        
        # calculate ed50 and both bounds
        ## left ed50
        half_con <- stats::approx(x = conf_interval_cv[doseRange_1st, 1], y = doseRange[doseRange_1st], xout = half_resp)$y
        half_con_1 <- try(stats::approx(x = conf_interval_cv[doseRange_1st, 2], y = doseRange[doseRange_1st], xout = half_resp)$y)
        half_con_2 <- try(stats::approx(x = conf_interval_cv[doseRange_1st, 3], y = doseRange[doseRange_1st], xout = half_resp)$y)
        
        # determine the upper and lower bound by monotonicity 
        if (inherits(half_con_1, "try-error")) { half_con_1 <- NA }
        if (inherits(half_con_2, "try-error")) { half_con_2 <- NA }
        if(monotonic_behaviour == "Down"){
          edl <- half_con_1
          edu <- half_con_2
        } else {
          edl <- half_con_2
          edu <- half_con_1
        }
        ## right ed50
        half_con2 <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 1], y = doseRange[doseRange_2nd], xout = half_resp)$y)
        half_con2_1 <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 2], y = doseRange[doseRange_2nd], xout = half_resp)$y)
        half_con2_2 <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 3], y = doseRange[doseRange_2nd], xout = half_resp)$y)
        
        # determine the upper and lower bound by monotonicity 
        if (inherits(half_con2, "try-error")) { half_con2 <- NA }
        if (inherits(half_con2_1, "try-error")) { half_con2_1 <- NA }
        if (inherits(half_con2_2, "try-error")) { half_con2_2 <- NA }
        if(monotonic_behaviour == "Down"){
          edl2 <- half_con2_2
          edu2 <- half_con2_1
        } else {
          edl2 <- half_con2_1
          edu2 <- half_con2_2
        }
        
        # Export the info.
        temp_res <- data.frame(
          max_res = max(conf_interval_cv[doseRange_1st, 1]), 
          min_res = min(conf_interval_cv[doseRange_1st, 1]),
          ED50_res = half_resp, 
          ED50_l_Mean = half_con, ED50_l_L = edl, ED50_l_U = edu, 
          ED50_r_Mean = half_con2, ED50_r_L = edl2, ED50_r_U = edu2
          )
        
      }, error = function(e) {
        print(e)
        temp_res <- matrix(NA, 1, 9) %>% as.data.frame()
      })
      
    }
    # Export the info.
    bmd_val <- cbind(bmd_val, temp_res)
  }
  return(as.data.frame(bmd_val))
  
}

# Export results
compute_ed_SG <- function(df, fct, bp, minidose, ed50_type){
  tempObj <- try(eval(parse(text = paste0("drm(Response ~ Conc, data = df, fct = ", fct, ")"))), silent = FALSE)
  if (!inherits(tempObj, "try-error")){
    tempED <- compute_ed(opt_mod = tempObj, rl = 1.349, dataframe = df, 
                         constantVar = TRUE, interval_type = "delta", conf_interval = 0.95, bp = bp, minidose, ed50_type)
  }else{
    tempED <- matrix(NA, 1, 12) %>% as.data.frame()
  }
  colnames(tempED) <- c("Curve_BestFit_data", "FctName", "Monotonicity", "SG_max_res", "SG_min_res", 
                        "SG_ED50_res", "SG_ED50_l", "SG_ED50L_l", "SG_ED50U_l", "SG_ED50_r", "SG_ED50L_r", "SG_ED50U_r")
  return(tempED)
  
}

## Reed-and-Muench method --------------------------------------------------

RM_method <- function(df){
  # df <- tempMat[[2]][[1]]
  if(df$Conc_low == 0){
    x1 <- 0
  }else{
    x1 <- log10(df$Conc_low)
  }
  x2 <- log10(df$Conc_high)
  y1 <- df$Res_low
  y2 <- df$Res_high
  k <- (y2-y1)/(x2-x1)
  IC50 <- 10 ^ (x2-(y2-50)/k)
  return(IC50)
}

RM_method_f <- function(df, ed50_type) {
  
  retMat_RM <- matrix(0, 1, 6)
  if (ed50_type == "Absolute" && all(df$Biphasic == "N")) {
    Df <- df %>%
      dplyr::group_by(Conc) %>%
      dplyr::summarise(mean = mean(Response)) %>% 
      arrange(Conc)
    Df <- Df %>% mutate("Conc_order" = rownames(Df))
    n <- as.numeric(n_distinct(df$Replicate))
    
    # whether the IC50 is out of the concentration range the user provided
    if (all(Df$mean > 50, na.rm = TRUE) | all(Df$mean < 50, na.rm = TRUE)){
      # Estimate
      retMat_RM[, 1] <- NA
      # Std. Dev
      retMat_RM[, 2] <- NA
      # CV
      retMat_RM[, 3] <- NA
      # Std. Error
      retMat_RM[, 4] <- NA
      # Confidence Interval
      retMat_RM[, 5] <- NA
      retMat_RM[, 6] <- NA
    }else{
      # identify the two points that bracket the ED50
      if (Df$mean[1] > 50) {
        j <- 1
        while(Df$mean[j] > 50) {j <- j +1}
        ic50_max <- Df$Conc[j - 1]
        ic50_min <- Df$Conc[j]
      } else {
        j <- 1
        while(Df$mean[j] < 50) {j <- j +1}
        ic50_max <- Df$Conc[j]
        ic50_min <- Df$Conc[j - 1]
      }
      tempMat <- df %>% filter(Conc == ic50_max | Conc == ic50_min) %>% spread(Conc, Response)
      clnm <- colnames(tempMat)
      tempMat <- tempMat %>% mutate("L" = as.numeric(clnm[3]), "H" = as.numeric(clnm[4]))
      colnames(tempMat) <- c("Biphasic", "Replicate", "Res_low", "Res_high", "Conc_low", "Conc_high")
      tempMat <- tempMat %>% group_by(Replicate) %>% nest() %>% mutate("ED50" = map(data, RM_method))
      tempMean <- mean(as.numeric(tempMat$ED50))
      tempSD <- sd(as.numeric(tempMat$ED50))
      # Estimate
      retMat_RM[, 1] <- tempMean
      # Std. Dev
      retMat_RM[, 2] <- tempSD
      # CV
      retMat_RM[, 3] <- (tempSD/tempMean)*100
      # Std. Error
      retMat_RM[, 4] <- tempSD/sqrt(n)
      # Confidence Interval
      ci_l <- tempMean - tempSD*1.96/sqrt(n)
      ci_u <- tempMean + tempSD*1.96/sqrt(n)
      retMat_RM[, 5] <- ci_l
      retMat_RM[, 6] <- ci_u
    }
    
  } else {
    # Estimate
    retMat_RM[, 1] <- NA
    # Std. Dev
    retMat_RM[, 2] <- NA
    # CV
    retMat_RM[, 3] <- NA
    # Std. Error
    retMat_RM[, 4] <- NA
    # Confidence Interval
    retMat_RM[, 5] <- NA
    retMat_RM[, 6] <- NA
  }
  colnames(retMat_RM) <- c("RM_ED50_Mean", "RM_ED50_SD", "RM_ED50_CV", "RM_ED50_SE", "RM_ED50L", "RM_ED50U")
  retMat_RM <- as.data.frame(retMat_RM)
  return(retMat_RM)
  
}

## Ritz-Gerhard method ---------------------------------------------------------

compute_ed_Std <- function(df, bp, fct, ed50_type, minidose) {
  # df <- Data$data[[1]]
  # fct <- "W1.4(fixed = c(NA, NA, NA, NA))"
  # fct <- "BC.5()"
  respns <- df$Response
  dose <- df$Conc
  tempObj <- try(eval(parse(text = paste0("drm(Response ~ Conc, data = df, fct = ", fct, ")"))), silent = FALSE)
  
  if (!inherits(tempObj, "try-error")){
    fctName <- summary(tempObj)[["fctName"]]
    
    # starting point - adopted from plot.drc()
    if (min(dose) == 0) {
      if (bp == "default") {
        min_dose <- minidose
      } else {
        min_dose <- as.numeric(bp)
      }
    } else {
      min_dose <- min(dose)
    }
    
    # doseRange
    doseRange <- seq_log(min_dose, max(dose), length.out = 1000)
    
    
    # Export the nested data for generate plot
    ## calculate the curve interval
    conf_interval_cv <- stats::predict(tempObj, newdata = data.frame(dose = as.vector(doseRange)), interval = "confidence", level = 0.95) %>% as.data.frame()
    curve_df <- conf_interval_cv %>% dplyr::select(1) %>% mutate(Conc = doseRange)
    colnames(curve_df) <- c("Prediction", "Conc")
    bmd_val <- nest(curve_df, data = everything())
    colnames(bmd_val) <- "Curve_BestFit_data"
    
    # Determine the monotonicity of the curve/the first part of curve
    monotonic_behaviour <- monotonicity(fittedModel = tempObj, mindose = min_dose)
    
    # Combine all the info.
    bmd_val <- bmd_val %>% 
      mutate(FctName = gsub("(.*)\\()", "\\1", fctName),
             Monotonicity = monotonic_behaviour)
             
    temp_res <- tryCatch({
      
      if (monotonic_behaviour == "Flat") {
        
        temp_res <- matrix(NA, 1, 5) %>% as.data.frame()
        
      } else {
        
        # ED50
        ED_fct <- try(ED(tempObj, 50, interval = "delta", level = 0.95, type = tolower(ed50_type), upper = max(dose), lower = min_dose, display = FALSE))
        if(inherits(ED_fct, "try-error")) { 
          ED_fct <- matrix(NA, 1, 4) %>% as.data.frame()
        } else {
          ED_fct <- ED_fct %>% as.data.frame()
        }
        colnames(ED_fct) <- c("Std_ED50_Mean", "Std_ED50_SE", "Std_ED50L", "Std_ED50U")
        half_resp <- try(predict(tempObj, newdata = data.frame(dose = ED_fct$Std_ED50_Mean)))
        if (!inherits(half_resp, "try-error")) { half_resp <- as.numeric(half_resp) } else { half_resp <- NA }
        # Export the info.
        temp_res <- data.frame(Std_ED50_res = half_resp) %>% cbind(ED_fct)
      }
      
    }, error = function(e) {
      print(e)
      temp_res <- matrix(NA, 1, 5) %>% as.data.frame()
    })
    # Export the info.
    bmd_val <- cbind(bmd_val, temp_res)

  } else {
    bmd_val <- matrix(NA, 1, 8) %>% as.data.frame()
  }
  colnames(bmd_val) <- c("Curve_BestFit_data", "FctName", "Monotonicity", 
                         "RG_ED50_res", "RG_ED50_Mean", "RG_ED50_SE", "RG_ED50L", "RG_ED50U")
  return(bmd_val)
}

## Fit the data to loess model ---------------------------------------------

loess_fit_drc <- function(df, bp, minidose) {
  # df <- loess_drc$data[[5]]
  dose <- df$Conc
  # Fit all the data to loess model
  tempObj <- try(loess(Response ~ log10(Conc), data = df, span = 0.5#, control=loess.control(surface="direct")
  ))
  #plot(tempObj)
  # Generate the dataframe
  if (!is.null(tempObj) & !inherits(tempObj, "try-error")) {
    # dose range
    if (min(dose) == 0) {
      if (bp == "default") {
        min_dose <- minidose
      } else {
        min_dose <- as.numeric(bp)
      }
    } else {
      min_dose <- min(dose)
    }
    doseRange <- seq_log(min_dose, max(dose), length.out = 500)
    temp_Ret <- predict(tempObj, newdata = data.frame(Conc = as.vector(doseRange)),
                        interval = TRUE, level = 0.95) %>% as.data.frame() %>% 
      mutate(Conc = doseRange) %>% drop_na()
    colnames(temp_Ret) <- c("Prediction", "Conc") 
  } else {
    temp_Ret <- NULL
  }
  return(temp_Ret)
}


## Statistical tests for the models ----------------------------------------

stats_test <- function(df, fct) {
  
  #df <- model_drc$data[[1]]
  #fct <- model_drc$fctList_f[1]
  tempObj <- try(eval(parse(text = paste0("drm(Response ~ Conc, data = df, fct = ", fct, ")"))), silent = FALSE)
  #plot(tempObj)
  
  retMat <- data.frame(matrix(0, 1, 6))
  # Test the model-fitting
  if (!inherits(tempObj, "try-error")){
    ## Lack-of-fit test (ANOVA-based)
    lactest <- try(modelFit(tempObj)[2, 5])
    if (!inherits(lactest, "try-error")&!is.na(lactest)) {retMat[, 1] <- formatC(lactest, format = "E", digits = 2)} else {retMat[, 1] <- "error"}
    # Lack-of-fit test - Neill's test
    ## The same test using 'neill.test'
    ntest <- try(neill.test(tempObj, df$Conc)[1, 2])
    if (!inherits(ntest, "try-error")&!is.na(ntest)) {retMat[, 2] <- formatC(ntest, format = "E", digits = 2)} else {retMat[, 2] <- "error"}
    ## No-effect test
    notest <- try(as.data.frame(noEffect(tempObj))[3, 1])
    if (!inherits(notest, "try-error")&!is.na(notest)) {retMat[, 3] <- formatC(notest, format = "E", digits = 2)} else {retMat[, 3] <- "error"}
    # Test for all parameters
    retMat[, 4] <- nest(tidy(tempObj, conf.int = TRUE))
    # Lack-of-fit test based on cumulated residuals
    ltest <- try(lin.test(tempObj, noksSim = 1000, plotit = FALSE) %>% as.numeric())
    if (!inherits(ltest, "try-error")&!is.na(ltest)) {retMat[, 5] <- formatC(ltest, format = "E", digits = 2)} else {retMat[, 5] <- "error"}
    # Runs Test
    rtest <- try(randtests::runs.test(resid(tempObj), threshold = 0)[["p.value"]])
    if (!inherits(rtest, "try-error")&!is.na(rtest)) {retMat[, 6] <- formatC(rtest, format = "E", digits = 2)} else {retMat[, 6] <- "error"}
    
  }else{
    retMat[, 1] <- NA
    retMat[, 2] <- NA
    retMat[, 3] <- NA
    retMat[, 4] <- NA
    retMat[, 5] <- NA
    retMat[, 6] <- NA
  }
  colnames(retMat) <- c("Lac_of_fit_p",  "Neills_Test_p", "No_Effect_Test_p", "Para_Info", "Lin_Test_p", "Runs_Test_p")
  return(retMat)
  
}

sig_test <- function(x, sign, p) {
  pvalue <- try(as.numeric(x))
  if (is.na(pvalue)) {
    sig <- "/"
  } else {
    sig <- eval(parse(text = paste0("ifelse(", x, sign, p, ", paste(x, '*'), x)")))
  }
  return(sig)
}

para_sig_test <- function(df, p) {
  sig <- ifelse(all(df$`p.value` < as.numeric(p)), "Significant", "Non-significant")
  return(sig)
}


# T50 ############################################################################################
## Function to remove columns with only NA in the column
remove_na_columns <- function(df) {
  df[, !sapply(df, function(col) all(is.na(col)))]
}

## Function to fit the time-to-event data to the best model annd estimate the T50-----
compute_et <- function(df_temp, fctList_monotnc, const, time_intv){
  
  #df_temp <- model_te$RawData[[1]]
  ## Select the list of functions based the shape of the curves
  fctList_f <- c(fctList_monotnc)
  lenFL <- length(fctList_f)
  
  ## Pick the best fit model
  retMat <- matrix(0, lenFL, 2)
  for (i in 1:lenFL) {
    tempObj <- try(eval(parse(text = paste0("drmte(Count ~ Before + After, data = df_temp, fct = ", 
                                            fctList_f[i], "())"))), silent = FALSE)
    
    
    if (!inherits(tempObj, "try-error")){
      retMat[i, 1] <- AIC(tempObj)
      retMat[i, 2] <- BIC(tempObj)
    }else{
      retMat[i, 1] <- NA
      retMat[i, 2] <- NA
    }
    
  }
  colnames(retMat) <- c("AIC", "BIC")
  
  ## Calculate the T50 for the best model
  
  if (all(is.na(retMat[, 1]))) {
    # If none of the models can be applied to the data
    temp_Ret <- matrix(NA, 1, 4) %>% as.data.frame()
  } else {
    retMat <- data.frame(retMat, FctName = fctList_f)
    retMat <- retMat %>% mutate(rowNmbr = rownames(retMat))
    eval(parse(text = paste0("retMat <- retMat %>% filter(", const, " > 0) %>% arrange(", const, ")")))
    fct_select <- as.numeric(retMat$rowNmbr[1])
    tempObj <- try(eval(parse(text = paste0("drmte(Count ~ Before + After, data = df_temp, fct = ", 
                                            fctList_f[fct_select], "())"))), silent = FALSE)
    #plot(tempObj)
    if (!inherits(tempObj, "try-error")){
      tempED <- ED(tempObj, 0.5, type = "absolute") %>% as.data.frame() %>% mutate(FctName = fctList_f[fct_select])
      
      ## curve_df
      timeRange <- seq_log(min(time_intv), max(time_intv), length.out = 500)
      df_curve <- predict(tempObj, newdata = data.frame(time = as.vector(timeRange)),
                          interval = TRUE, level = 0.95) %>% as.data.frame()
      
      # Combine all the dataframe
      temp_Ret <- nest(df_curve, data = everything()) %>% cbind(tempED)
      
    }else{
      temp_Ret <- matrix(NA, 1, 4) %>% as.data.frame()
    }
  }
  
  # More modification of the dataframe
  colnames(temp_Ret) <- c("Curve_BestFit_data", "T50_Mean", "T50_SE", "FctName1")
  # Change the function name
  df_fct <- data.frame(
    FctName1 = c("LL.2", "LL.3", "LN.2", "LN.3", "W1.2", "W1.3", "W2.2", "W2.3", NA),
    FctName = c(rep(c("Log-logistic (4 paras)", "Log-normal", "Weibull I", "Weibull II"), each = 2), NA)
  )
  temp_Ret <- temp_Ret %>% 
    left_join(df_fct, by = "FctName1") %>% 
    dplyr::select(Curve_BestFit_data, T50_Mean, T50_SE, FctName)
  
  return(temp_Ret)
}

## Function to fit the data to loess model---------------------------------------------
loess_fit_te <- function(df) {
  df_temp <- df
  # Fit all the data to loess model
  tempObj <- try(loess(Response ~ log10(After), data = df_temp, span = 0.5))
  # Generate the dataframe
  if (!is.null(tempObj) & !inherits(tempObj, "try-error")) {
    # time range
    timeRange <- seq_log(min(df_temp$After), max(df_temp$After), length.out = 500)
    temp_Ret <- predict(tempObj, newdata = data.frame(After = as.vector(timeRange)),
                        interval = TRUE, level = 0.95) %>% as.data.frame() %>% 
      mutate(time = timeRange) %>% drop_na()
    colnames(temp_Ret) <- c("Prediction", "time")
  } else {
    temp_Ret <- NULL
  }
  return(temp_Ret)
}


