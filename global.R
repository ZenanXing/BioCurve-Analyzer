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
    filter(!str_detect(fctList, ".*cedergreen.*")) %>% 
    separate(fctList, into = c("Fct", "c", "d"), sep = "_") %>% 
    mutate(parms_n = as.numeric(gsub(".*\\.([0-9]+).*", "\\1", Fct)),
           parm = pmap(list(c, d, parms_n), function (c,d,n) { if (!is.na(n)) {paste0("fixed = c(", paste(c("NA", c, d, rep("NA", n-3)), collapse = ", "), ")")} else {""}}),
           fctList_f = paste0(Fct, "(", parm, ")"))
  
  if (any(str_detect(fctList, ".*cedergreen.*"))) {
    fctList_df_2 <- data.frame(fctList = fctList) %>% 
      filter(str_detect(fctList, ".*cedergreen.*")) %>% 
      separate(fctList, into = c("Fct", "c", "d"), sep = "_") %>% 
      mutate(parms_n = 5,
             parm = pmap(list(c, d, parms_n), function (c,d,n) { if (!is.na(n)) {paste0("fixed = c(", paste(c("NA", c, d, rep("NA", n-3)), collapse = ", "), ")")} else {""}}))
    fctList_df_cg <- bind_rows(replicate(3, fctList_df_2, simplify = FALSE)) %>% 
      mutate(alpha = rep(c(1, 0.5, 0.25), each = nrow(fctList_df_2))) %>% 
      mutate(fctList_f = paste0(Fct, "(", parm, ", alpha = ", alpha, ")")) %>% 
      dplyr::select(-alpha)
    fctList_df <- rbind(fctList_df, fctList_df_cg)
  }
  
  
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
    Fct = c("LL.4", "LL.5", "W1.4", "W2.4", "BC.5", "CRS.6", "cedergreen", "ucedergreen", NA),
    Model = c("Log-logistic (4 paras)", "Log-logistic (5 paras)", "Weibull I", "Weibull II", "Brain-Cousens", rep("Cedergreen-Ritz-Streibig", 3), NA)
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
compute_ed <- function(opt_mod, dataframe, 
                       conf_interval, bp, minidose, ed50_type) {
  # dataframe <- Data[1, ] %>% unnest()
  # opt_mod <- drm(Response ~ Conc, data = dataframe, fct = LL.4(fixed = c(NA, 0, NA, NA)))
  # opt_mod <- drm(Response ~ Conc, data = dataframe, fct = BC.5(fixed = c(NA, 0, NA, NA, NA)))
  # dataframe <- df
  # opt_mod <- tempObj
  # minidose <- mini_dose
  # conf_interval <- 0.95
  # plot(opt_mod)
  
  respns <- dataframe$Response
  dose <- dataframe$Conc
  fctName <- summary(opt_mod)[["fctName"]]
  
  if(is.na(fctName)) {
    bmd_val <- matrix(NA, 1, 22) %>% as.data.frame()
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
    
    # dose range
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
      
      temp_res <- matrix(NA, 1, 22) %>% as.data.frame()
      
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
        
        # minimum and maxmum responses
        min_res <- min(conf_interval_cv[, 1])
        max_res <- max(conf_interval_cv[, 1])
        min_res_1 <- min(conf_interval_cv[doseRange_1st, 1])
        max_res_1 <- max(conf_interval_cv[doseRange_1st, 1])
        
        # calculate the half max response in the first phase under the concs tested in the experiment
        if (ed50_type == "Absolute") {
          half_resp <- 50
        } else {
          half_resp <- max_res_1-abs(max_res_1-min_res_1)/2
        }
        if (!(grepl("LL", fctName) || grepl("W", fctName))) {
          if (monotonic_behaviour == "Up") {
            start_res <- min_res_1
            m_res <- max_res
            if (ed50_type == "Absolute") { half_resp_2 <- NA} else { half_resp_2 <- start_res-(start_res-min_res)/2 }
          } else {
            start_res <- max_res_1
            m_res <- min_res
            if (ed50_type == "Absolute") { half_resp_2 <- NA} else { half_resp_2 <- start_res+(max_res-start_res)/2 }
          }
        }

        ### SG Method - approx()
        
        # calculate ed50 and both bounds
        ## left ed50
        half_con <- stats::approx(x = conf_interval_cv[doseRange_1st, 1], y = doseRange[doseRange_1st], xout = half_resp)$y
        half_con_1 <- try(stats::approx(x = conf_interval_cv[doseRange_1st, 2], y = doseRange[doseRange_1st], xout = half_resp)$y)
        half_con_2 <- try(stats::approx(x = conf_interval_cv[doseRange_1st, 3], y = doseRange[doseRange_1st], xout = half_resp)$y)
        
        # determine the upper and lower bound by monotonicity 
        if (inherits(half_con, "try-error")) { half_con <- NA }
        if (inherits(half_con_1, "try-error")) { half_con_1 <- NA }
        if (inherits(half_con_2, "try-error")) { half_con_2 <- NA }
        if(monotonic_behaviour == "Down"){
          edl <- half_con_1
          edu <- half_con_2
        } else {
          edl <- half_con_2
          edu <- half_con_1
        }
        
        ## additional eds for biphasic curves
        if (!(grepl("LL", fctName) || grepl("W", fctName))) {
          ## ED50-related
          if(ed50_type == "Relative") {
            half_con2 <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 1], y = doseRange[doseRange_2nd], xout = half_resp_2)$y)
            half_con2_1 <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 2], y = doseRange[doseRange_2nd], xout = half_resp_2)$y)
            half_con2_2 <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 3], y = doseRange[doseRange_2nd], xout = half_resp_2)$y)
            if (inherits(half_con2, "try-error")) { half_con2 <- NA }
            if (inherits(half_con2_1, "try-error")) { half_con2_1 <- NA }
            if (inherits(half_con2_2, "try-error")) { half_con2_2 <- NA }
          } else {
            half_con2 <- NA
            half_con2_1 <- NA
            half_con2_2 <- NA
          }
          
          ## LDS
          lds <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 1], y = doseRange[doseRange_2nd], xout = start_res)$y)
          lds_1 <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 2], y = doseRange[doseRange_2nd], xout = start_res)$y)
          lds_2 <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 3], y = doseRange[doseRange_2nd], xout = start_res)$y)
          if (inherits(lds, "try-error")) { lds <- NA }
          if (inherits(lds_1, "try-error")) { lds_1 <- NA }
          if (inherits(lds_2, "try-error")) { lds_2 <- NA }
          
          ## final CI
          if(monotonic_behaviour == "Down"){
            edl2 <- half_con2_2
            edu2 <- half_con2_1
            ldsl <- lds_2
            ldsu <- lds_1
          } else {
            edl2 <- half_con2_1
            edu2 <- half_con2_2
            ldsl <- lds_1
            ldsu <- lds_2
          }
          
          ## M
          # estimation
          m <- try(stats::approx(x = conf_interval_cv[doseRange_2nd, 1], y = doseRange[doseRange_2nd], xout = m_res)$y)
          if (inherits(m, "try-error")) { m <- NA }
          
        } else {
          half_resp_2 <- NA
          start_res <- NA
          m_res <- NA
          half_con2 <- NA
          edl2 <- NA
          edu2 <- NA
          lds <- NA
          ldsl <- NA
          ldsu <- NA
          m <- NA
        }
        
        # Export the info.
        temp_res <- data.frame(
          max_res = max_res, min_res = min_res, 
          start_res = start_res, m_res = m_res, 
          ED50_l_res = half_resp, ED50_r_res = half_resp_2, 
          ED50_l_Mean = half_con, ED50_l_SE = NA, ED50_l_L = edl, ED50_l_U = edu, ED50_l_SD = NA, 
          ED50_r_Mean = half_con2, ED50_r_SE = NA, ED50_r_L = edl2, ED50_r_U = edu2, ED50_r_SD = NA, 
          LDS_Mean = lds, LDS_SE = NA, LDS_L = ldsl, LDS_U = ldsu, LDS_SD = NA, 
          M = m
        )
        
      }, error = function(e) {
        print(e)
        temp_res <- matrix(NA, 1, 22) %>% as.data.frame()
      })
      
    }
    
    # Export the info.
    bmd_val <- cbind(bmd_val, temp_res) %>% as.data.frame()
    
  }
  
  return(bmd_val)
  
}

# Export results
compute_ed_SG <- function(df, fct, bp, minidose, ed50_type){
  #df <- Data[1, ] %>% unnest()
  #fct <- "BC.5(fixed = c(NA, 0, NA, NA, NA))"
  tempObj <- try(eval(parse(text = paste0("drm(Response ~ Conc, data = df, fct = ", fct, ")"))), silent = FALSE)
  if (!inherits(tempObj, "try-error")){
    tempED <- compute_ed(opt_mod = tempObj, dataframe = df, 
                         conf_interval = 0.95, bp = bp, minidose = minidose, ed50_type = ed50_type)
  }else{
    tempED <- matrix(NA, 1, 25) %>% as.data.frame()
  }
  colnames(tempED) <- c("Curve_BestFit_data", "FctName", "Monotonicity", 
                        "max_res", "min_res", "start_res", "m_res", "ED50_l_res", "ED50_r_res", 
                        "ED50_l", "ED50_l_SE", "ED50_l_L", "ED50_l_U", "ED50_l_SD", 
                        "ED50_r", "ED50_r_SE", "ED50_r_L", "ED50_r_U", "ED50_r_SD", 
                        "LDS", "LDS_SE", "LDS_L", "LDS_U", "LDS_SD", "M")
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
  if (ed50_type == "Absolute") {
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
  colnames(retMat_RM) <- c("RM_ED50", "RM_ED50_SD", "RM_ED50_CV", "RM_ED50_SE", "RM_ED50L", "RM_ED50U")
  retMat_RM <- as.data.frame(retMat_RM)
  return(retMat_RM)
  
}

## Ritz-Gerhard method ---------------------------------------------------------

compute_ed_Std <- function(df, bp, fct, ed50_type, minidose, c, d) {
  # df <- Data$data[[1]]
  # fct <- "W1.4(fixed = c(NA, NA, NA, NA))"
  # fct <- "BC.5(fixed = c(NA, 0, NA, NA, NA))"
  respns <- df$Response
  dose <- df$Conc
  n <- n_distinct(df$Replicate)
  
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
    if(min(dose) == 0){doseRange <- c(0, doseRange)}
    
    # Export the nested data for generate plot
    ## calculate the curve interval
    conf_interval_cv <- stats::predict(tempObj, newdata = data.frame(dose = as.vector(doseRange)), 
                                       interval = "confidence", level = 0.95) %>% as.data.frame()
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
        
        temp_res <- matrix(NA, 1, 22) %>% as.data.frame()
        
      } else {
        
        # minimum and maxmum responses
        min_res <- min(conf_interval_cv[, 1])
        max_res <- max(conf_interval_cv[, 1])
        
        # start response
        df_para <- tidy(tempObj, conf.int = TRUE)
        if (!"c" %in% df_para$term) { df_para <- rbind(df_para, c("c", "(Intercept)", as.numeric(c), rep(NA, 5))) }
        if (!"d" %in% df_para$term) { df_para <- rbind(df_para, c("d", "(Intercept)", as.numeric(d), rep(NA, 5))) }
        if (!grepl("ucedergreen", fctName) && !grepl("W2", fctName)) {
          if (as.numeric(df_para[df_para$term =="b", 3])>0) {
            start_res <- df_para[df_para$term =="d", 3] %>% as.numeric()
          } else {
            start_res <- df_para[df_para$term =="c", 3] %>% as.numeric()
          }
        } else {
          if (as.numeric(df_para[df_para$term =="b", 3])<0) {
            start_res <- df_para[df_para$term =="d", 3] %>% as.numeric()
          } else {
            start_res <- df_para[df_para$term =="c", 3] %>% as.numeric()
          }
        }
        
        # ED50
        ED_fct <- try(ED(tempObj, 50, interval = "delta", level = 0.95, type = tolower(ed50_type), upper = max(dose), lower = min_dose, display = FALSE))
        if(inherits(ED_fct, "try-error")) { 
          ED_fct <- matrix(NA, 1, 5) %>% as.data.frame()
        } else {
          ED_fct <- ED_fct %>% 
            as.data.frame() %>% 
            mutate(SD = `Std. Error`*sqrt(n))
        }
        half_resp <- try(predict(tempObj, newdata = data.frame(dose = ED_fct$Estimate)))
        if (!inherits(half_resp, "try-error")) { half_resp <- as.numeric(half_resp) } else { half_resp <- NA }
        
        ## additional eds for biphasic curves
        if (!(grepl("LL", fctName) || grepl("W", fctName))) {
          if (ed50_type == "Relative") {
            # modify ED_fct
            colnames(ED_fct) <- c("ED50_r_Mean", "ED50_r_SE", "ED50_r_L", "ED50_r_U", "ED50_r_SD")
            ED_fct_f <- data.frame(
              ED50_l_res = NA,
              ED50_r_res = half_resp,
              ED50_l_Mean = NA,
              ED50_l_SE = NA,
              ED50_l_L = NA,
              ED50_l_U = NA,
              ED50_l_SD = NA
            ) %>% cbind(ED_fct)
            
            # M value
            M_fct <- try(drc::MAX(tempObj, upper = max(dose), lower = min_dose))
            if(inherits(M_fct, "try-error")) { 
              M_fct <- matrix(NA, 1, 2) %>% as.data.frame()
            } else {
              M_fct <- M_fct %>% as.data.frame()
            }
            
            # LDS value
            if (grepl("ucedergreen", fctName)) {
              LDS_fct <- try(ED(tempObj, 99, interval = "delta", level = 0.95, 
                                type = tolower(ed50_type), upper = max(dose), lower = min_dose, display = FALSE))
            } else {
              LDS_fct <- try(ED(tempObj, 1, interval = "delta", level = 0.95, 
                                type = tolower(ed50_type), upper = max(dose), lower = min_dose, display = FALSE))
            }
            
            if(inherits(LDS_fct, "try-error")) { 
              LDS_fct <- matrix(NA, 1, 5) %>% as.data.frame()
            } else {
              LDS_fct <- LDS_fct %>% as.data.frame() %>% 
                mutate(SD = `Std. Error`*sqrt(n))
            }
            
          } else {
            ED_fct_f <- matrix(NA, 1, 12) %>% as.data.frame()
            M_fct <- matrix(NA, 1, 2) %>% as.data.frame()
            LDS_fct <- matrix(NA, 1, 5) %>% as.data.frame()
          }
          
        } else {
          
          # Modify the ED-related dataframe
          ED_fct_f <- data.frame(
            ED50_l_res = half_resp,
            ED50_r_res = NA
          ) %>% 
            cbind(ED_fct) %>% 
            cbind(data.frame(
              ED50_r_Mean = NA,
              ED50_r_SE = NA,
              ED50_r_L = NA,
              ED50_r_U = NA,
              ED50_r_SD = NA
            ))
          M_fct <- matrix(NA, 1, 2) %>% as.data.frame()
          LDS_fct <- matrix(NA, 1, 5) %>% as.data.frame()
          
        }
        
        # Change the column names
        colnames(ED_fct_f) <- c("ED50_l_res", "ED50_r_res", 
                                "ED50_l_Mean", "ED50_l_SE", "ED50_l_L", "ED50_l_U", "ED50_l_SD", 
                                "ED50_r_Mean", "ED50_r_SE", "ED50_r_L", "ED50_r_U", "ED50_r_SD")
        colnames(M_fct) <- c("M", "m_res")
        colnames(LDS_fct) <- c("LDS_Mean", "LDS_SE", "LDS_L", "LDS_U", "LDS_SD")
        
        # Export the info.
        temp_res <- data.frame(
          max_res = max_res, min_res = min_res,
          start_res = start_res, m_res = M_fct$m_res
        ) %>% 
          cbind(ED_fct_f) %>% 
          cbind(LDS_fct) %>% 
          cbind(data.frame(M = M_fct$M))
        
      }
      
    }, error = function(e) {
      print(e)
      temp_res <- matrix(NA, 1, 22) %>% as.data.frame()
    })
    # Export the info.
    bmd_val <- cbind(bmd_val, temp_res)

  } else {
    
    bmd_val <- matrix(NA, 1, 25) %>% as.data.frame()
    
  }
  colnames(bmd_val) <- c("Curve_BestFit_data", "FctName", "Monotonicity", 
                         "max_res", "min_res", "start_res", "m_res", "ED50_l_res", "ED50_r_res", 
                         "ED50_l", "ED50_l_SE", "ED50_l_L", "ED50_l_U", "ED50_l_SD", 
                         "ED50_r", "ED50_r_SE", "ED50_r_L", "ED50_r_U", "ED50_r_SD", 
                         "LDS", "LDS_SE", "LDS_L", "LDS_U", "LDS_SD", "M")
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


## Extract the f value -----------------------------------------------------

f_related <- function(df) {
  if ("f" %in% df$term) {
    f_df <- df %>% filter(term == "f") %>% dplyr::select(estimate, `p.value`, `conf.low`, `conf.high`)
  } else {
    f_df <- matrix(NA, 1, 4) %>% as.data.frame()
  }
  colnames(f_df) <- c("f", "f_p", "f_L", "f_U")
  return(f_df)
}


# T50 ############################################################################################
## Function to remove columns with only NA in the column
remove_na_columns <- function(df) {
  df[, !sapply(df, function(col) all(is.na(col)))]
}

## Function to fit the time-to-event data to the best model annd estimate the T50-----
compute_et <- function(df_temp, t50_type, fctList_monotnc, const, extra_arg, time_intv){
  
  #df_temp <- model_te$RawData[[1]]
  ## Select the list of functions based the shape of the curves
  fctList_f <- c(fctList_monotnc)
  lenFL <- length(fctList_f)
  
  ## Pick the best fit model for parametric models
  if (all(!c("KDE", "NPMLE") %in% fctList_f)) {
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
    if (all(is.na(retMat[, 1]))) {
      fct_select <- NA
    } else {
      retMat <- data.frame(retMat, FctName = fctList_f)
      retMat <- retMat %>% mutate(rowNmbr = rownames(retMat))
      eval(parse(text = paste0("retMat <- retMat %>% filter(", const, " > 0) %>% arrange(", const, ")")))
      fct_select_num <- as.numeric(retMat$rowNmbr[1])
      fct_select <- fctList_f[fct_select_num]
    }
  } else {
    fct_select <- fctList_f
  }
  
  
  ## Calculate the T50 for the best model
  
  if (is.na(fct_select)) {
    # If none of the models can be applied to the data
    temp_Ret <- matrix(NA, 1, 11) %>% as.data.frame()
  } else {
    if (fct_select == "KDE") {
      tempObj <- try(eval(parse(text = paste0("drmte(Count ~ Before + After, data = df_temp, fct = KDE(bw = '", extra_arg, "'))"))), silent = FALSE)
    } else {
      tempObj <- try(eval(parse(text = paste0("drmte(Count ~ Before + After, data = df_temp, fct = ", fct_select, "())"))), silent = FALSE)
    }
    
    #plot(tempObj)
    if (!inherits(tempObj, "try-error")){
      
      t50_txt <- "quantile(tempObj, probs = c(0.5), interval = TRUE"
      
      if (t50_type == "Relative") {
        t50_txt <- paste0(t50_txt, ", restricted = TRUE", collapse = "")
      } else {
        t50_txt <- paste0(t50_txt, ", restricted = FALSE", collapse = "")
      }
      
      if (fct_select == "NPMLE") {
        t50_txt <- paste0(t50_txt, ", npmle.type = ", extra_arg, ")", collapse = "")
      } else {
        t50_txt <- paste0(t50_txt, ")", collapse = "")
      }
      
      tempED <- eval(parse(text = t50_txt)) %>% as.data.frame()
      
      if (fct_select == "KDE") {
        tempED <- tempED %>% 
          dplyr::select(Estimate) %>% 
          mutate(SE = NA, Lower = NA, Upper = NA, SD = NA) %>% 
          mutate(FctName = fct_select)
      } else {
        if (fct_select != "NPMLE") {
          tempED <- tempED %>% dplyr::select(Estimate, SE, Lower, Upper)
        } else {
          tempED <- tempED %>% dplyr::select(Mean, SE, Lower, Upper)
        }
        ## calculate the SD
        n <- n_distinct(df_temp$Replicate)
        tempED <- tempED %>% 
          mutate(SD = SE*sqrt(n), 
                 FctName = fct_select)
      }
      
      ## curve_df
      if (t50_type == "Absolute" && fct_select == "NPMLE") {
        timeRange <- c(seq(min(time_intv), max(time_intv), length.out = 1000), Inf)
        timeRange2 <- c(time_intv, Inf)
      } else {
        timeRange <- seq(min(time_intv), max(time_intv), length.out = 1000)
        timeRange2 <- time_intv
      }
      
      if (fct_select == "NPMLE") {
        df_curve <- predict(tempObj, newdata = data.frame(time = timeRange), npmle.type = extra_arg,
                            interval = TRUE, level = 0.95) %>% as.data.frame()
      } else {
        df_curve <- predict(tempObj, newdata = data.frame(time = timeRange),
                            interval = TRUE, level = 0.95) %>% as.data.frame()
      }
      
      ## mean_df
      df_mean_all <- predict(tempObj, newdata = data.frame(time = timeRange2),
                         interval = TRUE, level = 0.95) %>% as.data.frame()
      df_mean <- data.frame(
        Before = df_mean_all$time[1:(nrow(df_mean_all)-1)],
        After = df_mean_all$time[-1],
        Ymin = df_mean_all$Prediction[1:(nrow(df_mean_all)-1)],
        Ymax = df_mean_all$Prediction[-1]
      )
      
      # minimum and maxmum responses
      min_res <- min(df_curve[, 2])
      max_res <- max(df_curve[, 2])
      if (t50_type == "Relative") {
        if (fct_select == "NPMLE") { 
          half_resp <- try(predict(tempObj, newdata = data.frame(dose = tempED[1, 1]), npmle.type = extra_arg))
        } else {
          half_resp <- try(predict(tempObj, newdata = data.frame(dose = tempED[1, 1])))
        }
        if (!inherits(half_resp, "try-error")) { half_resp <- as.numeric(half_resp$Prediction) } else { half_resp <- NA }
      } else {
        half_resp <- 0.5
      }
      
      # Combine all the dataframe
      temp_Ret <- nest(df_curve, data = everything()) %>% 
        mutate(MeanData = map(seq_len(n()), ~ df_mean)) %>% 
        mutate(min_res = min_res,
               max_res = max_res,
               t50_res = half_resp) %>% 
        cbind(tempED)
      
    }else{
      temp_Ret <- matrix(NA, 1, 11) %>% as.data.frame()
    }
  }
  
  # More modification of the dataframe
  colnames(temp_Ret) <- c("Curve_BestFit_data", "MeanData", "min_res", "max_res", "T50_res", "T50", "T50_SE", "T50_Lower", "T50_Upper", "T50_SD", "FctName1")
  # Change the function name
  df_fct <- data.frame(
    FctName1 = c("LL.2", "LL.3", "LN.2", "LN.3", "W1.2", "W1.3", "W2.2", "W2.3", "KDE", "NPMLE", NA),
    FctName = c(rep(c("Log-logistic (4 paras)", "Log-normal", "Weibull I", "Weibull II"), each = 2), "KDE", "NPMLE", NA)
  )
  temp_Ret <- temp_Ret %>% 
    left_join(df_fct, by = "FctName1") %>% 
    dplyr::select(MeanData, Curve_BestFit_data, FctName, min_res, max_res, T50_res, T50, T50_SE, T50_SD, T50_Lower, T50_Upper)
  
  return(temp_Ret)
}

## Function to fit the data to loess model---------------------------------------------
loess_fit_te <- function(df, FctName) {
  df_temp <- df %>% mutate(After = replace(After, is.infinite(After), NA)) %>% drop_na()
  # Fit all the data to loess model
  tempObj <- try(loess(Response ~ log10(After), data = df_temp, span = 0.5))
  # Generate the dataframe
  if (!is.null(tempObj) & !inherits(tempObj, "try-error")) {
    # time range
    timeRange <- seq(min(df_temp$After), max(df_temp$After), length.out = 1000)
    temp_Ret <- predict(tempObj, newdata = data.frame(After = as.vector(timeRange)),
                        interval = TRUE, level = 0.95) %>% as.data.frame() %>% 
      mutate(time = timeRange) %>% drop_na()
    colnames(temp_Ret) <- c("Prediction", "time")
  } else {
    temp_Ret <- NULL
  }
  return(temp_Ret)
}


