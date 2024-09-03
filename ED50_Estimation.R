#### Reactive Objects - Model & ED50 info. #################################################################

# Selected models and info. 
df_ed <- eventReactive(input$calculate_Butn, {
  
  # Nested the dataframe
  Data <- data()
  if (data_values$n_var == 0) {
    colnames(Data) <- c("Conc", "Replicate", "Response")
    Data <- Data %>% nest()
  } else {
    colnames(Data) <- c(colnames(data())[1:data_values$n_var], "Biphasic", "Conc", "Replicate", "Response")
    eval(parse(text = paste0("Data <- Data %>% group_by(", paste(colnames(data())[1:data_values$n_var], collapse = ","), ") %>% nest()")))
  }
  
  # Model list
  fctList_monotnc <- c(input$LL_list, input$W_1_list, input$W_2_list)
  fctList_biphsc <- c(input$BC_list, input$beta_list)
  
  # Criteria used to select the best model
  const <- input$crtrn_selected
  
  # Find the best model for each curve
  model_ll4 <- Data %>% mutate(ED_info = purrr::pmap(list(data), m_select_new, fctList_monotnc = fctList_monotnc, fctList_biphsc = fctList_biphsc, const = const)) %>% unnest(ED_info)
  
  # Estimate the ED values for each curve
  
  # # Whether you should find the best model for your curve
  # if (input$model_select == "best") {
  #     # Find the best constraints in the model
  #     model_ll4 <- Data %>%
  #         mutate(Model_select = map2(data, input$crtrn_selected, m_select_LL)) %>%
  #         mutate(LL4_Constraints = map(Model_select, best_m))
  #     model_ll4 <-  model_ll4[ , c(1:(data_values$n_var+1), (data_values$n_var+3))]
  # }else{
  #     model_ll4 <- Data %>% mutate(LL4_Constraints = input$constt_selected)
  # }
  # 
  # # Fit your curve with the best model selected in the previous step and export the ED50 info.
  # if (input$ed50_type == "Absolute") {ed50Mat <- M_fit_ED50(model_ll4, "absolute")} else {ed50Mat <- M_fit_ED50(model_ll4, "relative")}
  # Constraints <- NULL
  # for (i in 1:nrow(ed50Mat)) {
  #     if (ed50Mat$Estimation_Method[i] == "Log-logistic") {Constraints <- c(Constraints, model_ll4$LL4_Constraints[i])}
  #     else {Constraints <- c(Constraints, "/")}
  # }
  # df_ED50 <- cbind.data.frame(model_ll4, ed50Mat) %>%
  #     mutate(Constraints = map(unlist(Constraints), 
  #                              switch, "ll_4_1" = "No Constraints", "ll_4_2" = "Top = 100", "/" = "/", 
  #                              "ll_4_3" = "Bottom = 0", "ll_4_4" = " Bottom = 0 & Top = 100"))
  # 
  if (data_values$n_var != 0) {colnames(model_ll4)[1:data_values$n_var] <- colnames(data())[1:data_values$n_var]}
  
  return(model_ll4)
})

#### Output - ED50 table ###################################################################################
ED50_table <- reactive({
  req(df_ed())
  if (isolate({data_values$n_var}) != 0) {
    selected_var <- c(1:isolate({data_values$n_var}), # variable names
                      (isolate({data_values$n_var})+7),
                      (isolate({data_values$n_var})+2):(isolate({data_values$n_var})+6), # model-related
                      (isolate({data_values$n_var})+12):(isolate({data_values$n_var})+14),
                      (isolate({data_values$n_var})+18):(isolate({data_values$n_var})+20)) # ED50
  } else {
    selected_var <- c((isolate({data_values$n_var})+7),
                      (isolate({data_values$n_var})+2):(isolate({data_values$n_var})+6), # model-related
                      (isolate({data_values$n_var})+12):(isolate({data_values$n_var})+14),
                      (isolate({data_values$n_var})+18):(isolate({data_values$n_var})+20)) # ED50
  }
  df_temp <- df_ed()[ , selected_var]
  df_temp[, (isolate({data_values$n_var})+2):ncol(df_temp)] <- round(df_temp[, (isolate({data_values$n_var})+2):ncol(df_temp)], 2)
  return(df_temp)
})

output$df2 <- DT::renderDataTable({
  ED50_table()
})

#### Triggered UI ##########################################################################################
output$ed50_results <- renderUI({
  req(df_ed())
  tagList(
    list(
      h4(HTML(paste0("ED", tags$sub("50"), " Estimation Table")), align = 'center'),
      div(style = "margin-top: -10px"),
      hr(),
      div(style = "margin-top: -10px"),
      DT::dataTableOutput("df2") %>% shinycssloaders::withSpinner(),
      tags$b("Note:"),
#      p(HTML(paste0("1. The estimated ED", tags$sub("50"), " values with a ", tags$span("'*'", style = "font-size: 20px; background-color: #E6F2FF"), " indicate the ED", tags$sub("50"), 
#                    " is bracketed on one side by only a single point; in this case the ED", tags$sub("50"), 
#                    " is estimated by the", em("'Reed and Muench method'"), " (Ramakrishnan,", em("World J Virol"), 
#                    ", 2016). If you have many values with asterisks you should consider using a different concentration range."))),
      p(HTML(paste0("To prevent over-fitting, we highly recommand you to choose the constraints based on your own experiment setup, 
                    if you let the app to choose the best constraints for you, 
                    the best constraints reported for fitting to a 4 paramter log-Logistic model are selected from the drc analysis (Ritz C,", 
                    em("et al."), ", ", em("PLoS One"), ", 2015), based on the criteria you choose on the left."))),
      p(tags$b("Reference:")),
      p(em("Ritz C, Baty F, Streibig JC, Gerhard D (2015) Dose-Response Analysis Using R. PLoS One. 10(12)")), 
      p(em("Serra A. Et al. (2020) BMDx: a graphical Shiny application to perform Benchmark Dose analysis for transcriptomics data. Bioinformatics 36: 2932–2933"))
      # p(em("Ramakrishnan MA (2016) Determination of 50% endpoint titer using a simple formula. World J Virol. 5: 85–86"))
    )
  )
})


#### Functions ############################################################################################

##### Find the best model -----------------------------------------------------
m_select_new <- function(df_temp, fctList_monotnc, fctList_biphsc, const){
  
  ## Select the list of functions based the shape of the curves -------------
  if (unique(df_temp$Biphasic) == "Y") {
    fctList_f <- c(fctList_biphsc)
  } else {
    fctList_f <- c(fctList_monotnc)
  }
  
  lenFL <- length(fctList_f)
  
  ## Pick the best fit model --------------------------
  retMat <- matrix(0, lenFL, 5)
  for (i in 1:lenFL) {
    
    tempObj <- try(eval(parse(text = paste0("drm(Response ~ Conc, data = df_temp, fct = ", 
                                            fctList_f[i], "())"))), silent = FALSE)
    
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
  
  # If none of the models can be applied to the data.
  ## Pick the second best model ---------------
  if (all(is.na(retMat[, 1]))) {
    
    # function list with c =0
    fctList_f <- c(fctList_f, fctList_monotnc, "logistic")
    
    retMat <- matrix(NA, length(fctList_f), 5)
    
    for (i in (lenFL+1):length(fctList_f)) {
      
      tempObj <- try(eval(parse(text = paste0("drm(Response ~ Conc, data = df_temp, fct = ", 
                                              fctList_f[i], "())"))), silent = FALSE)
      
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
  }
  
  colnames(retMat) <- c("logLik", "AIC", "BIC", "Lac_of_fit", "resVar")
  
  retMat <- data.frame(retMat, FctName = fctList_f)
  retMat <- retMat %>% mutate(rowNmbr = rownames(retMat))
  
  if(const == "Lack_of_fit") {
    eval(parse(text = paste0("retMat <- retMat %>% arrange(desc(", const, "))")))
  } else {
    eval(parse(text = paste0("retMat <- retMat %>% arrange(", const, ")")))
  }
  
  ## Calculate the BMD and ED for the first model -----------------------
  fct_select <- as.numeric(retMat$rowNmbr[1])
  tempObj <- try(eval(parse(text = paste0("drm(Response ~ Conc, data = df_temp, fct = ", 
                                          fctList_f[fct_select], "())"))), silent = FALSE)
  if (!inherits(tempObj, "try-error")){
    tempED <- compute_ed(opt_mod = tempObj, rl = 1.349, dataframe = df_temp, 
                         constantVar = TRUE, interval_type = "delta", conf_interval = 0.95)
  }else{
    tempED <- matrix(NA, 1, 30) %>% as.data.frame()
    colnames(tempED) <- c("Curve_BestFit_data", "FctName", "Monotonicity", "BMD_res", "ED50_res", "max_res", "min_res", 
                          "BMD", "BMDL", "BMDU", "ED50", "ED50L", "ED50U")
  }
  
  ## Combine the dataframe ---------------------------------------------------
  temp_Ret <- dplyr::left_join(retMat[1, ], tempED, by = "FctName")
  
  return(temp_Ret)
}


##### Monotonicity ------------------------------------------------------------

monotonicity <- function(fittedModel){
  
  dose <- fittedModel$data$Conc
  doseRange <- seq_log(min(dose) + 1, max(dose), length.out = 100)
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

##### Equal variance ----------------------------------------------------------

control_variance <- function(model, constant_variance, training_dose){
  res = residuals(model)
  if(constant_variance){
    ctrl_variance = mean(res^2) # MSE - use residuals of all samples
  }else{
    ctrl_variance = mean(res[training_dose == 0]^2) # RMSE - use residuals of all controls samples (dose=0)
  }
  return(ctrl_variance)
}



##### ED estimation -----------------------------------------------------------

compute_ed <- function(opt_mod, rl = 1.349, dataframe, 
                       constantVar, conf_interval, interval_type) {
  
  respns <- dataframe$Response
  dose <- dataframe$Conc
  fctName <- summary(opt_mod)[["fctName"]]
  
  # dermine the monotonicity of the curve/the first part of curve
  monotonic_behaviour <- monotonicity(fittedModel = opt_mod)
  
  bmd_val <- tryCatch({
    
    # calcuate the response level for bmd estimation
    # starting point response and sd
    starting_point <- predict(opt_mod, data.frame(dose = 0.03))
    # if(predict(opt_mod, data.frame(dose = 0)) == 0) {
    #   starting_point <- predict(opt_mod, data.frame(dose = 0.03))
    # } else {
    #   starting_point <- predict(opt_mod, data.frame(dose = 0))
    # }
    
    sd_level <- rl *  sqrt(control_variance(model = opt_mod, constant_variance = constantVar, training_dose = dose ))
    
    if (monotonic_behaviour == "Flat") {
      bmd_val = NULL
    } else {
      
      if (monotonic_behaviour == "Up") {
        response_level = starting_point + sd_level
      } else if (monotonic_behaviour == "Down") {
        response_level = starting_point - sd_level
      }
      # calculate the 50% max response for ed50 estimation
      doseRange <- seq_log(min(dose) + 0.03, max(dose), length.out = 1000)
      # if (predict(opt_mod, data.frame(dose = 0)) == 0) {
      #   doseRange <- seq_log(min(dose) + 0.03, max(dose), length.out = 1000)
      # } else {
      #   doseRange <- c(0, seq_log(min(dose) + 0.03, max(dose), length.out = 1000))
      # }
      
      # calculate the curve interval
      conf_interval_cv <- stats::predict(opt_mod, newdata = data.frame(dose = as.vector(doseRange)), interval = "confidence", level = conf_interval) %>% as.data.frame()
      
      # define the doseRange before peak/trough
      if(monotonic_behaviour == "Up"){
        doseRange_1st <- c(1: which.max(conf_interval_cv[, 1]))
      }else if (monotonic_behaviour == "Down"){
        doseRange_1st <- c(1: which.min(conf_interval_cv[, 1]))
      }
      
      # calculate the half max response in the first phase under the concs tested in the experiment
      half_resp <- max(conf_interval_cv[doseRange_1st, 1]) - (abs(max(conf_interval_cv[doseRange_1st, 1]) - min(conf_interval_cv[doseRange_1st, 1]))/2)
      
      # Export the nested data for generate plot
      curve_df <- conf_interval_cv %>% dplyr::select(1) %>% mutate(Conc = doseRange)
      colnames(curve_df) <- c("Prediction", "Conc")
      bmd_val <- nest(curve_df, data = everything())
      colnames(bmd_val) <- "Curve_BestFit_data"
      
      ### Method 1 - approx() ----------------------
      
      # calculate bmd and both bounds
      
      bmd <- stats::approx(x = conf_interval_cv[doseRange_1st, 1], y = doseRange[doseRange_1st], xout = response_level)$y
      bmd1 <- try(stats::approx(x = conf_interval_cv[doseRange_1st, 2], y = doseRange[doseRange_1st], xout = response_level)$y)
      bmd2 <- try(stats::approx(x = conf_interval_cv[doseRange_1st, 3], y = doseRange[doseRange_1st], xout = response_level)$y)
      
      # determine the upper and lower bound by monotonicity 
      if (inherits(bmd1, "try-error")) { bmd1 <- NA }
      if (inherits(bmd2, "try-error")) { bmd2 <- NA }
      if(monotonic_behaviour == "Down"){
        bmdl <- bmd1
        bmdu <- bmd2
      } else {
        bmdl <- bmd2
        bmdu <- bmd1
      }
      
      # calculate ed50 and both bounds
      half_con <-  stats::approx(x = conf_interval_cv[doseRange_1st, 1], y = doseRange[doseRange_1st], xout = half_resp)$y
      half_con_1 <-  try(stats::approx(x = conf_interval_cv[doseRange_1st, 2], y = doseRange[doseRange_1st], xout = half_resp)$y)
      half_con_2 <-  try(stats::approx(x = conf_interval_cv[doseRange_1st, 3], y = doseRange[doseRange_1st], xout = half_resp)$y)
      
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
      
      # Export the info.
      bmd_val <- bmd_val %>% 
        mutate(FctName = gsub("(.*)\\()", "\\1", fctName),
               Monotonicity = monotonic_behaviour, 
               BMD_res = response_level, 
               ED50_res = half_resp, 
               max_res = max(conf_interval_cv[doseRange_1st, 1]), 
               min_res = min(conf_interval_cv[doseRange_1st, 1]),
               BMD_Mean = bmd, BMDL = bmdl, BMDU = bmdu, 
               ED50_Mean = half_con, ED50L = edl, ED50U = edu)
      
    }
    
  }, error = function(e) {
    print(e)
    bmd_val = NULL
  })
  
  return(as.data.frame(bmd_val))
  
}



