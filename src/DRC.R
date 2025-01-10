
############################################################ Data tab ##################################################################################################

#### Reactive objects ######################################################################################

##### tidy data ---------------------------------------------------------------------------------------------

data <- eventReactive(input$upldData_Butn_drc,
                      {if (input$input_select == "smp") {
                        data <- read.xlsx("data/Sample_data_OP_paper.xlsx")
                      } else {
                        if (input$input_select == "upld") {
                          req(input$file1)
                          inFile <- input$file1
                          data <- read.xlsx(inFile$datapath)
                        } else {
                          req(input$text1)
                          # Input the string from textArea
                          tmp <- matrix(strsplit(input$text1, "\n")[[1]])
                          # Separator selected
                          Sep <- switch(input$SepP, '1'=",", '2'="\t", '3'=";")
                          # Colnames
                          Clnames <- strsplit(tmp[1], Sep)[[1]]
                          # Generate the dataframe
                          data <- matrix(0, length(tmp)-1, length(Clnames), dimnames = list(NULL, Clnames))
                          for(i in 2:length(tmp)){
                            Row <- strsplit(paste(tmp[i], Sep, Sep, sep = ""), Sep)[[1]]
                            data[i-1, ] <- Row[-length(Row)]
                          }
                          data <- data.frame(data)
                          colnames(data) <- Clnames
                        }
                      }
                        # Convert the class of data in last 3 columns
                        n_var <- ncol(data)-4
                        if(n_var >= 0) {
                          data[ , (n_var+2):ncol(data)] <- apply(data[ , (n_var+2):ncol(data)], 2, function(x) as.numeric(x))
                        }
                        return(data)   
                      })

##### reactive Variables - data_values------------------------------------------------------------------------------------
data_values <- reactiveValues()

observe({
  data_values$n_var <- ncol(data())-4
  data_values$x <- colnames(data())[data_values$n_var+2]
  data_values$y <- colnames(data())[data_values$n_var+4]
  if (data_values$n_var < 1) {
    data_values$color_var <- "grey"
  } else {
    data_values$color_var <- colnames(data())[1]
  }
  
  if (data_values$n_var <= 1 ) {
    data_values$facet_var_row <- ""
    data_values$facet_var_col <- ""
  } else {
    if (data_values$n_var == 2) {
      data_values$facet_var_row <- setdiff(colnames(data()[1:data_values$n_var]), data_values$color_var)[1]
      data_values$facet_var_col <- "."
    } else {
      data_values$facet_var_row <- setdiff(colnames(data()[1:data_values$n_var]), data_values$color_var)[1]
      data_values$facet_var_col <- paste(setdiff(colnames(data()[1:data_values$n_var]), c(data_values$color_var, data_values$facet_var_row)), collapse = "+")
    }
  }
  # minimum dose
  ori_dose <- data()[, data_values$n_var+2]
  
  if (min(ori_dose) == 0) {
    if (input$bp == "default") {
      log10cl <- round(log10(min(ori_dose[ori_dose > 0]))) - 1
      data_values$min_dose <- 10^(log10cl)
    } else {
      data_values$min_dose <- as.numeric(input$bp)
    }
  } else {
    data_values$min_dose <- min(ori_dose)
  }
  
})

#### Message box to remind users if the file doesn't contain the necessary columns -------------------------------------------------------
observeEvent(input$upldData_Butn_drc, {
  if (data_values$n_var < 0) {
    shinyalert(title = "Attention", 
               text = h5("It seems the data you uploaded doesn't have the nessary columns, please upload the data in the correct format."), 
               type = "error",
               html = TRUE)
  }
})



#### Scatterplot_dataset -----------------------------------------------------------------------------------
data_scat <- reactive({
  req(data_values$n_var >= 0)
  data_scat <- data()
  if(data_values$n_var == 0) {
    colnames(data_scat) <- c("Biphasic", "Conc", "Replicate", "Response")
  } else {
    colnames(data_scat) <- c(colnames(data())[1:data_values$n_var], "Biphasic", "Conc", "Replicate", "Response")
  }
  if (min(data_scat$Conc) == 0) {
    data_scat$Conc[data_scat$Conc == 0] <- data_values$min_dose
  }
  return(data_scat)
})


#### Mean and SD_dataset -----------------------------------------------------------------------------------
# Mean and SD_dataset
data_m_sd <- reactive({
  req(data_values$n_var >= 0)
  if(data_values$n_var == 0) {
    data_m_sd <- data_scat() %>% group_by(Conc) %>% dplyr::summarise(Mean = mean(Response), SD = sd(Response), .groups = "drop")
  } else {
    data_m_sd <- data_scat() %>% 
      group_by(across(colnames(data_scat())[1: data_values$n_var]), Conc) %>% 
      dplyr::summarise(Mean = mean(Response), SD = sd(Response), .groups = "drop")
  }
  return(data_m_sd)
})

#### Line plot ---------------------------------------------------------------------------------------------
lineplot <- reactive({
  req(data_values$n_var >= 0)
  req(data())
  
  if (data_values$n_var == 0) {
    lineplot <- ggplot(data = data_scat(), aes(x = Conc, y = Response)) + 
      # lines generated by loess model
      # geom_smooth(se = FALSE, size = 0.5) + 
      # lines generated by connecting the mean value
      geom_line(data = data_m_sd(), aes(x = Conc, y = Mean))
    # geom_hline(aes(yintercept = 50), linetype = "twodash", color = "red", alpha = 0.7)
  } else {
    lineplot <- ggplot(data = data_scat(), aes(x = Conc, y = Response, 
                                               color = eval(parse(text = data_values$color_var)), 
                                               group = eval(parse(text = data_values$color_var)))) + 
      labs(color = data_values$color_var) +
      # lines generated by loess model
      # geom_smooth(se = FALSE, size = 0.5) + 
      # lines generated by connecting the mean value
      geom_line(data = data_m_sd(), aes(x = Conc, y = Mean))
    # geom_hline(aes(yintercept = 50), linetype = "twodash", color = "red", alpha = 0.7)
  }
  
  lineplot <- lineplot +
    geom_point(alpha = 0.5) + 
    scale_x_log10() +
    xlab(data_values$x) + 
    ylab(data_values$y) +
    theme_few() +
    panel_border(colour = "black", size = 1, remove = FALSE) +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
  
  if (data_values$n_var > 1) {
    lineplot <- lineplot +
      # facet_grid
      facet_grid(eval(parse(text = paste0(data_values$facet_var_row, "~", data_values$facet_var_col))))
    # facet_wrap
    # facet_wrap(eval(parse(text = paste0(data_values$facet_var_row, "~",data_values$ facet_var_col))), ncol = 2)
  }
  return(lineplot)
})

#### Output ################################################################################################

#### Sample Data - Excel Download --------------------------------------------------------------------------
output$dl_smp <- downloadHandler(
  filename = function(){"Sample_Data.xlsx"},
  content = function(file) {
    smp <- read.xlsx("data/Sample_data_OP_paper.xlsx")
    write.xlsx(smp, file)
  }
)

#### Output table ------------------------------------------------------------------------------------------ 
output$df1 <- DT::renderDataTable({
  df_temp <- data()
  if (data_values$n_var >= 0) {
    df_temp[ , (data_values$n_var+2)] <- round(df_temp[ , (data_values$n_var+2)], 3)
    df_temp[ , (data_values$n_var+4)] <- round(df_temp[ , (data_values$n_var+4)], 2)
  }
  DT::datatable(df_temp, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
})

#### Output plot -------------------------------------------------------------------------------------------
output$lineplot <- renderPlot({
  return(lineplot())
})



#### Clear Data Button -------------------------------------------------------------------------------------
observeEvent(input$clearText_Butn, {
  updateTextAreaInput(session, inputId = "text1", label = "", value = "")
})



############################################################ ED50 Estimation tab #######################################################################################

#### Reactive Objects - Model & ED50 info. #################################################################


# Selected models and info. 
df_ed <- eventReactive(input$calculate_Butn, {
  req(data())
  
  ###########################################################
  # number of variables
  n_var <- isolate({data_values$n_var})
  
  # Model list
  ## Monotonic
  fctList_monotnc <- NULL
  ### Log-logistic models (4 parms)
  if (input$LL4) {
    if(is.na(as.numeric(input$LL4_c))) {c <- c("0", "NA")} else {c <- input$LL4_c}
    if(is.na(as.numeric(input$LL4_d))) {d <- c("1", "NA")} else {d <- input$LL4_d}
    c_d <- expand.grid(c = c, d = d)
    fctList_monotnc <- c(fctList_monotnc, paste("LL.4", c_d$c, c_d$d, sep = "_"))
  }
  ### Log-logistic models (5 parms)
  if (input$LL5) {
    if(is.na(as.numeric(input$LL5_c))) {c <- c("0", "NA")} else {c <- input$LL5_c}
    if(is.na(as.numeric(input$LL5_d))) {d <- c("1", "NA")} else {d <- input$LL4_d}
    c_d <- expand.grid(c = c, d = d)
    fctList_monotnc <- c(fctList_monotnc, paste("LL.5", c_d$c, c_d$d, sep = "_"))
  }
  ### Weibull I models
  if (input$W1) {
    if(is.na(as.numeric(input$W1_c))) {c <- c("0", "NA")} else {c <- input$W1_c}
    if(is.na(as.numeric(input$W1_d))) {d <- c("1", "NA")} else {d <- input$W1_d}
    c_d <- expand.grid(c = c, d = d)
    fctList_monotnc <- c(fctList_monotnc, paste("W1.4", c_d$c, c_d$d, sep = "_"))
  }
  ### Weibull II models
  if (input$W2) {
    if(is.na(as.numeric(input$W2_c))) {c <- c("0", "NA")} else {c <- input$W2_c}
    if(is.na(as.numeric(input$W2_d))) {d <- c("1", "NA")} else {d <- input$W2_d}
    c_d <- expand.grid(c = c, d = d)
    fctList_monotnc <- c(fctList_monotnc, paste("W2.4", c_d$c, c_d$d, sep = "_"))
  }
  
  ## Biphasic
  fctList_biphsc <- NULL
  if (any(data_scat()$Biphasic == "Y")) {
    ### Brain-Cousens models
    if (input$BC) {
      if(is.na(as.numeric(input$BC_c))) {c <- c("0", "NA")} else {c <- input$BC_c}
      if(is.na(as.numeric(input$BC_d))) {d <- c("NA")} else {d <- input$BC_d}
      c_d <- expand.grid(c = c, d = d)
      fctList_biphsc <- c(fctList_biphsc, paste("BC.5", c_d$c, c_d$d, sep = "_"))
    }
    ### Cedergreen-Ritz-Streibig models
    if (input$CRS) {
      if(is.na(as.numeric(input$CRS_c))) {c <- c("0", "NA")} else {c <- input$CRS_c}
      c_d <- expand.grid(c = c, d = "NA")
      fctList_biphsc <- c(fctList_biphsc, 
                          paste("CRS.5a", c_d$c, c_d$d, sep = "_"), paste("CRS.5b", c_d$c, c_d$d, sep = "_"), paste("CRS.5c", c_d$c, c_d$d, sep = "_"),
                          paste("UCRS.5a", c_d$c, c_d$d, sep = "_"), paste("UCRS.5b", c_d$c, c_d$d, sep = "_"), paste("UCRS.5c", c_d$c, c_d$d, sep = "_"),
                          "CRS.6", "UCRS.6")
    }
    ### beta models
    if (input$beta) {
      fctList_biphsc <- c(fctList_biphsc, "DRC.beta")
    }
  }
  
  # Criteria used to select the best model
  const <- input$crtrn_selected
  
  # minimum dose
  bp <- input$bp
  
  # ED50 type
  ed50_type <- isolate({input$ed50_type})
  
  # Whether to include the Reed-Muench method
  # rm_ck <- input$two_point_method
  
  # p-values
  lac_p <- input$lac_p
  nell_p <- input$nell_p
  neffect_p <- input$neffect_p
  para0_p <- input$para0_p
  
  # ed_method
  ed_methods <- isolate({input$ed_methods})
  
  # mini_dose
  mini_dose <- isolate({data_values$min_dose})
  
  ############################# Compute ED info
  # Nested the dataframe
  Data <- data()
  if (n_var == 0) {
    colnames(Data) <- c("Biphasic", "Conc", "Replicate", "Response")
    Data <- Data %>% nest()
  } else {
    colnames(Data) <- c(colnames(data())[1:n_var], "Biphasic", "Conc", "Replicate", "Response")
    Data <- Data %>% 
      group_by(across(1:n_var)) %>% 
      nest()
  }
  
  # Find the best model for each curve
  model_drc <- Data %>% 
    mutate(best_model = purrr::pmap(list(data), m_select_new, fctList_monotnc = fctList_monotnc, fctList_biphsc = fctList_biphsc, const = const)) %>% 
    unnest(best_model)
  
  # Statistical Test for the model
  model_drc <- model_drc %>% 
    mutate(StatsTest = purrr::pmap(list(data), stats_test, fct = fctList_f)) %>% 
    unnest(StatsTest)
  
  model_drc <- model_drc %>% 
    mutate(Lac_of_fit_Test = map(Lac_of_fit_p, ~sig_test(.x, p = lac_p, sign = ">")),
           Neills_Test = map(Neills_Test_p, ~sig_test(.x, p = nell_p, sign = ">")),
           No_Effect_Test = map(No_Effect_Test_p, ~sig_test(.x, p = neffect_p, sign = "<")),
           Para_Test = map(Para_Info, ~para_sig_test(.x, p = para0_p)),
           f_related = map(Para_Info, f_related)) %>% 
    unnest(f_related)
  colnames(model_drc)[n_var+1] <- "RawData"
  
  # ED estimation method
  if (ed_methods == 'serra_greco_method') {
    model_drc <- model_drc %>% 
      mutate(Method = "Serra-Greco") %>% 
      mutate(ED_info = purrr::pmap(list(RawData), compute_ed_SG, bp = bp, fct = fctList_f, ed50_type = ed50_type, minidose = mini_dose)) %>% 
      unnest(ED_info)
  } else {
    model_drc <- model_drc %>% 
      mutate(Method = "Ritz-Gerhard") %>% 
      mutate(ED_info = purrr::pmap(list(RawData), compute_ed_Std, bp = bp, fct = fctList_f, ed50_type = ed50_type, minidose = mini_dose, c = c, d = d)) %>% 
      unnest(ED_info)
  }
  
  # Reed-and-Muench Method
  model_drc <- model_drc %>% 
    mutate(RM_method = purrr::pmap(list(RawData), RM_method_f, ed50_type = ed50_type)) %>% 
    unnest(RM_method)
  
  # Fit the data to loess model
  if (n_var == 0) {
    loess_drc <- data_scat() %>% 
      nest() %>% 
      mutate(Curve_Loess_data = purrr::pmap(list(data), loess_fit_drc, bp = bp, minidose = mini_dose))
    colnames(loess_drc)[1] <- "ScatterData"
  } else {
    loess_drc <- data_scat() %>% 
      group_by(across(1:n_var)) %>% 
      nest() %>% 
      mutate(Curve_Loess_data = purrr::pmap(list(data), loess_fit_drc, bp = bp, minidose = mini_dose))
    colnames(loess_drc)[n_var+1] <- "ScatterData"
  }
  
  # combine all the information
  if (n_var == 0) {
    df_ed <- data_m_sd() %>% nest() %>% cbind(loess_drc, model_drc)
  } else {
    df_ed <- data_m_sd() %>% 
      group_by(across(1:n_var)) %>% nest() %>% 
      left_join(loess_drc, by = colnames(loess_drc)[1:n_var]) %>% 
      left_join(model_drc, by = colnames(model_drc)[1:n_var])
  }
  colnames(df_ed)[n_var+1] <- "MeanData"
  
  df_ed <- df_ed %>%
    ungroup() %>% 
    mutate(across(where(~ !is.list(.x)), ~ {
      replaced <- if (is.logical(.x) || is.numeric(.x)) as.character(.x) else .x
      replaced <- replace_na(replaced, "/") # Replace NA with "/"
      ifelse(replaced == "NaN", "/", replaced) # Replace "NaN" with "/"
    }))
  
  return(df_ed)
  
})


#### Output - ED50 table ###################################################################################

## exported data frame
df_ed_exp <- reactive({
  req(df_ed())
  n_var <- isolate({data_values$n_var})
  ed_methods <- isolate({input$ed_methods})
  ed50_type <- isolate({input$ed50_type})
  # variable names
  if (n_var != 0) { selected_var <- c(1:n_var, (n_var+11)) } else {selected_var <- NULL}
  # ed related
  selected_var <- c(selected_var, (n_var+18):(n_var+21), (n_var+26), (n_var+29):(n_var+48))
  colnm <- c("Model", "Lack-of-fit test", "Neill's test", "No effet test", "Parameters ≠ 0", "Method", 
             "Monotonicity", "Maximum_Response", "Minimum_Response", "Start_Response", "M_Response", "Response_at_Low_ED\u2085\u2080", "Response_at_High_ED\u2085\u2080", 
             "Low_ED\u2085\u2080_Mean", "Low_ED\u2085\u2080_SE", "Low_ED\u2085\u2080_LowerBound", "Low_ED\u2085\u2080_UpperBound", 
             "High_ED\u2085\u2080_Mean", "High_ED\u2085\u2080_SE", "High_ED\u2085\u2080_LowerBound", "High_ED\u2085\u2080_UpperBound",
             "LDS_Mean", "LDS_SE", "LDS_LowerBound", "LDS_UpperBound", "M")
  # RM method
  if (ed50_type == "absolute") {
    selected_var <- c(selected_var, (n_var+49):(n_var+54))
    colnm <- c(colnm, "RM_ED\u2085\u2080_Mean", "RM_ED\u2085\u2080_SD", "RM_ED\u2085\u2080_CV", "RM_ED\u2085\u2080_SE", "RM_ED\u2085\u2080_LowerBound", "RM_ED\u2085\u2080_UpperBound")
  }
  # f values
  if (any(c("Brain-Cousens", "beta", "Cedergreen-Ritz-Streibig") %in% df_ed()$Model)) {
    selected_var <- c(selected_var, (n_var+22):(n_var+25)) 
  }
  colnm <- c(colnm, "f", "f_p_value", "f_LowerBound", "f_UpperBound")
  
  df_temp <- df_ed()[ , selected_var]
  colnames(df_temp)[(n_var+1):ncol(df_temp)] <- colnm
  return(df_temp)
})

## tables for display
# low ED50s for monotonic curves
ED50_table_Monotonic <- reactive({
  
  req(df_ed())
  n_var <- isolate({data_values$n_var})
  ed_methods <- isolate({input$ed_methods})
  ed50_type <- isolate({input$ed50_type})
  
  # variable names
  if (n_var != 0) { selected_var <- c(1:n_var) } else {selected_var <- NULL}
  
  # low ED50s for monotonic curves
  selected_var <- c(selected_var, (n_var+26), (n_var+34), (n_var+36):(n_var+39))
  df_temp <- df_ed() %>% filter(Model %in% c("Log-logistic (4 paras)", "Log-logistic (5 paras)", "Weibull I", "Weibull II")) %>% dplyr:: select(selected_var)
  colnames(df_temp)[(n_var+1):ncol(df_temp)] <- c("Method", "Response at ED\u2085\u2080", "ED\u2085\u2080 Mean", "ED\u2085\u2080 SE", "ED\u2085\u2080 Lower Bound", "ED\u2085\u2080 Upper Bound")
  
  # change the format of digit display
  df_temp <- df_temp %>% mutate(across((n_var+2):ncol(df_temp), ~ map_chr(.x, display_format)))

  return(df_temp)
  
})

# both low&high ED50s for biphasic curves
ED50_table_Biphasic <- reactive({
  req(df_ed())
  n_var <- isolate({data_values$n_var})
  ed_methods <- isolate({input$ed_methods})
  ed50_type <- isolate({input$ed50_type})
  
  # variable names
  if (n_var != 0) { selected_var <- c(1:n_var) } else {selected_var <- NULL}
  
  # low ED50s for biphasic curves
  if (input$bi_ed == "low_ed") {
    selected_var <- c(selected_var, (n_var+26), (n_var+34), (n_var+36):(n_var+39))
    colnms <- c("Method", "Response at Low ED\u2085\u2080", "Low ED\u2085\u2080 Mean", "Low ED\u2085\u2080 SE", "Low ED\u2085\u2080 Lower Bound", "Low ED\u2085\u2080 Upper Bound")
  }
  # high ED50s for biphasic curves
  if (input$bi_ed == "high_ed") {
    selected_var <- c(selected_var, (n_var+26), (n_var+35), (n_var+40):(n_var+43))
    colnms <- c("Method", "Response at High ED\u2085\u2080", "High ED\u2085\u2080 Mean", "High ED\u2085\u2080 SE", "High ED\u2085\u2080 Lower Bound", "High ED\u2085\u2080 Upper Bound")
  }
  # LDS for biphasic curves
  if (input$bi_ed == "lds") {
    selected_var <- c(selected_var, (n_var+26), (n_var+32), (n_var+44):(n_var+47))
    colnms <- c("Method", "Response at LDS", "LDS Mean", "LDS SE", "LDS Lower Bound", "LDS Upper Bound")
  }
  # M for biphasic curves
  if (input$bi_ed == "m") {
    selected_var <- c(selected_var, (n_var+26), (n_var+33), (n_var+48))
    colnms <- c("Method", "Response at M", "M (Maximum Stimulation/Inhibition)")
  }
  # f for biphasic curves
  if (input$bi_ed == "f") {
    selected_var <- c(selected_var, (n_var+26), (n_var+22):(n_var+25))
    colnms <- c("Method", "f", "p-value (f=0)", "f Lower Bound", "f Upper Bound")
  }
  
  df_temp <- df_ed() %>% filter(Model %in% c("Brain-Cousens", "beta", "Cedergreen-Ritz-Streibig")) %>% dplyr:: select(selected_var)
  colnames(df_temp)[(n_var+1):ncol(df_temp)] <- colnms
  # change the format of digit display
  df_temp <- df_temp %>% mutate(across((n_var+2):ncol(df_temp), ~ map_chr(.x, display_format)))
  
  return(df_temp)
  
})


RM_ED50_table <- reactive({
  req(df_ed())
  n_var <- isolate({data_values$n_var})
  ed_methods <- isolate({input$ed_methods})
  
  # variable names
  if (n_var != 0) { selected_var <- c(1:n_var) } else {selected_var <- NULL}
  
  # ED means of RM method
  selected_var <- c(selected_var, (n_var+49):(n_var+50), (n_var+53):(n_var+54))

  df_temp <- df_ed()[ , selected_var]
  colnames(df_temp)[(n_var+1):ncol(df_temp)] <- c("ED\u2085\u2080 Mean", "ED\u2085\u2080 SD", "ED\u2085\u2080 Lower Bound", "ED\u2085\u2080 Upper Bound")
  df_temp <- df_temp %>% mutate(across((n_var+1):ncol(df_temp), ~ map_chr(.x, display_format)))

  return(df_temp)
})

# Statistical tests
Stats_table <- reactive({
  req(df_ed())
  n_var <- isolate({data_values$n_var})
  ed_methods <- isolate({input$ed_methods})
  ed50_type <- isolate({input$ed50_type})
  lac <- isolate({input$lac})
  nell <- isolate({input$nell})
  neffect <- isolate({input$neffect})
  para0 <- isolate({input$para0})
  # variable names
  if (n_var != 0) { selected_var <- c(1:n_var) } else {selected_var <- NULL}
  # Model
  selected_var <- c(selected_var, (n_var+11)) 
  # p-value
  colnm <- NULL
  if (lac) {selected_var <- c(selected_var, n_var+18); colnm <- c(colnm, "Lack-of-fit test")}
  if (nell) {selected_var <- c(selected_var, n_var+19); colnm <- c(colnm, "Neill's test")}
  if (neffect) {selected_var <- c(selected_var, n_var+20); colnm <- c(colnm, "No effet test")}
  if (para0) {selected_var <- c(selected_var, n_var+21); colnm <- c(colnm, "Parameters ≠ 0")}
  df_temp <- df_ed()[ , selected_var]
  colnames(df_temp)[(n_var+2):ncol(df_temp)] <- colnm
  return(df_temp)
})

output$tb_stats <- DT::renderDataTable({
  Stats_table()
})

output$tb_ed_mono <- DT::renderDataTable({
  ED50_table_Monotonic()
})

output$tb_ed_bi <- DT::renderDataTable({
  ED50_table_Biphasic()
})

output$tb_ed_rm <- DT::renderDataTable({
  RM_ED50_table()
})


#### Triggered UI ##########################################################################################

#### Biphasic models ui ------------------------------------------------------

output$biphasicmodels <- renderUI({
  req(data_scat())
  if (any(data_scat()$Biphasic == "Y")) {
    wellPanel(
      tags$span("- Biphasic Curves") %>% 
        helper(icon = "question-circle", 
               type = "markdown",
               content = "Biphasic",
               buttonLabel = "Close"),
      div(style = "margin-top: 10px"), 
      fluidRow(
        div(style = "text-align: center; display:flex;align-items:center;", 
            column(6, ""),
            column(3, "Lower"),
            column(3, "Upper")
        )
      ),
      fluidRow(
        div(style = "display: flex; justify-content: center; align-items: center;", 
            column(6, div(style = "display:flex;align-items:center;", checkboxInput("BC", "Brain-Cousens", TRUE))),
            column(3, style = "margin-right: 5px;", 
                   textInput("BC_c", NULL, "Not Fixed")),
            column(3, style = "margin-right: 5px;", 
                   textInput("BC_d", NULL, "Not Fixed"))
        )
      ),
      fluidRow(
        div(style = "display: flex; justify-content: center; align-items: center;", 
            column(6, div(style = "display:flex;align-items:center;", checkboxInput("CRS", "Cedergreen-Ritz-Streibig", FALSE))),
            column(3, style = "margin-right: 5px;", 
                   textInput("CRS_c", NULL, "Not Fixed")),
            column(3, style = "margin-right: 5px;")
        )
      ),
      fluidRow(
        div(style = "display: flex; justify-content: center; align-items: center;", 
            column(6, div(style = "display:flex;align-items:center;", checkboxInput("beta", "beta", FALSE))),
            column(6, style = "margin-right: 10px;")
        )
      )
    )
  }
})

output$ed50_results <- renderUI({
  req(df_ed())
  tagList(
    if (any(df_ed()$Model %in% c("Log-logistic (4 paras)", "Log-logistic (5 paras)", "Weibull I", "Weibull II"))) {
      tagList(
        h5(HTML(paste0("ED", tags$sub("50"), " Estimation Table (Monotonic Curves)")), align = 'center'),
        div(style = "margin-top: -10px"),
        hr(),
        div(style = "margin-top: -10px"),
        DT::dataTableOutput("tb_ed_mono") %>% shinycssloaders::withSpinner()
      )
    },
    if (any(df_ed()$Model %in% c("Brain-Cousens", "beta", "Cedergreen-Ritz-Streibig"))) {
      tagList(
        h5(HTML(paste0("ED", tags$sub("50"), " Estimation Table (Biphasic Curves)")), align = 'center'),
        div(style = "margin-top: -10px"),
        hr(),
        radioButtons(inputId = "bi_ed",
                     label = NULL, 
                     choiceNames = list(HTML(paste0("Low ED", tags$sub("50"), "&nbsp;")), 
                                        HTML(paste0("High ED", tags$sub("50"), "&nbsp;")),
                                        HTML("LDS&nbsp;"), HTML("M (Maximum Stimulation/Inhibition)&nbsp;"), "f (Hormetic factor)"),
                     choiceValues = list("low_ed", "high_ed", "lds", "m", "f"),
                     selected = "low_ed",
                     inline = TRUE), 
        div(style = "margin-top: -10px"),
        DT::dataTableOutput("tb_ed_bi") %>% shinycssloaders::withSpinner()
      )
    },
    conditionalPanel(condition = "input.ed50_type == 'Absolute' && input.two_point_method == true",
                     div(style = "margin-top: 20px"),
                     h5(HTML(paste0("ED", tags$sub("50"), " Estimation - Reed-and-Muench Method")), align = 'center'),
                     div(style = "margin-top: -10px"),
                     hr(),
                     div(style = "margin-top: -10px"),
                     DT::dataTableOutput("tb_ed_rm") %>% shinycssloaders::withSpinner()
    ),
    div(style = "margin-top: 20px"),
    h5("Model Assessment Results", align = 'center'),
    div(style = "margin-top: -10px"),
    hr(),
    div(style = "margin-top: -10px"),
    DT::dataTableOutput("tb_stats") %>% shinycssloaders::withSpinner(),
    tags$b("Note:"),
    #      p(HTML(paste0("1. The estimated ED", tags$sub("50"), " values with a ", tags$span("'*'", style = "font-size: 20px; background-color: #E6F2FF"), " indicate the ED", tags$sub("50"), 
    #                    " is bracketed on one side by only a single point; in this case the ED", tags$sub("50"), 
    #                    " is estimated by the", em("'Reed and Muench method'"), " (Ramakrishnan,", em("World J Virol"), 
    #                    ", 2016). If you have many values with asterisks you should consider using a different concentration range."))),
    p(HTML(paste0("To prevent over-fitting, we highly recommand you to choose the models based on your own experiment setup, 
                    if you let the app to choose the best models for you, 
                    the best models reported are selected from the drc analysis (Ritz C,", 
                  em("et al."), ", ", em("PLoS One"), ", 2015), based on the criteria you choose on the left."))),
    p(tags$b("Reference:")),
    p(em("Ritz C, Baty F, Streibig JC, Gerhard D (2015) Dose-Response Analysis Using R. PLoS One. 10(12)")), 
    div(style = "margin-top: -15px"), 
    p(em("Serra A. Et al. (2020) BMDx: a graphical Shiny application to perform Benchmark Dose analysis for transcriptomics data. Bioinformatics 36: 2932–2933")),
    div(style = "margin-top: -15px"), 
    p(em("Ramakrishnan MA (2016) Determination of 50% endpoint titer using a simple formula. World J Virol. 5: 85–86"))
  )
})



############################################################ Plot tab ################################################################################################## 
#### Triggered UI ####################################################################################################

#### Message box to remind users to choose the model -------------------------------------------------------
observeEvent(input$tabs1, {
  req(df_ed())
  # Show a message if some of curves cannot be fitted to any of the models
  if (input$tabs1 == "Step 3: Generate plot" & any(df_ed()$FctName=="/") & input$datatype == 'drc') {
    shinyalert(title = "Attention", 
               text = h5("Some data could not be fitted with the selected models. Please select one of the options on the left to determine how to plot these data."), 
               type = "warning",
               html = TRUE)
  }
})

#### Plot_model_UI ---------------------------------------------------------------------------------------
output$plot_model_ui <- renderUI({
  req(df_ed())
  if (any(df_ed()$FctName=="/") & input$datatype == 'drc') {
    wellPanel(p("Some data couldn't be fitted with the selected models. Please choose the way to plot them."),
              selectInput(inputId = "model_selected",
                          label = "Select the method:",
                          choices = c("Simple line plot" = "line",
                                      "Using Loess model" = "loess"),
                          selected = "line")
    )
  }
})


#### Plot_layout_ui ---------------------------------------------------------------------------------------

output$plot_layout_ui <- renderUI({
  req(data())
  req(data_values$n_var >= 0)
  if(data_values$n_var != 0) {
    wellPanel(h5("Layout"),
              selectInput(inputId = "line_color_v",
                          label = "Set line colors according to:",
                          choices = colnames(data())[1:data_values$n_var],
                          selected = colnames(data())[1]),   
              if (data_values$n_var > 1) {
                selectInput(inputId = "facet_row_v",
                            label = "Set faceting groups on the rows by:",
                            choices = setdiff(colnames(data())[1:data_values$n_var], colnames(data())[1]),
                            selected = setdiff(colnames(data())[1:data_values$n_var], colnames(data())[1])[1])
              }
    )
  }
})

observeEvent(input$line_color_v, {
  updateSelectInput(session, inputId = "facet_row_v",
                    choices = setdiff(colnames(data())[1:data_values$n_var], input$line_color_v),
                    selected = setdiff(colnames(data())[1:data_values$n_var], input$line_color_v)[1])
})

#### Plot_appearance_ui ---------------------------------------------------------------------------------------
output$plot_appearance_ui <- renderUI({
  wellPanel(h5("Appearance"),
            div(style = "margin-top: -20px"),
            selectInput(inputId = "plot_appearance",
                        label = "",
                        choices = c("All Replicates" = "all", "Mean and SD" = "m_sd"),
                        selected = "all")
  )
})


#### Plot_ED&Responses_Line_UI -----------------------------------------------------------------------------

#### ED50 & Max/Min
output$plot_resline_ui <- renderUI({
  wellPanel(h6("Show the ED values and the corresponding responses："), 
            div(style = "display: inline-block; vertical-align: top;", 
                checkboxInput(inputId = "plot_ed50_ck", label = HTML(paste0("ED", tags$sub("50"))), value = FALSE)), 
            uiOutput(outputId = "lds_m"), 
            div(), 
            div(style = "display: inline-block; vertical-align: top;margin-top: -15px;", 
                checkboxInput(inputId = "plot_resline_ck", label = "Max & Min Responses", value = FALSE)), 
            div(), 
            div(style = "display: inline-block; vertical-align: top;margin-top: -15px;", 
                checkboxInput(inputId = "plot_ci_ck", label = "Confidence Intervals", value = FALSE))
            
  )
})

#### Biphasic LDS & M
output$lds_m <- renderUI({
  req(data_scat())
  if (any(data_scat()$Biphasic == "Y")) {
    tagList(
      div(), 
      div(style = "display: inline-block; vertical-align: top; margin-top: -15px;", 
          checkboxInput(inputId = "plot_lds_m_ck", label = "LDS & M", value = FALSE)
      )
    )
  }
})

#### Download UI -------------------------------------------------------------------------------------------
output$dl <- renderUI({
  req(input$plot_Butn_1)
  tagList(
    list(
      h5("Download"),
      div(style = "margin-top: -10px"),
      hr(),
      div(style = "margin-top: -10px"),
      
      # Plot Download related
      textInput(inputId = "file_name_1", label = "Enter a file name: ", value = Sys.time()),
      div(style = "margin-top: -10px"),
      div(style = "display: inline-block; vertical-align:top; width: 100px;",
          textInput(inputId = "width_1", label = "Width", value = 8)),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 100px;",
          textInput(inputId = "height_1", label = "Height", value = 4)),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 150px;",
          selectInput(inputId = "file_type_1", 
                      label = "Select file type: ", 
                      choices = list("PNG", "JPEG", "PDF", "TIFF", "BMP", "SVG"),
                      selected = "PNG")),
      
      # Download button
      br(),
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_plot", label = "Download Plot")),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_plot_df", label = "Download Dataframe")),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_report", label = "Download Report")),
      
      
      # Notes
      div(style = "margin-top: 10px"),
      tags$b("Note:"),
      p("1. You can only show up to 10 different dose-response-curves in the plots, and please try to avoid ", 
        a(href = "https://www.storytellingwithdata.com/blog/2013/03/avoiding-spaghetti-graph", "spaghetti graph"), "."),  
      div(style = "margin-top: -15px"),
      p("2. The default size is only suitable for two plots; you can specify the aspect ratio for downloading."),  
      div(style = "margin-top: -15px"),
      p(HTML(paste0("3. The excel contains ED", tags$sub("50"), 
                    " table, both dataframes for generating scatterplot and lineplot.")))
      
    )
  )
})

#### Reactive Objects ######################################################################################

#### Lineplot_dataset --------------------------------------------------------------------------------------
data_predct <- eventReactive(input$plot_Butn_1, {
  req(df_ed())
  n_var <- isolate({data_values$n_var})
  
  if (any(df_ed()$FctName=="/")) {
    data_predct_na <- df_ed() %>% filter(FctName == "/")
    if (input$model_selected == "loess") {
      if (n_var == 0) {
        data_predct_na <- data_predct_na %>% 
          dplyr::select("Curve_Loess_data") %>% 
          unnest()
      } else {
        data_predct_na <- data_predct_na %>% 
          dplyr::select(1:n_var, "Curve_Loess_data") %>% 
          unnest()
      }
    }
    if (input$model_selected == "line") {
      if (n_var == 0) {
        data_predct_na <- data_predct_na %>% 
          dplyr::select("MeanData") %>% 
          unnest() %>% dplyr::select(2,1)
      } else {
        data_predct_na <- data_predct_na %>% 
          dplyr::select(1:n_var, "MeanData") %>% 
          unnest() %>% dplyr::select(1:n_var, (n_var+2), (n_var+1))
      }
    }
    colnames(data_predct_na)[(n_var+1)] <- c("Response")
  } else {
    data_predct_na <- NULL
  }
  
  if (!all(df_ed()$FctName == "/")) {
    data_predct <- df_ed() %>% filter(FctName != "/")
    if (n_var == 0) {
      data_predct <- data_predct %>% dplyr::select("Curve_BestFit_data")
    } else {
      data_predct <- data_predct %>% dplyr::select(1:n_var, "Curve_BestFit_data")
    }
    data_predct <- data_predct %>% unnest() %>% dplyr::select(1:(n_var+2))
    colnames(data_predct)[(n_var+1)] <- c("Response")
    if (!is.null(data_predct_na)) {data_predct <- rbind(data_predct, data_predct_na)}
  } else {
    data_predct <- data_predct_na
  }
  
  return(data_predct)
  
})


#### Default Plot -------------------------------------------------------------------------------------------

L_P <- reactive({
  
  n_var <- ncol(data_predct())-2
  
  color_var <- isolate({input$line_color_v})
  if (n_var <= 1 ) {
    facet_var_row <- ""
    facet_var_col <- ""
  } else {
    if (n_var == 2) {
      facet_var_row <- isolate({input$facet_row_v})
      facet_var_col <- "."
    } else {
      facet_var_row <- isolate({input$facet_row_v})
      facet_var_col <- isolate({paste(setdiff(colnames(isolate({data()}))[1:n_var], c(color_var, facet_var_row)), collapse = "+")})
    }
  }
  
  # Legend
  if (n_var != 0) {legend_order <- isolate({unique(isolate({data()})[[color_var]])})}
  
  # Palette
  cbPalette <- c("#00A4FF", "#FD7FEE", "#03DFCA", "#990A3A", "#F37B63", "#05B756", "#A3FB86", "#097C91", "#015EC9","#840EAA")
  
  # Appearance
  plot_appearance <- isolate({input$plot_appearance})
  
  # Annotation dataframe
  ed_methods <- isolate({input$ed_methods})
  ed50_type <- isolate({input$ed50_type})
  if (n_var == 0) { selected_var <- c((n_var+30):(n_var+48)) } else { selected_var <- c(1:n_var, (n_var+30):(n_var+48))}
  anno_df <- isolate({df_ed()})[ , selected_var]
  anno_df[, (n_var+1):ncol(anno_df)] <- lapply(anno_df[, (n_var+1):ncol(anno_df)], as.numeric)
  
  if (n_var == 0) {
    # color
    clr <- get_palette(cbPalette, 1)
    # plot
    p <- ggplot(data = data_predct(), aes(x = Conc, y = Response)) + geom_line(color = clr)
    # appearance
    if (plot_appearance == "all") {
      p <- p + geom_point(data = isolate({data_scat()}), aes(x = Conc, y = Response), alpha = 0.5, color = clr)
    } else {
      p <- p + geom_point(data = isolate({data_m_sd()}), aes(x = Conc, y = Mean), alpha = 0.5, color = clr) + 
        geom_errorbar(data = isolate({data_m_sd()}), aes(x = Conc, y = Mean, ymin = Mean - SD, ymax = Mean + SD), width = 0.2, alpha = 0.8, color = clr)
    }
    # ED50
    if (input$plot_ed50_ck == TRUE) {
      p <- p + 
        # response lines
        geom_hline(data = anno_df, aes(yintercept = ED50_l_res), linetype = "longdash", alpha = 0.5, color = clr) + 
        geom_hline(data = anno_df, aes(yintercept = ED50_r_res), linetype = "longdash", alpha = 0.5, color = clr) + 
        # ed lines - left
        geom_vline(data = anno_df, aes(xintercept = ED50_l_Mean), linetype = "longdash", alpha = 0.5, color = clr) + 
        # ed lines - right
        geom_vline(data = anno_df, aes(xintercept = ED50_r_Mean), linetype = "longdash", alpha = 0.5, color = clr)
      
      if (input$plot_ci_ck == TRUE) {
        p <- p +
          # ed lines - left
          geom_vline(data = anno_df, aes(xintercept = ED50_l_L), linetype = "dotted", alpha = 0.5, color = clr) + 
          geom_vline(data = anno_df, aes(xintercept = ED50_l_U), linetype = "dotted", alpha = 0.5, color = clr) +
          # ed lines - right
          geom_vline(data = anno_df, aes(xintercept = ED50_r_L), linetype = "dotted", alpha = 0.5, color = clr) + 
          geom_vline(data = anno_df, aes(xintercept = ED50_r_U), linetype = "dotted", alpha = 0.5, color = clr)
      }
      
    }
    # Max & Min
    if (input$plot_resline_ck == TRUE) {
      p <- p +
        # response lines
        geom_hline(data = anno_df, aes(yintercept = max_res), linetype = "longdash", alpha = 0.5, color = clr) + 
        geom_hline(data = anno_df, aes(yintercept = min_res), linetype = "longdash", alpha = 0.5, color = clr)
    }
    # LDS & M
    if (any(isolate({df_ed()})$Model %in% c("Brain-Cousens", "beta", "Cedergreen-Ritz-Streibig"))) {
      if (input$plot_lds_m_ck == TRUE) {
        p <- p +
          # LDS
          geom_hline(data = anno_df, aes(yintercept = start_res), linetype = "longdash", alpha = 0.5, color = clr) + 
          geom_vline(data = anno_df, aes(xintercept = LDS_Mean), linetype = "longdash", alpha = 0.5, color = clr) + 
          # M
          geom_hline(data = anno_df, aes(yintercept = m_res), linetype = "longdash", alpha = 0.5, color = clr) + 
          geom_vline(data = anno_df, aes(xintercept = M), linetype = "longdash", alpha = 0.5, color = clr)
        
        if (input$plot_ci_ck == TRUE) {
          p <- p +
            # LDS
            geom_vline(data = anno_df, aes(xintercept = LDS_L), linetype = "dotted", alpha = 0.5, color = clr) + 
            geom_vline(data = anno_df, aes(xintercept = LDS_U), linetype = "dotted", alpha = 0.5, color = clr)
        }
      }
    }
    
  } else {
    # color
    n_color <- isolate({n_distinct(isolate({data_scat()})[[color_var]])})
    # plot
    p <- ggplot(data = data_predct(), aes(x = Conc, y = Response, color = eval(parse(text = color_var)), 
                                          group = eval(parse(text = color_var)))) + 
      geom_line() + 
      scale_color_manual(color_var, values = get_palette(cbPalette, n_color), limits = legend_order)
    # appearance
    if (plot_appearance == "all") {
      p <- p + geom_point(data = isolate({data_scat()}), aes(x = Conc, y = Response, group = eval(parse(text = color_var))), alpha = 0.5)
    } else {
      p <- p + geom_point(data = isolate({data_m_sd()}), aes(x = Conc, y = Mean, group = eval(parse(text = color_var))), alpha = 0.5) + 
        geom_errorbar(data = isolate({data_m_sd()}), aes(x = Conc, y = Mean, group = eval(parse(text = color_var)),
                                                         ymin = Mean - SD, ymax = Mean + SD), width = 0.2, alpha = 0.8)
    }
    # ED50
    if (input$plot_ed50_ck == TRUE) {
      p <- p +
        # response lines
        geom_hline(data = anno_df, aes(yintercept = ED50_l_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
        geom_hline(data = anno_df, aes(yintercept = ED50_r_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
        # ed lines - left
        geom_vline(data = anno_df, aes(xintercept = ED50_l_Mean, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
        # ed lines - right
        geom_vline(data = anno_df, aes(xintercept = ED50_r_Mean, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5)
      
      if (input$plot_ci_ck == TRUE) {
        p <- p +
          # ed lines - left
          geom_vline(data = anno_df, aes(xintercept = ED50_l_L, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5) + 
          geom_vline(data = anno_df, aes(xintercept = ED50_l_U, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5) +
          # ed lines - right
          geom_vline(data = anno_df, aes(xintercept = ED50_r_L, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5) + 
          geom_vline(data = anno_df, aes(xintercept = ED50_r_U, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5)
      }
    }
    # Max & Min
    if (input$plot_resline_ck == TRUE) {
      p <- p +
        # response lines
        geom_hline(data = anno_df, aes(yintercept = max_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
        geom_hline(data = anno_df, aes(yintercept = min_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5)
    }
    # LDS & M
    if (any(isolate({df_ed()})$Model %in% c("Brain-Cousens", "beta", "Cedergreen-Ritz-Streibig"))) {
      if (input$plot_lds_m_ck == TRUE) {
        p <- p +
          # LDS
          geom_hline(data = anno_df, aes(yintercept = start_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
          geom_vline(data = anno_df, aes(xintercept = LDS_Mean, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) +
          # M
          geom_hline(data = anno_df, aes(yintercept = m_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
          geom_vline(data = anno_df, aes(xintercept = M, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5)
        
        if(input$plot_ci_ck == TRUE) {
          p <- p +
            # LDS
            geom_vline(data = anno_df, aes(xintercept = LDS_L, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5) + 
            geom_vline(data = anno_df, aes(xintercept = LDS_U, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5)
        }
      }
    }
    
  }
  
  if (n_var >= 3) {
    p <- p +
      # facet_grid
      facet_grid(eval(parse(text = paste0(facet_var_row, "~", facet_var_col))))
  } else {
    if (n_var > 1){
      p <- p +
        # facet_wrap
        facet_wrap(eval(parse(text = paste0(facet_var_row, "~", facet_var_col))), ncol = 4)
    }
  }
  
  p <- p + 
    scale_x_log10(labels = function(x) format(x, scientific = FALSE)) +
    xlab(isolate({data_values$x})) + 
    ylab(isolate({data_values$y})) +
    theme_few() +
    panel_border(colour = "black", size = 1, remove = FALSE) +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))

  p
  
})



#### Output ################################################################################################

#### Output plot -------------------------------------------------------------------------------------------
output$drc_curve <- renderPlot({
  return(L_P())
})

#### Plot Download -----------------------------------------------------------------------------------------
output$dl_plot<- downloadHandler(
  #Specify The File Name 
  filename = function(){paste0(input$file_name_1, ".", tolower(input$file_type_1))},
  content = function(file){
    ggsave(file, L_P(), device = tolower(input$file_type_1),
           width = as.numeric(input$width_1), height = as.numeric(input$height_1))
  }
)

#### Excel Download ----------------------------------------------------------------------------------------
output$dl_plot_df <- downloadHandler(
  filename = function(){paste0(input$file_name_1, ".xlsx")},
  content = function(file) {
    list_of_datasets <- list("ED_related" = df_ed_exp(), 
                             "Bestfit_dataframe" = data_predct(), 
                             "ScatterPlot_dataframe" = data_scat(), 
                             "Mean_SD_dataframe" = data_m_sd()
    )
    write.xlsx(list_of_datasets, file)
  }
)

#### Report Download ---------------------------------------------------------------------------------------
output$dl_report <- downloadHandler(
  filename = function(){paste0(input$file_name_1, ".html")},
  content = function(file) {
    tempReport <- file.path(tempdir(), "Report_Default.Rmd")
    file.copy("reports/Report_Default.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params_1 <- list(table_mono = ED50_table_Monotonic(),
                     table_rm = RM_ED50_table(),
                     table_stats = Stats_table(),
                     ed50_type = input$ed50_type, 
                     ed_methods = input$ed_methods,
                     two_point_method = input$two_point_method,
                     n_var = ncol(data_predct())-2,
                     color_var = input$line_color_v,
                     legend_order = eval(parse(text = paste0("unique(data()$", input$line_color_v, ")"))),
                     ED_related = df_ed_exp(),
                     Bestfit_dataframe = data_predct(),
                     ScatterPlot_dataframe = data_scat(),
                     Mean_SD_dataframe = data_m_sd(),
                     Plot_appearance = input$plot_appearance,
                     plot_ed50_ck = input$plot_ed50_ck, 
                     plot_lds_m_ck = input$plot_lds_m_ck, 
                     plot_resline_ck = input$plot_resline_ck,
                     plot_ci_ck = input$plot_ci_ck,
                     label_x_axis = data_values$x,
                     label_y_axis = data_values$y
    )
    
    n_var <- ncol(data_predct())-2
    if (n_var <= 1 ) {
      params_1 <- list.append(params_1,
                              facet_var_row = "",
                              facet_var_col = "")
    } else {
      if (n_var == 2) {
        params_1 <- list.append(params_1,
                                facet_var_row = input$facet_row_v,
                                facet_var_col = ".")
      } else {
        params_1 <- list.append(params_1,
                                facet_var_row = input$facet_row_v,
                                facet_var_col = paste(setdiff(colnames(data())[1:n_var], c(input$line_color_v, input$facet_row_v)), collapse = "+"))
      }
    }
    
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params_1,
                      envir = new.env(parent = globalenv())
    )
  }
)


############################################################ Customized plot tab ##################################################################################################

source(file.path("src/CustomizedPlot.R"), local = TRUE)$value
