
############################################################ Data tab ##################################################################################################

#### Reactive objects ######################################################################################

##### tidy data ---------------------------------------------------------------------------------------------
data_te <- eventReactive(input$upldData_Butn_te,
                         {if (input$input_select_te == "smp") {
                           data <- read.xlsx("data/Sample_data_spencer_paper.xlsx")
                         } else {
                           if (input$input_select_te == "upld") {
                             req(input$file1_te)
                             inFile <- input$file1_te
                             data <- read.xlsx(inFile$datapath)
                           } else {
                             req(input$text1_te)
                             # Input the string from textArea
                             tmp <- matrix(strsplit(input$text1_te, "\n")[[1]])
                             # Separator selected
                             Sep <- switch(input$SepP_te, '1'=",", '2'="\t", '3'=";")
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
                             data[ , (n_var+1):ncol(data)] <- apply(data[ , (n_var+1):ncol(data)], 2, function(x) as.numeric(x))
                           }
                           return(data)   
                         })

##### reactive Variables - data_values------------------------------------------------------------------------------------
data_values_te <- reactiveValues()

observe({
  data_values_te$n_var <- ncol(data_te())-4
  data_values_te$before <- colnames(data_te())[data_values_te$n_var+2]
  data_values_te$after <- colnames(data_te())[data_values_te$n_var+3]
  data_values_te$count <- colnames(data_te())[data_values_te$n_var+4]
  if (data_values_te$n_var < 1) {
    data_values_te$color_var <- "grey"
  } else {
    data_values_te$color_var <- colnames(data_te())[1]
  }
  
  if (data_values_te$n_var <= 1 ) {
    data_values_te$facet_var_row <- ""
    data_values_te$facet_var_col <- ""
  } else {
    if (data_values_te$n_var == 2) {
      data_values_te$facet_var_row <- setdiff(colnames(data_te()[1:data_values_te$n_var]), data_values_te$color_var)[1]
      data_values_te$facet_var_col <- "."
    } else {
      data_values_te$facet_var_row <- setdiff(colnames(data_te()[1:data_values_te$n_var]), data_values_te$color_var)[1]
      data_values_te$facet_var_col <- paste(setdiff(colnames(data_te()[1:data_values_te$n_var]), c(data_values_te$color_var, data_values_te$facet_var_row)), collapse = "+")
    }
  }
})

#### Message box to remind users if the file doesn't contain the necessary columns -------------------------------------------------------
observeEvent(input$upldData_Butn_te, {
  # req(data())
  # Show a message if some of ED50s cannot be estimated by log-logistic model
  if (data_values_te$n_var < 0) {
    shinyalert(title = "Attention", 
               text = h5("It seems the data you uploaded doesn't have the nessary columns, please upload the data in the correct format."), 
               type = "error",
               html = TRUE)
  }
})



#### Scatterplot_dataset -----------------------------------------------------------------------------------
data_scat_te <- reactive({
  req(data_values_te$n_var >= 0)
  # number of variables
  n_var <- isolate({data_values_te$n_var})
  df_sum <- data_te() %>%
    #tidyr::unite(col = "colComb", all_of(colnames(data_te())[1:(n_var+1)]), sep = "_") %>%
    #group_by(colComb) %>%
    group_by(across(1:(n_var+1))) %>% 
    dplyr::summarise(sum = sum(.data[[data_values_te$count]], na.rm = TRUE), .groups = 'drop')
  df_accsum <- data_te() %>% 
    arrange(across(1:(n_var+2))) %>% 
    group_by(across(1:(n_var+3))) %>% 
    dplyr::summarise(count_sum = sum(.data[[data_values_te$count]], na.rm = TRUE), .groups = 'drop') %>% 
    #tidyr::unite(col = 'colComb', all_of(colnames(data_te())[1:(n_var+1)]), sep = '_') %>% 
    #left_join(df_sum, by = 'colComb') %>% 
    left_join(df_sum, by = colnames(df_sum)[1:(n_var+1)]) %>% 
    mutate(pct = count_sum/sum)
  
  data_scat_temp <- df_accsum %>% 
    #arrange(across(1:(n_var+1))) %>% 
    #group_by(colComb) %>% 
    arrange(across(1:(n_var+2))) %>% 
    group_by(across(1:(n_var+1))) %>% 
    dplyr::reframe(respns = cumsum(pct))
  
  data_scat_te <- df_accsum %>% 
    #arrange(across(1:2)) %>%
    mutate(respns = data_scat_temp$respns) %>% 
    #tidyr::separate(colComb, into = all_of(colnames(data_te())[1: (n_var+1)]), sep = '_') %>% 
    dplyr::select(all_of(colnames(data_te())[1: (n_var+3)]), respns)
  
  if(n_var == 0) {
    colnames(data_scat_te) <- c("Replicate", "Before", "After", "Response")
  } else {
    colnames(data_scat_te) <- c(colnames(data_te())[1:n_var], "Replicate", "Before", "After", "Response")
  }
  # data_scat_te$Before[data_scat_te$Before == 0] <- 0.03
  data_scat_te <- data_scat_te %>% mutate(After = replace(After, is.infinite(After), NA)) %>% drop_na()
  
  return(data_scat_te)
})

#### Mean dataset ------------------------------------------------------------------------------------------
# Mean and SD_dataset
data_m_te <- reactive({
  req(data_values_te$n_var >= 0)
  if(data_values_te$n_var == 0) {
    data_m_te <- data_scat_te() %>% 
      tidyr::unite(Before, After, col = 'time_int') %>% 
      pivot_wider(names_from = time_int, values_from = Response) %>% 
      pivot_longer(2:last_col(), names_to = 'time_int', values_to = 'Response') %>% 
      pivot_wider(names_from = Replicate, values_from = Response) %>% 
      dplyr::arrange(time_int) %>% 
      filter(!if_all(all_of(2:last_col()), is.na)) %>% 
      fill(everything(), .direction = 'down') %>% 
      mutate_all(~ replace_na(., 0)) %>% 
      dplyr::arrange(time_int) %>% 
      pivot_longer(2:last_col(), names_to = 'Replicate', values_to = 'Response') %>% 
      separate(time_int, into = c('Before', 'After')) %>% 
      group_by(After) %>% 
      mutate(After = as.numeric(After)) %>% 
      dplyr::summarise(Mean = mean(Response), .groups = 'drop')
  } else {
    n_var <- data_values_te$n_var
    data_m_te <- data_scat_te() %>%
      tidyr::unite(Before, After, col = "time_int") %>%
      pivot_wider(names_from = time_int, values_from = Response) %>%
      pivot_longer(cols = (n_var + 2):last_col(), names_to = "time_int", values_to = "Response") %>%
      pivot_wider(names_from = Replicate, values_from = Response) %>%
      dplyr::arrange(across(1:n_var), time_int) %>%
      filter(!if_all((n_var + 2):last_col(), is.na)) %>%
      group_by(across(1:n_var)) %>%
      nest() %>%
      mutate(data = lapply(data, remove_na_columns)) %>% 
      mutate(data = map(data, ~ .x %>% fill(everything(), .direction = "down") %>% pivot_longer(cols = 2:ncol(.x), names_to = "Replicate", values_to = "Response" ))) %>%
      unnest(cols = c(data)) %>%
      mutate(across(everything(), ~ replace_na(., 0))) %>%
      dplyr::arrange(across(1:n_var), time_int) %>%
      separate(time_int, into = c("Before", "After")) %>%
      group_by(across(1:n_var), After) %>%
      mutate(After = as.numeric(After)) %>%
      dplyr::summarise(Mean = mean(Response, na.rm = TRUE), .groups = 'drop')
  }
})

#### Line plot ---------------------------------------------------------------------------------------------
lineplot_te <- reactive({
  req(data_values_te$n_var >= 0)
  req(data_m_te())
  req(data_scat_te())
  
  if (data_values_te$n_var == 0) {
    lineplot <- ggplot(data = data_scat_te(), aes(x = After, y = Response)) + 
      # lines generated by connecting the mean value
      geom_line(data = data_m_te(), aes(x = After, y = Mean))
  } else {
    lineplot <- ggplot(data = data_scat_te(), aes(x = After, y = Response, 
                                                  color = eval(parse(text = data_values_te$color_var)), 
                                                  group = eval(parse(text = data_values_te$color_var)))) + 
      labs(color = data_values_te$color_var) +
      # lines generated by connecting the mean value
      geom_line(data = data_m_te(), aes(x = After, y = Mean))
  }
  
  lineplot <- lineplot +
    geom_point(alpha = 0.5) + 
    scale_x_log10() +
    xlab("Time") + 
    ylab("Rate of Event") +
    theme_few() +
    panel_border(colour = "black", size = 1, remove = FALSE) +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
  
  if (data_values_te$n_var > 1) {
    lineplot <- lineplot +
      # facet_grid
      facet_grid(eval(parse(text = paste0(data_values_te$facet_var_row, "~", data_values_te$facet_var_col))))
    # facet_wrap
    # facet_wrap(eval(parse(text = paste0(data_values$facet_var_row, "~",data_values$ facet_var_col))), ncol = 2)
  }
  return(lineplot)
})

#### Output ################################################################################################

#### Sample Data - Excel Download --------------------------------------------------------------------------
output$dl_smp_te <- downloadHandler(
  filename = function(){"Sample_Data.xlsx"},
  content = function(file) {
    smp <- read.xlsx("data/Sample_data_spencer_paper.xlsx")
    write.xlsx(smp, file)
  }
)

#### Output table ------------------------------------------------------------------------------------------ 
output$df1_te <- DT::renderDataTable({
  df_temp <- data_te()
  DT::datatable(df_temp, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
})

#### Output plot -------------------------------------------------------------------------------------------
output$lineplot_te <- renderPlot({
  return(lineplot_te())
})



#### Clear Data Button -------------------------------------------------------------------------------------
observeEvent(input$clearText_Butn_te, {
  updateTextAreaInput(session, inputId = "text1_te", label = "", value = "")
})


############################################################ T50 Estimation tab #######################################################################################

#### Reactive Objects - Model & ED50 info. #################################################################

# Selected models and info. 
df_et <- eventReactive(input$calculate_Butn_te, {
  
  # number of variables
  n_var <- isolate({data_values_te$n_var})
  # Model list
  fctList_monotnc <- NULL
  ## Log-logistic models
  if (input$LL4_te) {
    if (input$LL4_te_d == "1") {fctList_monotnc <- c(fctList_monotnc, "LL.2")}
    if (input$LL4_te_d == "NA") {fctList_monotnc <- c(fctList_monotnc, "LL.2", "LL.3")}
  }
  ## Log-normal models
  if (input$LN) {
    if (input$LN_d == "1") {fctList_monotnc <- c(fctList_monotnc, "LN.2")} 
    if (input$LN_d == "NA") {fctList_monotnc <- c(fctList_monotnc, "LN.2", "LN.3")}
  }
  ## Weibull I models
  if (input$W1_te) {
    if (input$W1_te_d == "1") {fctList_monotnc <- c(fctList_monotnc, "W1.2")}
    if (input$W1_te_d == "NA") {fctList_monotnc <- c(fctList_monotnc, "W1.2", "W1.3")}
  }
  ## Weibull II models
  if (input$W2_te) {
    if (input$W2_te_d == "1") {fctList_monotnc <- c(fctList_monotnc, "W2.2")}
    if (input$W2_te_d == "NA") {fctList_monotnc <- c(fctList_monotnc, "W2.2", "W2.3")}
  }
  
  # Criteria used to select the best model
  const <- input$crtrn_selected_te
  
  # Time intervals
  time_intv <- as.numeric(strsplit(input$time_intv, ",")[[1]])
  
  # Find the best model for each curve
  Data <- data_te()
  if (n_var == 0) {
    colnames(Data) <- c("Replicate", "Before", "After", "Count")
    model_te <- Data %>% 
      nest() %>% 
      mutate(ED_info = purrr::pmap(list(data), compute_et, fctList_monotnc = fctList_monotnc, const = const, time_intv = time_intv)) %>% 
      unnest(ED_info)
    colnames(model_te)[1] <- "RawData"
  } else {
    colnames(Data) <- c(colnames(data_te())[1:n_var], "Replicate", "Before", "After", "Count")
    model_te <- Data %>% 
      group_by(across(1:n_var)) %>% 
      nest() %>% 
      mutate(ED_info = purrr::pmap(list(data), compute_et, fctList_monotnc = fctList_monotnc, const = const, time_intv = time_intv)) %>% 
      unnest(ED_info)
    colnames(model_te)[n_var+1] <- "RawData"
  }
  
  # Fit the data to loess model
  if (n_var == 0) {
    loess_te <- data_scat_te() %>% 
      nest() %>% 
      mutate(Curve_Loess_data = purrr::map(data, loess_fit_te))
    colnames(loess_te)[1] <- "ScatterData"
  } else {
    loess_te <- data_scat_te() %>% 
      group_by(across(1:n_var)) %>% 
      nest() %>% 
      mutate(Curve_Loess_data = purrr::map(data, loess_fit_te))
    colnames(loess_te)[n_var+1] <- "ScatterData"
  }
  
  # combine all the information
  if (n_var == 0) {
    df_et <- data_m_te() %>% nest() %>% cbind(loess_te, model_te)
  } else {
    df_et <- data_m_te() %>% 
      group_by(across(1:n_var)) %>% nest() %>% 
      left_join(loess_te, by = colnames(loess_te)[1:n_var]) %>% 
      left_join(model_te, by = colnames(model_te)[1:n_var])
  }
  colnames(df_et)[n_var+1] <- "MeanData"
  df_et <- df_et %>% 
    ungroup() %>% 
    mutate(across(where(~ !is.list(.x)), ~ {
      replaced <- if (is.logical(.x) || is.numeric(.x)) as.character(.x) else .x
      replaced <- replace_na(replaced, "/") # Replace NA with "/"
      ifelse(replaced == "NaN", "/", replaced) # Replace "NaN" with "/"
    }))
  return(df_et)
})

#### Output - T50 table ###################################################################################
## exported data frame
df_et_exp <- reactive({
  req(df_et())
  n_var <- isolate({data_values_te$n_var})
  if (n_var != 0) {
    selected_var <- c(1:n_var, # variable names
                      (n_var+8), # model-related
                      (n_var+6):(n_var+7)) # T50
  } else {
    selected_var <- c((n_var+8), # model-related
                      (n_var+6):(n_var+7)) # T50
  }
  df_temp <- df_et()[ , selected_var] 
  colnames(df_temp)[(n_var+1):ncol(df_temp)] <- c("Model", "T\u2085\u2080_Mean", "T\u2085\u2080_SE")
  return(df_temp)
})

T50_table <- reactive({
  req(df_et_exp())
  n_var <- isolate({data_values_te$n_var})
  df_temp <- df_et_exp() 
  df_temp <- df_temp %>% mutate(across((n_var + 2):ncol(df_temp), ~ map_chr(.x, display_format)))
  colnames(df_temp)[(n_var+1):ncol(df_temp)] <- c("Model", "T\u2085\u2080 Mean", "T\u2085\u2080 SE")
  return(df_temp)
})

output$df2_te <- DT::renderDataTable({
  T50_table()
})

#### Triggered UI ##########################################################################################
output$et50_results <- renderUI({
  req(df_et())
  tagList(
    list(
      h5(HTML(paste0("T", tags$sub("50"), " Estimation Table")), align = 'center'),
      div(style = "margin-top: -10px"),
      hr(),
      div(style = "margin-top: -10px"),
      DT::dataTableOutput("df2_te") %>% shinycssloaders::withSpinner(),
      tags$b("Note:"),
      p(HTML(paste0("To prevent over-fitting, we highly recommand you to choose the models based on your own experiment setup, 
                    if you let the app to choose the best models for you, 
                    the best models reported are selected from the drc analysis (Ritz C,", 
                    em("et al."), ", ", em("PLoS One"), ", 2015), based on the criteria you choose on the left."))),
      p(tags$b("Reference:")),
      p(em("Ritz C, Baty F, Streibig JC, Gerhard D (2015) Dose-Response Analysis Using R. PLoS One. 10(12)"))
    )
  )
})



############################################################ Plot tab ################################################################################################## 
#### Triggered UI ####################################################################################################

#### Message box to remind users to choose the model -------------------------------------------------------
observeEvent(input$tabs1, {
  req(df_et())
  # Show a message if some of curves cannot be fitted to any of the models
  if (input$tabs1 == "Step 3: Generate plot" & any(df_et()$FctName=="/") & input$datatype == 'te') {
    shinyalert(title = "Attention", 
               text = h5("Some data could not be fitted with the selected models. Please select one of the options on the left to determine how to plot these data."), 
               type = "warning",
               html = TRUE)
  }
})

#### Plot_model_UI ---------------------------------------------------------------------------------------
output$plot_model_ui_te <- renderUI({
  req(df_et())
  if (any(df_et()$FctName=="/") & input$datatype == 'te') {
    wellPanel(p("Some data couldn't be fitted with the selected models. Please choose the way to plot them."),
              selectInput(inputId = "model_selected_te",
                          label = "Select the method:",
                          choices = c("Simple line plot" = "line",
                                      "Using Loess model" = "loess"),
                          selected = "line")
    )
  }
})

#### Plot_layout_ui ---------------------------------------------------------------------------------------
output$plot_layout_ui_te <- renderUI({
  req(data_te())
  req(data_values_te$n_var >= 0)
  if(data_values_te$n_var != 0) {
    wellPanel(h5("Layout"),
              selectInput(inputId = "line_color_v_te",
                          label = "Set line colors according to:",
                          choices = colnames(data_te())[1:data_values_te$n_var],
                          selected = colnames(data_te())[1]),   
              if (data_values_te$n_var > 1) {
                selectInput(inputId = "facet_row_v_te",
                            label = "Set faceting groups on the rows by:",
                            choices = setdiff(colnames(data_te())[1:data_values_te$n_var], colnames(data_te())[1]),
                            selected = setdiff(colnames(data_te())[1:data_values_te$n_var], colnames(data_te())[1])[1])
              }
    )
  }
})

observeEvent(input$line_color_v_te, {
  updateSelectInput(session, inputId = "facet_row_v_te",
                    choices = setdiff(colnames(data_te())[1:data_values_te$n_var], input$line_color_v_te),
                    selected = setdiff(colnames(data_te())[1:data_values_te$n_var], input$line_color_v_te)[1])
})

#### Plot_ED&Responses_Line_UI -----------------------------------------------------------------------------
output$plot_resline_ui_te <- renderUI({
  wellPanel(
    h6(HTML(paste0("Show the T", tags$sub("50"), " values："))),
    div(style = "display: inline-block; vertical-align: top;",
        checkboxInput(inputId = "plot_ed50_ck_te", label = HTML(paste0("T", tags$sub("50"))), value = FALSE))#,
    #div(style = "display: inline-block; vertical-align: top;",
    #    checkboxInput(inputId = "plot_resline_ck_te", label = "Max & Min Responses", value = FALSE))
  )
})


#### Download UI -------------------------------------------------------------------------------------------
output$dl_te <- renderUI({
  req(input$plot_Butn_1_te)
  tagList(
    list(
      h5("Download"),
      div(style = "margin-top: -10px"),
      hr(),
      div(style = "margin-top: -10px"),
      
      # Plot Download related
      textInput(inputId = "file_name_1_te", label = "Enter a file name: ", value = Sys.time()),
      div(style = "margin-top: -10px"),
      div(style = "display: inline-block; vertical-align:top; width: 100px;",
          textInput(inputId = "width_1_te", label = "Width", value = 8)),
      div(style = "display: inline-block; vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 100px;",
          textInput(inputId = "height_1_te", label = "Height", value = 4)),
      div(style = "display: inline-block; vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 150px;",
          selectInput(inputId = "file_type_1_te", 
                      label = "Select file type: ", 
                      choices = list("PNG", "JPEG", "PDF", "TIFF", "BMP", "SVG"),
                      selected = "PNG")),
      
      # Download button
      br(),
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_plot_te", label = "Download Plot")),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_plot_df_te", label = "Download Dataframe")),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_report_te", label = "Download Report")),
      
      
      # Notes
      div(style = "margin-top: 10px"),
      tags$b("Note:"),
      p("1. You can only show up to 10 different time-to-event-curves in the plots, and please try to avoid ", 
        a(href = "https://www.storytellingwithdata.com/blog/2013/03/avoiding-spaghetti-graph", "spaghetti graph"), "."),  
      div(style = "margin-top: -15px"),
      p("2. The default size is only suitable for two plots; you can specify the aspect ratio for downloading."),  
      div(style = "margin-top: -15px"),
      p(HTML(paste0("3. The excel contains T", tags$sub("50"), 
                    " table, both dataframes for generating scatterplot and lineplot.")))
      
    )
  )
})

#### Reactive Objects ######################################################################################

#### Lineplot_dataset --------------------------------------------------------------------------------------
data_predct_te <- eventReactive(input$plot_Butn_1_te, {
  n_var <- ncol(df_et())-8
  
  if (any(df_et()$FctName=="/")) {
    data_predct_te_na <- df_et() %>% filter(FctName == "/")
    if (input$model_selected_te == "loess") {
      if (n_var == 0) {
        data_predct_te_na <- data_predct_te_na %>% 
          dplyr::select("Curve_Loess_data") %>% 
          unnest() %>% dplyr::select(2,1)
      } else {
        data_predct_te_na <- data_predct_te_na %>% 
          dplyr::select(1:n_var, "Curve_Loess_data") %>% 
          unnest() %>% dplyr::select(1:n_var, (n_var+2), (n_var+1))
      }
    }
    if (input$model_selected_te == "line") {
      if (n_var == 0) {
        data_predct_te_na <- data_predct_te_na %>% 
          dplyr::select("MeanData") %>% 
          unnest()
      } else {
        data_predct_te_na <- data_predct_te_na %>% 
          dplyr::select(1:n_var, "MeanData") %>% 
          unnest()
      }
      
    }
    colnames(data_predct_te_na)[(n_var+1):(n_var+2)] <- c("After", "Response")
  } else {
    data_predct_te_na <- NULL
  }
  
  if (!all(df_et()$FctName == "/")) {
    data_predct_te <- df_et() %>% filter(FctName != "/")
    if (n_var == 0) {
      data_predct_te <- data_predct_te %>% dplyr::select("Curve_BestFit_data")
    } else {
      data_predct_te <- data_predct_te %>% dplyr::select(1:n_var, "Curve_BestFit_data")
    }
    data_predct_te <- data_predct_te %>% unnest(cols = c(Curve_BestFit_data)) %>% dplyr::select(1:(n_var+2))
    colnames(data_predct_te)[(n_var+1):(n_var+2)] <- c("After", "Response")
    if (is.null(data_predct_te_na)) {data_predct_te <- rbind(data_predct_te, data_predct_te_na)}
    
  } else {
    data_predct_te <- data_predct_te_na
  }
  return(data_predct_te)
  
})


#### Default Plot -------------------------------------------------------------------------------------------

L_P_te <- reactive({
  req(data_predct_te())
  req(data_te())
  n_var <- ncol(data_predct_te())-2
  
  # facet plot related
  color_var <- isolate({input$line_color_v_te})
  if (n_var <= 1 ) {
    facet_var_row <- ""
    facet_var_col <- ""
  } else {
    if (n_var == 2) {
      facet_var_row <- isolate({input$facet_row_v_te})
      facet_var_col <- "."
    } else {
      facet_var_row <- isolate({input$facet_row_v_te})
      facet_var_col <- isolate({paste(setdiff(colnames(data_te())[1:n_var], c(color_var, facet_var_row)), collapse = "+")})
    }
  }
  
  # Legend
  if (n_var != 0) {legend_order <- isolate({unique(data_te()[[color_var]])})}
  #if (n_var != 0) {legend_order <- eval(parse(text = paste0("unique(isolate({data_te()})$", color_var, ")")))}
  
  # Palette
  cbPalette <- c("#00A4FF", "#FD7FEE", "#03DFCA", "#990A3A", "#F37B63", "#05B756", "#A3FB86", "#097C91", "#015EC9","#840EAA")
  
  if (n_var == 0) {
    # color
    clr <- get_palette(cbPalette, 1)
    # annotation dataframe
    anno_df <- df_et() %>% dplyr::select((n_var+6):(n_var+8)) %>% 
      mutate(T50_res = 0.5, max_res = 1, min_res = 0)
    anno_df[, (n_var+1):ncol(anno_df)] <- lapply(anno_df[, (n_var+1):ncol(anno_df)], as.numeric)
    # plot
    p <- ggplot(data = data_predct_te(), aes(x = After, y = Response)) + 
      geom_line(color = clr) + 
      geom_point(data = isolate({data_scat_te()}), aes(x = After, y = Response), alpha = 0.5, color = clr)
    # T50
    if (input$plot_ed50_ck_te == TRUE) {
      p <- p +
        # response lines
        geom_hline(data = anno_df, aes(yintercept = T50_res), linetype = "longdash", alpha = 0.5, color = clr) + 
        # ed lines
        geom_vline(data = anno_df, aes(xintercept = T50_Mean), linetype = "longdash", alpha = 0.5, color = clr)
    }
    
  } else {
    # color
    n_color <- isolate({n_distinct(data_scat_te()[[color_var]])})
    # annotation dataframe
    anno_df <- df_et() %>% dplyr::select(1:n_var, (n_var+6):(n_var+8)) %>% 
      mutate(T50_res = 0.5, max_res = 1, min_res = 0)
    anno_df[, (n_var+1):ncol(anno_df)] <- lapply(anno_df[, (n_var+1):ncol(anno_df)], as.numeric)
    # plot
    p <- ggplot(data = data_predct_te(), aes(x = After, y = Response, color = eval(parse(text = color_var)), 
                                             group = eval(parse(text = color_var)))) + 
      geom_line() + 
      geom_point(data = isolate({data_scat_te()}), aes(x = After, y = Response, group = eval(parse(text = color_var))), alpha = 0.5) +
      scale_color_manual(color_var, values = get_palette(cbPalette, n_color), limits = legend_order)
    # T50
    if (input$plot_ed50_ck_te == TRUE) {
      p <- p +
        # response lines
        geom_hline(data = anno_df, aes(yintercept = T50_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
        # ed lines
        geom_vline(data = anno_df, aes(xintercept = T50_Mean, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5)
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
    scale_x_log10() +
    scale_y_continuous(labels = scales::percent) +
    xlab(paste0("Time (", isolate({input$unit}), ")")) + 
    ylab("Rate to Event") +
    theme_few() +
    panel_border(colour = "black", size = 1, remove = FALSE) +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 18),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18))
  
  
  
  # # Responses
  # if (input$plot_resline_ck_te == TRUE) {
  #   p <- p +
  #     # response lines
  #     geom_hline(data = anno_df, aes(yintercept = max_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5) + 
  #     geom_hline(data = anno_df, aes(yintercept = min_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5)
  # } 
  
  p
  
})



#### Output ################################################################################################

#### Output plot -------------------------------------------------------------------------------------------
output$drc_curve_te <- renderPlot({
  return(L_P_te())
})

#### Plot Download -----------------------------------------------------------------------------------------
output$dl_plot_te <- downloadHandler(
  #Specify The File Name 
  filename = function(){paste0(input$file_name_1_te, ".", tolower(input$file_type_1_te))},
  content = function(file){
    ggsave(file, L_P_te(), device = tolower(input$file_type_1_te),
           width = as.numeric(input$width_1_te), height = as.numeric(input$height_1_te))
  }
)

#### Excel Download ----------------------------------------------------------------------------------------
output$dl_plot_df_te <- downloadHandler(
  filename = function(){paste0(input$file_name_1_te, ".xlsx")},
  content = function(file) {
    list_of_datasets <- list("T50_related" = df_et_exp(), 
                             "Bestfit_dataframe" = data_predct_te(), 
                             "ScatterPlot_dataframe" = data_scat_te()
    )
    write.xlsx(list_of_datasets, file)
  }
)

#### Report Download ---------------------------------------------------------------------------------------
output$dl_report_te <- downloadHandler(
  filename = function(){paste0(input$file_name_1_te, ".html")},
  content = function(file) {
    tempReport <- file.path(tempdir(), "Report_Default_TE.Rmd")
    file.copy("reports/Report_Default_TE.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params_1_te <- list(table = T50_table(),
                        unit = input$unit,
                        n_var = ncol(data_predct_te())-2,
                        color_var = input$line_color_v_te,
                        plot_ed50_ck_te = input$plot_ed50_ck_te,
                        T50_related = df_et_exp(),
                        Bestfit_dataframe = data_predct_te(),
                        ScatterPlot_dataframe = data_scat_te()
    )
    n_var <- ncol(data_predct_te())-2
    # add the facet info.
    if (n_var <= 1 ) {
      params_1_te <- list.append(params_1_te,
                                 facet_var_row = "",
                                 facet_var_col = "")
    } else {
      if (n_var == 2) {
        params_1_te <- list.append(params_1_te,
                                   facet_var_row = input$facet_row_v_te,
                                   facet_var_col = ".")
      } else {
        params_1_te <- list.append(params_1_te,
                                   facet_var_row = input$facet_row_v_te,
                                   facet_var_col = paste(setdiff(colnames(data_te())[1:n_var], c(input$line_color_v_te, input$facet_row_v_te)), collapse = "+"))
      }
    }
    # add the legend info.
    if (n_var >= 1) {
      params_1 <- list.append(params_1,
                              legend_order = eval(parse(text = paste0("unique(data_te()$", input$line_color_v_te, ")"))))
    }
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params_1_te,
                      envir = new.env(parent = globalenv())
    )
  }
)

############################################################ Customized plot tab ##################################################################################################

source(file.path("src/CustomizedPlot_TE.R"), local = TRUE)$value
