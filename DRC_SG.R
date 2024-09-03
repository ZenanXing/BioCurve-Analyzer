
############################################################ Data tab ##################################################################################################

#### Reactive objects ######################################################################################

##### tidy data ---------------------------------------------------------------------------------------------

data <- eventReactive(input$upldData_Butn_drc,
                      {if (input$input_select == "smp") {
                        data <- read.xlsx("Sample_data_OP_paper.xlsx")
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
})

#### Message box to remind users if the file doesn't contain the necessary columns -------------------------------------------------------
observeEvent(input$upldData_Butn_drc, {
  if (data_values$n_var < 0) {
    shinyalert(title = "Attention", 
               text = h4(tags$b("It seems the data you uploaded doesn't have the nessary columns, please upload the data in the correct format.")), 
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
  data_scat$Conc[data_scat$Conc == 0] <- 0.03
  return(data_scat)
})


#### Mean and SD_dataset -----------------------------------------------------------------------------------
# Mean and SD_dataset
data_m_sd <- reactive({
  req(data_values$n_var >= 0)
  if(data_values$n_var == 0) {
    data_m_sd <- data_scat() %>% group_by(Conc) %>% dplyr::summarise(Mean = mean(Response), SD = sd(Response))
  } else {
    data_m_sd <- data_scat() %>% 
      group_by(Conc, across(colnames(data_scat())[1: n_var])) %>% 
      dplyr::summarise(Mean = mean(Response), SD = sd(Response))
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
      geom_line(data = data_m_sd, aes(x = Conc, y = Mean))
    # geom_hline(aes(yintercept = 50), linetype = "twodash", color = "red", alpha = 0.7)
  } else {
    lineplot <- ggplot(data = data_scat(), aes(x = Conc, y = Response, 
                                               color = eval(parse(text = data_values$color_var)), 
                                               group = eval(parse(text = data_values$color_var)))) + 
      labs(color = data_values$color_var) +
      # lines generated by loess model
      # geom_smooth(se = FALSE, size = 0.5) + 
      # lines generated by connecting the mean value
      geom_line(data = data_m_sd, aes(x = Conc, y = Mean, group = eval(parse(text = color_var))))
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
    smp <- read.xlsx("Sample_data_OP_paper.xlsx")
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
  # Nested the dataframe
  Data <- data()
  if (data_values$n_var == 0) {
    colnames(Data) <- c("Conc", "Replicate", "Response")
    Data <- Data %>% nest()
  } else {
    colnames(Data) <- c(colnames(data())[1:data_values$n_var], "Biphasic", "Conc", "Replicate", "Response")
    Data <- Data %>% group_by(across(colnames(data())[1:data_values$n_var])) %>% nest()
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
                      #(isolate({data_values$n_var})+5), # model-related
                      (isolate({data_values$n_var})+12):(isolate({data_values$n_var})+14),
                      (isolate({data_values$n_var})+18):(isolate({data_values$n_var})+20)) # ED50
  } else {
    selected_var <- c((isolate({data_values$n_var})+7),
                      #(isolate({data_values$n_var})+5), # model-related
                      (isolate({data_values$n_var})+12):(isolate({data_values$n_var})+14),
                      (isolate({data_values$n_var})+18):(isolate({data_values$n_var})+20)) # ED50
  }
  df_temp <- df_ed()[ , selected_var]
  df_temp[, (isolate({data_values$n_var})+2):ncol(df_temp)] <- round(df_temp[, (isolate({data_values$n_var})+2):ncol(df_temp)], 2)
  colnames(df_temp)[(isolate({data_values$n_var})+1):ncol(df_temp)] <- c("Function\nName", #"p value\n(lack-of-fit test)", 
                                                                         "Response\nat ED50", "Maximum\nResponse", "Maximum\nResponse",
                                                                         "ED50\nMean", "ED50\nLower Bound", "ED50\nUpper Bound")
  return(df_temp)
})

output$df2 <- DT::renderDataTable({
  ED50_table()
})

#### Triggered UI ##########################################################################################

#### Biphasic models ui ------------------------------------------------------

output$biphasicmodels <- renderUI({
  req(data_scat())
  if (any(data_scat()$Biphasic == "Y")) {
    wellPanel(
      tags$span("- Biphasic Curves", style = "font-size: 16px; font-weight: bold;") %>% 
        helper(icon = "question-circle", 
               type = "markdown",
               content = "Biphasic",
               buttonLabel = "Close"),
      div(style = "margin-top: 10px"), 
      fluidRow(
        div(style = "text-align: center; font-weight: bold;", 
            column(6, ""),
            column(3, "Lower"),
            column(3, "Upper")
        )
      ),
      fluidRow(
        div(style = "display: flex; justify-content: center; align-items: center;", 
            column(6, div(style = "display:flex;align-items:center;", checkboxInput("BC", "Brain-Cousens", TRUE))),
            column(3, textInput("BC_c", NULL, "NA")),
            column(3, textInput("BC_d", NULL, "NA"))
        )
      ),
      fluidRow(
        div(style = "display: flex; justify-content: center; align-items: center;", 
            column(6, div(style = "display:flex;align-items:center;", checkboxInput("beta", "beta", TRUE))),
            column(6)
        )
      )
    )
  }
})

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
      p(HTML(paste0("To prevent over-fitting, we highly recommand you to choose the models based on your own experiment setup, 
                    if you let the app to choose the best models for you, 
                    the best models reported are selected from the drc analysis (Ritz C,", 
                    em("et al."), ", ", em("PLoS One"), ", 2015), based on the criteria you choose on the left."))),
      p(tags$b("Reference:")),
      p(em("Ritz C, Baty F, Streibig JC, Gerhard D (2015) Dose-Response Analysis Using R. PLoS One. 10(12)")), 
      p(em("Serra A. Et al. (2020) BMDx: a graphical Shiny application to perform Benchmark Dose analysis for transcriptomics data. Bioinformatics 36: 2932–2933"))
      # p(em("Ramakrishnan MA (2016) Determination of 50% endpoint titer using a simple formula. World J Virol. 5: 85–86"))
    )
  )
})



############################################################ Plot tab ################################################################################################## 
#### Triggered UI ####################################################################################################

#### Message box to remind users to choose the model -------------------------------------------------------
#    observeEvent(input$tabs1, {
#        # Show a message if some of ED50s cannot be estimated by log-logistic model
#        if (input$tabs1 == "Step 3: Generate plot" & length(unique(df_ed()$Estimation_Method)) != 1) {
#            shinyalert(title = "Attention", 
#                       text =h4(tags$b("It seems the log-logistic model is not the best fitting model for some of your data, 
#                                       so you may need to choose the model on the left to plot them.")), 
#                       type = "warning",
#                       html = TRUE)
#        }
#    })

#### Plot_model_UI ---------------------------------------------------------------------------------------
#    output$plot_model_ui <- renderUI({
#        if (length(unique(df_ed()$Estimation_Method)) != 1) {
#           tagList(
#            list(
#                hr(),
#                h4("Models"),
#                p(HTML(paste0("The ED", tags$sub("50"), "s cannot be estimated by the log-logistic models for some of your data, ",
#                              "so it seems the log-logistic model is not the best fitting model. Please choose the appropriate model to plot them."))),
#                selectInput(inputId = "model_selected",
#                            label = "Select the models:",
#                            choices = c("Simple line plot" = "line",
#                                        "Loess model" = "loess",
#                                        "Brain-Cousens hormesis model" = "bc5"),
#                            selected = "line")
#                
#            )
#        ) 
#       }
#    })


#### Plot_layout_ui ---------------------------------------------------------------------------------------

output$plot_layout_ui <- renderUI({
  req(data())
  req(data_values$n_var >= 0)
  if(data_values$n_var != 0) {
    wellPanel(style = "background-color: #eaeaea;",
      h4(tags$b("Layout")),
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
  wellPanel(style = "background-color: #eaeaea;",
            h4(tags$b("Appearance")),
            div(style = "margin-top: -20px"),
            selectInput(inputId = "plot_appearance",
                        label = "",
                        choices = c("All Replicates" = "all", "Mean and SD" = "m_sd"),
                        selected = "all")
  )
})


#### Plot_ED&Responses_Line_UI -----------------------------------------------------------------------------
output$plot_resline_ui <- renderUI({
  tagList(
    list(
      h5("Show the ED values and the corresponding responses："),
      div(style = "margin-top: -20px"),
      div(style = "display: inline-block; vertical-align: top;",
          checkboxInput(inputId = "plot_ed50_ck", label = HTML(paste0("ED", tags$sub("50"))), value = FALSE)),
      #div(style = "display: inline-block; vertical-align: top;",
      #    checkboxInput(inputId = "plot_bmd_ck", label = "BMD", value = FALSE)),
      div(style = "display: inline-block; vertical-align: top;",
          checkboxInput(inputId = "plot_resline_ck", label = "Max & Min Responses", value = FALSE))
    )
  )
})


#### Download UI -------------------------------------------------------------------------------------------
output$dl <- renderUI({
  req(input$plot_Butn_1)
  tagList(
    list(
      h4("Download"),
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
        tags$b(a(href = "https://www.storytellingwithdata.com/blog/2013/03/avoiding-spaghetti-graph", "spaghetti graph")), "."),  
      div(style = "margin-top: -10px"),
      p("2. The default size is only suitable for two plots; you can specify the aspect ratio for downloading."),  
      div(style = "margin-top: -10px"),
      p(HTML(paste0("3. The excel contains ED", tags$sub("50"), 
                    " table, both dataframes for generating scatterplot and lineplot.")))
      
    )
  )
})

#### Reactive Objects ######################################################################################

#### Lineplot_dataset --------------------------------------------------------------------------------------
data_predct <- eventReactive(input$plot_Butn_1, {
  n_var <-  ncol(df_ed())-20
  if (n_var == 0) {
    data_predct <- df_ed() %>% 
      dplyr::select("Curve_BestFit_data") %>% 
      unnest()
    colnames(data_predct)[1] <- "Response"
  } else {
    data_predct <- df_ed() %>% 
      dplyr::select(1:n_var, "Curve_BestFit_data") %>% 
      unnest()
    colnames(data_predct)[n_var+1] <- "Response"
  }
  
  return(data_predct)
  #         fits <- data.frame(matrix(ncol = 4, nrow = 0))
  #         n_var <- ncol(df_ed())-9
  #         for (i in 1:nrow(df_ed())) {
  #             df_model <- df_ed()[[n_var+1]][[i]]
  #             df_model$Conc[df_model$Conc == 0] <- 0.03
  #             tempObj <- NULL
  #             # Test if the curve should be fit to models
  #             if (df_ed()$Estimation_Method[i] == "Log-logistic") {
  #                 tempObj <- try(eval(parse(text = paste0(df_ed()$LL4_Constraints[i], "(df_model)"))))
  #             } else {
  #                 if(input$model_selected == "loess") {
  #                     tempObj <- try(loess(Response ~ log10(Conc), data = df_model))
  #                 } else {
  #                     if (input$model_selected == "bc5") {
  #                         tempObj <- try(drm(Response ~ Conc, data = df_model, fct = BC.5()))
  #                     } else { tempObj <- NULL }
  #                 }
  #             }
  #             
  #             # Generate the dataframe
  #             if (!is.null(tempObj) & !inherits(tempObj, "try-error")) {
  #                 # concentration range
  #                 conc_min <- min(df_model$Conc)
  #                 conc_max <- max(df_model$Conc)
  #                 # predict the value
  #                 temp_fits <- expand.grid(Conc = exp(seq(log(conc_min), log(conc_max), length = 100)))
  #                 temp_pm <- data.frame(predict(tempObj, newdata = temp_fits))
  #                 temp_fits_2 <- temp_fits %>%
  #                     mutate("predct" = temp_pm[ , 1])
  #             } else {
  #                 temp_fits_2 <- df_model %>% group_by(Conc) %>% 
  #                     summarise(predct = mean(Response))
  #             }
  #             
  #             if (n_var == 0) {
  #                 temp_fits_2 <- temp_fits_2
  #             } else {
  #                 temp_fits_2 <- temp_fits_2 %>% cbind(df_ed()[i, 1:n_var])
  #             }
  #             
  #             # Add the fit data to the final dataframe
  #             fits <- rbind.data.frame(fits, temp_fits_2)
  #         }
  #         
  #         if(n_var == 0) {
  #             var_names <- c((n_var+1), (n_var+3))
  #             var_order <- c(1:2)
  #         } else {
  #             var_names <- c((n_var+1), (n_var+3), 1:n_var)
  #             var_order <- c(3:(2+n_var), 1:2)
  #         }
  #         
  #         colnames(fits) <- isolate({c(colnames(data_scat()[ , var_names]))})
  #         data_predct <- fits[ , var_order]
  #         return(data_predct)
  
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
      facet_var_col <- isolate({paste(setdiff(colnames(data())[1:n_var], c(color_var, facet_var_row)), collapse = "+")})
    }
  }
  
  # Legend
  legend_order <- eval(parse(text = paste0("unique(isolate({data()})$", color_var, ")")))
  
  # Palette
  cbPalette = c("#00A4FF", "#FD7FEE", "#03DFCA", "#990A3A", "#F37B63", "#05B756", "#A3FB86", "#097C91", "#015EC9","#840EAA")
  if (n_var == 0) {
    n_color <- 1
    p <- ggplot(data = data_predct(), aes(x = Conc, y = Response)) + 
      scale_color_manual(values = get_palette(cbPalette, n_color))
  } else {
    eval(parse(text = paste0("n_color <- isolate({n_distinct(data_scat()$", color_var, ")})")))
    p <- ggplot(data = data_predct(), aes(x = Conc, y = Response, color = eval(parse(text = color_var)), 
                                          group = eval(parse(text = color_var)))) + 
      scale_color_manual(color_var, values = get_palette(cbPalette, n_color), limits = legend_order)
  }
  
  if (input$plot_appearance == "all") {
    if (n_var == 0) {
      p <- p + geom_point(data = isolate({data_scat()}), aes(x = Conc, y = Response), alpha = 0.5)
    } else {
      p <- p + geom_point(data = isolate({data_scat()}), aes(x = Conc, y = Response, group = eval(parse(text = color_var))), alpha = 0.5)
    }
  } else {
    if (n_var == 0) {
      p <- p + geom_point(data = isolate({data_m_sd()}), aes(x = Conc, y = Mean), alpha = 0.5) + 
        geom_errorbar(data = isolate({data_m_sd()}), aes(x = Conc, y = Mean, ymin = Mean - SD, ymax = Mean + SD), width = 0.2, alpha = 0.8)
    } else {
      p <- p + geom_point(data = isolate({data_m_sd()}), aes(x = Conc, y = Mean, group = eval(parse(text = color_var))), alpha = 0.5) + 
        geom_errorbar(data = isolate({data_m_sd()}), aes(x = Conc, y = Mean, group = eval(parse(text = color_var)),
                                                         ymin = Mean - SD, ymax = Mean + SD), width = 0.2, alpha = 0.8)
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
    geom_line() +
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
  
  # Annotation dataframe
  if (n_var == 0) {
    anno_df <- df_ed() %>% dplyr::select((n_var+11):(n_var+20))
  } else {
    anno_df <- df_ed() %>% dplyr::select(1:n_var, (n_var+11):(n_var+20))
  }
  
  # ED50
  if (input$plot_ed50_ck == TRUE) {
    p <- p +
      # response lines
      geom_hline(data = anno_df, aes(yintercept = ED50_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
      # ed lines
      geom_vline(data = anno_df, aes(xintercept = ED50_Mean, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
      geom_vline(data = anno_df, aes(xintercept = ED50L, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5) + 
      geom_vline(data = anno_df, aes(xintercept = ED50U, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5)
  }
  
  # BMD
#  if (input$plot_bmd_ck == TRUE) {
#    p <- p +
#      # response lines
#      geom_hline(data = anno_df, aes(yintercept = BMD_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
#      # bmd lines
#      geom_vline(data = anno_df, aes(xintercept = BMD_Mean, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "longdash", alpha = 0.5) + 
#      geom_vline(data = anno_df, aes(xintercept = BMDL, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5) + 
#      geom_vline(data = anno_df, aes(xintercept = BMDU, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5)        
#  }
  
  # Responses
  if (input$plot_resline_ck == TRUE) {
    p <- p +
      # response lines
      geom_hline(data = anno_df, aes(yintercept = max_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5) + 
      geom_hline(data = anno_df, aes(yintercept = min_res, group = eval(parse(text = color_var)), color = eval(parse(text = color_var))), linetype = "dotted", alpha = 0.5)
  } 
  
  p
  
})

#### Show the EDs and related responses --------------------------------------------------------------------



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
    list_of_datasets <- list("ED50_related" = ED50_table(), 
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
    file.copy("Report_Default.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params_1 <- list(table = ED50_table(),
                     n_var = ncol(data_predct())-2,
                     color_var = input$line_color_v,
                     legend_order = eval(parse(text = paste0("unique(data()$", input$line_color_v, ")"))),
                     Bestfit_dataframe = data_predct(),
                     ScatterPlot_dataframe = data_scat(),
                     Mean_SD_dataframe = data_m_sd(),
                     Plot_appearance = input$plot_appearance,
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

source(file.path("CustomizedPlot.R"), local = TRUE)$value
