
# Load the required packages ########################################

library(shiny)
library(shinyjs)
library(shinyalert)
library(shinycssloaders)
library(shinyhelper)
library(openxlsx)
library(tidyverse)
library(bigsnpr)
library(purrr)
library(broom)
library(drc)
library(drcte)
library(aomisc)
library(scales)
library(car)
library(stats)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(cowplot)
library(extrafont)
library(rmarkdown)
library(knitr)
library(DT)
library(kableExtra)
library(rlist)

library(bslib)# theme

# Define UI for application
navbarPage(
  title = "BioCurve Analyzer", 
  id = "tabs1",
  
  theme = bs_theme(bootswatch = "flatly"), # theme from bslib
  useShinyjs(),
  # Welcome Tab ----------------------------------------------------------------------------------------------
  tabPanel(title = "Welcome",
    div(style = "margin: 20px;",
        img(src = "biocurve_analyzer_logo.png", align = "right", style = "float:right; height:220px; margin: 10px;"),
        h5("Main functions of the app:"),
        hr(),
        p("1. Analyze both the dose-response data and the time-to-event data."),
        p("2. Most popular models used to describe the data are provided as candidates, and it also helps to select the best model."),
        p(HTML(paste0("3. Calculate the ED", tags$sub("50"), "/ T", tags$sub("50"), " values from the curves with diverse patterns."))),
        p("4. Generate dose-response or time-to-event curves with customized appearance."),
        p("5. All the dataframes, figures, and a report can be downloaded."),
        br(),
        h5("References:"),
        hr(),
        p(em("Wickham H (2014) Tidy Data. Journal of Statistical Software, Articles 59: 1–23")),
        p(em("Ritz C, Baty F, Streibig JC, Gerhard D (2015) Dose-Response Analysis Using R. PLoS One. 10(12)")),
        p(em("Reed LJ, Muench H (1938) A simple method of estimating fifty percent endpoints. Am J Epidemiol 27: 493–497")),
        p(em("Ramakrishnan MA (2016) Determination of 50% endpoint titer using a simple formula. World J Virol. 5: 85–86")),
        p(em("Serra A. Et al. (2020) BMDx: a graphical Shiny application to perform Benchmark Dose analysis for transcriptomics data. Bioinformatics 36: 2932–2933")),
        p(em("Ritz C, Pipper CB, Streibig JC (2013) Analysis of germination data from agricultural experiments. Eur J Agron 45: 1–6")),
        p(em("Onofri A, Mesgaran MB, Ritz C (2022) A unified framework for the analysis of germination, emergence, and other time-to-event data in weed science. Weed Sci 70: 259–271")),
        p(em("Vaidya, A.S. et al. (2019) Dynamic control of plant water use using designed ABA receptor agonists. Science, 366(6464)")),
        p(em("Eckhardt J, et al. (2024) Robotic Imaging and Machine Learning Analysis of Seed Germination: Dissecting the Influence of ABA and DOG1 on Germination Uniformity. Plant Biology"))
    )
  ),
  
  # Data Tab -------------------------------------------------------------------------------------------------
  tabPanel(title = "Step 1: Input data", 
    div(style = "margin: 20px;", 
        sidebarLayout(
          sidebarPanel(
            # Select the data type 
            wellPanel(h5("Data type:"),
                      div(style = "margin-top: 15px"), 
                      radioButtons(inputId = "datatype",
                                   label = NULL,
                                   choices = c("Dose-response data" = "drc", 
                                               "Time-to-event data" = "te"),
                                   selected = "drc")
            ),
            div(style = "margin-top: 10px"), 
            # Dose-response data:
            conditionalPanel(condition = "input.datatype == 'drc'",
                             
                             # Input data
                             wellPanel(h5("Input data:"),
                                       
                                       # Introduce the format of data
                                       p("The data should be in", tags$b(a(href = "https://r4ds.had.co.nz/tidy-data.html", "tidy format")),
                                         "with ", tags$b("column order"), " as the follows:"),
                                       p(tags$span("Factors", style = "font-weight: bold; background-color: #c5c5c5"), 
                                         " (if applicable, e.g. Protein, Compound, ", tags$em("et al."), "; try to ", tags$b("avoid using underscore"), " in the name), ",
                                         tags$span("Biphasic", style = "font-weight: bold; background-color: #c5c5c5"), ", ",
                                         tags$span("Concentration", style = "font-weight: bold; background-color: #c5c5c5"), ", ",
                                         tags$span("Replicate", style = "font-weight: bold; background-color: #c5c5c5"), ", ",
                                         tags$span("Response", style = "font-weight: bold; background-color: #c5c5c5"), "."  
                                       ),
                                       p(em("Click the question mark to see the detailed requirments")) %>% 
                                         helper(icon = "question-circle", 
                                                type = "markdown",
                                                content = "TidyFormat_DRC",
                                                buttonLabel = "Close"),
                                       
                                       # Select the way to input data
                                       div(style = "margin-top: -30px;") , 
                                       selectInput(inputId = "input_select",
                                                   label = "",
                                                   choices = c("Load the sample data" = "smp",
                                                               "Upload the file" = "upld",
                                                               "Paste data" = "pst"),
                                                   selected = "smp"),
                                       
                                       # Load the sample data
                                       conditionalPanel(condition = "input.input_select == 'smp'", 
                                                        div(style = "text-align: right;", 
                                                            downloadButton(outputId = "dl_smp", label = "Download SampleData")), 
                                                        p(style = "text-align: left; margin-top: 10px;", 
                                                          tags$b("Note: "), "The sample data is from a previously published paper. (Vaidya, A.S. et al, ", em("Science"), ", 2019)")
                                       ),
                                       
                                       # Upload the excel file
                                       conditionalPanel(condition = "input.input_select == 'upld'", 
                                                        tags$b(h6("Choose File :")), 
                                                        p(style = "text-align: right; margin-top: 10px;", tags$b("Note: "), "The first row is used as header."), 
                                                        div(style = "margin-top: -20px;"), 
                                                        fileInput(inputId = "file1", label = "")
                                       ), 
                                       
                                       # Paste the data
                                       conditionalPanel(condition = "input.input_select == 'pst'", 
                                                        
                                                        # TextArea
                                                        tags$b(h6("Paste data below:")), 
                                                        div(style = "margin-top: -20px;"), 
                                                        textAreaInput(inputId = "text1", label = ""), 
                                                        p(style = "text-align: right;", tags$b("Note: "), "The first row is used as header."), 
                                                        
                                                        # Select the separator
                                                        tags$b(h6("Separator:")), 
                                                        div(style = "margin-top: -20px;"), 
                                                        radioButtons(inputId = "SepP", 
                                                                     label = "", 
                                                                     choices = c("Comma" = 1, "Tab" = 2, "Semicolon" = 3)), 
                                                        
                                                        # Clear the data
                                                        actionButton(inputId = "clearText_Butn", 
                                                                     label = "Clear data")
                                                        
                                       )
                             ), 
                             # Confirm the upload
                             div(style = "text-align: right; margin-top: 10px;", 
                                 actionButton(inputId = "upldData_Butn_drc", 
                                              label = "Go"))
            ), 
            
            # Time-to-event data:
            conditionalPanel(condition = "input.datatype == 'te'",
                             
                             # Input data - te
                             wellPanel(h5("Input data:"), 
                                       
                                       # Introduce the format of data
                                       p("The data should be in", tags$b(a(href = "https://r4ds.had.co.nz/tidy-data.html", "tidy format")), 
                                         "with ", tags$b("column order"), " as the follows:"), 
                                       p(tags$span("Factors", style = "font-weight: bold; background-color: #c5c5c5"), 
                                         " (if applicable, e.g. Genotype, Treatment, ", tags$em("et al."), "; try to ", tags$b("avoid using underscore"), " in the name), ", 
                                         tags$span("Replicate", style = "font-weight: bold; background-color: #c5c5c5"), ", ", 
                                         tags$span("Before", style = "font-weight: bold; background-color: #c5c5c5"), ", ", 
                                         tags$span("After", style = "font-weight: bold; background-color: #c5c5c5"), ", ", 
                                         tags$span("Count", style = "font-weight: bold; background-color: #c5c5c5"), "."  
                                       ),
                                       
                                       p(em("Click the question mark to see the detailed requirments")) %>% 
                                         helper(icon = "question-circle", 
                                                type = "markdown", 
                                                content = "TidyFormat_TE", 
                                                buttonLabel = "Close"), 
                                       div(style = "margin-top: -30px;"), 
                                       # Select the way to input data
                                       selectInput(inputId = "input_select_te", 
                                                   label = "", 
                                                   choices = c("Load the sample data" = "smp", 
                                                               "Upload the file" = "upld", 
                                                               "Paste data" = "pst"), 
                                                   selected = "smp"),
                                       
                                       
                                       # Load the sample data
                                       conditionalPanel(condition = "input.input_select_te == 'smp'", 
                                                        div(style = "text-align: right;", 
                                                            downloadButton(outputId = "dl_smp_te", label = "Download SampleData")), 
                                                        p(style = "text-align: left; margin-top: 10px;", 
                                                          tags$b("Note: "), "The sample data is from a previously published paper. (Eckhardt J. et al., ", em("Plant Biology"), ", 2024)")
                                       ),
                                       
                                       # Upload the excel file
                                       conditionalPanel(condition = "input.input_select_te == 'upld'",
                                                        tags$b(h6("Choose File :")),
                                                        p(style = "text-align: right; margin-top: 10px;", tags$b("Note: "), "The first row is used as header."),
                                                        div(style = "margin-top: -20px;"), 
                                                        fileInput(inputId = "file1_te", label = "")
                                       ), 
                                       
                                       # Paste the data
                                       conditionalPanel(condition = "input.input_select_te == 'pst'", 
                                                        
                                                        # TextArea
                                                        tags$b(h6("Paste data below:")), 
                                                        div(style = "margin-top: -20px;"), 
                                                        textAreaInput(inputId = "text1_te", label = ""), 
                                                        p(style = "text-align: right; margin-top: 10px;", tags$b("Note: "), "The first row is used as header."), 
                                                        
                                                        # Select the separator
                                                        tags$b(h6("Separator:")), 
                                                        div(style = "margin-top: -20px;"), 
                                                        radioButtons(inputId = "SepP_te", 
                                                                     label = "", 
                                                                     choices = c("Comma" = 1, "Tab" = 2, "Semicolon" = 3)), 
                                                        
                                                        # Clear the data
                                                        actionButton(inputId = "clearText_Butn_te", 
                                                                     label = "Clear data")
                                                        
                                       ),
                                       # Time intervals in the Experiment:
                                       div(style = "margin-top: 10px;"), 
                                       h5("Time points & Unit:"), 
                                       p("Please provide a comma-separated full list of time points (e.g. 2,4,6,8) in the experiment and the unit (e.g. min):"), 
                                       
                                       div(style = "display: inline-block; vertical-align:top; width: 200px;", 
                                           textInput(inputId = "time_intv", label = "Time Points", 
                                                     value = "24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96")), 
                                       div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
                                       div(style = "display: inline-block; vertical-align:top; width: 100px;",
                                           textInput(inputId = "unit", label = "Unit", value = "h"))
                             ),
                             
                             # Confirm the upload
                             div(style = "text-align: right; margin-top: 10px;",
                                 actionButton(inputId = "upldData_Butn_te",
                                              label = "Go"))
            )
          ),
          mainPanel(
            h5("Data table", align = 'center'),
            hr(),
            conditionalPanel(condition = "input.datatype == 'drc'",
                             DT::dataTableOutput("df1") %>% shinycssloaders::withSpinner()),
            conditionalPanel(condition = "input.datatype == 'te'",
                             DT::dataTableOutput("df1_te") %>% shinycssloaders::withSpinner()),
            # show the lineplot
            
            br(),
            h5("Line plot", align = 'center'),
            hr(),
            conditionalPanel(condition = "input.datatype == 'drc'",
                             plotOutput("lineplot") %>% shinycssloaders::withSpinner()),
            conditionalPanel(condition = "input.datatype == 'te'",
                             plotOutput("lineplot_te") %>% shinycssloaders::withSpinner())
          )
        )
    )
  ),

  # ED50/T50 Calculation Tab ------------------------------------------------
  tabPanel(title = HTML(paste0("Step 2: ED", tags$sub("50"), "/T", tags$sub("50"), " Estimation")),
           div(style = "margin: 20px;", 
               sidebarLayout(
                 sidebarPanel(
                   # Dose-response data
                   conditionalPanel(condition = "input.datatype == 'drc'", 
                                    
                                    # Select the candidate models
                                    wellPanel(h5("Select the candidate models:"),
                                              wellPanel(
                                                tags$span("- Monotonic Curves") %>% 
                                                  helper(icon = "question-circle", 
                                                         type = "markdown",
                                                         content = "Monotonic_DRC",
                                                         buttonLabel = "Close"),
                                                div(style = "margin-top: 10px"), 
                                                fluidRow(
                                                  div(style = "text-align: center; display:flex; align-items:center", 
                                                      column(6, ""),
                                                      column(3, "Lower"),
                                                      column(3, "Upper")
                                                  )
                                                ),
                                                div(style = "margin-top: 10px"), 
                                                fluidRow(
                                                  div(style = "display: flex; justify-content: center; align-items: center;", 
                                                      column(6, div(style = "display:flex;align-items:center;", 
                                                                    checkboxInput("LL4", "Log-logistic (4 parms)", TRUE))),
                                                      column(3, style = "margin-right: 5px;",
                                                             textInput("LL4_c", NULL, "Not Fixed", width = "100%")),
                                                      column(3, style = "margin-right: 5px;",
                                                             textInput("LL4_d", NULL, "Not Fixed", width = "100%"))
                                                  )
                                                ),
                                                fluidRow(
                                                  div(style = "display: flex; justify-content: center; align-items: center;", 
                                                      column(6, div(style = "display:flex;align-items:center;", 
                                                                    checkboxInput("LL5", "Log-logistic (5 parms)", TRUE))),
                                                      column(3, style = "margin-right: 5px;",
                                                             textInput("LL5_c", NULL, "Not Fixed")),
                                                      column(3, style = "margin-right: 5px;",
                                                             textInput("LL5_d", NULL, "Not Fixed"))
                                                  )
                                                ),
                                                fluidRow(
                                                  div(style = "display: flex; justify-content: center; align-items: center;", 
                                                      column(6, div(style = "display:flex;align-items:center;", 
                                                                    checkboxInput("W1", "Weibull I", TRUE))),
                                                      column(3, style = "margin-right: 5px;",
                                                             textInput("W1_c", NULL, "Not Fixed")),
                                                      column(3, style = "margin-right: 5px;",
                                                             textInput("W1_d", NULL, "Not Fixed"))
                                                  )
                                                ),
                                                fluidRow(
                                                  div(style = "display: flex; justify-content: center; align-items: center;", 
                                                      column(6, div(style = "display:flex;align-items:center;", 
                                                                    checkboxInput("W2", "Weibull II", TRUE))),
                                                      column(3, style = "margin-right: 5px;",
                                                             textInput("W2_c", NULL, "Not Fixed")),
                                                      column(3, style = "margin-right: 5px;",
                                                             textInput("W2_d", NULL, "Not Fixed"))
                                                  )
                                                )
                                              ), 
                                              div(style = "margin-top: 10px;"), 
                                              uiOutput(outputId = "biphasicmodels")
                                              
                                    ),
                                    
                                    # Select the methods
                                    div(style = "margin-top: 10px;"), 
                                    wellPanel(h5(HTML(paste0("Methods & Type of ED", tags$sub("50"), ":"))) %>% 
                                                helper(icon = "question-circle", 
                                                       type = "markdown",
                                                       content = "ED_Estimation_Methods",
                                                       buttonLabel = "Close"),
                                              wellPanel(
                                                tags$span(HTML(paste0("- Type of ED", tags$sub("50"), ":"))), 
                                                div(style = "margin-top: -10px"), 
                                                radioButtons(inputId = "ed50_type",
                                                             label = "",
                                                             choices = c("Absolute", "Relative"),
                                                             selected = "Absolute")
                                              ),
                                              div(style = "margin-top: 10px;"), 
                                              wellPanel(
                                                tags$span("- ED estimation method:"),
                                                div(style = "margin-top: -10px"), 
                                                # Select the Type of ED50
                                                selectInput(inputId = "ed_methods",
                                                            label = "",
                                                            choices = c("Ritz-Gerhard Method" = "stdd_method", 
                                                                        "Serra-Greco Method" = "serra_greco_method"),
                                                            selected = "stdd_method"), 
                                                conditionalPanel(condition = "input.ed50_type == 'Absolute'",
                                                                 div(style = "margin-top: -10px"), 
                                                                 checkboxInput(inputId = "two_point_method",
                                                                               label = "Include Reed-and-Muench Method",
                                                                               value = FALSE)
                                                )
                                              )                 
                                    ),
                                    
                                    
                                    # Model Assessment
                                    div(style = "margin-top: 10px"), 
                                    wellPanel(h5("Model assessment methods:") %>% 
                                                helper(icon = "question-circle", 
                                                       type = "markdown",
                                                       content = "Model_Assessment_Methods",
                                                       buttonLabel = "Close"),
                                              fluidRow(
                                                div(style = "text-align:center; display:flex; align-items:center;", 
                                                    column(8, ""),
                                                    column(4, tags$em("p-value"))
                                                )
                                              ),
                                              div(style = "margin-top: 10px"), 
                                              fluidRow(
                                                div(style = "display:flex;align-items:center;", 
                                                    column(8, div(style = "display:flex; align-items:center;", checkboxInput("lac", "Lack-of-fit test", TRUE))),
                                                    column(4, textInput("lac_p", NULL, "0.05"))
                                                )
                                              ),
                                              fluidRow(
                                                div(style = "display:flex;align-items:center;", 
                                                    column(8, div(style = "display:flex;align-items:center;", checkboxInput("nell", "Neill's test", TRUE))),
                                                    column(4, textInput("nell_p", NULL, "0.05"))
                                                )
                                              ),
                                              fluidRow(
                                                div(style = "display:flex;align-items:center;", 
                                                    column(8, div(style = "display:flex;align-items:center;", checkboxInput("neffect", "No effect test", TRUE))),
                                                    column(4, textInput("neffect_p", NULL, "0.05"))
                                                )
                                              ),
                                              fluidRow(
                                                div(style = "display:flex;align-items:center;", 
                                                    column(8, div(style = "display:flex;align-items:center;", checkboxInput("para0", "Parameters ≠ 0", TRUE))),
                                                    column(4, textInput("para0_p", NULL, "0.05"))
                                                )
                                              )
                                    ),
                                    
                                    # Select the criterion
                                    div(style = "margin-top: 10px"), 
                                    wellPanel(h5("Criteria for model selection:") %>% 
                                                helper(icon = "question-circle", 
                                                       type = "markdown",
                                                       content = "Model_Selection_Criteria",
                                                       buttonLabel = "Close"),
                                              div(style = "margin-top: -20px"), 
                                              selectInput(inputId = "crtrn_selected",
                                                          label = "",
                                                          choices = c("Akaike's Information Criterion" = "AIC", 
                                                                      "Bayesian Information Criteria" = "BIC", 
                                                                      "Lack-of-fit Test (against a one-way ANOVA model)" = "Lack_of_fit", 
                                                                      "Residual Standard Errors" = "Res_var"),
                                                          selected = "AIC")
                                    ),
                                    
                                    # Minimum Dose
                                    div(style = "margin-top: 10px"), 
                                    wellPanel(h5("Minimum dose:"), 
                                              div(style = "margin-top: -20px"), 
                                              textInput(inputId = "bp", label = "", value = "default"),
                                              p(style = "text-align: left;", 
                                                tags$b("Note: "), "Please specify the minimum dose to achieve a logarithmic scale visual effect, applicable only when the minimum dose is zero. 
                                                          The default is the base-10 value corresponding to the rounded minimum log10 value of all positive doses, as recommended by the ", 
                                                tags$span("drc", style = "font-weight: bold; background-color: #c5c5c5"), " package.")
                                              
                                    ),
                                    
                                    # Confirm the calculation
                                    div(style = "margin-top: 10px"), 
                                    div(style = "text-align: right;",
                                        actionButton(inputId = "calculate_Butn",
                                                     label = "Calculate"))
                                    
                   ),
                   # Time-to-event data:
                   conditionalPanel(condition = "input.datatype == 'te'", 
                                    # Select the model
                                    wellPanel(h5("Select the candidate models:") %>% 
                                                helper(icon = "question-circle", 
                                                       type = "markdown",
                                                       content = "Monotonic_TE",
                                                       buttonLabel = "Close"),
                                              fluidRow(
                                                div(style = "display: flex; align-items: center; text-align: center;", 
                                                    column(8, ""),
                                                    column(4, "Upper")
                                                )
                                              ),
                                              div(style = "margin-top: 10px"), 
                                              fluidRow(
                                                div(style = "display:flex;align-items:center;", 
                                                    column(8, div(style = "display: flex;align-items: center;", checkboxInput("LL4_te", "Log-logistic (4 parms)", TRUE))),
                                                    column(4, selectInput("LL4_te_d", NULL, c("Not Fixed" = "NA", "100%" = "1"), "NA"))
                                                )
                                              ),
                                              fluidRow(
                                                div(style = "display: flex; justify-content: center; align-items: center;", 
                                                    column(8, div(style = "display:flex;align-items:center;", checkboxInput("LN", "Log-normal", TRUE))),
                                                    column(4, selectInput("LN_d", NULL, c("Not Fixed" = "NA", "100%" = "1"), "NA"))
                                                )
                                              ), 
                                              fluidRow(
                                                div(style = "display:flex;align-items:center;", 
                                                    column(8, div(style = "display:flex;align-items:center;", checkboxInput("W1_te", "Weibull I", TRUE))),
                                                    column(4, selectInput("W1_te_d", NULL, c("Not Fixed" = "NA", "100%" = "1"), "NA"))
                                                )
                                              ), 
                                              fluidRow(
                                                div(style = "display:flex;align-items:center;", 
                                                    column(8, div(style = "display:flex;align-items:center;", checkboxInput("W2_te", "Weibull II", TRUE))),
                                                    column(4, selectInput("W2_te_d", NULL, c("Not Fixed" = "NA", "100%" = "1"), "NA"))
                                                )
                                              )
                                    ),
                                    
                                    # Select the criterion
                                    div(style = "margin-top: 10px;"), 
                                    wellPanel(h5("Criteria for model selection:") %>% 
                                                helper(icon = "question-circle", 
                                                       type = "markdown",
                                                       content = "Model_Selection_Criteria_TE",
                                                       buttonLabel = "Close"),
                                              div(style = "margin-top: -10px;"), 
                                              selectInput(inputId = "crtrn_selected_te",
                                                          label = "",
                                                          choices = c("Akaike's Information Criterion" = "AIC", 
                                                                      "Bayesian Information Criteria" = "BIC"),
                                                          selected = "AIC")
                                    ),
                                    
                                    # Confirm the calculation
                                    div(style = "margin-top: 10px;"), 
                                    div(style = "text-align: right;",
                                        actionButton(inputId = "calculate_Butn_te",
                                                     label = "Calculate"))
                                    
                                    
                   )
                 ),
                 mainPanel(
                   conditionalPanel(condition = "input.datatype == 'drc'",
                                    uiOutput(outputId = "ed50_results")),
                   conditionalPanel(condition = "input.datatype == 'te'",
                                    uiOutput(outputId = "et50_results"))
                 )
               )
           )
  ),

  # Plot Tab ----------------------------------------------------------------
  tabPanel(title = "Step 3: Generate plot",
           div(style = "margin: 20px;", 
               sidebarLayout(
                 sidebarPanel(
                   # Show alert message if the user have curves cannot fit to any of the selected models
                   useShinyalert(),
                   conditionalPanel(condition = "input.datatype == 'drc'",
                                    # Select the layout, appearance and model
                                    uiOutput(outputId = "plot_layout_ui"),
                                    div(style = "margin-top: 10px;"), 
                                    uiOutput(outputId = "plot_appearance_ui"),
                                    div(style = "margin-top: 10px;"), 
                                    uiOutput(outputId = "plot_model_ui"),
                                    div(style = "margin-top: 10px;"), 
                                    uiOutput(outputId = "plot_resline_ui"),
                                    
                                    # Confirm to plot
                                    div(style = "text-align: right; margin-top: 10px;",
                                        actionButton(inputId = "plot_Butn_1",
                                                     label = "Generate plot")),
                                    
                                    
                                    # Customize the plot
                                    div(style = "margin-top: 10px;"), 
                                    checkboxInput(inputId = "custmz_plot_ck",
                                                  label = "Customize the plot",
                                                  value = FALSE)),
                   
                   conditionalPanel(condition = "input.datatype == 'te'",
                                    # Select the layout, appearance and model
                                    uiOutput(outputId = "plot_layout_ui_te"),
                                    div(style = "margin-top: 10px;"), 
                                    uiOutput(outputId = "plot_appearance_ui_te"),
                                    div(style = "margin-top: 10px;"), 
                                    uiOutput(outputId = "plot_model_ui_te"),
                                    div(style = "margin-top: 10px;"), 
                                    uiOutput(outputId = "plot_resline_ui_te"),
                                    
                                    # Confirm to plot
                                    div(style = "margin-top: 10px;"), 
                                    div(style = "text-align: right;",actionButton(inputId = "plot_Butn_1_te",
                                                                                  label = "Generate plot")),
                                    
                                    
                                    # Customize the plot
                                    div(style = "margin-top: 10px;"), 
                                    checkboxInput(inputId = "custmz_plot_ck_te",
                                                  label = "Customize the plot",
                                                  value = FALSE))
                 ),
                 mainPanel(
                   conditionalPanel(condition = "input.datatype == 'drc'",
                                    plotOutput("drc_curve") %>% shinycssloaders::withSpinner(), 
                                    uiOutput(outputId = "dl")),
                   conditionalPanel(condition = "input.datatype == 'te'",
                                    plotOutput("drc_curve_te") %>% shinycssloaders::withSpinner(), 
                                    uiOutput(outputId = "dl_te"))
                 )
               )
           )

  ),
  
  # Customized Plot Tab -----------------------------------------------------
  tabPanel(title = "Step 4: Customize plot",
           div(style = "margin: 20px;", 
               sidebarLayout(
                 sidebarPanel(
                   conditionalPanel(condition = "input.datatype == 'drc'",
                                    uiOutput(outputId = "tab4_side")),
                   conditionalPanel(condition = "input.datatype == 'te'",
                                    uiOutput(outputId = "tab4_side_te"))
                 ),
                 mainPanel(
                   conditionalPanel(condition = "input.datatype == 'drc'",
                                    plotOutput("drc_curve_2") %>% shinycssloaders::withSpinner(),
                                    uiOutput(outputId = "dl_2")),
                   conditionalPanel(condition = "input.datatype == 'te'",
                                    plotOutput("drc_curve_2_te") %>% shinycssloaders::withSpinner(),
                                    uiOutput(outputId = "dl_2_te"))
                 )
               )
           )
  )
  
)
