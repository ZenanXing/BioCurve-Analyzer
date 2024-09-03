
# Load the required packages ########################################

library(shiny)
library(shinyjs)
library(tidyverse)
library(drc)
library(drcte)
library(DT)
# install.packages("devtools")
# devtools::install_github("onofriAndreaPG/aomisc")
library(aomisc)
library(ggthemes)
library(cowplot)
library(extrafont)
library(ggpubr)
library(writexl)
library(openxlsx)
library(shinyalert)
library(shinycssloaders)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(colourpicker)
library(rlist)
library(purrr)
library(bigsnpr)
library(shinyhelper)
library(broom)
library(scales)
library(bmd)
library(randtests)
library(car)
library(stats)

#library(bslib)# theme

# Define UI for application
ui <- fluidPage(
    #theme = bs_theme(version = 4, bootswatch = "minty"), #theme
    useShinyjs(),
    # Apply css
    tags$head(
        tags$link(rel="stylesheet", type = "text/css", href = file.path("app.css")),
        tags$link(rel="stylesheet", type = "text/css", href = file.path("plotHelper.css"))
    ),

    # Application title ############################################################################################
    titlePanel("BioCurve Analyzer"),
    
    # Sidebar  #####################################################################################################
    sidebarLayout(
        sidebarPanel(
            # Welcome Tab ------------------------------------------------------------------------------------------
            conditionalPanel(condition = "input.tabs1 == 'Welcome'",
                             h4(tags$b("Introduction"))
            ),
            # Data Tab ---------------------------------------------------------------------------------------------
            conditionalPanel(condition = "input.tabs1 == 'Step 1: Input data'",
                             
                             # Select the data type 
                             wellPanel(style = "background-color: #eaeaea;",
                               h4(tags$b("Data type:")),
                               div(style = "margin-top: 20px"),
                               radioButtons(inputId = "datatype",
                                            label = NULL,
                                            choices = c("Dose-response data" = "drc", 
                                                        "Time-to-event data" = "te"),
                                            selected = "drc")
                             ),
                             
                             # Dose-response data:
                             conditionalPanel(condition = "input.datatype == 'drc'",
                                              
                                              # Input data
                                              wellPanel(style = "background-color: #eaeaea;",
                                                h4(tags$b("Input data:")),
                                                div(style = "margin-top: -5px"),
                                                
                                                # Introduce the format of data
                                                p("The data should be in", tags$b(a(href = "https://r4ds.had.co.nz/tidy-data.html", "tidy format")),
                                                  "with ", tags$b("column order"), " as the follows:"),
                                                div(style = "margin-top: -10px"),
                                                p(tags$span("Factors", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), 
                                                  " (if applicable, e.g. Protein, Compound, ", tags$em("et al."), "; try to avoid using underscore in the name), ",
                                                  tags$span("Biphasic", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), ", ",
                                                  tags$span("Concentration", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), ", ",
                                                  tags$span("Replicate", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), ", ",
                                                  tags$span("Response", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), "."  
                                                ),
                                                div(style = "margin-top: -10px"),
                                                p(em("Click the question mark to see the detailed requirments")) %>% 
                                                  helper(icon = "question-circle", 
                                                         type = "markdown",
                                                         content = "TidyFormat_DRC",
                                                         buttonLabel = "Close"),
                                                
                                                # Select the way to input data
                                                selectInput(inputId = "input_select",
                                                            label = "",
                                                            choices = c("Load the sample data" = "smp",
                                                                        "Upload the file" = "upld",
                                                                        "Paste data" = "pst"),
                                                            selected = "smp"),
                                                
                                                # Load the sample data
                                                conditionalPanel(condition = "input.input_select == 'smp'", 
                                                                 div(style = "margin-top: -15px"), 
                                                                 div(style = "text-align: right;", 
                                                                     downloadButton(outputId = "dl_smp", label = "Download SampleData")), 
                                                                 p(style = "text-align: left;", 
                                                                   tags$b("Note: "), "The sample data is from a previously published paper. (Vaidya et al., ", em("Science"), ", 2019)")
                                                ),
                                                
                                                # Upload the excel file
                                                conditionalPanel(condition = "input.input_select == 'upld'", 
                                                                 tags$b(h5("Choose File :")), 
                                                                 p(style = "text-align: right;", tags$b("Note: "), "The first row is used as header."), 
                                                                 div(style = "margin-top: -30px"), 
                                                                 fileInput(inputId = "file1", label = "")
                                                ), 
                                                
                                                # Paste the data
                                                conditionalPanel(condition = "input.input_select == 'pst'", 
                                                                 
                                                                 # TextArea
                                                                 tags$b(h5("Paste data below:")), 
                                                                 div(style = "margin-top: -20px"), 
                                                                 textAreaInput(inputId = "text1", label = ""), 
                                                                 div(style = "margin-top: -15px"), 
                                                                 p(style = "text-align: right;", tags$b("Note: "), "The first row is used as header."), 
                                                                 
                                                                 # Select the separator
                                                                 tags$b(h5("Separator:")), 
                                                                 div(style = "margin-top: -20px"), 
                                                                 radioButtons(inputId = "SepP", 
                                                                              label = "", 
                                                                              choices = c("Comma" = 1, "Tab" = 2, "Semicolon" = 3)), 
                                                                 
                                                                 # Clear the data
                                                                 actionButton(inputId = "clearText_Butn", 
                                                                              label = "Clear data")
                                                                 
                                                )
                                               ), 
                                              
                                              div(style = "margin-top: 10px"), 
                                              
                                              # Confirm the upload
                                              div(style = "text-align: right;", 
                                                  actionButton(inputId = "upldData_Butn_drc", 
                                                               label = tags$b("Go")))
                                              ), 
                             
                             # Time-to-event data:
                             conditionalPanel(condition = "input.datatype == 'te'",
                                              
                                              # Input data - te
                                              wellPanel(style = "background-color: #eaeaea;",
                                                h4(tags$b("Input data:")), 
                                                div(style = "margin-top: -5px"), 
                                                
                                                # Introduce the format of data
                                                p("The data should be in", tags$b(a(href = "https://r4ds.had.co.nz/tidy-data.html", "tidy format")), 
                                                  "with ", tags$b("column order"), " as the follows:"), 
                                                div(style = "margin-top: -10px"), 
                                                p(tags$span("Factors", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), 
                                                  " (if applicable, e.g. Genotype, Treatment, ", tags$em("et al."), "; try to avoid using underscore in the name), ", 
                                                  tags$span("Replicate", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), ", ", 
                                                  tags$span("Before", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), ", ", 
                                                  tags$span("After", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), ", ", 
                                                  tags$span("Count", style = "font-size: 13px; font-weight: bold; background-color: #E3E3E3"), "."  
                                                ), 
                                                div(style = "margin-top: -10px"), 
                                                p(em("Click the question mark to see the detailed requirments")) %>% 
                                                  helper(icon = "question-circle", 
                                                         type = "markdown", 
                                                         content = "TidyFormat_TE", 
                                                         buttonLabel = "Close"), 
                                                
                                                # Select the way to input data
                                                selectInput(inputId = "input_select_te", 
                                                            label = "", 
                                                            choices = c("Load the sample data" = "smp", 
                                                                        "Upload the file" = "upld", 
                                                                        "Paste data" = "pst"), 
                                                            selected = "smp"), 
                                                
                                                # Load the sample data
                                                conditionalPanel(condition = "input.input_select_te == 'smp'", 
                                                                 div(style = "margin-top: -15px"), 
                                                                 div(style = "text-align: right;", 
                                                                     downloadButton(outputId = "dl_smp_te", label = "Download SampleData")), 
                                                                 p(style = "text-align: left;", tags$b("Note: "), "The sample data is from an unpublished paper.")
                                                ),
                                                
                                                # Upload the excel file
                                                conditionalPanel(condition = "input.input_select_te == 'upld'",
                                                                 tags$b(h5("Choose File :")),
                                                                 p(style = "text-align: right;", tags$b("Note: "), "The first row is used as header."),
                                                                 div(style = "margin-top: -30px"),
                                                                 fileInput(inputId = "file1_te",
                                                                           label = "")
                                                ), 
                                                
                                                # Paste the data
                                                conditionalPanel(condition = "input.input_select_te == 'pst'", 
                                                                 
                                                                 # TextArea
                                                                 tags$b(h5("Paste data below:")), 
                                                                 div(style = "margin-top: -20px"), 
                                                                 textAreaInput(inputId = "text1_te", label = ""), 
                                                                 div(style = "margin-top: -15px"), 
                                                                 p(style = "text-align: right;", tags$b("Note: "), "The first row is used as header."), 
                                                                 
                                                                 # Select the separator
                                                                 tags$b(h5("Separator:")), 
                                                                 div(style = "margin-top: -20px"), 
                                                                 radioButtons(inputId = "SepP_te", 
                                                                              label = "", 
                                                                              choices = c("Comma" = 1, "Tab" = 2, "Semicolon" = 3)), 
                                                                 
                                                                 # Clear the data
                                                                 actionButton(inputId = "clearText_Butn_te", 
                                                                              label = "Clear data")
                                                                 
                                                ),
                                                # Time intervals in the Experiment:
                                                h5(tags$b("Time points & Unit:")), 
                                                div(style = "margin-top: -5px"), 
                                                p("Please provide a comma-separated full list of time points (e.g. 2,4,6,8) in the experiment and the unit (e.g. min):"), 
                                                
                                                div(style = "display: inline-block; vertical-align:top; width: 200px;", 
                                                    textInput(inputId = "time_intv", label = "Time Points", 
                                                              value = "24,26,28,30,32,34,36,38,40,42,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95")), 
                                                div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
                                                div(style = "display: inline-block; vertical-align:top; width: 100px;",
                                                    textInput(inputId = "unit", label = "Unit", value = "s"))
                                               ),
                                              
                                              div(style = "margin-top: 10px"), 
                                              # Confirm the upload
                                              div(style = "text-align: right;",
                                                  actionButton(inputId = "upldData_Butn_te",
                                                               label = tags$b("Go")))
                                              )
                             
                             
            ),
            
            # ED50/ET50 Calculation Tab --------------------------------------------------------------------------------
            conditionalPanel(condition = HTML(paste0("input.tabs1 ==", "'Step 2: ED", tags$sub("50"), "/ET", tags$sub("50"), " Estimation'")),
                             # Dose-response data
                             conditionalPanel(condition = "input.datatype == 'drc'", 
                                              
                                              # Select the methods
                                              wellPanel(style = "background-color: #eaeaea;",
                                                h4(tags$b("Select the method:")) %>% 
                                                  helper(icon = "question-circle", 
                                                         type = "markdown",
                                                         content = "ED_Estimation_Methods",
                                                         buttonLabel = "Close"),
                                                div(style = "margin-top: -10px"), 
                                                selectInput(inputId = "ed_methods",
                                                            label = "",
                                                            choices = c("Standard Method" = "stdd_method", 
                                                                        "Serra-Greco Method" = "serra_greco_method"),
                                                            selected = "stdd_method"),
                                                conditionalPanel(condition = "input.ed_methods == 'stdd_method'",
                                                                 # Select the Type of ED50
                                                                 wellPanel(h4(tags$b(HTML(paste0("Type of ED", tags$sub("50"), ":")))) %>% 
                                                                             helper(icon = "question-circle", 
                                                                                    type = "markdown",
                                                                                    content = "ED50_Type",
                                                                                    buttonLabel = "Close"),
                                                                           div(style = "margin-top: -15px"),
                                                                           radioButtons(inputId = "ed50_type",
                                                                                        label = "",
                                                                                        choices = c("Absolute", "Relative"),
                                                                                        selected = "Absolute"),
                                                                 ),
                                                                 conditionalPanel(condition = "input.ed50_type == 'Absolute'",
                                                                   checkboxInput(inputId = "two_point_method",
                                                                                 label = "Include Reed-Muench Method",
                                                                                 value = FALSE)
                                                                 )
                                                                 
                                                ),
                                                textInput(inputId = "bp", label = "Minimum dose:", value = "default")
                                              ),
                                              
                                              
                                              # Select the candidate models
                                              wellPanel(style = "background-color: #eaeaea;",
                                                h4(tags$b("Select the candidate models:")),
                                                div(style = "margin-top: 10px"), 
                                                wellPanel(
                                                  tags$span("- Monotonic Curves", style = "font-size: 16px; font-weight: bold;") %>% 
                                                    helper(icon = "question-circle", 
                                                           type = "markdown",
                                                           content = "Monotonic_DRC",
                                                           buttonLabel = "Close"),
                                                  div(style = "margin-top: 10px"), 
                                                  fluidRow(
                                                    div(style = "text-align: center; font-weight: bold;", 
                                                        column(6, ""),
                                                        column(3, "Lower"),
                                                        column(3, "Upper")
                                                    )
                                                  ),
                                                  div(style = "margin-top: 10px"), 
                                                  fluidRow(
                                                    div(style = "display:flex;align-items:center;", 
                                                        column(6, div(style = "display:flex;align-items:center;", checkboxInput("LL4", "Log-logistic (4 parms)", TRUE))),
                                                        column(3, textInput("LL4_c", NULL, "NA")),
                                                        column(3, textInput("LL4_d", NULL, "NA"))
                                                    )
                                                  ),
                                                  fluidRow(
                                                    div(style = "display: flex; justify-content: center; align-items: center;", 
                                                        column(6, div(style = "display:flex;align-items:center;", checkboxInput("LL5", "Log-logistic (5 parms)", TRUE))),
                                                        column(3, textInput("LL5_c", NULL, "NA")),
                                                        column(3, textInput("LL5_d", NULL, "NA"))
                                                    )
                                                  ),
                                                  fluidRow(
                                                    div(style = "display: flex; justify-content: center; align-items: center;", 
                                                        column(6, div(style = "display:flex;align-items:center;", checkboxInput("W1", "Weibull I", TRUE))),
                                                        column(3, textInput("W1_c", NULL, "NA")),
                                                        column(3, textInput("W1_d", NULL, "NA"))
                                                    )
                                                  ),
                                                  fluidRow(
                                                    div(style = "display: flex; justify-content: center; align-items: center;", 
                                                        column(6, div(style = "display:flex;align-items:center;", checkboxInput("W2", "Weibull II", TRUE))),
                                                        column(3, textInput("W2_c", NULL, "NA")),
                                                        column(3, textInput("W2_d", NULL, "NA"))
                                                    )
                                                  )
                                                ), 
                                                
                                                uiOutput(outputId = "biphasicmodels")
                                                
                                              ),
                                              
                                              # Model Assessment
                                              wellPanel(style = "background-color: #eaeaea;",
                                                h4(tags$b("Model assessment methods:")) %>% 
                                                  helper(icon = "question-circle", 
                                                         type = "markdown",
                                                         content = "Model_Assessment_Methods",
                                                         buttonLabel = "Close"),
                                                fluidRow(
                                                  div(style = "text-align: center; font-weight: bold;", 
                                                      column(8, ""),
                                                      column(4, tags$em("p-value"))
                                                  )
                                                ),
                                                div(style = "margin-top: 10px"), 
                                                fluidRow(
                                                  div(style = "display:flex;align-items:center;", 
                                                      column(8, div(style = "display:flex;align-items:center;", checkboxInput("lac", "Lack-of-fit test", TRUE))),
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
                                              wellPanel(style = "background-color: #eaeaea;",
                                                h4(tags$b("Criteria for model selection:")) %>% 
                                                  helper(icon = "question-circle", 
                                                         type = "markdown",
                                                         content = "Model_Selection_Criteria",
                                                         buttonLabel = "Close"),
                                                
                                                selectInput(inputId = "crtrn_selected",
                                                            label = "",
                                                            choices = c("Akaike's Information Criterion" = "AIC", 
                                                                        "Bayesian Information Criteria" = "BIC", 
                                                                        "Log Likelihood values" = "logLik", 
                                                                        "Lack-of-fit Test (against a one-way ANOVA model)" = "Lack_of_fit", 
                                                                        "Residual Standard Error" = "Res_var"),
                                                            selected = "AIC")
                                              ),
                                              
                                              # Confirm the calculation
                                              div(style = "text-align: right;",
                                                  actionButton(inputId = "calculate_Butn",
                                                               label = tags$b("Calculate")))# ,
                                              
                                              
                                              # 
                                              # checkboxGroupInput(inputId = "LL_list", 
                                              #                    label = tags$em("- Log-logistic models"),
                                              #                    choices = c("LL.2", "LL.3", "LL.3u", "LL.4", "LL.5"),
                                              #                    inline = TRUE,
                                              #                    selected = "LL.4"),
                                              # checkboxGroupInput(inputId = "W_1_list",
                                              #                    label = tags$em("- Weibull I models"),
                                              #                    choices = c("W1.2", "W1.3", "W1.3u", "W1.4"),
                                              #                    inline = TRUE, 
                                              #                    selected = "W1.4"),
                                              # checkboxGroupInput(inputId = "W_2_list",
                                              #                    label = tags$em("- Weibull II models"),
                                              #                    choices = c("W2.2", "W2.3", "W2.3u", "W2.4"),
                                              #                    inline = TRUE, 
                                              #                    selected = "W2.4"),
                                              # 
                                              # checkboxGroupInput(inputId = "BC_list", 
                                              #                    label = tags$em("- Brain Cousens models"),
                                              #                    choices = c("BC.4", "BC.5"),
                                              #                    inline = TRUE, 
                                              #                    selected = "BC.5"),
                                              # checkboxGroupInput(inputId = "beta_list",
                                              #                    label = tags$em("- beta model"),
                                              #                    choices = c("DRC.beta"),
                                              #                    inline = TRUE,
                                              #                    selected = "DRC.beta")
                                              ),
                             # Time-to-event data:
                             conditionalPanel(condition = "input.datatype == 'te'", 
                                              # Select the model
                                              wellPanel(style = "background-color: #eaeaea;",
                                                h4(tags$b("Select the candidate models:")) %>% 
                                                  helper(icon = "question-circle", 
                                                         type = "markdown",
                                                         content = "Monotonic_TE",
                                                         buttonLabel = "Close"),
                                                div(style = "margin-top: 10px"), 
                                                fluidRow(
                                                  div(style = "text-align: center; font-weight: bold;", 
                                                      column(8, ""),
                                                      column(4, "Upper")
                                                  )
                                                ),
                                                div(style = "margin-top: 10px"), 
                                                fluidRow(
                                                  div(style = "display:flex;align-items:center;", 
                                                      column(8, div(style = "display:flex;align-items:center;", checkboxInput("LL4_te", "Log-logistic (4 parms)", TRUE))),
                                                      column(4, selectInput("LL4_te_d", NULL, c("NA", "1"), "NA"))
                                                  )
                                                ),
                                                fluidRow(
                                                  div(style = "display: flex; justify-content: center; align-items: center;", 
                                                      column(8, div(style = "display:flex;align-items:center;", checkboxInput("LN", "Log-normal", TRUE))),
                                                      column(4, selectInput("LN_d", NULL, c("NA", "1"), "NA"))
                                                  )
                                                ), 
                                                fluidRow(
                                                  div(style = "display:flex;align-items:center;", 
                                                      column(8, div(style = "display:flex;align-items:center;", checkboxInput("W1_te", "Weibull I", TRUE))),
                                                      column(4, selectInput("W1_te_d", NULL, c("NA", "1"), "NA"))
                                                  )
                                                ), 
                                                fluidRow(
                                                  div(style = "display:flex;align-items:center;", 
                                                      column(8, div(style = "display:flex;align-items:center;", checkboxInput("W2_te", "Weibull II", TRUE))),
                                                      column(4, selectInput("W2_te_d", NULL, c("NA", "1"), "NA"))
                                                  )
                                                )
                                              ),
                                             
                                              # Select the criterion
                                              wellPanel(style = "background-color: #eaeaea;",
                                                h4(tags$b("Criteria for model selection:")) %>% 
                                                  helper(icon = "question-circle", 
                                                         type = "markdown",
                                                         content = "Model_Selection_Criteria_TE",
                                                         buttonLabel = "Close"),
                                                
                                                selectInput(inputId = "crtrn_selected_te",
                                                            label = "",
                                                            choices = c("Akaike's Information Criterion" = "AIC", 
                                                                        "Bayesian Information Criteria" = "BIC"),
                                                            selected = "AIC")
                                              ),
                                              
                                              # Confirm the calculation
                                              div(style = "text-align: right;",
                                                  actionButton(inputId = "calculate_Butn_te",
                                                               label = tags$b("Calculate")))
                                              
                                              # , 
                                              # checkboxGroupInput(inputId = "LL_list_te", 
                                              #                    label = tags$em("- Log-logistic models"),
                                              #                    choices = c("LL.2", "LL.3"),
                                              #                    inline = TRUE,
                                              #                    selected = "LL.3"),
                                              # checkboxGroupInput(inputId = "W_1_list_te",
                                              #                    label = tags$em("- Weibull I models"),
                                              #                    choices = c("W1.2", "W1.3"),
                                              #                    inline = TRUE, 
                                              #                    selected = "W1.3"),
                                              # checkboxGroupInput(inputId = "W_2_list_te",
                                              #                    label = tags$em("- Weibull II models"),
                                              #                    choices = c("W2.2", "W2.3"),
                                              #                    inline = TRUE, 
                                              #                    selected = "W2.3"),
                                              # checkboxGroupInput(inputId = "LN_list_te",
                                              #                    label = tags$em("- Log-normal models"),
                                              #                    choices = c("LN.2", "LN.3"),
                                              #                    inline = TRUE, 
                                              #                    selected = "LN.3")
                                              
                                              )
                             
            ),
            
            # Plot Tab -----------------------------------------------------------------------------------------
            conditionalPanel(condition = "input.tabs1 == 'Step 3: Generate plot'",
                             # Show alert message if the user have curves cannot fit to any of the selected models
                             useShinyalert(),
                             conditionalPanel(condition = "input.datatype == 'drc'",
                                              # Select the layout, appearance and model
                                              uiOutput(outputId = "plot_layout_ui"),
                                              uiOutput(outputId = "plot_appearance_ui"),
                                              uiOutput(outputId = "plot_model_ui"),
                                              uiOutput(outputId = "plot_resline_ui"),
                                              
                                              # Confirm to plot
                                              div(style = "text-align: right;",actionButton(inputId = "plot_Butn_1",
                                                                                            label = tags$b("Generate plot"))),
                                              
                                              
                                              # Customize the plot
                                              checkboxInput(inputId = "custmz_plot_ck",
                                                            label = tags$b("Customize the plot"),
                                                            value = FALSE)),
                             
                             conditionalPanel(condition = "input.datatype == 'te'",
                                              # Select the layout, appearance and model
                                              uiOutput(outputId = "plot_layout_ui_te"),
                                              uiOutput(outputId = "plot_appearance_ui_te"),
                                              uiOutput(outputId = "plot_model_ui_te"),
                                              uiOutput(outputId = "plot_resline_ui_te"),
                                              
                                              # Confirm to plot
                                              div(style = "text-align: right;",actionButton(inputId = "plot_Butn_1_te",
                                                                                            label = tags$b("Generate plot"))),
                                              
                                              
                                              # Customize the plot
                                              checkboxInput(inputId = "custmz_plot_ck_te",
                                                            label = tags$b("Customize the plot"),
                                                            value = FALSE))
                             
                             
            ),
            
            # Customized Plot Tab --------------------------------------------------------------------------------
            conditionalPanel(condition = "input.tabs1 == 'Step 4: Customize plot'",
                             conditionalPanel(condition = "input.datatype == 'drc'",
                                              uiOutput(outputId = "tab4_side")),
                             conditionalPanel(condition = "input.datatype == 'te'",
                                              uiOutput(outputId = "tab4_side_te"))
                             )
        ),
        
        # MainPanel  ############################################################################################
        mainPanel(
            tabsetPanel(
                id = "tabs1",
                
                tabPanel(title = "Welcome",
                         h4("Main functions of the app:"),
                         div(style = "margin-top: -10px"),
                         hr(),
                         div(style = "margin-top: -10px"),
                         p(HTML(paste0("1. Calculate the ED", tags$sub("50"), ", or ET", tags$sub("50"), " values from dose-response or time-to-event curves;"))),
                         p("2. Generate dose-response or time-to-event curves with customized appearance;"),
                         p("3. Export the dataframe used for plotting"),
                         br(),
                         h4("Reference:"),
                         div(style = "margin-top: -10px"),
                         hr(),
                         div(style = "margin-top: -10px"),
                         p(em("Wickham H (2014) Tidy Data. Journal of Statistical Software, Articles 59: 1–23")),
                         p(em("Ritz C, Baty F, Streibig JC, Gerhard D (2015) Dose-Response Analysis Using R. PLoS One. 10(12)")),
                         p(em("Ramakrishnan MA (2016) Determination of 50% endpoint titer using a simple formula. World J Virol. 5: 85–86")),
                         p(em("Serra A. Et al. (2020) BMDx: a graphical Shiny application to perform Benchmark Dose analysis for transcriptomics data. Bioinformatics 36: 2932–2933")),
                         p(em("Vaidya, A.S. et al. (2019) Dynamic control of plant water use using designed ABA receptor agonists. Science, 366(6464)")),
                         p(em("Ritz C, Pipper CB, Streibig JC (2013) Analysis of germination data from agricultural experiments. Eur J Agron 45: 1–6")),
                         p(em("Onofri A, Mesgaran MB, Ritz C (2022) A unified framework for the analysis of germination, emergence, and other time-to-event data in weed science. Weed Sci 70: 259–271"))
                ),
                
                tabPanel(title = "Step 1: Input data",
                         h4("Data table", align = 'center'),
                         div(style = "margin-top: -10px"),
                         hr(),
                         div(style = "margin-top: -10px"),
                         conditionalPanel(condition = "input.datatype == 'drc'",
                                          DT::dataTableOutput("df1") %>% shinycssloaders::withSpinner()),
                         conditionalPanel(condition = "input.datatype == 'te'",
                                          DT::dataTableOutput("df1_te") %>% shinycssloaders::withSpinner()),
                         # show the lineplot
                         
                         br(),
                         h4("Line plot", align = 'center'),
                         div(style = "margin-top: -10px"),
                         hr(),
                         div(style = "margin-top: -10px"),
                         conditionalPanel(condition = "input.datatype == 'drc'",
                                          plotOutput("lineplot") %>% shinycssloaders::withSpinner()),
                         conditionalPanel(condition = "input.datatype == 'te'",
                                          plotOutput("lineplot_te") %>% shinycssloaders::withSpinner())
                         
                         
                ),
                
                tabPanel(title = HTML(paste0("Step 2: ED", tags$sub("50"), "/ET", tags$sub("50"), " Estimation")),
                         conditionalPanel(condition = "input.datatype == 'drc'",
                                          uiOutput(outputId = "ed50_results")),
                         conditionalPanel(condition = "input.datatype == 'te'",
                                          uiOutput(outputId = "et50_results"))
                ),
                
                tabPanel(title = "Step 3: Generate plot",
                         conditionalPanel(condition = "input.datatype == 'drc'",
                                          plotOutput("drc_curve") %>% shinycssloaders::withSpinner(), 
                                          uiOutput(outputId = "dl")),
                         conditionalPanel(condition = "input.datatype == 'te'",
                                          plotOutput("drc_curve_te") %>% shinycssloaders::withSpinner(), 
                                          uiOutput(outputId = "dl_te"))
                ),
                
                tabPanel(title = "Step 4: Customize plot",
                         # Plot
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
