############################################################ Customized plot tab ##################################################################################################

#### Show/Hide "Step 4" Tab -----------------------------------------------------------------------------------
observe({
  if (input$custmz_plot_ck_te == TRUE) {
    showTab(inputId = "tabs1", target = "Step 4: Customize plot")
  } else {
    hideTab(inputId = "tabs1", target = "Step 4: Customize plot")
  }
})

#### Create reactiveValues used in the plot ------------------------------------------------------------------------
# so that these values won't change until the data_predict dataframe changed and avoid errors if users accidentally select the step 4 Tab
cus_plt_values_te <- reactiveValues()

observe({
  cus_plt_values_te$var_predict <- colnames(data_predct_te()[1]) ## Make sure the customized plot will change just when the data_predict change, in other words, when the "Generate Plot" Button is pressed.
  g <- isolate({ggplot_build(L_P_te())})
  
  cus_plt_values_te$x <- g[["plot"]][["labels"]][["x"]]
  cus_plt_values_te$y <- g[["plot"]][["labels"]][["y"]]
  cus_plt_values_te$legnd_tt <- isolate({input$line_color_v_te})
  cus_plt_values_te$fontsize <- 16
  eval(parse(text = paste0("cus_plt_values_te$legnd_order <- paste0(unique(g[['plot']][['data']][['", isolate({input$line_color_v_te}), "']]), collapse = ', ')")))
  cus_plt_values_te$legnd_nm <- isolate({cus_plt_values_te$legnd_order_te})
  
  # Line_color--reactiveValues
  cus_plt_values_te$palette <- paste0(unlist(unique(g$data[[1]]["colour"])), collapse = ", ")
  
  #cus_plt_values_te$selectedCols <- unlist(strsplit(cus_plt_values_te$palette, ", "))
  #cus_plt_values_te$selectedNum <- 1
  #colUpdateSrc = 0
  #plotError = NULL
  
  
  
  
})


#### tab4_side_ui ------------------------------------------------------------------------------------------------------
output$tab4_side_te <- renderUI({
  req(data_predct_te())
  tagList(
    list(
      # Label
      checkboxInput(inputId = "label_ck_te",
                    label = tags$b("Label"),
                    value = FALSE),
      conditionalPanel(condition = "input.label_ck_te == 1",
                       wellPanel(style = "background-color: #eaeaea;",
                                 # Title
                                 textInput(inputId = "plot_title_te", label = "Plot title:",
                                           value = ""),
                                 div(style = "margin-top: -10px"),
                                 # X axis
                                 div(style = "vertical-align:top; width: 200px;",
                                     textInput(inputId = "x_label_te", label = "x axis:",
                                               value = cus_plt_values_te$x)),
                                 div(style = "margin-top: -10px"),
                                 # Y axis
                                 div(style = "vertical-align:top; width: 200px;",
                                     textInput(inputId = "y_label_te", label = "y axis:",
                                               value = cus_plt_values_te$y))
                       )),
      
      # Legend
      if (data_values_te$n_var != 0) {
        tagList(
          list(
            checkboxInput(inputId = "legend_ck_te",
                          label = tags$b("Legend"),
                          value = FALSE),
            conditionalPanel(condition = "input.legend_ck_te == 1",
                             wellPanel(style = "background-color: #eaeaea;",
                                       div(style = "vertical-align:top; width: 200px;",
                                           textInput(inputId = "legend_title_te", label = "Legend title:",
                                                     value = cus_plt_values_te$legnd_tt)),
                                       div(style = "margin-top: -10px"),
                                       div(style = "vertical-align:top; width: 200px;",
                                           textInput(inputId = "legend_order_te", label = "Legend order:",
                                                     value = cus_plt_values_te$legnd_order)),
                                       div(style = "margin-top: -10px"),
                                       div(style = "vertical-align:top; width: 200px;",
                                           textInput(inputId = "legend_names_te", label = "Rename legend labels:",
                                                     value = cus_plt_values_te$legnd_nm))
                             ))
          )
        )
      },
      
      # Font-related
      checkboxInput(inputId = "font_ck_te",
                    label = tags$b("Font"),
                    value = FALSE),
      conditionalPanel(condition = "input.font_ck_te == 1",
                       wellPanel(style = "background-color: #eaeaea;",
                                 div(style = "vertical-align:top; width: 200px;",
                                     textInput(inputId = "font_size_te", label = "Base font size:",
                                               value = cus_plt_values_te$fontsize)),
                                 div(style = "margin-top: -10px"),
                                 div(style = "vertical-align:top; width: 200px;",
                                     selectInput(inputId = "font_family_te", label = "Font family:",
                                                 choices = c("Times New Roman" = "serif",
                                                             "Arial" = "sans",
                                                             "Courier New" = "mono"),
                                                 selected = "sans"))
                       )),
      
      # Line-related
      checkboxInput(inputId = "line_ck_te",
                    label = tags$b("Line"),
                    value = FALSE),
      conditionalPanel(condition = "input.line_ck_te == 1",
                       wellPanel(style = "background-color: #eaeaea;",
                                 # Line color
                                 p(tags$b("Please provide a comma-separated list of hex colors.")),
                                 textInput(inputId = "palette_provd_te", label = NULL,
                                           value = cus_plt_values_te$palette)
                       )
      )
    )
  )
})


# Change the legend label automatically when the legend order changes ----
observeEvent(input$legend_order_te, {
  updateTextInput(session, inputId = "legend_names_te",
                  value = input$legend_order_te)
})



#### Customized Plot -------------------------------------------------------------------------------------------
custmz_P_te <- reactive({
  req(input$palette_provd_te)
  #  req(input$palette_slct)
  n_var <- ncol(data_predct_te())-2
  p <- L_P_te()
  
  my_colors <- unlist(strsplit(input$palette_provd_te, ", "))
  
  if (n_var == 0) {
    p <- p + 
      scale_color_manual(values = my_colors)
  } else {
    p <- p + 
      scale_color_manual(values = my_colors, name = input$legend_title_te, 
                         limits = unlist(strsplit(input$legend_order_te, ", ")), labels = unlist(strsplit(input$legend_names_te, ", "))
      )
  }
  
  font_size <- as.numeric(input$font_size_te)
  p <- p + 
    xlab(input$x_label_te) +  
    ylab(input$y_label_te) + 
    ggtitle(input$plot_title_te) + 
    theme_few() + 
    panel_border(colour = "black", size = 1, remove = FALSE) + 
    theme(axis.title = element_text(size = font_size + 2),
          plot.title = element_text(hjust = 0.5, size = font_size + 4), 
          axis.text = element_text(size = font_size),
          strip.text = element_text(size = font_size + 2),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size + 2),
          text = element_text(family = input$font_family))
  
  
  
  p
  
})

#### Output plot -------------------------------------------------------------------------------------------
output$drc_curve_2_te <- renderPlot({
  return(custmz_P_te())
})

#### Download UI -------------------------------------------------------------------------------------------
output$dl_2_te <- renderUI({
  req(input$plot_Butn_1_te)
  tagList(
    list(
      h4("Download"),
      div(style = "margin-top: -10px"),
      hr(),
      div(style = "margin-top: -10px"),
      
      # Plot Download related
      textInput(inputId = "file_name_2_te", label = "Enter a file name: ", value = Sys.time()),
      div(style = "margin-top: -10px"),
      div(style = "display: inline-block; vertical-align:top; width: 100px;",
          textInput(inputId = "width_2_te", label = "Width", value = 8)),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 100px;",
          textInput(inputId = "height_2_te", label = "Height", value = 4)),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 150px;",
          selectInput(inputId = "file_type_2_te", 
                      label = "Select file type: ", 
                      choices = list("PNG", "JPEG", "PDF", "TIFF", "BMP", "SVG"),
                      selected = "PNG")),
      
      # Download button
      br(),
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_plot_2_te", label = "Download Plot")),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_plot_df_2_te", label = "Download Dataframe")),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_report_2_te", label = "Download Report")),
      
      
      # Notes
      div(style = "margin-top: 10px"),
      tags$b("Note:"),
      p("1. You can only show up to 10 different dose-response-curves in the plots, and please try to avoid ", 
        tags$b(a(href = "https://www.storytellingwithdata.com/blog/2013/03/avoiding-spaghetti-graph", "spaghetti graph")), "."),  
      div(style = "margin-top: -10px"),
      p("2. The default size is only suitable for two plots; you can specify the aspect ratio for downloading."),  
      div(style = "margin-top: -10px"),
      p(HTML(paste0("2. The excel contains ET", tags$sub("50"), 
                    " table, both dataframes for generating scatterplot and lineplot.")))
      
    )
  )
})



#### Plot Download -----------------------------------------------------------------------------------------
output$dl_plot_2_te<- downloadHandler(
  #Specify The File Name 
  filename = function(){paste0(input$file_name_2_te, ".", tolower(input$file_type_2_te))},
  content = function(file){
    ggsave(file, custmz_P_te(), device = tolower(input$file_type_2_te),
           width = as.numeric(input$width_2_te), height = as.numeric(input$height_2_te))
  }
)

#### Excel Download ----------------------------------------------------------------------------------------
output$dl_plot_df_2_te <- downloadHandler(
  filename = function(){paste0(input$file_name_2_te, ".xlsx")},
  content = function(file) {
    list_of_datasets <- list("ED50_related" = ET50_table(), 
                             "Bestfit_dataframe" = data_predct_te(), 
                             "ScatterPlot_dataframe" = data_scat_te()
    )
    write.xlsx(list_of_datasets, file)
  }
)

#### Report Download ---------------------------------------------------------------------------------------
output$dl_report_2_te <- downloadHandler(
  filename = function(){paste0(input$file_name_2_te, ".html")},
  content = function(file) {
    tempReport <- file.path(tempdir(), "Report_Custom_TE.Rmd")
    file.copy("Report_Custom_TE.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params_2_te <- list(table = ET50_table(),
                        n_var = ncol(data_predct_te())-2,
                        color_var = input$line_color_v_te,
                        Bestfit_dataframe = data_predct_te(),
                        ScatterPlot_dataframe = data_scat_te(),
                        plot_title = input$plot_title_te,
                        label_x_axis = input$x_label_te,
                        label_y_axis = input$y_label_te,
                        font_size = input$font_size_te,
                        font_family = input$font_family_te,
                        palette = input$palette_provd_te
    )
    n_var <- ncol(data_predct_te())-2
    if (n_var <= 1 ) {
      params_2_te <- list.append(params_2_te, facet_var_row = "", facet_var_col = "")
    } else {
      if (n_var == 2) {
        params_2_te <- list.append(params_2_te, 
                                   facet_var_row = input$facet_row_v_te, 
                                   facet_var_col = ".",
                                   legend_title = input$legend_title_te,
                                   legend_label = input$legend_names_te,
                                   legend_order = input$legend_order_te)
      } else {
        params_2_te <- list.append(params_2_te, 
                                   facet_var_row = input$facet_row_v_te, 
                                   facet_var_col = paste(setdiff(colnames(data_te())[1:n_var], c(input$line_color_v_te, input$facet_row_v_te)), collapse = "+"),
                                   legend_title = input$legend_title_te,
                                   legend_label = input$legend_names_te,
                                   legend_order = input$legend_order_te)
      }
    }
    
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params_2_te,
                      envir = new.env(parent = globalenv())
    )
  }
)


