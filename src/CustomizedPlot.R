############################################################ Customized plot tab ##################################################################################################

#### Show/Hide "Step 4" Tab --------------------------------------------------------------------------------------------
observe({
  if (input$custmz_plot_ck == TRUE) {
    showTab(inputId = "tabs1", target = "Step 4: Customize plot")
  } else {
    hideTab(inputId = "tabs1", target = "Step 4: Customize plot")
  }
})

#### Create reactiveValues used in the plot ----------------------------------------------------------------------------
# so that these values won't change until the data_predict dataframe changed and avoid errors if users accidentally select the step 4 Tab
cus_plt_values <- reactiveValues()

observe({
  cus_plt_values$var_predict <- colnames(data_predct()[1]) ## Make sure the customized plot will change just when the data_predict change, in other words, when the "Generate Plot" Button is pressed.
  g <- isolate({ggplot_build(L_P())})
  
  cus_plt_values$x <- g[["plot"]][["labels"]][["x"]]
  cus_plt_values$y <- g[["plot"]][["labels"]][["y"]]
  cus_plt_values$legnd_tt <- isolate({input$line_color_v})
  cus_plt_values$fontsize <- 16
  eval(parse(text = paste0("cus_plt_values$legnd_order <- paste0(unique(g[['plot']][['data']][['", isolate({input$line_color_v}), "']]), collapse = ', ')")))
  cus_plt_values$legnd_nm <- isolate({cus_plt_values$legnd_order})
  
  # Line_color--reactiveValues
  cus_plt_values$palette <- paste0(unlist(unique(g$data[[1]]["colour"])), collapse = ", ")

})


#### tab4_side_ui ------------------------------------------------------------------------------------------------------
output$tab4_side <- renderUI({
  req(data_predct())
  tagList(
    list(
      # Label
      checkboxInput(inputId = "label_ck",
                    label = "Label",
                    value = FALSE),
      conditionalPanel(condition = "input.label_ck == 1",
                       wellPanel(# Title
                                 textInput(inputId = "plot_title", label = "Plot title:",
                                           value = ""),
                                 div(style = "margin-top: -10px"),
                                 # X axis
                                 div(style = "vertical-align:top; width: 200px;",
                                     textInput(inputId = "x_label", label = "x axis:",
                                               value = cus_plt_values$x)),
                                 div(style = "margin-top: -10px"),
                                 # Y axis
                                 div(style = "vertical-align:top; width: 200px;",
                                     textInput(inputId = "y_label", label = "y axis:",
                                               value = cus_plt_values$y))
                       )),
      
      # Legend
      if (data_values$n_var != 0) {
        tagList(
          list(
            div(style = "margin-top: 20px"),
            checkboxInput(inputId = "legend_ck",
                          label = "Legend",
                          value = FALSE),
            conditionalPanel(condition = "input.legend_ck == 1",
                             wellPanel(div(style = "vertical-align:top; width: 200px;",
                                           textInput(inputId = "legend_title", label = "Legend title:",
                                                     value = cus_plt_values$legnd_tt)),
                                       div(style = "margin-top: -10px"),
                                       div(style = "vertical-align:top; width: 200px;",
                                           textInput(inputId = "legend_order", label = "Legend order:",
                                                     value = cus_plt_values$legnd_order)),
                                       div(style = "margin-top: -10px"),
                                       div(style = "vertical-align:top; width: 200px;",
                                           textInput(inputId = "legend_names", label = "Rename legend labels:",
                                                     value = cus_plt_values$legnd_nm))
                             ))
          )
        )
      },
      
      # Font-related
      div(style = "margin-top: 20px"),
      checkboxInput(inputId = "font_ck",
                    label = "Font",
                    value = FALSE),
      conditionalPanel(condition = "input.font_ck == 1",
                       wellPanel(div(style = "vertical-align:top; width: 200px;",
                                     textInput(inputId = "font_size", label = "Base font size:",
                                               value = cus_plt_values$fontsize)),
                                 div(style = "margin-top: -10px"),
                                 div(style = "vertical-align:top; width: 200px;",
                                     selectInput(inputId = "font_family", label = "Font family:",
                                                 choices = c("Times New Roman" = "serif",
                                                             "Arial" = "sans",
                                                             "Courier New" = "mono"),
                                                 selected = "sans"))
                       )),
      
      # Line-related
      div(style = "margin-top: 20px"),
      checkboxInput(inputId = "line_ck",
                    label = "Line",
                    value = FALSE),
      conditionalPanel(condition = "input.line_ck == 1",
                       wellPanel(# Line color
                                 p("Please provide a comma-separated list of hex colors."),
                                 textInput(inputId = "palette_provd", label = NULL,
                                           value = cus_plt_values$palette)
                       )
      )
    )
  )
})

# Change the legend label automatically when the legend order changes ----
observeEvent(input$legend_order, {
  updateTextInput(session, inputId = "legend_names",
                  value = input$legend_order)
})



#### Customized Plot -------------------------------------------------------------------------------------------
custmz_P <- reactive({
  
  req(input$palette_provd)
  
  n_var <- isolate({data_values$n_var})
  p <- L_P()
  
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
  
  
  # Palette
  palette <- unlist(strsplit(input$palette_provd, ", "))
  
  # Appearance
  plot_appearance <- isolate({input$plot_appearance})
  
  # Annotation dataframe
  ed_methods <- isolate({input$ed_methods})
  ed50_type <- isolate({input$ed50_type})
  if (n_var == 0) { selected_var <- c((n_var+30):(n_var+51)) } else { selected_var <- c(1:n_var, (n_var+30):(n_var+51))}
  anno_df <- isolate({df_ed()})[ , selected_var]
  anno_df[, (n_var+1):ncol(anno_df)] <- lapply(anno_df[, (n_var+1):ncol(anno_df)], as.numeric)
  
  if (n_var == 0) {
    # color
    clr <- palette
    # plot
    p <- ggplot(data = data_predct(), aes(x = Conc, y = Response)) + 
      geom_line(color = clr) + 
      scale_x_log10(labels = function(x) format(x, scientific = FALSE))
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
    if (any(isolate({df_ed()})$Model %in% c("Brain-Cousens", "Cedergreen-Ritz-Streibig"))) {
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
    p <- L_P() + 
      scale_color_manual(values = palette, name = input$legend_title, 
                         limits = unlist(strsplit(input$legend_order, ", ")), labels = unlist(strsplit(input$legend_names, ", "))
      )
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
  
  font_size <- as.numeric(input$font_size)
  p <- p + 
    xlab(input$x_label) +  
    ylab(input$y_label) + 
    ggtitle(input$plot_title) + 
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
output$drc_curve_2 <- renderPlot({
  return(custmz_P())
})

#### Download UI -------------------------------------------------------------------------------------------
output$dl_2 <- renderUI({
  req(input$plot_Butn_1)
  tagList(
    list(
      h5("Download"),
      div(style = "margin-top: -10px"),
      hr(),
      div(style = "margin-top: -10px"),
      
      # Plot Download related
      textInput(inputId = "file_name_2", label = "Enter a file name: ", value = Sys.time()),
      div(style = "margin-top: -10px"),
      div(style = "display: inline-block; vertical-align:top; width: 100px;",
          textInput(inputId = "width_2", label = "Width", value = 8)),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 100px;",
          textInput(inputId = "height_2", label = "Height", value = 4)),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align:top; width: 150px;",
          selectInput(inputId = "file_type_2", 
                      label = "Select file type: ", 
                      choices = list("PNG", "JPEG", "PDF", "TIFF", "BMP", "SVG"),
                      selected = "PNG")),
      
      # Download button
      br(),
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_plot_2", label = "Download Plot")),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_plot_df_2", label = "Download Dataframe")),
      div(style = "display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")), 
      div(style = "display: inline-block; vertical-align: top;",
          downloadButton(outputId = "dl_report_2", label = "Download Report")),
      
      
      # Notes
      div(style = "margin-top: 10px"),
      tags$b("Note:"),
      p("1. You can only show up to 10 different dose-response-curves in the plots, and please try to avoid ", 
        a(href = "https://www.storytellingwithdata.com/blog/2013/03/avoiding-spaghetti-graph", "spaghetti graph"), "."),  
      div(style = "margin-top: -15px"),
      p("2. The default size is only suitable for two plots; you can specify the aspect ratio for downloading."),  
      div(style = "margin-top: -15px"),
      p(HTML(paste0("2. The excel contains ED", tags$sub("50"), 
                    " table, both dataframes for generating scatterplot and lineplot.")))
      
    )
  )
})



#### Plot Download -----------------------------------------------------------------------------------------
output$dl_plot_2<- downloadHandler(
  #Specify The File Name 
  filename = function(){paste0(input$file_name_2, ".", tolower(input$file_type_2))},
  content = function(file){
    ggsave(file, custmz_P(), device = tolower(input$file_type_2),
           width = as.numeric(input$width_2), height = as.numeric(input$height_2))
  }
)

#### Excel Download ----------------------------------------------------------------------------------------
output$dl_plot_df_2 <- downloadHandler(
  filename = function(){paste0(input$file_name_2, ".xlsx")},
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
output$dl_report_2 <- downloadHandler(
  filename = function(){paste0(input$file_name_2, ".html")},
  content = function(file) {
    tempReport <- file.path(tempdir(), "Report_Custom.Rmd")
    file.copy("reports/Report_Custom.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to Rmd document
    params_2 <- list(table_mono = ED50_table_Monotonic(),
                     table_rm = RM_ED50_table(),
                     table_stats = Stats_table(),
                     ed50_type = input$ed50_type, 
                     ed_methods = input$ed_methods,
                     two_point_method = input$two_point_method,
                     n_var = ncol(data_predct())-2,
                     color_var = input$line_color_v,
                     ED_related = df_ed_exp(),
                     Bestfit_dataframe = data_predct(),
                     ScatterPlot_dataframe = data_scat(),
                     Mean_SD_dataframe = data_m_sd(),
                     Plot_appearance = input$plot_appearance,
                     plot_ed50_ck = input$plot_ed50_ck, 
                     plot_lds_m_ck = input$plot_lds_m_ck, 
                     plot_resline_ck = input$plot_resline_ck,
                     plot_ci_ck = input$plot_ci_ck,
                     plot_title = input$plot_title,
                     label_x_axis = input$x_label,
                     label_y_axis = input$y_label,
                     font_size = input$font_size,
                     font_family = input$font_family,
                     palette = input$palette_provd
    )
    n_var <- ncol(data_predct())-2
    if (n_var <= 1 ) {
      params_2 <- list.append(params_2, facet_var_row = "", facet_var_col = "")
      if(n_var == 1){
        params_2 <- list.append(params_2,
                                legend_title = input$legend_title,
                                legend_label = input$legend_names,
                                legend_order = input$legend_order)
      }
    } else {
      params_2 <- list.append(params_2,
                              legend_title = input$legend_title,
                              legend_label = input$legend_names,
                              legend_order = input$legend_order)
      if (n_var == 2) {
        params_2 <- list.append(params_2, 
                                facet_var_row = input$facet_row_v, 
                                facet_var_col = ".")
      } else {
        params_2 <- list.append(params_2, 
                                facet_var_row = input$facet_row_v, 
                                facet_var_col = paste(setdiff(colnames(data())[1:n_var], c(input$line_color_v, input$facet_row_v)), collapse = "+"))
      }
    }
    
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      params = params_2,
                      envir = new.env(parent = globalenv())
    )
  }
)


