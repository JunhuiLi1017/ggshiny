library(shiny)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(DT)  # For data preview
library(colourpicker)  # For color pickers
library(plotly)  # For interactive plots
library(htmlwidgets)  # For HTML export

# Main app function with esquisse-style parameters
volcanoProfiler <- function(data = NULL,
                           import_data = TRUE,
                           show_data = TRUE,
                           update_variable = TRUE,
                           create_column = TRUE,
                           update_factor = TRUE,
                           settings = TRUE,
                           close = TRUE,
                           controls = c("options", "labs", "axes", "geoms", "theme", "filters", "code")) {
  
  # Default data if none provided
  if (is.null(data)) {
    data <- data.frame(
      external_gene_name = rownames(mtcars),
      logFC = mtcars$mpg - mtcars$cyl,
      FDR = runif(nrow(mtcars), 0.01, 0.1)
    )
  }
  
  ui <- fluidPage(
    # Custom CSS for esquisse-like styling
    tags$head(
      tags$style(HTML("
        .well {
          background-color: #f8f9fa;
          border: 1px solid #e9ecef;
          border-radius: 8px;
          margin-bottom: 15px;
          padding: 15px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .well h4 {
          font-weight: 600;
          font-size: 14px;
          margin-bottom: 15px;
          padding-bottom: 8px;
          border-bottom: 2px solid #e9ecef;
        }
        
        .well h5 {
          font-weight: 500;
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin-bottom: 8px;
        }
        
        .form-group {
          margin-bottom: 12px;
        }
        
        .form-group label {
          font-size: 12px;
          font-weight: 500;
          color: #495057;
          margin-bottom: 4px;
        }
        
        .form-control {
          border-radius: 4px;
          border: 1px solid #ced4da;
          font-size: 12px;
          padding: 6px 8px;
        }
        
        .form-control:focus {
          border-color: #80bdff;
          box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25);
        }
        
        .btn {
          border-radius: 4px;
          font-size: 12px;
          font-weight: 500;
        }
        
        .sidebar-panel {
          background-color: #f8f9fa;
          border-right: 1px solid #e9ecef;
          padding: 20px;
        }
        
        .main-panel {
          background-color: white;
          padding: 20px;
        }
        
        .tab-panel {
          background-color: white;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    titlePanel("VolcanoProfiler: Interactive Volcano Plot for KRAS Gene Sets"),
    
    tabsetPanel(
      # Data tab
      if (import_data) {
        tabPanel("Data",
                 div(
                   class = "well",
                   h4("📁 Data Upload", style = "margin-top: 0; color: #2c3e50;"),
                   p("Upload your differential expression data and gene set files, or use the sample data for testing."),
                   fileInput("data_file", "Upload Differential Expression CSV (columns: external_gene_name, logFC, FDR)"),
                   fileInput("gmt_file", "Upload GMT File (e.g., Hallmark gene sets)"),
                   checkboxInput("use_sample_data", "Use Sample Data (mtcars example)", value = FALSE),
                   hr(),
                   h5("Instructions:", style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"),
                   p("• Check 'Use Sample Data' to test the app with example data", style = "font-size: 11px; color: #6c757d;"),
                   p("• Or upload your own CSV and GMT files", style = "font-size: 11px; color: #6c757d;"),
                   p("• CSV should have columns: external_gene_name, logFC, FDR", style = "font-size: 11px; color: #6c757d;")
                 ),
                 textInput("gene_set1_name", "Gene Set 1 Name (e.g., HALLMARK_KRAS_SIGNALING_DN)", value = "HALLMARK_KRAS_SIGNALING_DN"),
                 textInput("gene_set2_name", "Gene Set 2 Name (e.g., HALLMARK_KRAS_SIGNALING_UP)", value = "HALLMARK_KRAS_SIGNALING_UP"),
                 textInput("gene_sig", "Signature Genes (comma-separated, e.g., KRAS,FOSL1,MYC)", value = "KRAS,FOSL1,MYC"),
                 if (show_data) DTOutput("data_preview")
        )
      },
      
      # Volcano Plot tab with modular controls
      tabPanel("Volcano Plot",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   class = "sidebar-panel",
                   
                   # Options control
                   if ("options" %in% controls) {
                     div(
                       class = "well",
                       h4("⚙️ Options", style = "margin-top: 0; color: #2c3e50;"),
                       checkboxInput("enable_zoom", "Enable Zoom", value = TRUE),
                       checkboxInput("enable_hover", "Enable Hover Tooltips", value = TRUE),
                       checkboxInput("enable_selection", "Enable Point Selection", value = FALSE),
                       checkboxInput("show_legend", "Show Legend", value = FALSE),
                       checkboxInput("draw_lineranges", "Draw Lineranges", value = TRUE)
                     )
                   },
                   
                   # Filters control
                   if ("filters" %in% controls) {
                     div(
                       class = "well",
                       h4("🔍 Filters", style = "margin-top: 0; color: #2c3e50;"),
                       numericInput("fdr_cutoff_deg", "FDR Cutoff for DEGs:", 
                                   value = 0.05, min = 0, max = 1, step = 0.01),
                       numericInput("logfc_cutoff_down", "LogFC Cutoff (Down):", 
                                   value = -0.5, step = 0.1),
                       numericInput("logfc_cutoff_up", "LogFC Cutoff (Up):", 
                                   value = 0.5, step = 0.1),
                       numericInput("fdr_cutoff_top", "FDR Cutoff (Top 200):", 
                                   value = 0.01, min = 0, max = 1, step = 0.01)
                     )
                   },
                   
                   # Geoms control
                   if ("geoms" %in% controls) {
                     div(
                       class = "well",
                       h4("🎨 Visual Elements", style = "margin-top: 0; color: #2c3e50;"),
                       numericInput("point_size_other", "Point Size (Other):", 
                                   value = 1.2, min = 0.1, step = 0.1),
                       numericInput("point_size_set", "Point Size (Gene Sets):", 
                                   value = 1.8, min = 0.1, step = 0.1),
                       numericInput("lab_size", "Label Size:", 
                                   value = 5, min = 1, step = 0.5)
                     )
                   },
                   
                   # Colors control
                   if ("theme" %in% controls) {
                     div(
                       class = "well",
                       h4("🎨 Colors", style = "margin-top: 0; color: #2c3e50;"),
                       div(
                         style = "margin-bottom: 10px;",
                         h5("Gene Sets:", style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"),
                         colourInput("col_set1", "Gene Set 1 (DN)", value = "#C6C6C6"),
                         colourInput("col_set2", "Gene Set 2 (UP)", value = "#FFBF00"),
                         colourInput("col_sig", "Signature Genes", value = "#07F1F9")
                       ),
                       div(
                         style = "margin-bottom: 10px;",
                         h5("Highlights:", style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"),
                         colourInput("col_up_highlight", "Up Highlight", value = "#DCEABB"),
                         colourInput("col_down_highlight", "Down Highlight", value = "#CCDFF1")
                       ),
                       div(
                         style = "margin-bottom: 10px;",
                         h5("Text & Lines:", style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"),
                         colourInput("col_hline_dotted", "FDR Line", value = "#A2CA55"),
                         colourInput("col_text_up", "Text (UP)", value = "#5EA7D3"),
                         colourInput("col_text_down", "Text (DOWN)", value = "#A2CA55")
                       )
                     )
                   },
                   
                   # Axes control
                   if ("axes" %in% controls) {
                     div(
                       class = "well",
                       h4("📐 Axes", style = "margin-top: 0; color: #2c3e50;"),
                       div(
                         style = "margin-bottom: 10px;",
                         h5("X-axis Limits:", style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"),
                         numericInput("xlim_min", "Min:", value = -3, step = 0.5),
                         numericInput("xlim_max", "Max:", value = 3, step = 0.5)
                       ),
                       div(
                         style = "margin-bottom: 10px;",
                         h5("Y-axis Limits:", style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"),
                         numericInput("ylim_min", "Min:", value = -2, step = 0.5),
                         numericInput("ylim_max", "Max:", value = 7, step = 0.5)
                       )
                     )
                   },
                   
                   # Labs control
                   if ("labs" %in% controls) {
                     div(
                       class = "well",
                       h4("🏷️ Labels", style = "margin-top: 0; color: #2c3e50;"),
                       textInput("x_lab", "X-axis Label:", 
                                value = "KRAS vs NS siRNA (log2FC)"),
                       textInput("y_lab", "Y-axis Label:", 
                                value = "Significance (-log10 adj.p-value)")
                     )
                   }
                 ),
                 mainPanel(
                   width = 9,
                   class = "main-panel",
                   plotlyOutput("volcano_plot", height = "700px")
                 )
               )
      ),
      
      # Code tab
      if ("code" %in% controls) {
        tabPanel("Code",
                 verbatimTextOutput("generated_code"),
                 downloadButton("download_plot", "Download Plot as PNG"),
                 downloadButton("download_plot_html", "Download Plot as HTML"),
                 downloadButton("download_code", "Download R Code")
        )
      }
    )
  )

  server <- function(input, output, session) {
    data <- reactive({
      # Check if use_sample_data input exists (app initialization)
      if (is.null(input$use_sample_data)) {
        return(NULL)
      }
      
      if (input$use_sample_data) {
        # Use mtcars as a sample data
        data.frame(
          external_gene_name = rownames(mtcars),
          logFC = mtcars$mpg - mtcars$cyl,
          FDR = runif(nrow(mtcars), 0.01, 0.1)
        )
      } else {
        # Require file upload when not using sample data
        if (is.null(input$data_file)) {
          return(NULL)
        }
        read.csv(input$data_file$datapath)
      }
    })
    
    gmt <- reactive({
      # Check if use_sample_data input exists (app initialization)
      if (is.null(input$use_sample_data)) {
        return(NULL)
      }
      
      if (input$use_sample_data) {
        # Use a dummy GMT file for sample data
        c(
          "HALLMARK_KRAS_SIGNALING_DN\tKRAS,FOSL1,MYC",
          "HALLMARK_KRAS_SIGNALING_UP\tKRAS,FOSL1,MYC"
        )
      } else {
        # Require file upload when not using sample data
        if (is.null(input$gmt_file)) {
          return(NULL)
        }
        readLines(input$gmt_file$datapath)
      }
    })
    
    gene_sets <- reactive({
      gmt_lines <- gmt()
      if (is.null(gmt_lines)) return(NULL)
      
      if (input$use_sample_data) {
        # For sample data, use simple gene sets
        gene_set1 <- c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710")
        gene_set2 <- c("Hornet 4 Drive", "Hornet Sportabout", "Valiant")
        gene_sig <- strsplit(input$gene_sig, ",")[[1]]
      } else {
        # For uploaded data, process GMT file
        gene_set1 <- strsplit(gmt_lines[grepl(input$gene_set1_name, gmt_lines)], "\t")[[1]][-c(1, 2)]
        gene_set2 <- strsplit(gmt_lines[grepl(input$gene_set2_name, gmt_lines)], "\t")[[1]][-c(1, 2)]
        gene_sig <- strsplit(input$gene_sig, ",")[[1]]
      }
      
      list(set1 = gene_set1, set2 = gene_set2, sig = gene_sig)
    })
    
    processed_data <- reactive({
      df <- data()
      if (is.null(df)) return(NULL)
      
      sets <- gene_sets()
      if (is.null(sets)) return(NULL)
      
      df %>%
        mutate(gene_set = case_when(
          external_gene_name %in% sets$set1 ~ "gene_set1",
          external_gene_name %in% sets$set2 ~ "gene_set2",
          external_gene_name %in% sets$sig ~ "gene_sig",
          TRUE ~ "other"
        ))
    })
    
    filters <- reactive({
      df <- data()
      if (is.null(df)) return(NULL)
      
      down_genes <- df %>% filter(FDR < input$fdr_cutoff_deg, logFC < input$logfc_cutoff_down)
      up_genes <- df %>% filter(FDR < input$fdr_cutoff_deg, logFC > input$logfc_cutoff_up)
      top_down200 <- df %>% filter(FDR < input$fdr_cutoff_top, logFC < 0) %>% arrange(logFC) %>% head(200)
      top_up200 <- df %>% filter(FDR < input$fdr_cutoff_top, logFC > 0) %>% arrange(desc(logFC)) %>% head(200)
      
      list(down = down_genes, up = up_genes, top_down = top_down200, top_up = top_up200)
    })
    
    if (show_data) {
      output$data_preview <- renderDT({
        df <- data()
        if (is.null(df)) {
          # Show welcome message instead of error
          welcome_df <- data.frame(
            Message = c(
              "Welcome to VolcanoProfiler!",
              "To get started:",
              "1. Check 'Use Sample Data' to test with example data",
              "2. Or upload your CSV and GMT files",
              "3. Configure your gene sets and signature genes",
              "4. Go to 'Volcano Plot' tab to create your plot"
            )
          )
          datatable(welcome_df, 
                   options = list(scrollX = TRUE, dom = 't'),
                   rownames = FALSE,
                   colnames = "Instructions")
        } else {
          datatable(head(df, 100), options = list(scrollX = TRUE))
        }
      })
    }
    
    volcano_plot <- reactive({
      df <- processed_data()
      if (is.null(df)) {
        return(NULL)
      }
      
      f <- filters()
      if (is.null(f)) {
        return(NULL)
      }
      
      data_other <- df %>% filter(gene_set == "other")
      data_gene_set <- df %>% filter(gene_set != "other")
      
      # Add tooltip information
      df <- df %>%
        mutate(
          tooltip_text = case_when(
            gene_set == "gene_set1" ~ paste0("Gene: ", external_gene_name, "<br>",
                                            "Gene Set: KRAS Signaling DN<br>",
                                            "logFC: ", round(logFC, 3), "<br>",
                                            "FDR: ", format(FDR, scientific = TRUE, digits = 3)),
            gene_set == "gene_set2" ~ paste0("Gene: ", external_gene_name, "<br>",
                                            "Gene Set: KRAS Signaling UP<br>",
                                            "logFC: ", round(logFC, 3), "<br>",
                                            "FDR: ", format(FDR, scientific = TRUE, digits = 3)),
            gene_set == "gene_sig" ~ paste0("Gene: ", external_gene_name, "<br>",
                                           "Type: Signature Gene<br>",
                                           "logFC: ", round(logFC, 3), "<br>",
                                           "FDR: ", format(FDR, scientific = TRUE, digits = 3)),
            TRUE ~ paste0("Gene: ", external_gene_name, "<br>",
                         "logFC: ", round(logFC, 3), "<br>",
                         "FDR: ", format(FDR, scientific = TRUE, digits = 3))
          )
        )
      
      # Update data subsets with tooltip information
      data_other <- df %>% filter(gene_set == "other")
      data_gene_set <- df %>% filter(gene_set != "other")
      
      p1 <- ggplot(df, aes(x = logFC, y = -log10(FDR), text = tooltip_text)) +
        annotate("rect", xmin = min(f$up$logFC, na.rm = TRUE), xmax = max(f$up$logFC, na.rm = TRUE), ymin = -log10(input$fdr_cutoff_deg), ymax = max(-log10(df$FDR), na.rm = TRUE), fill = input$col_up_highlight, alpha = 0.5) +
        annotate("rect", xmin = min(f$down$logFC, na.rm = TRUE), xmax = max(f$down$logFC, na.rm = TRUE), ymin = -log10(input$fdr_cutoff_deg), ymax = max(-log10(df$FDR), na.rm = TRUE), fill = input$col_down_highlight, alpha = 0.5) +
        geom_vline(xintercept = 0, color = "grey60", linewidth = 0.6) +
        geom_hline(yintercept = 0, color = "grey60", linewidth = 0.6) +
        geom_hline(yintercept = -log10(input$fdr_cutoff_deg), linetype = "dotted", color = input$col_hline_dotted, linewidth = 0.6) +
        geom_point(data = data_other, aes(text = tooltip_text), shape = 21, color = "black", alpha = 0.1, size = input$point_size_other, stroke = 0.7) +
        geom_point(data = data_gene_set, aes(fill = gene_set, text = tooltip_text), shape = 21, color = "black", size = input$point_size_set, stroke = 0.8) +
        scale_fill_manual(values = c("gene_set1" = input$col_set1, "gene_set2" = input$col_set2, "gene_sig" = input$col_sig)) +
        annotate("rect", xmin = min(f$top_up$logFC, na.rm = TRUE), xmax = max(f$top_up$logFC, na.rm = TRUE), ymin = -log10(input$fdr_cutoff_top), ymax = max(-log10(df$FDR), na.rm = TRUE), fill = "transparent", linetype = "dotted", color = "black", linewidth = 0.6) +
        annotate("rect", xmin = min(f$top_down$logFC, na.rm = TRUE), xmax = max(f$top_down$logFC, na.rm = TRUE), ymin = -log10(input$fdr_cutoff_top), ymax = max(-log10(df$FDR), na.rm = TRUE), fill = "transparent", linetype = "dotted", color = "black", linewidth = 0.6) +
        geom_label_repel(data = df %>% filter(gene_set == "gene_sig"), aes(label = external_gene_name), size = input$lab_size, box.padding = unit(0.35, "lines"), segment.colour = "grey30") +
        scale_x_continuous(limits = c(input$xlim_min, input$xlim_max), breaks = seq(input$xlim_min, input$xlim_max, 1)) +
        labs(x = input$x_lab, y = input$y_lab) +
        theme_classic(base_size = 15) +
        theme(legend.position = if(input$show_legend) "right" else "none")
      p2 <- p1 + scale_y_continuous(limits = c(input$ylim_min, input$ylim_max), breaks = seq(0, input$ylim_max, 2), expand = c(0, 0))
      if (input$draw_lineranges) {
        p2 <- p2 +
          geom_linerange(data = data_gene_set %>% filter(gene_set == "gene_set2"), aes(x = logFC, ymin = input$ylim_min + 1, ymax = input$ylim_min + 1.65), color = input$col_set2, size = 0.5) +
          geom_linerange(data = data_gene_set %>% filter(gene_set == "gene_set1"), aes(x = logFC, ymin = input$ylim_min + 0.25, ymax = input$ylim_min + 0.9), color = input$col_set1, size = 0.5)
      }
      p2 +
        annotate("text", x = 1.6, y = input$ylim_max - 0.8, label = "PDAC KRAS UP\n(KRAS-dependent)", color = input$col_text_up, size = 5, lineheight = 0.8, vjust = 0) +
        annotate("text", x = -1.7, y = input$ylim_max - 0.8, label = "PDAC KRAS DOWN\n(KRAS-inhibited)", color = input$col_text_down, size = 5, lineheight = 0.8, vjust = 0) +
        annotate("text", x = 2.1, y = input$ylim_max - 1.2, label = "Top 200", color = "black", size = 5) +
        annotate("text", x = -2.5, y = input$ylim_max - 1.2, label = "Top 200", color = "black", size = 5) +
        annotate("text", x = -2.8, y = -log10(input$fdr_cutoff_deg) - 0.1, label = "α = 0.05", color = input$col_hline_dotted, size = 4.5) +
        annotate("text", x = 2.3, y = input$ylim_min + 1.3, label = "KRAS_SIGNALING_UP", color = input$col_set2, size = 4.5) +
        annotate("text", x = 2.3, y = input$ylim_min + 0.6, label = "KRAS_SIGNALING_DN", color = input$col_set1, size = 4.5) +
        annotate("text", x = -3.0, y = input$ylim_min + 1.3, label = "ES:-0.51\npval:4.9e-12", color = input$col_set2, size = 4.5, lineheight = 0.8, hjust = 0) +
        annotate("text", x = -3.0, y = input$ylim_min + 0.6, label = "ES:0.54\npval:1.6e-8", color = input$col_set1, size = 4.5, lineheight = 0.8, hjust = 0)
    }
    
    output$volcano_plot <- renderPlotly({
      p <- volcano_plot()
      if (is.null(p)) {
        # Return empty plotly object
        plot_ly() %>% 
          add_annotations(
            text = "No data available for plotting",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE
          ) %>%
          layout(
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      } else {
        # Configure plotly based on user settings
        plotly_obj <- ggplotly(p, tooltip = if(input$enable_hover) c("text", "x", "y") else "none") %>%
          layout(
            hovermode = "closest",
            showlegend = input$show_legend
          )
        
        # Configure mode bar based on user settings
        mode_bar_buttons <- c("resetScale2d")
        
        if (input$enable_zoom) {
          mode_bar_buttons <- c(mode_bar_buttons, "zoomIn2d", "zoomOut2d")
        }
        
        if (input$enable_selection) {
          mode_bar_buttons <- c(mode_bar_buttons, "pan2d", "lasso2d", "select2d")
        }
        
        plotly_obj %>%
          config(
            displayModeBar = TRUE,
            modeBarButtonsToRemove = setdiff(c("pan2d", "lasso2d", "select2d", "zoomIn2d", "zoomOut2d", "resetScale2d"), mode_bar_buttons),
            displaylogo = FALSE
          )
      }
    })
    
    if ("code" %in% controls) {
      output$generated_code <- renderText({
        paste0(
          "# Load libraries\n",
          "library(ggplot2)\nlibrary(dplyr)\nlibrary(ggrepel)\nlibrary(plotly)\n\n",
          "# Load data\n",
          "data <- read.csv('your_data.csv')\n",
          "gmt <- readLines('your_gmt.gmt')\n\n",
          "# Gene sets\n",
          "gene_set1 <- strsplit(gmt[grepl('", input$gene_set1_name, "', gmt)], '\t')[[1]][-c(1, 2)]\n",
          "gene_set2 <- strsplit(gmt[grepl('", input$gene_set2_name, "', gmt)], '\t')[[1]][-c(1, 2)]\n",
          "gene_sig <- c(", paste0("'", strsplit(input$gene_sig, ",")[[1]], "'", collapse = ", "), ")\n\n",
          "# Processing and plot code...\n",
          "# (Full code would be expanded here based on inputs)"
        )
      })
      
      output$download_plot <- downloadHandler(
        filename = "volcano_plot.png",
        content = function(file) {
          # For static PNG download, we need to convert plotly back to ggplot
          p <- volcano_plot()
          if (is.null(p)) {
            stop("No plot available for download")
          }
          ggsave(file, plot = p, device = "png", width = 10, height = 8, dpi = 300)
        }
      )
      
      # Add HTML download for interactive plot
      output$download_plot_html <- downloadHandler(
        filename = "volcano_plot.html",
        content = function(file) {
          p <- volcano_plot()
          if (is.null(p)) {
            stop("No plot available for download")
          }
          plotly_obj <- ggplotly(p, tooltip = c("text", "x", "y")) %>%
            layout(
              hovermode = "closest",
              showlegend = input$show_legend
            ) %>%
            config(
              displayModeBar = TRUE,
              modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d"),
              displaylogo = FALSE
            )
          htmlwidgets::saveWidget(plotly_obj, file)
        }
      )
      
      output$download_code <- downloadHandler(
        filename = "volcano_code.R",
        content = function(file) {
          code <- output$generated_code()
          writeLines(code, file)
        }
      )
    }
  }
  
  shinyApp(ui, server)
}

# Example usage:
# volcanoProfiler() # Default to mtcars example
# volcanoProfiler(data = read.csv("your_data.csv"), gmt = readLines("your_gmt.gmt"))
# volcanoProfiler(import_data = FALSE, show_data = FALSE, controls = c("geoms", "theme", "axes", "labs"))

# Run the app with default settings
volcanoProfiler()




