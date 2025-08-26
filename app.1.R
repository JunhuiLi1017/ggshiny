# This app allows users to create interactive ggplot2 plots with multiple layers and dynamic UI elements.
# It provides a user-friendly interface for selecting variables, aesthetics, and plot types.
# The app supports various geom types, including points, lines, bars, histograms, and more.
# Users can add, remove, and configure multiple layers, and customize plot settings such as colors, sizes, and labels.
# The app includes faceting options, statistical summary layers, t-test/Wald test for boxplot/violin plots, and dynamic theme parameter customization with color pickers.
# The final plot is displayed using ggplot2 and can be exported as an interactive plotly object.

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr) # For data manipulation, especially with faceting
library(rlang) # For tidy evaluation with !!! operator
library(shinyjs) # For JavaScript functionalities like toggling elements
library(stats) # For t.test and pnorm
library(colourpicker) # For color picker input
library(plotly) # For interactive plots

# Use the built-in mtcars dataset
data(mtcars)

# Convert relevant columns to factors for categorical plotting
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

# Define a helper function to generate UI for a single layer
layer_controls_ui <- function(id_suffix, df) {
  div(
    h3(paste("Layer", id_suffix)),
    
    # Add collapsible aesthetics section
    checkboxInput(paste0("show_aes_", id_suffix), "Aesthetics", value = TRUE),
    conditionalPanel(
      condition = paste0("input.show_aes_", id_suffix, " == true"),
      selectInput(paste0("geom_type_", id_suffix), "Geometric:",
                  choices = c(
                    "point", "line", "bar", "boxplot", "violin", "histogram", "density",
                    "area", "smooth", "text", "label", "jitter", "count", "dotplot"
                  ),
                  selected = "point"),
      
      # X and Y variable selection
      selectInput(paste0("x_var_", id_suffix), "X variable:", 
                  choices = names(df), selected = "wt"),
      
      conditionalPanel(
        condition = paste0(
          "input.geom_type_", id_suffix, " != 'histogram' && ",
          "input.geom_type_", id_suffix, " != 'density' && ",
          "input.geom_type_", id_suffix, " != 'dotplot' && ",
          "input.geom_type_", id_suffix, " != 'count' && ",
          "input.geom_type_", id_suffix, " != 'bar'"
        ),
        selectInput(paste0("y_var_", id_suffix), "Y variable:", 
                    choices = names(df)[sapply(df, is.numeric)], selected = "mpg")
      ),
      
      # Color and fill variables
      selectInput(paste0("color_var_", id_suffix), "Color variable:", 
                  choices = c("None", names(df)), selected = "None"),
      selectInput(paste0("fill_var_", id_suffix), "Fill variable:", 
                  choices = c("None", names(df)), selected = "None"),
      
      # Size and alpha
      selectInput(paste0("size_var_", id_suffix), "Size variable:", 
                  choices = c("None", names(df)[sapply(df, is.numeric)]), selected = "None"),
      numericInput(paste0("size_val_", id_suffix), "Fixed size:", value = 1, min = 0.1, step = 0.1),
      
      selectInput(paste0("alpha_var_", id_suffix), "Alpha variable:", 
                  choices = c("None", names(df)[sapply(df, is.numeric)]), selected = "None"),
      numericInput(paste0("alpha_val_", id_suffix), "Fixed alpha (0-1):", value = 1, min = 0, max = 1, step = 0.1)
    ),
    
    # Add collapsible Axis & Scale section
    hr(),
    checkboxInput(paste0("show_axis_scale_", id_suffix), "Axis & Scale", value = FALSE),
    conditionalPanel(
      condition = paste0("input.show_axis_scale_", id_suffix, " == true"),
      selectInput(paste0("x_trans_", id_suffix), "X-axis Transformation:",
                  choices = c("none", "log2", "log10", "sqrt", "reverse"),
                  selected = "none"),
      numericInput(paste0("xmin_", id_suffix), "X-axis Minimum:", value = NA),
      numericInput(paste0("xmax_", id_suffix), "X-axis Maximum:", value = NA),
      selectInput(paste0("y_trans_", id_suffix), "Y-axis Transformation:",
                  choices = c("none", "log2", "log10", "sqrt", "reverse"),
                  selected = "none"),
      numericInput(paste0("ymin_", id_suffix), "Y-axis Minimum:", value = NA),
      numericInput(paste0("ymax_", id_suffix), "Y-axis Maximum:", value = NA)
    ),
    
    # Add collapsible Labels & Title section
    hr(),
    checkboxInput(paste0("show_labels_title_", id_suffix), "Labels & Title", value = FALSE),
    conditionalPanel(
      condition = paste0("input.show_labels_title_", id_suffix, " == true"),
      textInput(paste0("layer_title_", id_suffix), "Title:", value = ""),
      textInput(paste0("x_label_", id_suffix), "X-axis Label:", value = ""),
      textInput(paste0("y_label_", id_suffix), "Y-axis Label:", value = "")
    )
  )
}

# Define the User Interface (UI)
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Interactive ggplot2 Plotting App"),
  sidebarLayout(
    sidebarPanel(
      id = "controls_panel",
      tabsetPanel(
        id = "plot_settings_tabs",
        tabPanel("Layer 1",
                 layer_controls_ui("1", mtcars)
        ),
        uiOutput("dynamic_layer_tabs"),
        tabPanel("Layer + / -",
                 h3("Manage Plot Layers"),
                   actionButton("add_layer_btn", "Add New Plot Layer"),
                   actionButton("remove_layer_btn", "Remove Last Layer")
        ),
        tabPanel("Faceting",
                 h3("Split Plot by Variables"),
                 selectInput("facet_row", "Facet by Row:", 
                            choices = c("None", names(mtcars)[sapply(mtcars, is.factor)]), selected = "None"),
                 selectInput("facet_col", "Facet by Column:", 
                            choices = c("None", names(mtcars)[sapply(mtcars, is.factor)]), selected = "None"),
                   selectInput("facet_scales", "Facet Scales:",
                              choices = c("fixed", "free_x", "free_y", "free"), selected = "fixed")
        ),
        tabPanel("Statistics",
                 h3("Add Statistical Layers"),
                   checkboxInput("add_stat_summary", "Add Statistical Summary", value = FALSE),
                   conditionalPanel(
                     condition = "input.add_stat_summary == true",
                     selectInput("stat_summary_fun", "Summary Function:",
                                choices = c("mean", "median"), selected = "mean"),
                     selectInput("stat_summary_geom", "Summary Geom:",
                                choices = c("point", "crossbar", "errorbar"), selected = "crossbar"),
                     numericInput("stat_summary_width", "Summary Bar Width:", value = 0.5, min = 0.1, step = 0.1)
                   ),
                   checkboxInput("add_stat_tests", "Add Statistical Tests (for Boxplot/Violin)", value = FALSE),
                   conditionalPanel(
                     condition = "input.add_stat_tests == true && (input.geom_type_1 == 'boxplot' || input.geom_type_1 == 'violin')",
                     selectInput("stat_test_type", "Test Type:",
                                choices = c("t-test", "Wald test"), selected = "t-test"),
                     numericInput("stat_test_alpha", "Significance Level (alpha):", value = 0.05, min = 0.01, max = 0.5, step = 0.01)
                 )
        ),
        tabPanel("Plot Theme",
                 h3("Overall Plot Theme"),
                   selectInput("plot_theme", "Theme:",
                            choices = c("theme_gray", "theme_bw", "theme_minimal", "theme_classic", "theme_dark", "theme_light"),
                              selected = "theme_gray"),
                 checkboxInput("show_legend", "Show Legend", value = TRUE),
                 
                 hr(),
                 h4("Advanced Theme Customization"),
                 checkboxInput("show_theme_params", "Show Advanced Theme Options", value = FALSE),
                 
                   conditionalPanel(
                     condition = "input.show_theme_params == true",
                   
                   # Text customization
                   h5("Text Settings"),
                   colourInput("text_color", "Text Color:", value = "#000000"),
                   numericInput("text_size", "Text Size:", value = 11, min = 6, max = 20, step = 0.5),
                   selectInput("text_family", "Font Family:", 
                              choices = c("sans", "serif", "mono"), selected = "sans"),
                   
                   # Axis customization
                   h5("Axis Settings"),
                   colourInput("axis_text_color", "Axis Text Color:", value = "#000000"),
                   numericInput("axis_text_size", "Axis Text Size:", value = 11, min = 6, max = 20, step = 0.5),
                   numericInput("axis_text_angle_x", "X-axis Text Angle:", value = 0, min = -90, max = 90, step = 15),
                   numericInput("axis_text_angle_y", "Y-axis Text Angle:", value = 0, min = -90, max = 90, step = 15),
                   colourInput("axis_line_color", "Axis Line Color:", value = "#000000"),
                   numericInput("axis_line_size", "Axis Line Size:", value = 0.5, min = 0.1, max = 3, step = 0.1),
                   
                   # Title customization
                   h5("Title Settings"),
                   colourInput("title_color", "Title Color:", value = "#000000"),
                   numericInput("title_size", "Title Size:", value = 14, min = 8, max = 24, step = 0.5),
                   selectInput("title_hjust", "Title Horizontal Position:", 
                              choices = c("left" = 0, "center" = 0.5, "right" = 1), selected = 0.5),
                   
                   # Panel customization
                   h5("Panel Settings"),
                   colourInput("panel_background_fill", "Panel Background:", value = "#FFFFFF"),
                   colourInput("panel_border_color", "Panel Border Color:", value = "#000000"),
                   numericInput("panel_border_size", "Panel Border Size:", value = 0.5, min = 0, max = 3, step = 0.1),
                   
                   # Grid customization
                   h5("Grid Settings"),
                   colourInput("grid_major_color", "Major Grid Color:", value = "#CCCCCC"),
                   colourInput("grid_minor_color", "Minor Grid Color:", value = "#E5E5E5"),
                   numericInput("grid_major_size", "Major Grid Size:", value = 0.5, min = 0, max = 3, step = 0.1),
                   numericInput("grid_minor_size", "Minor Grid Size:", value = 0.25, min = 0, max = 3, step = 0.1),
                   checkboxInput("show_major_grid", "Show Major Grid", value = TRUE),
                   checkboxInput("show_minor_grid", "Show Minor Grid", value = TRUE),
                   
                   # Legend customization
                   h5("Legend Settings"),
                   colourInput("legend_background_fill", "Legend Background:", value = "#FFFFFF"),
                   colourInput("legend_border_color", "Legend Border Color:", value = "#000000"),
                   numericInput("legend_border_size", "Legend Border Size:", value = 0.5, min = 0, max = 3, step = 0.1),
                   numericInput("legend_text_size", "Legend Text Size:", value = 11, min = 6, max = 20, step = 0.5),
                   
                   # Plot background
                   h5("Plot Background"),
                   colourInput("plot_background_fill", "Plot Background:", value = "#FFFFFF"),
                   colourInput("plot_border_color", "Plot Border Color:", value = "#000000"),
                   numericInput("plot_border_size", "Plot Border Size:", value = 0.5, min = 0, max = 3, step = 0.1)
                 )
        )
      )
    ),
    mainPanel(
      plotlyOutput("plot"),
      br(),
      textOutput("stat_test_output"),
      br(),
      textOutput("status")
    )
  )
)

# Define the Server logic
server <- function(input, output, session) {
  df <- mtcars

  # Reactive values for managing layers
  rv <- reactiveValues(num_layers = 1)
  
  # Status output
  output$status <- renderText({
    paste("Active layers:", rv$num_layers, "| Current geom:", input$geom_type_1, "| Theme:", input$plot_theme)
  })
  
  # Add layer functionality
  observeEvent(input$add_layer_btn, {
    new_layer_id <- rv$num_layers + 1
    rv$num_layers <- new_layer_id
    
    appendTab(inputId = "plot_settings_tabs",
              tabPanel(paste("Layer", new_layer_id),
                       layer_controls_ui(as.character(new_layer_id), df)
              ))
    
    updateTabsetPanel(session, "plot_settings_tabs", selected = paste("Layer", new_layer_id))
  })
  
  # Remove layer functionality
  observeEvent(input$remove_layer_btn, {
    if (rv$num_layers > 1) {
      removeTab(inputId = "plot_settings_tabs", target = paste("Layer", rv$num_layers))
      rv$num_layers <- rv$num_layers - 1
    }
  })
  
  # Generate geom layer function
  generate_geom_layer <- function(layer_idx) {
    id_suffix <- as.character(layer_idx)
    
    geom_type <- input[[paste0("geom_type_", id_suffix)]]
    x_var <- input[[paste0("x_var_", id_suffix)]]
    y_var <- input[[paste0("y_var_", id_suffix)]]
    color_var <- input[[paste0("color_var_", id_suffix)]]
    fill_var <- input[[paste0("fill_var_", id_suffix)]]
    size_var <- input[[paste0("size_var_", id_suffix)]]
    alpha_var <- input[[paste0("alpha_var_", id_suffix)]]
    
    # Define single-variable geom types that don't need Y variable
    single_var_geoms <- c("histogram", "density", "dotplot", "count", "bar")
    
    # For single-variable geoms, set y_var to NULL if it's not provided
    if (geom_type %in% single_var_geoms && (is.null(y_var) || y_var == "None")) {
      y_var <- NULL
    }
    
    # Build geom arguments
    geom_args <- list()
    
    # Add fixed values
    size_val <- input[[paste0("size_val_", id_suffix)]]
    alpha_val <- input[[paste0("alpha_val_", id_suffix)]]
    if (!is.null(size_val) && !is.na(size_val)) geom_args$size <- size_val
    if (!is.null(alpha_val) && !is.na(alpha_val)) geom_args$alpha <- alpha_val
    
    # Create geom layer with proper aesthetics for each type
    geom_layer <- switch(geom_type,
                        "point" = {
    aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (!is.null(size_var) && size_var != "None") aes_list$size <- sym(size_var)
                          if (!is.null(alpha_var) && alpha_var != "None") aes_list$alpha <- sym(alpha_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_point, geom_args)
                        },
                        "line" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (!is.null(size_var) && size_var != "None") aes_list$size <- sym(size_var)
                          if (!is.null(alpha_var) && alpha_var != "None") aes_list$alpha <- sym(alpha_var)
                          # Add group aesthetic for line plots to avoid warnings
                          if (is.null(aes_list$color) && is.null(aes_list$fill)) {
      aes_list$group <- 1
                          }
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_line, geom_args)
                        },
             "bar" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
               do.call(geom_bar, geom_args)
             },
                        "boxplot" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_boxplot, geom_args)
                        },
                        "violin" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_violin, geom_args)
             },
             "histogram" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
               do.call(geom_histogram, geom_args)
             },
                        "density" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_density, geom_args)
                        },
                        "area" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          # Add group aesthetic for area plots to avoid warnings
                          if (is.null(aes_list$color) && is.null(aes_list$fill)) {
                            aes_list$group <- 1
                          }
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_area, geom_args)
                        },
                        "smooth" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_smooth, geom_args)
                        },
                        "text" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_text, geom_args)
                        },
                        "label" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_label, geom_args)
                        },
                        "jitter" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (!is.null(size_var) && size_var != "None") aes_list$size <- sym(size_var)
                          if (!is.null(alpha_var) && alpha_var != "None") aes_list$alpha <- sym(alpha_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_jitter, geom_args)
                        },
                        "count" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_count, geom_args)
                        },
                        "dotplot" = {
                          aes_list <- list()
                          if (!is.null(x_var) && x_var != "None") aes_list$x <- sym(x_var)
                          if (!is.null(y_var) && y_var != "None") aes_list$y <- sym(y_var)
                          if (!is.null(color_var) && color_var != "None") aes_list$color <- sym(color_var)
                          if (!is.null(fill_var) && fill_var != "None") aes_list$fill <- sym(fill_var)
                          if (length(aes_list) > 0) geom_args$mapping <- aes(!!!aes_list)
                          do.call(geom_dotplot, geom_args)
                        })
    
    return(geom_layer)
  }
  
  # Statistical test results
  stat_test_results <- reactive({
    if (!isTRUE(input$add_stat_tests) || !input$geom_type_1 %in% c("boxplot", "violin")) {
      return(NULL)
    }
    
    x_var <- input$x_var_1
    y_var <- input$y_var_1
    test_type <- input$stat_test_type
    alpha <- input$stat_test_alpha
    
    if (is.null(x_var) || is.null(y_var) || x_var == "None" || y_var == "None") {
      return(NULL)
    }
    
    tryCatch({
      x_data <- df[[x_var]]
      y_data <- df[[y_var]]
      
      if (!is.factor(x_data) && !is.character(x_data)) {
        return(NULL)
      }
      
      if (!is.numeric(y_data)) {
        return(NULL)
      }
      
      # Convert to factor if character
      if (is.character(x_data)) {
        x_data <- factor(x_data)
      }
      
      if (nlevels(x_data) != 2) {
        return(NULL)
      }
      
      # Create data frame for testing
      test_df <- data.frame(x = x_data, y = y_data)
      group1 <- test_df$y[test_df$x == levels(test_df$x)[1]]
      group2 <- test_df$y[test_df$x == levels(test_df$x)[2]]
      
      if (length(group1) < 2 || length(group2) < 2) {
        return(NULL)
      }
      
      if (test_type == "t-test") {
        test_result <- t.test(group1, group2, conf.level = 1 - alpha)
        result <- list(
          test_name = "t-test",
          statistic = test_result$statistic,
          p_value = test_result$p.value,
          significant = test_result$p.value < alpha,
          groups = paste(levels(test_df$x)[1], "vs", levels(test_df$x)[2])
        )
      } else if (test_type == "Wald test") {
        mean1 <- mean(group1)
        mean2 <- mean(group2)
        var1 <- var(group1)
        var2 <- var(group2)
        n1 <- length(group1)
        n2 <- length(group2)
        se <- sqrt(var1/n1 + var2/n2)
        wald_stat <- (mean1 - mean2) / se
        p_value <- 2 * pnorm(-abs(wald_stat))
        result <- list(
          test_name = "Wald test",
          statistic = wald_stat,
          p_value = p_value,
          significant = p_value < alpha,
          groups = paste(levels(test_df$x)[1], "vs", levels(test_df$x)[2])
        )
      }
      return(result)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Render statistical test results
  output$stat_test_output <- renderText({
    result <- stat_test_results()
    if (is.null(result)) {
      return("")
    }
    
    sig_text <- if (result$significant) "significant" else "not significant"
    return(sprintf("%s results (%s): Statistic = %.3f, p-value = %.4f (%s at alpha = %.2f)",
            result$test_name, result$groups, result$statistic, result$p_value, sig_text, input$stat_test_alpha))
  })
  
  # Main plot rendering
  output$plot <- renderPlotly({
    req(input$geom_type_1, input$x_var_1)
    
    # Check if we need Y variable for the current geom type
    single_var_geoms <- c("histogram", "density", "dotplot", "count", "bar")
    needs_y_var <- !input$geom_type_1 %in% single_var_geoms
    
    # For single-variable geoms, we don't need to check Y variable
    if (needs_y_var) {
      req(input$y_var_1)
    }
    
    # Start with base plot
    p <- ggplot(data = df)
    
    # Add layers
    for (i in 1:rv$num_layers) {
      geom_type_id <- paste0("geom_type_", i)
      x_var_id <- paste0("x_var_", i)
      
      if (is.null(input[[geom_type_id]]) || is.null(input[[x_var_id]])) {
        next
      }
      
      layer <- generate_geom_layer(i)
      if (!is.null(layer)) {
        p <- p + layer
      }
      
      # Add labels if specified
      title <- input[[paste0("layer_title_", i)]]
      x_label <- input[[paste0("x_label_", i)]]
      y_label <- input[[paste0("y_label_", i)]]
      
      if (!is.null(title) && title != "") {
        p <- p + labs(title = title)
      }
      if (!is.null(x_label) && x_label != "") {
        p <- p + labs(x = x_label)
      }
      if (!is.null(y_label) && y_label != "") {
        p <- p + labs(y = y_label)
      }
      
      # Add transformations
      x_trans <- input[[paste0("x_trans_", i)]]
      y_trans <- input[[paste0("y_trans_", i)]]
      
      if (x_trans != "none") {
        p <- p + switch(x_trans,
                        "log2" = scale_x_continuous(trans = "log2"),
                        "log10" = scale_x_log10(),
                        "sqrt" = scale_x_continuous(trans = "sqrt"),
                        "reverse" = scale_x_reverse())
      }
      
      if (y_trans != "none") {
        p <- p + switch(y_trans,
                        "log2" = scale_y_continuous(trans = "log2"),
                        "log10" = scale_y_log10(),
                        "sqrt" = scale_y_continuous(trans = "sqrt"),
                        "reverse" = scale_y_reverse())
      }
    }
    
    # Add statistical summary if requested
    if (isTRUE(input$add_stat_summary) && !is.null(input$x_var_1) && !is.null(input$y_var_1) && input$y_var_1 != "None") {
      x_var <- input$x_var_1
      y_var <- input$y_var_1
      
      if (x_var %in% names(df) && y_var %in% names(df) && is.numeric(df[[y_var]])) {
        # Create mapping for stat_summary
        summary_mapping <- aes(x = !!sym(x_var), y = !!sym(y_var))
        
        if (input$stat_summary_fun == "mean") {
          p <- p + stat_summary(mapping = summary_mapping, fun = mean, geom = input$stat_summary_geom, 
                               width = input$stat_summary_width, colour = "red")
        } else if (input$stat_summary_fun == "median") {
          p <- p + stat_summary(mapping = summary_mapping, fun = median, geom = input$stat_summary_geom, 
                               width = input$stat_summary_width, colour = "blue")
        }
      }
    }
    
    # Add faceting
    if (!is.null(input$facet_row) && input$facet_row != "None" || 
        !is.null(input$facet_col) && input$facet_col != "None") {
      
      # Build facet formula properly
      row_var <- if (input$facet_row != "None") input$facet_row else "."
      col_var <- if (input$facet_col != "None") input$facet_col else "."
      
      # Create the formula string
      if (row_var == "." && col_var == ".") {
        # No faceting needed
        return(p)
      } else {
        facet_formula_str <- paste(row_var, "~", col_var)
        p <- p + facet_grid(as.formula(facet_formula_str), scales = input$facet_scales)
      }
    }
    
    # Add theme
    if (!is.null(input$plot_theme)) {
      tryCatch({
        p <- p + do.call(input$plot_theme, list())
      }, error = function(e) {
        warning("Theme application failed: ", e$message, " - using default theme")
        p <- p + theme_gray()
      })
    }
    
    # Add custom theme elements
    if (isTRUE(input$show_theme_params)) {
      tryCatch({
        custom_theme <- theme()
        
        # Only apply custom colors if user explicitly changes them from defaults
        # This preserves the base theme's color scheme
        
        # Text settings - only apply if user changes from default
        if (!is.null(input$text_size) && input$text_size != 11) {
          custom_theme <- custom_theme + 
            theme(text = element_text(size = input$text_size, family = input$text_family))
        }
        
        # Axis settings - only apply size and angle, preserve theme colors
        if (!is.null(input$axis_text_size) && input$axis_text_size != 11) {
          custom_theme <- custom_theme + 
            theme(axis.text = element_text(size = input$axis_text_size))
        }
        
        if (!is.null(input$axis_text_angle_x) && input$axis_text_angle_x != 0) {
          custom_theme <- custom_theme + 
            theme(axis.text.x = element_text(angle = input$axis_text_angle_x, hjust = 1))
        }
        
        if (!is.null(input$axis_text_angle_y) && input$axis_text_angle_y != 0) {
          custom_theme <- custom_theme + 
            theme(axis.text.y = element_text(angle = input$axis_text_angle_y))
        }
        
        if (!is.null(input$axis_line_size) && input$axis_line_size != 0.5) {
          custom_theme <- custom_theme + 
            theme(axis.line = element_line(linewidth = input$axis_line_size))
        }
        
        # Title settings - only apply size and position, preserve theme colors
        if (!is.null(input$title_size) && input$title_size != 14) {
          custom_theme <- custom_theme + 
            theme(plot.title = element_text(
              size = input$title_size,
              hjust = as.numeric(input$title_hjust)
            ))
        }
        
        # Grid settings - only apply if user explicitly changes
        if (isTRUE(input$show_major_grid) && !is.null(input$grid_major_size) && input$grid_major_size != 0.5) {
          custom_theme <- custom_theme + 
            theme(panel.grid.major = element_line(linewidth = input$grid_major_size))
        } else if (!isTRUE(input$show_major_grid)) {
          custom_theme <- custom_theme + theme(panel.grid.major = element_blank())
        }
        
        if (isTRUE(input$show_minor_grid) && !is.null(input$grid_minor_size) && input$grid_minor_size != 0.25) {
          custom_theme <- custom_theme + 
            theme(panel.grid.minor = element_line(linewidth = input$grid_minor_size))
        } else if (!isTRUE(input$show_minor_grid)) {
          custom_theme <- custom_theme + theme(panel.grid.minor = element_blank())
        }
        
        # Legend settings - only apply size, preserve theme colors
        if (!is.null(input$legend_text_size) && input$legend_text_size != 11) {
          custom_theme <- custom_theme + 
            theme(legend.text = element_text(size = input$legend_text_size))
        }
        
        # Only apply custom colors if user explicitly wants to override theme
        if (!is.null(input$text_color) && input$text_color != "#000000") {
          custom_theme <- custom_theme + 
            theme(text = element_text(colour = input$text_color))
        }
        
        if (!is.null(input$axis_text_color) && input$axis_text_color != "#000000") {
          custom_theme <- custom_theme + 
            theme(axis.text = element_text(colour = input$axis_text_color))
        }
        
        if (!is.null(input$title_color) && input$title_color != "#000000") {
          custom_theme <- custom_theme + 
            theme(plot.title = element_text(colour = input$title_color))
        }
        
        if (!is.null(input$axis_line_color) && input$axis_line_color != "#000000") {
          custom_theme <- custom_theme + 
            theme(axis.line = element_line(colour = input$axis_line_color))
        }
        
        if (!is.null(input$grid_major_color) && input$grid_major_color != "#CCCCCC") {
          custom_theme <- custom_theme + 
            theme(panel.grid.major = element_line(colour = input$grid_major_color))
        }
        
        if (!is.null(input$grid_minor_color) && input$grid_minor_color != "#E5E5E5") {
          custom_theme <- custom_theme + 
            theme(panel.grid.minor = element_line(colour = input$grid_minor_color))
        }
        
        # Panel and plot backgrounds - only apply if user explicitly changes
        if (!is.null(input$panel_background_fill) && input$panel_background_fill != "#FFFFFF") {
          custom_theme <- custom_theme + 
            theme(panel.background = element_rect(fill = input$panel_background_fill))
        }
        
        if (!is.null(input$panel_border_color) && input$panel_border_color != "#000000" && 
            !is.null(input$panel_border_size) && input$panel_border_size != 0.5) {
          custom_theme <- custom_theme + 
            theme(panel.border = element_rect(
              colour = input$panel_border_color,
              linewidth = input$panel_border_size
            ))
        }
        
        if (!is.null(input$plot_background_fill) && input$plot_background_fill != "#FFFFFF") {
          custom_theme <- custom_theme + 
            theme(plot.background = element_rect(fill = input$plot_background_fill))
        }
        
        if (!is.null(input$plot_border_color) && input$plot_border_color != "#000000" && 
            !is.null(input$plot_border_size) && input$plot_border_size != 0.5) {
          custom_theme <- custom_theme + 
            theme(plot.background = element_rect(
              fill = input$plot_background_fill,
              colour = input$plot_border_color,
              linewidth = input$plot_border_size
            ))
        }
        
        # Legend background - only apply if user explicitly changes
        if (!is.null(input$legend_background_fill) && input$legend_background_fill != "#FFFFFF") {
          custom_theme <- custom_theme + 
            theme(legend.background = element_rect(fill = input$legend_background_fill))
        }
        
        if (!is.null(input$legend_border_color) && input$legend_border_color != "#000000" && 
            !is.null(input$legend_border_size) && input$legend_border_size != 0.5) {
          custom_theme <- custom_theme + 
            theme(legend.box.background = element_rect(
              fill = input$legend_background_fill,
              colour = input$legend_border_color,
              linewidth = input$legend_border_size
            ))
        }
        
        p <- p + custom_theme
      }, error = function(e) {
        # If theme customization fails, continue without it
        warning("Theme customization failed: ", e$message)
      })
    }
    
    # Add legend control
    if (!isTRUE(input$show_legend)) {
      p <- p + theme(legend.position = "none")
    }
    
    # Convert to plotly
    tryCatch({
      plotly_obj <- ggplotly(p, tooltip = c("text", "x", "y")) %>%
        layout(
          hovermode = "closest",
          showlegend = input$show_legend
        )
      
      return(plotly_obj)
    }, error = function(e) {
      warning("Plotly conversion failed: ", e$message, " - returning ggplot object")
      # Return a simple plotly object as fallback
      return(plot_ly() %>% 
               add_annotations(
                 text = "Plot rendering failed. Please check your settings.",
                 xref = "paper", yref = "paper",
                 x = 0.5, y = 0.5, showarrow = FALSE
               ) %>%
               layout(
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
               ))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
