# This app allows users to create interactive ggplot2 plots with multiple layers and dynamic UI elements.
# It provides a user-friendly interface for selecting variables, aesthetics, and plot types.
# The app supports various geom types, including points, lines, bars, histograms, and more.
# Users can add, remove, and configure multiple layers, and customize plot settings such as colors, sizes, and labels.
# The app includes faceting options, statistical summary layers, t-test/Wald test for boxplot/violin plots, and dynamic theme parameter customization with color pickers.
# The final plot is displayed using ggplot2 and can be exported as an interactive plotly object.

# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly) # For interactive plots
library(dplyr) # For data manipulation, especially with faceting
library(rlang) # For tidy evaluation with !!! operator
library(shinyjs) # For JavaScript functionalities like toggling elements
library(stats) # For t.test and pnorm
library(colourpicker) # For color picker input

# Use the built-in mtcars dataset
data(mtcars)

# Convert relevant columns to factors for categorical plotting
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

# Define a helper function to generate UI for a single layer
layer_controls_ui <- function(id_suffix, df, numeric_cols_choices, categorical_cols_choices) {
  div(
    h3(paste("Layer", id_suffix)),
    
    # Add collapsible aesthetics section
    checkboxInput(paste0("show_aes_", id_suffix), "Aesthetics", value = TRUE),
    conditionalPanel(
      condition = paste0("input.show_aes_", id_suffix, " == true"),
      selectInput(paste0("geom_type_", id_suffix), "Geometric:",
                  choices = c(
                    "abline", "area", "bar", "bin_2d", "blank", "boxplot", "col", "contour", "contour_filled",
                    "count", "crossbar", "curve", "density", "density_2d", "density_2d_filled", "dotplot",
                    "errorbar", "errorbarh", "freqpoly", "function", "hex", "histogram", "hline", "jitter",
                    "label", "line", "linerange", "map", "path", "point", "pointrange", "polygon", "qq",
                    "qq_line", "quantile", "rect", "ribbon", "raster", "rug", "segment", "sf", "sf_label",
                    "sf_text", "smooth", "spoke", "text", "tile", "violin", "vline"
                  ),
                  selected = "point"),
      
      uiOutput(paste0("x_var_ui_", id_suffix)),
      
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " == 'point' || input.geom_type_", id_suffix, " == 'line' || input.geom_type_", id_suffix, " == 'bar' || input.geom_type_", id_suffix, " == 'col' || input.geom_type_", id_suffix, " == 'area' || input.geom_type_", id_suffix, " == 'tile' || input.geom_type_", id_suffix, " == 'smooth' || input.geom_type_", id_suffix, " == 'bin_2d' || input.geom_type_", id_suffix, " == 'contour' || input.geom_type_", id_suffix, " == 'contour_filled' || input.geom_type_", id_suffix, " == 'count' || input.geom_type_", id_suffix, " == 'density_2d' || input.geom_type_", id_suffix, " == 'density_2d_filled' || input.geom_type_", id_suffix, " == 'dotplot' || input.geom_type_", id_suffix, " == 'errorbar' || input.geom_type_", id_suffix, " == 'crossbar' || input.geom_type_", id_suffix, " == 'linerange' || input.geom_type_", id_suffix, " == 'pointrange' || input.geom_type_", id_suffix, " == 'polygon' || input.geom_type_", id_suffix, " == 'quantile' || input.geom_type_", id_suffix, " == 'ribbon' || input.geom_type_", id_suffix, " == 'rug' || input.geom_type_", id_suffix, " == 'segment' || input.geom_type_", id_suffix, " == 'curve' || input.geom_type_", id_suffix, " == 'spoke' || input.geom_type_", id_suffix, " == 'label' || input.geom_type_", id_suffix, " == 'text' || input.geom_type_", id_suffix, " == 'raster' || input.geom_type_", id_suffix, " == 'rect' || input.geom_type_", id_suffix, " == 'sf' || input.geom_type_", id_suffix, " == 'sf_label' || input.geom_type_", id_suffix, " == 'sf_text' || input.geom_type_", id_suffix, " == 'boxplot' || input.geom_type_", id_suffix, " == 'violin'"),
        uiOutput(paste0("y_var_ui_", id_suffix))
      ),
      
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " == 'boxplot' || input.geom_type_", id_suffix, " == 'violin'"),
        helpText("For boxplots or violin plots, select a categorical variable for X (e.g., cyl) and a numeric variable for Y (e.g., mpg).")
      ),
      
      uiOutput(paste0("color_var_ui_", id_suffix)),
      uiOutput(paste0("fill_var_ui_", id_suffix)),
      uiOutput(paste0("group_var_ui_", id_suffix))
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
      textInput(paste0("layer_subtitle_", id_suffix), "Subtitle:", value = ""),
      textInput(paste0("layer_caption_", id_suffix), "Caption:", value = ""),
      uiOutput(paste0("x_label_ui_", id_suffix)),
      uiOutput(paste0("y_label_ui_", id_suffix))
    ),
    hr(),
    
    conditionalPanel(
      condition = paste0("input.geom_type_", id_suffix, " == 'abline'"),
      numericInput(paste0("abline_intercept_", id_suffix), "Intercept:", value = 0),
      numericInput(paste0("abline_slope_", id_suffix), "Slope:", value = 1)
    ),
    conditionalPanel(
      condition = paste0("input.geom_type_", id_suffix, " == 'hline'"),
      numericInput(paste0("hline_yintercept_", id_suffix), "Y-intercept:", value = 0)
    ),
    conditionalPanel(
      condition = paste0("input.geom_type_", id_suffix, " == 'vline'"),
      numericInput(paste0("vline_xintercept_", id_suffix), "X-intercept:", value = 0)
    ),
    conditionalPanel(
      condition = paste0("input.geom_type_", id_suffix, " == 'function'"),
      textInput(paste0("function_fun_", id_suffix), "Function (e.g., 'sin(x)', 'x^2', 'dnorm(x)', 'abs(x)'):", value = "dnorm(x)")
    ),
    
    checkboxInput(paste0("show_advanced_aesthetics_", id_suffix), "Advanced Aesthetics", value = FALSE),
    
    conditionalPanel(
      condition = paste0("input.show_advanced_aesthetics_", id_suffix, " == true"),
      uiOutput(paste0("size_var_ui_", id_suffix)),
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " %in% c('point', 'line', 'smooth', 'boxplot', 'violin', 'abline', 'hline', 'vline', 'count', 'dotplot', 'errorbar', 'errorbarh', 'crossbar', 'linerange', 'pointrange', 'path', 'segment', 'curve', 'spoke', 'label', 'text', 'sf_label', 'sf_text')"),
        numericInput(paste0("size_val_", id_suffix), "Fixed Size:", value = NA, min = 0.1, step = 0.1)
      ),
      
      uiOutput(paste0("alpha_var_ui_", id_suffix)),
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " %in% c('point', 'bar', 'col', 'histogram', 'density', 'violin', 'area', 'tile', 'smooth', 'bin_2d', 'boxplot', 'contour', 'contour_filled', 'count', 'density_2d', 'density_2d_filled', 'dotplot', 'hex', 'jitter', 'crossbar', 'linerange', 'pointrange', 'map', 'path', 'polygon', 'quantile', 'ribbon', 'rug', 'segment', 'curve', 'spoke', 'label', 'text', 'raster', 'rect', 'sf', 'sf_label', 'sf_text')"),
        numericInput(paste0("alpha_val_", id_suffix), "Fixed Alpha (0-1):", value = NA, min = 0, max = 1, step = 0.1)
      ),
      
      uiOutput(paste0("shape_var_ui_", id_suffix)),
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " %in% c('point', 'count', 'jitter', 'sf_text')"),
        numericInput(paste0("shape_val_", id_suffix), "Fixed Shape (0-25):", value = NA, min = 0, max = 25, step = 1)
      ),
      
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " %in% c('line', 'smooth', 'abline', 'hline', 'vline', 'freqpoly', 'function', 'path', 'segment', 'curve', 'spoke', 'ribbon')"),
        uiOutput(paste0("linetype_var_ui_", id_suffix)),
        selectInput(paste0("linetype_val_", id_suffix), "Fixed Linetype:",
                    choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                    selected = "solid")
      ),
      
      uiOutput(paste0("label_var_ui_", id_suffix))
    )
  )
}

# Define the User Interface (UI)
ui <- fluidPage(
  useShinyjs(),
  titlePanel("mtcars Interactive Plotting with ggplot2 and Plotly"),
  sidebarLayout(
    sidebarPanel(
      id = "controls_panel",
      tabsetPanel(
        id = "plot_settings_tabs",
        tabPanel("Layer 1",
                 layer_controls_ui("1", mtcars, names(mtcars)[sapply(mtcars, is.numeric)], names(mtcars)[sapply(mtcars, is.factor) | sapply(mtcars, is.character)])
        ),
        uiOutput("dynamic_layer_tabs"),
        tabPanel("Layer + / -",
                 h3("Manage Plot Layers"),
                 actionButton("add_layer_btn", "Add New Plot Layer"),
                 actionButton("remove_layer_btn", "Remove Last Layer")
        ),
        tabPanel("Faceting",
                 h3("Split Plot by Variables"),
                 uiOutput("facet_row_ui"),
                 uiOutput("facet_col_ui"),
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
                             choices = c("theme_gray", "theme_bw", "theme_minimal", "theme_classic", "theme_dark", "theme_light", "theme_linedraw", "theme_void"),
                             selected = "theme_gray"),
                 checkboxInput("show_theme_params", "Customize Theme Parameters", value = FALSE),
                 conditionalPanel(
                   condition = "input.show_theme_params == true",
                   uiOutput("theme_params_ui")
                 )
        )
      )
    ),
    mainPanel(
      actionButton("reset_to_default", "Reset to Default"),
      br(),
      textOutput("warnings"),
      plotlyOutput("plot"),
      br(),
      textOutput("stat_test_output")
    )
  )
)

# Define the Server logic
server <- function(input, output, session) {
  df <- mtcars
  rv <- reactiveValues(num_layers = 1, warnings = character(0))
  
  # Define default values
  default_values <- list(
    geom_type = "point",
    x_var = "wt",
    y_var = "mpg",
    color_var = "None",
    fill_var = "None",
    layer_title = "",
    layer_subtitle = "",
    layer_caption = "",
    x_label = "",
    y_label = "",
    size_var = "None",
    size_val = NA,
    alpha_var = "None",
    alpha_val = NA,
    shape_var = "None",
    shape_val = NA,
    linetype_var = "None",
    linetype_val = "solid",
    label_var = "None",
    show_advanced_aesthetics = FALSE,
    show_axis_scale = FALSE,
    show_labels_title = FALSE,
    x_trans = "none",
    y_trans = "none",
    xmin = NA,
    xmax = NA,
    ymin = NA,
    ymax = NA,
    facet_row = "None",
    facet_col = "None",
    facet_scales = "fixed",
    add_stat_summary = FALSE,
    stat_summary_fun = "mean",
    stat_summary_geom = "crossbar",
    stat_summary_width = 0.5,
    add_stat_tests = FALSE,
    stat_test_type = "t-test",
    stat_test_alpha = 0.05,
    plot_theme = "theme_gray",
    show_theme_params = FALSE,
    group_var = "None",
    show_aes = TRUE
  )
  
  # Define theme parameters for customization
  theme_parameters <- list(
    text = list(
      text_size = list(type = "numeric", default = 12, label = "Text Size"),
      text_color = list(type = "color", default = "#000000", label = "Text Color")
    ),
    panel = list(
      panel_background_fill = list(type = "color", default = "#FFFFFF", label = "Panel Background Fill"),
      panel_grid_major_color = list(type = "color", default = "#D3D3D3", label = "Major Grid Line Color"),
      panel_grid_major_size = list(type = "numeric", default = 0.5, label = "Major Grid Line Size"),
      panel_grid_minor_color = list(type = "color", default = "#E8E8E8", label = "Minor Grid Line Color"),
      panel_grid_minor_size = list(type = "numeric", default = 0.25, label = "Minor Grid Line Size")
    ),
    plot = list(
      plot_background_fill = list(type = "color", default = "#FFFFFF", label = "Plot Background Fill"),
      plot_margin_top = list(type = "numeric", default = 5, label = "Plot Margin Top (pt)"),
      plot_margin_right = list(type = "numeric", default = 5, label = "Plot Margin Right (pt)"),
      plot_margin_bottom = list(type = "numeric", default = 5, label = "Plot Margin Bottom (pt)"),
      plot_margin_left = list(type = "numeric", default = 5, label = "Plot Margin Left (pt)")
    ),
    legend = list(
      legend_position = list(type = "select", default = "right", label = "Legend Position", choices = c("right", "left", "top", "bottom", "none")),
      legend_background_fill = list(type = "color", default = "#FFFFFF", label = "Legend Background Fill"),
      legend_text_size = list(type = "numeric", default = 10, label = "Legend Text Size")
    )
  )
  
  # Render theme parameters UI
  output$theme_params_ui <- renderUI({
    req(input$plot_theme, input$show_theme_params)
    
    param_ui <- list()
    
    # Text Parameters
    param_ui <- append(param_ui, list(h4("Text Parameters")))
    for (param in names(theme_parameters$text)) {
      param_info <- theme_parameters$text[[param]]
      if (param_info$type == "numeric") {
        param_ui <- append(param_ui, list(
          numericInput(paste0("theme_", param), param_info$label, value = param_info$default, min = 0, step = 0.1)
        ))
      } else if (param_info$type == "color") {
        param_ui <- append(param_ui, list(
          colourInput(paste0("theme_", param), param_info$label, value = param_info$default, allowTransparent = TRUE)
        ))
      }
    }
    
    # Panel Parameters
    param_ui <- append(param_ui, list(h4("Panel Parameters")))
    for (param in names(theme_parameters$panel)) {
      param_info <- theme_parameters$panel[[param]]
      if (param_info$type == "numeric") {
        param_ui <- append(param_ui, list(
          numericInput(paste0("theme_", param), param_info$label, value = param_info$default, min = 0, step = 0.1)
        ))
      } else if (param_info$type == "color") {
        param_ui <- append(param_ui, list(
          colourInput(paste0("theme_", param), param_info$label, value = param_info$default, allowTransparent = TRUE)
        ))
      }
    }
    
    # Plot Parameters
    param_ui <- append(param_ui, list(h4("Plot Parameters")))
    for (param in names(theme_parameters$plot)) {
      param_info <- theme_parameters$plot[[param]]
      if (param_info$type == "numeric") {
        param_ui <- append(param_ui, list(
          numericInput(paste0("theme_", param), param_info$label, value = param_info$default, min = 0, step = 0.1)
        ))
      } else if (param_info$type == "color") {
        param_ui <- append(param_ui, list(
          colourInput(paste0("theme_", param), param_info$label, value = param_info$default, allowTransparent = TRUE)
        ))
      }
    }
    
    # Legend Parameters
    param_ui <- append(param_ui, list(h4("Legend Parameters")))
    for (param in names(theme_parameters$legend)) {
      param_info <- theme_parameters$legend[[param]]
      if (param_info$type == "select") {
        param_ui <- append(param_ui, list(
          selectInput(paste0("theme_", param), param_info$label, choices = param_info$choices, selected = param_info$default)
        ))
      } else if (param_info$type == "numeric") {
        param_ui <- append(param_ui, list(
          numericInput(paste0("theme_", param), param_info$label, value = param_info$default, min = 0, step = 0.1)
        ))
      } else if (param_info$type == "color") {
        param_ui <- append(param_ui, list(
          colourInput(paste0("theme_", param), param_info$label, value = param_info$default, allowTransparent = TRUE)
        ))
      }
    }
    
    do.call(tagList, param_ui)
  })
  
  # Reactive expression to collect theme settings
  theme_settings <- reactive({
    if (!isTRUE(input$show_theme_params)) {
      return(NULL)
    }
    
    theme_args <- list()
    
    # Text Parameters
    if (!is.null(input$theme_text_size) && !is.na(input$theme_text_size)) {
      theme_args$text <- element_text(size = input$theme_text_size, colour = input$theme_text_color)
    }
    
    # Panel Parameters
    if (!is.null(input$theme_panel_background_fill)) {
      theme_args$panel.background <- element_rect(fill = input$theme_panel_background_fill)
    }
    if (!is.null(input$theme_panel_grid_major_color) && !is.null(input$theme_panel_grid_major_size)) {
      theme_args$panel.grid.major <- element_line(colour = input$theme_panel_grid_major_color, size = input$theme_panel_grid_major_size)
    }
    if (!is.null(input$theme_panel_grid_minor_color) && !is.null(input$theme_panel_grid_minor_size)) {
      theme_args$panel.grid.minor <- element_line(colour = input$theme_panel_grid_minor_color, size = input$theme_panel_grid_minor_size)
    }
    
    # Plot Parameters
    if (!is.null(input$theme_plot_background_fill)) {
      theme_args$plot.background <- element_rect(fill = input$theme_plot_background_fill)
    }
    if (!is.null(input$theme_plot_margin_top) && !is.na(input$theme_plot_margin_top) &&
        !is.null(input$theme_plot_margin_right) && !is.na(input$theme_plot_margin_right) &&
        !is.null(input$theme_plot_margin_bottom) && !is.na(input$theme_plot_margin_bottom) &&
        !is.null(input$theme_plot_margin_left) && !is.na(input$theme_plot_margin_left)) {
      theme_args$plot.margin <- margin(
        t = input$theme_plot_margin_top,
        r = input$theme_plot_margin_right,
        b = input$theme_plot_margin_bottom,
        l = input$theme_plot_margin_left,
        unit = "pt"
      )
    }
    
    # Legend Parameters
    if (!is.null(input$theme_legend_position)) {
      theme_args$legend.position <- input$theme_legend_position
    }
    if (!is.null(input$theme_legend_background_fill)) {
      theme_args$legend.background <- element_rect(fill = input$theme_legend_background_fill)
    }
    if (!is.null(input$theme_legend_text_size) && !is.na(input$theme_legend_text_size)) {
      theme_args$legend.text <- element_text(size = input$theme_legend_text_size)
    }
    
    if (length(theme_args) > 0) {
      return(theme_args)
    } else {
      return(NULL)
    }
  })
  
  # Observe reset button click
  observeEvent(input$reset_to_default, {
    updateSelectInput(session, "geom_type_1", selected = default_values$geom_type)
    updateSelectInput(session, "x_var_1", selected = default_values$x_var)
    updateSelectInput(session, "y_var_1", selected = default_values$y_var)
    updateSelectInput(session, "color_var_1", selected = default_values$color_var)
    updateSelectInput(session, "fill_var_1", selected = default_values$fill_var)
    updateTextInput(session, "layer_title_1", value = default_values$layer_title)
    updateTextInput(session, "layer_subtitle_1", value = default_values$layer_subtitle)
    updateTextInput(session, "layer_caption_1", value = default_values$layer_caption)
    updateTextInput(session, "x_label_1", value = default_values$x_label)
    updateTextInput(session, "y_label_1", value = default_values$y_label)
    updateSelectInput(session, "size_var_1", selected = default_values$size_var)
    updateNumericInput(session, "size_val_1", value = default_values$size_val)
    updateSelectInput(session, "alpha_var_1", selected = default_values$alpha_var)
    updateNumericInput(session, "alpha_val_1", value = default_values$alpha_val)
    updateSelectInput(session, "shape_var_1", selected = default_values$shape_var)
    updateNumericInput(session, "shape_val_1", value = default_values$shape_val)
    updateSelectInput(session, "linetype_var_1", selected = default_values$linetype_var)
    updateSelectInput(session, "linetype_val_1", selected = default_values$linetype_val)
    updateSelectInput(session, "label_var_1", selected = default_values$label_var)
    updateCheckboxInput(session, "show_advanced_aesthetics_1", value = default_values$show_advanced_aesthetics)
    updateCheckboxInput(session, "show_axis_scale_1", value = default_values$show_axis_scale)
    updateCheckboxInput(session, "show_labels_title_1", value = default_values$show_labels_title)
    updateSelectInput(session, "x_trans_1", selected = default_values$x_trans)
    updateSelectInput(session, "y_trans_1", selected = default_values$y_trans)
    updateNumericInput(session, "xmin_1", value = default_values$xmin)
    updateNumericInput(session, "xmax_1", value = default_values$xmax)
    updateNumericInput(session, "ymin_1", value = default_values$ymin)
    updateNumericInput(session, "ymax_1", value = default_values$ymax)
    updateSelectInput(session, "facet_row", selected = default_values$facet_row)
    updateSelectInput(session, "facet_col", selected = default_values$facet_col)
    updateSelectInput(session, "facet_scales", selected = default_values$facet_scales)
    updateCheckboxInput(session, "add_stat_summary", value = default_values$add_stat_summary)
    updateSelectInput(session, "stat_summary_fun", selected = default_values$stat_summary_fun)
    updateSelectInput(session, "stat_summary_geom", selected = default_values$stat_summary_geom)
    updateNumericInput(session, "stat_summary_width", value = default_values$stat_summary_width)
    updateCheckboxInput(session, "add_stat_tests", value = default_values$add_stat_tests)
    updateSelectInput(session, "stat_test_type", selected = default_values$stat_test_type)
    updateNumericInput(session, "stat_test_alpha", value = default_values$stat_test_alpha)
    updateSelectInput(session, "plot_theme", selected = default_values$plot_theme)
    updateCheckboxInput(session, "show_theme_params", value = default_values$show_theme_params)
    updateSelectInput(session, "group_var_1", selected = default_values$group_var)
    updateCheckboxInput(session, "show_aes_1", value = default_values$show_aes)
    
    # Reset theme parameters
    for (category in names(theme_parameters)) {
      for (param in names(theme_parameters[[category]])) {
        param_info <- theme_parameters[[category]][[param]]
        if (param_info$type == "numeric") {
          updateNumericInput(session, paste0("theme_", param), value = param_info$default)
        } else if (param_info$type == "color") {
          updateColourInput(session, paste0("theme_", param), value = param_info$default)
        } else if (param_info$type == "select") {
          updateSelectInput(session, paste0("theme_", param), selected = param_info$default)
        }
      }
    }
    
    while (rv$num_layers > 1) {
      removeTab(inputId = "plot_settings_tabs", target = paste("Layer", rv$num_layers, "Settings"))
      rv$num_layers <- rv$num_layers - 1
    }
    
    updateTabsetPanel(session, "plot_settings_tabs", selected = "Layer 1")
    rv$warnings <- character(0) # Clear warnings on reset
  })
  
  # Reactive expressions for column names
  numeric_cols <- reactive({
    names(df)[sapply(df, is.numeric)]
  })
  
  categorical_cols <- reactive({
    names(df)[sapply(df, is.factor) | sapply(df, is.character)]
  })
  
  # Helper to get aesthetic parameter
  get_aesthetic_value_simplified <- function(input_obj, id_suffix, var_input_id, fixed_val_input_id = NULL) {
    var_val <- input_obj[[paste0(var_input_id, "_", id_suffix)]]
    if (!is.null(var_val) && var_val != "" && var_val != "None") {
      return(list(aesthetic = sym(var_val), fixed = NULL))
    } else if (!is.null(fixed_val_input_id)) {
      fixed_val <- input_obj[[paste0(fixed_val_input_id, "_", id_suffix)]]
      if (!is.null(fixed_val) && !is.na(fixed_val)) {
        return(list(aesthetic = NULL, fixed = fixed_val))
      }
    }
    return(list(aesthetic = NULL, fixed = NULL))
  }
  
  # Observe button click to add a new layer tab
  observeEvent(input$add_layer_btn, {
    new_layer_id <- rv$num_layers + 1
    rv$num_layers <- new_layer_id
    
    layer1_geom <- input$geom_type_1
    layer1_x_var <- input$x_var_1
    layer1_y_var <- input$y_var_1
    layer1_color <- input$color_var_1
    layer1_fill <- input$fill_var_1
    layer1_title <- input$layer_title_1
    layer1_subtitle <- input$layer_subtitle_1
    layer1_caption <- input$layer_caption_1
    layer1_x_label <- input$x_label_1
    layer1_y_label <- input$y_label_1
    layer1_show_axis_scale <- input$show_axis_scale_1
    layer1_show_labels_title <- input$show_labels_title_1
    layer1_group <- input$group_var_1
    layer1_show_aes <- input$show_aes_1
    
    appendTab(inputId = "plot_settings_tabs",
              tabPanel(paste("Layer", new_layer_id, "Settings"),
                       layer_controls_ui(as.character(new_layer_id), df, numeric_cols(), categorical_cols())
              ))
    
    updateSelectInput(session, paste0("geom_type_", new_layer_id), selected = layer1_geom)
    updateSelectInput(session, paste0("x_var_", new_layer_id), selected = layer1_x_var)
    if (!is.null(layer1_y_var) && layer1_y_var != "") {
      updateSelectInput(session, paste0("y_var_", new_layer_id), selected = layer1_y_var)
    }
    updateSelectInput(session, paste0("color_var_", new_layer_id), selected = layer1_color)
    updateSelectInput(session, paste0("fill_var_", new_layer_id), selected = layer1_fill)
    updateTextInput(session, paste0("layer_title_", new_layer_id), value = layer1_title)
    updateTextInput(session, paste0("layer_subtitle_", new_layer_id), value = layer1_subtitle)
    updateTextInput(session, paste0("layer_caption_", new_layer_id), value = layer1_caption)
    updateTextInput(session, paste0("x_label_", new_layer_id), value = layer1_x_label)
    updateTextInput(session, paste0("y_label_", new_layer_id), value = layer1_y_label)
    updateCheckboxInput(session, paste0("show_axis_scale_", new_layer_id), value = layer1_show_axis_scale)
    updateCheckboxInput(session, paste0("show_labels_title_", new_layer_id), value = layer1_show_labels_title)
    updateSelectInput(session, paste0("group_var_", new_layer_id), selected = layer1_group)
    updateCheckboxInput(session, paste0("show_aes_", new_layer_id), value = layer1_show_aes)
    
    updateTabsetPanel(session, "plot_settings_tabs", selected = paste("Layer", new_layer_id, "Settings"))
  })
  
  # Observe button click to remove the last layer tab
  observeEvent(input$remove_layer_btn, {
    if (rv$num_layers > 1) {
      removeTab(inputId = "plot_settings_tabs", target = paste("Layer", rv$num_layers, "Settings"))
      rv$num_layers <- rv$num_layers - 1
    }
  })
  
  # Dynamic UI elements for variable selection
  observe({
    lapply(1:rv$num_layers, function(i) {
      id_suffix <- as.character(i)
      
      output[[paste0("x_var_ui_", id_suffix)]] <- renderUI({
        req(input[[paste0("geom_type_", id_suffix)]])
        geom <- input[[paste0("geom_type_", id_suffix)]]
        num_cols <- numeric_cols()
        cat_cols <- categorical_cols()
        
        choices_x <- if (geom == "boxplot" || geom == "violin") {
          c(cat_cols, num_cols)
        } else {
          c(num_cols, cat_cols)
        }
        
        selected_x <- if (length(choices_x) > 0 && !is.null(input[[paste0("x_var_", id_suffix)]]) && input[[paste0("x_var_", id_suffix)]] %in% choices_x) {
          input[[paste0("x_var_", id_suffix)]]
        } else {
          choices_x[1]
        }
        selectInput(paste0("x_var_", id_suffix), "X variable:", choices = choices_x, selected = selected_x)
      })
      
      output[[paste0("y_var_ui_", id_suffix)]] <- renderUI({
        req(input[[paste0("geom_type_", id_suffix)]])
        geom <- input[[paste0("geom_type_", id_suffix)]]
        num_cols <- numeric_cols()
        
        choices_y <- if (geom %in% c("point", "line", "bar", "col", "area", "tile", "smooth", "bin_2d", "contour", "contour_filled", "count", "density_2d", "density_2d_filled", "dotplot", "errorbar", "errorbarh", "hex", "jitter", "crossbar", "linerange", "pointrange", "map", "path", "polygon", "quantile", "ribbon", "segment", "curve", "spoke", "label", "text", "raster", "rect", "sf", "sf_label", "sf_text", "boxplot", "violin")) {
          num_cols
        } else {
          NULL
        }
        
        selected_y <- if (!is.null(input[[paste0("y_var_", id_suffix)]]) && input[[paste0("y_var_", id_suffix)]] %in% choices_y) {
          input[[paste0("y_var_", id_suffix)]]
        } else {
          if (geom %in% c("boxplot", "violin") && length(choices_y) > 0) choices_y[1] else NULL
        }
        selectInput(paste0("y_var_", id_suffix), "Y variable:", choices = choices_y, selected = selected_y)
      })
      
      output[[paste0("color_var_ui_", id_suffix)]] <- renderUI({
        all_cols <- c(numeric_cols(), categorical_cols())
        choices <- c("None", if(length(all_cols) > 0) all_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("color_var_", id_suffix)]]) && input[[paste0("color_var_", id_suffix)]] %in% choices) input[[paste0("color_var_", id_suffix)]] else "None"
        selectInput(paste0("color_var_", id_suffix), "Variable for color:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("fill_var_ui_", id_suffix)]] <- renderUI({
        all_cols <- c(numeric_cols(), categorical_cols())
        choices <- c("None", if(length(all_cols) > 0) all_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("fill_var_", id_suffix)]]) && input[[paste0("fill_var_", id_suffix)]] %in% choices) input[[paste0("fill_var_", id_suffix)]] else "None"
        selectInput(paste0("fill_var_", id_suffix), "Variable for fill:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("size_var_ui_", id_suffix)]] <- renderUI({
        num_cols <- numeric_cols()
        choices <- c("None", if(length(num_cols) > 0) num_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("size_var_", id_suffix)]]) && input[[paste0("size_var_", id_suffix)]] %in% choices) input[[paste0("size_var_", id_suffix)]] else "None"
        selectInput(paste0("size_var_", id_suffix), "Variable for size:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("alpha_var_ui_", id_suffix)]] <- renderUI({
        num_cols <- numeric_cols()
        choices <- c("None", if(length(num_cols) > 0) num_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("alpha_var_", id_suffix)]]) && input[[paste0("alpha_var_", id_suffix)]] %in% choices) input[[paste0("alpha_var_", id_suffix)]] else "None"
        selectInput(paste0("alpha_var_", id_suffix), "Variable for alpha:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("shape_var_ui_", id_suffix)]] <- renderUI({
        cat_cols <- categorical_cols()
        choices <- c("None", if(length(cat_cols) > 0) cat_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("shape_var_", id_suffix)]]) && input[[paste0("shape_var_", id_suffix)]] %in% choices) input[[paste0("shape_var_", id_suffix)]] else "None"
        selectInput(paste0("shape_var_", id_suffix), "Variable for shape:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("linetype_var_ui_", id_suffix)]] <- renderUI({
        cat_cols <- categorical_cols()
        choices <- c("None", if(length(cat_cols) > 0) cat_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("linetype_var_", id_suffix)]]) && input[[paste0("linetype_var_", id_suffix)]] %in% choices) input[[paste0("linetype_var_", id_suffix)]] else "None"
        selectInput(paste0("linetype_var_", id_suffix), "Variable for linetype:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("label_var_ui_", id_suffix)]] <- renderUI({
        all_cols <- c(numeric_cols(), names(df)[sapply(df, is.character)])
        choices <- c("None", if(length(all_cols) > 0) all_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("label_var_", id_suffix)]]) && input[[paste0("label_var_", id_suffix)]] %in% choices) input[[paste0("label_var_", id_suffix)]] else "None"
        selectInput(paste0("label_var_", id_suffix), "Variable for label:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("group_var_ui_", id_suffix)]] <- renderUI({
        all_cols <- c(numeric_cols(), categorical_cols())
        choices <- c("None", if(length(all_cols) > 0) all_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("group_var_", id_suffix)]]) && input[[paste0("group_var_", id_suffix)]] %in% choices) input[[paste0("group_var_", id_suffix)]] else "None"
        selectInput(paste0("group_var_", id_suffix), "Variable for group:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("x_label_ui_", id_suffix)]] <- renderUI({
        current_x_var <- input[[paste0("x_var_", id_suffix)]]
        label_value <- if (!is.null(current_x_var) && current_x_var != "") current_x_var else ""
        textInput(paste0("x_label_", id_suffix), "X-axis Label:", value = label_value)
      })
      
      output[[paste0("y_label_ui_", id_suffix)]] <- renderUI({
        req(input[[paste0("geom_type_", id_suffix)]])
        geom <- input[[paste0("geom_type_", id_suffix)]]
        current_y_var <- input[[paste0("y_var_", id_suffix)]]
        default_y_label <- if (geom %in% c("histogram", "density", "freqpoly", "violin") || is.null(current_y_var) || current_y_var == "") "Count" else current_y_var
        textInput(paste0("y_label_", id_suffix), "Y-axis Label:", value = default_y_label)
      })
    })
  })
  
  # Dynamic UI elements for Faceting
  output$facet_row_ui <- renderUI({
    choices <- c("None", categorical_cols())
    selectInput("facet_row", "Facet by Row (optional):", choices = choices, selected = "None")
  })
  
  output$facet_col_ui <- renderUI({
    choices <- c("None", categorical_cols())
    selectInput("facet_col", "Facet by Column (optional):", choices = choices, selected = "None")
  })
  
  # Generate a single geom layer
  generate_geom_layer <- function(layer_idx, input_obj, df_data) {
    id_suffix <- as.character(layer_idx)
    
    req(input_obj[[paste0("geom_type_", id_suffix)]])
    geom_type <- input_obj[[paste0("geom_type_", id_suffix)]]
    
    x_var_input <- input_obj[[paste0("x_var_", id_suffix)]]
    x_var <- if (!is.null(x_var_input) && x_var_input != "") sym(x_var_input) else NULL
    
    y_var_input <- input_obj[[paste0("y_var_", id_suffix)]]
    y_var <- if (!is.null(y_var_input) && y_var_input != "") sym(y_var_input) else NULL
    
    # For boxplot and violin, ensure y_var is numeric
    if (geom_type %in% c("boxplot", "violin")) {
      if (is.null(y_var_input) || y_var_input == "" || y_var_input == "None" || !is.numeric(df_data[[y_var_input]])) {
        rv$warnings <- c(rv$warnings, paste("Layer", id_suffix, ": For geom_", geom_type, ", y-variable must be numeric. Skipping this layer.", sep = ""))
        return(NULL)
      }
    }
    
    aes_list <- list()
    
    if (!geom_type %in% c("hline", "vline", "abline")) {
      if (!is.null(x_var)) {
        if (geom_type %in% c("boxplot", "violin") && !is.factor(df_data[[x_var_input]])) {
          aes_list$x <- expr(factor(!!x_var))
        } else {
          aes_list$x <- x_var
        }
      }
      if (!is.null(y_var)) aes_list$y <- y_var
    }
    
    color_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "color_var")
    if (!is.null(color_res$aesthetic)) aes_list$color <- color_res$aesthetic
    
    fill_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "fill_var")
    if (!is.null(fill_res$aesthetic)) aes_list$fill <- fill_res$aesthetic
    
    size_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "size_var", "size_val")
    if (!is.null(size_res$aesthetic)) aes_list$size <- size_res$aesthetic
    
    alpha_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "alpha_var", "alpha_val")
    if (!is.null(alpha_res$aesthetic)) aes_list$alpha <- alpha_res$aesthetic
    
    shape_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "shape_var", "shape_val")
    if (!is.null(shape_res$aesthetic)) aes_list$shape <- shape_res$aesthetic
    
    if (geom_type %in% c("line", "smooth", "abline", "hline", "vline", "freqpoly", "function", "path", "segment", "curve", "spoke", "ribbon")) {
      linetype_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "linetype_var", "linetype_val")
      if (!is.null(linetype_res$aesthetic)) aes_list$linetype <- linetype_res$aesthetic
    }
    
    label_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "label_var")
    if (!is.null(label_res$aesthetic)) aes_list$label <- label_res$aesthetic
    
    group_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "group_var")
    if (!is.null(group_res$aesthetic)) {
      aes_list$group <- group_res$aesthetic
    } else if (geom_type %in% c("line", "path", "smooth") && !is.null(x_var)) {
      aes_list$group <- 1
    } else if (geom_type %in% c("boxplot", "violin") && !is.null(x_var)) {
      aes_list$group <- x_var
    }
    
    geom_args <- list()
    if (length(aes_list) > 0) {
      geom_args$mapping <- aes(!!!aes_list[!sapply(aes_list, is.null)])
    }
    
    if (!is.null(size_res$fixed)) geom_args$size <- size_res$fixed
    if (!is.null(alpha_res$fixed)) geom_args$alpha <- alpha_res$fixed
    if (!is.null(shape_res$fixed)) geom_args$shape <- shape_res$fixed
    
    if (geom_type %in% c("line", "smooth", "abline", "hline", "vline", "freqpoly", "function", "path", "segment", "curve", "spoke", "ribbon")) {
      if (!is.null(linetype_res$fixed)) geom_args$linetype <- linetype_res$fixed
    }
    
    if (geom_type == "abline") {
      geom_args$intercept <- input_obj[[paste0("abline_intercept_", id_suffix)]]
      geom_args$slope <- input_obj[[paste0("abline_slope_", id_suffix)]]
      geom_args$intercept <- if(is.null(geom_args$intercept) || is.na(geom_args$intercept)) 0 else geom_args$intercept
      geom_args$slope <- if(is.null(geom_args$slope) || is.na(geom_args$slope)) 1 else geom_args$slope
    } else if (geom_type == "hline") {
      geom_args$yintercept <- input_obj[[paste0("hline_yintercept_", id_suffix)]]
      geom_args$yintercept <- if(is.null(geom_args$yintercept) || is.na(geom_args$yintercept)) 0 else geom_args$yintercept
    } else if (geom_type == "vline") {
      geom_args$xintercept <- input_obj[[paste0("vline_xintercept_", id_suffix)]]
      geom_args$xintercept <- if(is.null(geom_args$xintercept) || is.na(geom_args$xintercept)) 0 else geom_args$xintercept
    } else if (geom_type == "function") {
      fun_str <- input_obj[[paste0("function_fun_", id_suffix)]]
      if (!is.null(fun_str) && nchar(fun_str) > 0) {
        geom_args$fun <- rlang::parse_expr(fun_str)
      }
    }
    
    geom_layer <- tryCatch({
      switch(geom_type,
             "point" = do.call(geom_point, geom_args),
             "line" = do.call(geom_line, geom_args),
             "bar" = {
               if (!is.null(y_var) && y_var != "") {
                 geom_args$stat <- "identity"
                 geom_args$position <- "dodge"
               } else {
                 geom_args$stat <- "count"
                 geom_args$position <- "dodge"
               }
               do.call(geom_bar, geom_args)
             },
             "boxplot" = do.call(geom_boxplot, geom_args),
             "col" = {
               geom_args$stat = "identity"
               do.call(geom_col, geom_args)
             },
             "histogram" = {
               if (!is.null(x_var_input) && x_var_input %in% names(df_data) && is.numeric(df_data[[x_var_input]])) {
                 geom_args$binwidth <- diff(range(df_data[[x_var_input]], na.rm = TRUE)) / 30
               } else {
                 geom_args$binwidth <- 1
               }
               do.call(geom_histogram, geom_args)
             },
             "freqpoly" = {
               if (!is.null(x_var_input) && x_var_input %in% names(df_data) && is.numeric(df_data[[x_var_input]])) {
                 geom_args$binwidth <- diff(range(df_data[[x_var_input]], na.rm = TRUE)) / 30
               } else {
                 geom_args$binwidth <- 1
               }
               geom_args$geom <- "freqpoly"
               do.call(geom_histogram, geom_args)
             },
             "density" = do.call(geom_density, geom_args),
             "violin" = do.call(geom_violin, geom_args),
             "area" = do.call(geom_area, geom_args),
             "tile" = do.call(geom_tile, geom_args),
             "smooth" = do.call(geom_smooth, geom_args),
             "abline" = do.call(geom_abline, geom_args),
             "hline" = do.call(geom_hline, geom_args),
             "vline" = do.call(geom_vline, geom_args),
             "bin_2d" = do.call(geom_bin2d, geom_args),
             "blank" = geom_blank(),
             "contour" = do.call(geom_contour, geom_args),
             "contour_filled" = do.call(geom_contour_filled, geom_args),
             "count" = do.call(geom_count, geom_args),
             "density_2d" = do.call(geom_density_2d, geom_args),
             "density_2d_filled" = do.call(geom_density_2d_filled, geom_args),
             "dotplot" = do.call(geom_dotplot, geom_args),
             "errorbar" = do.call(geom_errorbar, geom_args),
             "errorbarh" = do.call(geom_errorbarh, geom_args),
             "function" = do.call(geom_function, geom_args),
             "hex" = do.call(geom_hex, geom_args),
             "jitter" = do.call(geom_jitter, geom_args),
             "crossbar" = do.call(geom_crossbar, geom_args),
             "linerange" = do.call(geom_linerange, geom_args),
             "pointrange" = do.call(geom_pointrange, geom_args),
             "map" = do.call(geom_map, geom_args),
             "path" = do.call(geom_path, geom_args),
             "polygon" = do.call(geom_polygon, geom_args),
             "qq_line" = do.call(geom_qq_line, geom_args),
             "qq" = do.call(geom_qq, geom_args),
             "quantile" = do.call(geom_quantile, geom_args),
             "ribbon" = do.call(geom_ribbon, geom_args),
             "rug" = do.call(geom_rug, geom_args),
             "segment" = do.call(geom_segment, geom_args),
             "curve" = do.call(geom_curve, geom_args),
             "spoke" = do.call(geom_spoke, geom_args),
             "label" = do.call(geom_label, geom_args),
             "text" = do.call(geom_text, geom_args),
             "raster" = do.call(geom_raster, geom_args),
             "rect" = do.call(geom_rect, geom_args),
             "sf" = do.call(geom_sf, geom_args),
             "sf_label" = do.call(geom_sf_label, geom_args),
             "sf_text" = do.call(geom_sf_text, geom_args)
      )
    }, error = function(e) {
      rv$warnings <- c(rv$warnings, paste("Layer", id_suffix, ": Error creating geom layer:", e$message))
      NULL
    })
    
    list(aes_list = aes_list, geom_layer = geom_layer)
  }
  
  # Reactive expression for statistical tests
  stat_test_results <- reactive({
    if (!isTRUE(input$add_stat_tests) || !input$geom_type_1 %in% c("boxplot", "violin")) {
      return(NULL)
    }
    
    x_var <- input$x_var_1
    y_var <- input$y_var_1
    test_type <- input$stat_test_type
    alpha <- input$stat_test_alpha
    
    if (is.null(x_var) || is.null(y_var) || x_var == "" || y_var == "" || y_var == "None") {
      rv$warnings <- c(rv$warnings, "Statistical test: X and Y variables must be selected.")
      return(NULL)
    }
    
    if (!is.numeric(df[[y_var]])) {
      rv$warnings <- c(rv$warnings, "Statistical test: Y-variable must be numeric.")
      return(NULL)
    }
    
    if (!is.factor(df[[x_var]]) || nlevels(df[[x_var]]) != 2) {
      rv$warnings <- c(rv$warnings, "Statistical test: X-variable must be a categorical variable with exactly two levels.")
      return(NULL)
    }
    
    group1 <- df[df[[x_var]] == levels(df[[x_var]])[1], y_var]
    group2 <- df[df[[x_var]] == levels(df[[x_var]])[2], y_var]
    
    if (length(group1) < 2 || length(group2) < 2 || any(is.na(group1)) || any(is.na(group2))) {
      rv$warnings <- c(rv$warnings, "Statistical test: Each group must have at least two non-NA observations.")
      return(NULL)
    }
    
    tryCatch({
      if (test_type == "t-test") {
        test_result <- t.test(group1, group2, conf.level = 1 - alpha)
        result <- list(
          test_name = "t-test",
          statistic = test_result$statistic,
          p_value = test_result$p.value,
          significant = test_result$p.value < alpha,
          groups = paste(levels(df[[x_var]])[1], "vs", levels(df[[x_var]])[2])
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
          groups = paste(levels(df[[x_var]])[1], "vs", levels(df[[x_var]])[2])
        )
      }
      return(result)
    }, error = function(e) {
      rv$warnings <- c(rv$warnings, paste("Statistical test error:", e$message))
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
    sprintf("%s results (%s): Statistic = %.3f, p-value = %.4f (%s at alpha = %.2f)",
            result$test_name, result$groups, result$statistic, result$p_value, sig_text, input$stat_test_alpha)
  })
  
  # Reactive expression to generate the full ggplot object
  ggplot_plot <- reactive({
    req(input$x_var_1)
    
    layer1_aes_list <- list(x = sym(input$x_var_1))
    if (!is.null(input$y_var_1) && input$y_var_1 != "" && input$y_var_1 != "None" && (!input$geom_type_1 %in% c("boxplot", "violin") || is.numeric(df[[input$y_var_1]]))) {
      layer1_aes_list$y <- sym(input$y_var_1)
    }
    
    color_res <- get_aesthetic_value_simplified(input, "1", "color_var")
    if (!is.null(color_res$aesthetic)) layer1_aes_list$color <- color_res$aesthetic
    
    fill_res <- get_aesthetic_value_simplified(input, "1", "fill_var")
    if (!is.null(fill_res$aesthetic)) layer1_aes_list$fill <- fill_res$aesthetic
    
    p <- ggplot(data = df, mapping = aes(!!!layer1_aes_list[!sapply(layer1_aes_list, is.null)]))
    
    for (i in 1:rv$num_layers) {
      geom_type_id <- paste0("geom_type_", i)
      x_var_id <- paste0("x_var_", i)
      if (is.null(input[[geom_type_id]]) || is.null(input[[x_var_id]])) {
        next
      }
      
      layer_result <- generate_geom_layer(i, input, df)
      if (!is.null(layer_result$geom_layer)) {
        p <- p + layer_result$geom_layer
      }
      
      if (!is.null(input[[paste0("layer_title_", i)]]) && input[[paste0("layer_title_", i)]] != "") {
        p <- p + labs(title = input[[paste0("layer_title_", i)]])
      }
      if (!is.null(input[[paste0("layer_subtitle_", i)]]) && input[[paste0("layer_subtitle_", i)]] != "") {
        p <- p + labs(subtitle = input[[paste0("layer_subtitle_", i)]])
      }
      if (!is.null(input[[paste0("layer_caption_", i)]]) && input[[paste0("layer_caption_", i)]] != "") {
        p <- p + labs(caption = input[[paste0("layer_caption_", i)]])
      }
      if (!is.null(input[[paste0("x_label_", i)]]) && input[[paste0("x_label_", i)]] != "") {
        p <- p + labs(x = input[[paste0("x_label_", i)]])
      }
      if (!is.null(input[[paste0("y_label_", i)]]) && input[[paste0("y_label_", i)]] != "") {
        p <- p + labs(y = input[[paste0("y_label_", i)]])
      }
      
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
      
      xlim <- c(input[[paste0("xmin_", i)]], input[[paste0("xmax_", i)]])
      ylim <- c(input[[paste0("ymin_", i)]], input[[paste0("ymax_", i)]])
      
      if (any(!is.na(xlim)) || any(!is.na(ylim))) {
        p <- p + coord_cartesian(
          xlim = if (any(!is.na(xlim))) xlim,
          ylim = if (any(!is.na(ylim))) ylim,
          default = TRUE
        )
      }
    }
    
    if (isTRUE(input$add_stat_summary) && !is.null(input$x_var_1) && !is.null(input$y_var_1) && input$y_var_1 != "" && input$y_var_1 != "None" && is.numeric(df[[input$y_var_1]])) {
      if (input$stat_summary_fun == "mean") {
        p <- p + stat_summary(fun = mean, geom = input$stat_summary_geom, width = input$stat_summary_width, colour = "red")
      } else if (input$stat_summary_fun == "median") {
        p <- p + stat_summary(fun = median, geom = input$stat_summary_geom, width = input$stat_summary_width, colour = "blue")
      }
    }
    
    if (!is.null(input$facet_row) && input$facet_row != "None" || !is.null(input$facet_col) && input$facet_col != "None") {
      facet_formula_str <- paste(input$facet_row, "~", input$facet_col)
      facet_formula_str <- gsub("None ~ ", "~ ", facet_formula_str)
      facet_formula_str <- gsub(" ~ None", " ~ ", facet_formula_str)
      facet_formula_str <- gsub("None", ".", facet_formula_str)
      p <- p + facet_grid(as.formula(facet_formula_str), scales = input$facet_scales)
    }
    
    if (!is.null(input$plot_theme)) {
      p <- p + do.call(input$plot_theme, list())
    }
    
    # Apply custom theme settings
    custom_theme <- theme_settings()
    if (!is.null(custom_theme)) {
      p <- p + do.call(theme, custom_theme)
    }
    
    return(p)
  })
  
  # Render warnings
  output$warnings <- renderText({
    paste(rv$warnings, collapse = "\n")
  })
  
  # Render the plot using plotly
  output$plot <- renderPlotly({
    p <- ggplot_plot()
    if (!is.null(p)) {
      ggplotly(p)
    } else {
      NULL
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)# This app allows users to create interactive ggplot2 plots with multiple layers and dynamic UI elements.
# It provides a user-friendly interface for selecting variables, aesthetics, and plot types.
# The app supports various geom types, including points, lines, bars, histograms, and more.
# Users can add, remove, and configure multiple layers, and customize plot settings such as colors, sizes, and labels.
# The app includes faceting options, statistical summary layers, t-test/Wald test for boxplot/violin plots, and dynamic theme parameter customization with color pickers.
# The final plot is displayed using ggplot2 and can be exported as an interactive plotly object.

# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly) # For interactive plots
library(dplyr) # For data manipulation, especially with faceting
library(rlang) # For tidy evaluation with !!! operator
library(shinyjs) # For JavaScript functionalities like toggling elements
library(stats) # For t.test and pnorm
library(colourpicker) # For color picker input

# Use the built-in mtcars dataset
data(mtcars)

# Convert relevant columns to factors for categorical plotting
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)

# Define a helper function to generate UI for a single layer
layer_controls_ui <- function(id_suffix, df, numeric_cols_choices, categorical_cols_choices) {
  div(
    h3(paste("Layer", id_suffix)),
    
    # Add collapsible aesthetics section
    checkboxInput(paste0("show_aes_", id_suffix), "Aesthetics", value = TRUE),
    conditionalPanel(
      condition = paste0("input.show_aes_", id_suffix, " == true"),
      selectInput(paste0("geom_type_", id_suffix), "Geometric:",
                  choices = c(
                    "abline", "area", "bar", "bin_2d", "blank", "boxplot", "col", "contour", "contour_filled",
                    "count", "crossbar", "curve", "density", "density_2d", "density_2d_filled", "dotplot",
                    "errorbar", "errorbarh", "freqpoly", "function", "hex", "histogram", "hline", "jitter",
                    "label", "line", "linerange", "map", "path", "point", "pointrange", "polygon", "qq",
                    "qq_line", "quantile", "rect", "ribbon", "raster", "rug", "segment", "sf", "sf_label",
                    "sf_text", "smooth", "spoke", "text", "tile", "violin", "vline"
                  ),
                  selected = "point"),
      
      uiOutput(paste0("x_var_ui_", id_suffix)),
      
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " == 'point' || input.geom_type_", id_suffix, " == 'line' || input.geom_type_", id_suffix, " == 'bar' || input.geom_type_", id_suffix, " == 'col' || input.geom_type_", id_suffix, " == 'area' || input.geom_type_", id_suffix, " == 'tile' || input.geom_type_", id_suffix, " == 'smooth' || input.geom_type_", id_suffix, " == 'bin_2d' || input.geom_type_", id_suffix, " == 'contour' || input.geom_type_", id_suffix, " == 'contour_filled' || input.geom_type_", id_suffix, " == 'count' || input.geom_type_", id_suffix, " == 'density_2d' || input.geom_type_", id_suffix, " == 'density_2d_filled' || input.geom_type_", id_suffix, " == 'dotplot' || input.geom_type_", id_suffix, " == 'errorbar' || input.geom_type_", id_suffix, " == 'crossbar' || input.geom_type_", id_suffix, " == 'linerange' || input.geom_type_", id_suffix, " == 'pointrange' || input.geom_type_", id_suffix, " == 'polygon' || input.geom_type_", id_suffix, " == 'quantile' || input.geom_type_", id_suffix, " == 'ribbon' || input.geom_type_", id_suffix, " == 'rug' || input.geom_type_", id_suffix, " == 'segment' || input.geom_type_", id_suffix, " == 'curve' || input.geom_type_", id_suffix, " == 'spoke' || input.geom_type_", id_suffix, " == 'label' || input.geom_type_", id_suffix, " == 'text' || input.geom_type_", id_suffix, " == 'raster' || input.geom_type_", id_suffix, " == 'rect' || input.geom_type_", id_suffix, " == 'sf' || input.geom_type_", id_suffix, " == 'sf_label' || input.geom_type_", id_suffix, " == 'sf_text' || input.geom_type_", id_suffix, " == 'boxplot' || input.geom_type_", id_suffix, " == 'violin'"),
        uiOutput(paste0("y_var_ui_", id_suffix))
      ),
      
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " == 'boxplot' || input.geom_type_", id_suffix, " == 'violin'"),
        helpText("For boxplots or violin plots, select a categorical variable for X (e.g., cyl) and a numeric variable for Y (e.g., mpg).")
      ),
      
      uiOutput(paste0("color_var_ui_", id_suffix)),
      uiOutput(paste0("fill_var_ui_", id_suffix)),
      uiOutput(paste0("group_var_ui_", id_suffix))
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
      textInput(paste0("layer_subtitle_", id_suffix), "Subtitle:", value = ""),
      textInput(paste0("layer_caption_", id_suffix), "Caption:", value = ""),
      uiOutput(paste0("x_label_ui_", id_suffix)),
      uiOutput(paste0("y_label_ui_", id_suffix))
    ),
    hr(),
    
    conditionalPanel(
      condition = paste0("input.geom_type_", id_suffix, " == 'abline'"),
      numericInput(paste0("abline_intercept_", id_suffix), "Intercept:", value = 0),
      numericInput(paste0("abline_slope_", id_suffix), "Slope:", value = 1)
    ),
    conditionalPanel(
      condition = paste0("input.geom_type_", id_suffix, " == 'hline'"),
      numericInput(paste0("hline_yintercept_", id_suffix), "Y-intercept:", value = 0)
    ),
    conditionalPanel(
      condition = paste0("input.geom_type_", id_suffix, " == 'vline'"),
      numericInput(paste0("vline_xintercept_", id_suffix), "X-intercept:", value = 0)
    ),
    conditionalPanel(
      condition = paste0("input.geom_type_", id_suffix, " == 'function'"),
      textInput(paste0("function_fun_", id_suffix), "Function (e.g., 'sin(x)', 'x^2', 'dnorm(x)', 'abs(x)'):", value = "dnorm(x)")
    ),
    
    checkboxInput(paste0("show_advanced_aesthetics_", id_suffix), "Advanced Aesthetics", value = FALSE),
    
    conditionalPanel(
      condition = paste0("input.show_advanced_aesthetics_", id_suffix, " == true"),
      uiOutput(paste0("size_var_ui_", id_suffix)),
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " %in% c('point', 'line', 'smooth', 'boxplot', 'violin', 'abline', 'hline', 'vline', 'count', 'dotplot', 'errorbar', 'errorbarh', 'crossbar', 'linerange', 'pointrange', 'path', 'segment', 'curve', 'spoke', 'label', 'text', 'sf_label', 'sf_text')"),
        numericInput(paste0("size_val_", id_suffix), "Fixed Size:", value = NA, min = 0.1, step = 0.1)
      ),
      
      uiOutput(paste0("alpha_var_ui_", id_suffix)),
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " %in% c('point', 'bar', 'col', 'histogram', 'density', 'violin', 'area', 'tile', 'smooth', 'bin_2d', 'boxplot', 'contour', 'contour_filled', 'count', 'density_2d', 'density_2d_filled', 'dotplot', 'hex', 'jitter', 'crossbar', 'linerange', 'pointrange', 'map', 'path', 'polygon', 'quantile', 'ribbon', 'rug', 'segment', 'curve', 'spoke', 'label', 'text', 'raster', 'rect', 'sf', 'sf_label', 'sf_text')"),
        numericInput(paste0("alpha_val_", id_suffix), "Fixed Alpha (0-1):", value = NA, min = 0, max = 1, step = 0.1)
      ),
      
      uiOutput(paste0("shape_var_ui_", id_suffix)),
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " %in% c('point', 'count', 'jitter', 'sf_text')"),
        numericInput(paste0("shape_val_", id_suffix), "Fixed Shape (0-25):", value = NA, min = 0, max = 25, step = 1)
      ),
      
      conditionalPanel(
        condition = paste0("input.geom_type_", id_suffix, " %in% c('line', 'smooth', 'abline', 'hline', 'vline', 'freqpoly', 'function', 'path', 'segment', 'curve', 'spoke', 'ribbon')"),
        uiOutput(paste0("linetype_var_ui_", id_suffix)),
        selectInput(paste0("linetype_val_", id_suffix), "Fixed Linetype:",
                    choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                    selected = "solid")
      ),
      
      uiOutput(paste0("label_var_ui_", id_suffix))
    )
  )
}

# Define the User Interface (UI)
ui <- fluidPage(
  useShinyjs(),
  titlePanel("mtcars Interactive Plotting with ggplot2 and Plotly"),
  sidebarLayout(
    sidebarPanel(
      id = "controls_panel",
      tabsetPanel(
        id = "plot_settings_tabs",
        tabPanel("Layer 1",
                 layer_controls_ui("1", mtcars, names(mtcars)[sapply(mtcars, is.numeric)], names(mtcars)[sapply(mtcars, is.factor) | sapply(mtcars, is.character)])
        ),
        uiOutput("dynamic_layer_tabs"),
        tabPanel("Layer + / -",
                 h3("Manage Plot Layers"),
                 actionButton("add_layer_btn", "Add New Plot Layer"),
                 actionButton("remove_layer_btn", "Remove Last Layer")
        ),
        tabPanel("Faceting",
                 h3("Split Plot by Variables"),
                 uiOutput("facet_row_ui"),
                 uiOutput("facet_col_ui"),
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
                             choices = c("theme_gray", "theme_bw", "theme_minimal", "theme_classic", "theme_dark", "theme_light", "theme_linedraw", "theme_void"),
                             selected = "theme_gray"),
                 checkboxInput("show_theme_params", "Customize Theme Parameters", value = FALSE),
                 conditionalPanel(
                   condition = "input.show_theme_params == true",
                   uiOutput("theme_params_ui")
                 )
        )
      )
    ),
    mainPanel(
      actionButton("reset_to_default", "Reset to Default"),
      br(),
      textOutput("warnings"),
      plotlyOutput("plot"),
      br(),
      textOutput("stat_test_output")
    )
  )
)

# Define the Server logic
server <- function(input, output, session) {
  df <- mtcars
  rv <- reactiveValues(num_layers = 1, warnings = character(0))
  
  # Define default values
  default_values <- list(
    geom_type = "point",
    x_var = "wt",
    y_var = "mpg",
    color_var = "None",
    fill_var = "None",
    layer_title = "",
    layer_subtitle = "",
    layer_caption = "",
    x_label = "",
    y_label = "",
    size_var = "None",
    size_val = NA,
    alpha_var = "None",
    alpha_val = NA,
    shape_var = "None",
    shape_val = NA,
    linetype_var = "None",
    linetype_val = "solid",
    label_var = "None",
    show_advanced_aesthetics = FALSE,
    show_axis_scale = FALSE,
    show_labels_title = FALSE,
    x_trans = "none",
    y_trans = "none",
    xmin = NA,
    xmax = NA,
    ymin = NA,
    ymax = NA,
    facet_row = "None",
    facet_col = "None",
    facet_scales = "fixed",
    add_stat_summary = FALSE,
    stat_summary_fun = "mean",
    stat_summary_geom = "crossbar",
    stat_summary_width = 0.5,
    add_stat_tests = FALSE,
    stat_test_type = "t-test",
    stat_test_alpha = 0.05,
    plot_theme = "theme_gray",
    show_theme_params = FALSE,
    group_var = "None",
    show_aes = TRUE
  )
  
  # Define theme parameters for customization
  theme_parameters <- list(
    text = list(
      text_size = list(type = "numeric", default = 12, label = "Text Size"),
      text_color = list(type = "color", default = "#000000", label = "Text Color")
    ),
    panel = list(
      panel_background_fill = list(type = "color", default = "#FFFFFF", label = "Panel Background Fill"),
      panel_grid_major_color = list(type = "color", default = "#D3D3D3", label = "Major Grid Line Color"),
      panel_grid_major_size = list(type = "numeric", default = 0.5, label = "Major Grid Line Size"),
      panel_grid_minor_color = list(type = "color", default = "#E8E8E8", label = "Minor Grid Line Color"),
      panel_grid_minor_size = list(type = "numeric", default = 0.25, label = "Minor Grid Line Size")
    ),
    plot = list(
      plot_background_fill = list(type = "color", default = "#FFFFFF", label = "Plot Background Fill"),
      plot_margin_top = list(type = "numeric", default = 5, label = "Plot Margin Top (pt)"),
      plot_margin_right = list(type = "numeric", default = 5, label = "Plot Margin Right (pt)"),
      plot_margin_bottom = list(type = "numeric", default = 5, label = "Plot Margin Bottom (pt)"),
      plot_margin_left = list(type = "numeric", default = 5, label = "Plot Margin Left (pt)")
    ),
    legend = list(
      legend_position = list(type = "select", default = "right", label = "Legend Position", choices = c("right", "left", "top", "bottom", "none")),
      legend_background_fill = list(type = "color", default = "#FFFFFF", label = "Legend Background Fill"),
      legend_text_size = list(type = "numeric", default = 10, label = "Legend Text Size")
    )
  )
  
  # Render theme parameters UI
  output$theme_params_ui <- renderUI({
    req(input$plot_theme, input$show_theme_params)
    
    param_ui <- list()
    
    # Text Parameters
    param_ui <- append(param_ui, list(h4("Text Parameters")))
    for (param in names(theme_parameters$text)) {
      param_info <- theme_parameters$text[[param]]
      if (param_info$type == "numeric") {
        param_ui <- append(param_ui, list(
          numericInput(paste0("theme_", param), param_info$label, value = param_info$default, min = 0, step = 0.1)
        ))
      } else if (param_info$type == "color") {
        param_ui <- append(param_ui, list(
          colourInput(paste0("theme_", param), param_info$label, value = param_info$default, allowTransparent = TRUE)
        ))
      }
    }
    
    # Panel Parameters
    param_ui <- append(param_ui, list(h4("Panel Parameters")))
    for (param in names(theme_parameters$panel)) {
      param_info <- theme_parameters$panel[[param]]
      if (param_info$type == "numeric") {
        param_ui <- append(param_ui, list(
          numericInput(paste0("theme_", param), param_info$label, value = param_info$default, min = 0, step = 0.1)
        ))
      } else if (param_info$type == "color") {
        param_ui <- append(param_ui, list(
          colourInput(paste0("theme_", param), param_info$label, value = param_info$default, allowTransparent = TRUE)
        ))
      }
    }
    
    # Plot Parameters
    param_ui <- append(param_ui, list(h4("Plot Parameters")))
    for (param in names(theme_parameters$plot)) {
      param_info <- theme_parameters$plot[[param]]
      if (param_info$type == "numeric") {
        param_ui <- append(param_ui, list(
          numericInput(paste0("theme_", param), param_info$label, value = param_info$default, min = 0, step = 0.1)
        ))
      } else if (param_info$type == "color") {
        param_ui <- append(param_ui, list(
          colourInput(paste0("theme_", param), param_info$label, value = param_info$default, allowTransparent = TRUE)
        ))
      }
    }
    
    # Legend Parameters
    param_ui <- append(param_ui, list(h4("Legend Parameters")))
    for (param in names(theme_parameters$legend)) {
      param_info <- theme_parameters$legend[[param]]
      if (param_info$type == "select") {
        param_ui <- append(param_ui, list(
          selectInput(paste0("theme_", param), param_info$label, choices = param_info$choices, selected = param_info$default)
        ))
      } else if (param_info$type == "numeric") {
        param_ui <- append(param_ui, list(
          numericInput(paste0("theme_", param), param_info$label, value = param_info$default, min = 0, step = 0.1)
        ))
      } else if (param_info$type == "color") {
        param_ui <- append(param_ui, list(
          colourInput(paste0("theme_", param), param_info$label, value = param_info$default, allowTransparent = TRUE)
        ))
      }
    }
    
    do.call(tagList, param_ui)
  })
  
  # Reactive expression to collect theme settings
  theme_settings <- reactive({
    if (!isTRUE(input$show_theme_params)) {
      return(NULL)
    }
    
    theme_args <- list()
    
    # Text Parameters
    if (!is.null(input$theme_text_size) && !is.na(input$theme_text_size)) {
      theme_args$text <- element_text(size = input$theme_text_size, colour = input$theme_text_color)
    }
    
    # Panel Parameters
    if (!is.null(input$theme_panel_background_fill)) {
      theme_args$panel.background <- element_rect(fill = input$theme_panel_background_fill)
    }
    if (!is.null(input$theme_panel_grid_major_color) && !is.null(input$theme_panel_grid_major_size)) {
      theme_args$panel.grid.major <- element_line(colour = input$theme_panel_grid_major_color, size = input$theme_panel_grid_major_size)
    }
    if (!is.null(input$theme_panel_grid_minor_color) && !is.null(input$theme_panel_grid_minor_size)) {
      theme_args$panel.grid.minor <- element_line(colour = input$theme_panel_grid_minor_color, size = input$theme_panel_grid_minor_size)
    }
    
    # Plot Parameters
    if (!is.null(input$theme_plot_background_fill)) {
      theme_args$plot.background <- element_rect(fill = input$theme_plot_background_fill)
    }
    if (!is.null(input$theme_plot_margin_top) && !is.na(input$theme_plot_margin_top) &&
        !is.null(input$theme_plot_margin_right) && !is.na(input$theme_plot_margin_right) &&
        !is.null(input$theme_plot_margin_bottom) && !is.na(input$theme_plot_margin_bottom) &&
        !is.null(input$theme_plot_margin_left) && !is.na(input$theme_plot_margin_left)) {
      theme_args$plot.margin <- margin(
        t = input$theme_plot_margin_top,
        r = input$theme_plot_margin_right,
        b = input$theme_plot_margin_bottom,
        l = input$theme_plot_margin_left,
        unit = "pt"
      )
    }
    
    # Legend Parameters
    if (!is.null(input$theme_legend_position)) {
      theme_args$legend.position <- input$theme_legend_position
    }
    if (!is.null(input$theme_legend_background_fill)) {
      theme_args$legend.background <- element_rect(fill = input$theme_legend_background_fill)
    }
    if (!is.null(input$theme_legend_text_size) && !is.na(input$theme_legend_text_size)) {
      theme_args$legend.text <- element_text(size = input$theme_legend_text_size)
    }
    
    if (length(theme_args) > 0) {
      return(theme_args)
    } else {
      return(NULL)
    }
  })
  
  # Observe reset button click
  observeEvent(input$reset_to_default, {
    updateSelectInput(session, "geom_type_1", selected = default_values$geom_type)
    updateSelectInput(session, "x_var_1", selected = default_values$x_var)
    updateSelectInput(session, "y_var_1", selected = default_values$y_var)
    updateSelectInput(session, "color_var_1", selected = default_values$color_var)
    updateSelectInput(session, "fill_var_1", selected = default_values$fill_var)
    updateTextInput(session, "layer_title_1", value = default_values$layer_title)
    updateTextInput(session, "layer_subtitle_1", value = default_values$layer_subtitle)
    updateTextInput(session, "layer_caption_1", value = default_values$layer_caption)
    updateTextInput(session, "x_label_1", value = default_values$x_label)
    updateTextInput(session, "y_label_1", value = default_values$y_label)
    updateSelectInput(session, "size_var_1", selected = default_values$size_var)
    updateNumericInput(session, "size_val_1", value = default_values$size_val)
    updateSelectInput(session, "alpha_var_1", selected = default_values$alpha_var)
    updateNumericInput(session, "alpha_val_1", value = default_values$alpha_val)
    updateSelectInput(session, "shape_var_1", selected = default_values$shape_var)
    updateNumericInput(session, "shape_val_1", value = default_values$shape_val)
    updateSelectInput(session, "linetype_var_1", selected = default_values$linetype_var)
    updateSelectInput(session, "linetype_val_1", selected = default_values$linetype_val)
    updateSelectInput(session, "label_var_1", selected = default_values$label_var)
    updateCheckboxInput(session, "show_advanced_aesthetics_1", value = default_values$show_advanced_aesthetics)
    updateCheckboxInput(session, "show_axis_scale_1", value = default_values$show_axis_scale)
    updateCheckboxInput(session, "show_labels_title_1", value = default_values$show_labels_title)
    updateSelectInput(session, "x_trans_1", selected = default_values$x_trans)
    updateSelectInput(session, "y_trans_1", selected = default_values$y_trans)
    updateNumericInput(session, "xmin_1", value = default_values$xmin)
    updateNumericInput(session, "xmax_1", value = default_values$xmax)
    updateNumericInput(session, "ymin_1", value = default_values$ymin)
    updateNumericInput(session, "ymax_1", value = default_values$ymax)
    updateSelectInput(session, "facet_row", selected = default_values$facet_row)
    updateSelectInput(session, "facet_col", selected = default_values$facet_col)
    updateSelectInput(session, "facet_scales", selected = default_values$facet_scales)
    updateCheckboxInput(session, "add_stat_summary", value = default_values$add_stat_summary)
    updateSelectInput(session, "stat_summary_fun", selected = default_values$stat_summary_fun)
    updateSelectInput(session, "stat_summary_geom", selected = default_values$stat_summary_geom)
    updateNumericInput(session, "stat_summary_width", value = default_values$stat_summary_width)
    updateCheckboxInput(session, "add_stat_tests", value = default_values$add_stat_tests)
    updateSelectInput(session, "stat_test_type", selected = default_values$stat_test_type)
    updateNumericInput(session, "stat_test_alpha", value = default_values$stat_test_alpha)
    updateSelectInput(session, "plot_theme", selected = default_values$plot_theme)
    updateCheckboxInput(session, "show_theme_params", value = default_values$show_theme_params)
    updateSelectInput(session, "group_var_1", selected = default_values$group_var)
    updateCheckboxInput(session, "show_aes_1", value = default_values$show_aes)
    
    # Reset theme parameters
    for (category in names(theme_parameters)) {
      for (param in names(theme_parameters[[category]])) {
        param_info <- theme_parameters[[category]][[param]]
        if (param_info$type == "numeric") {
          updateNumericInput(session, paste0("theme_", param), value = param_info$default)
        } else if (param_info$type == "color") {
          updateColourInput(session, paste0("theme_", param), value = param_info$default)
        } else if (param_info$type == "select") {
          updateSelectInput(session, paste0("theme_", param), selected = param_info$default)
        }
      }
    }
    
    while (rv$num_layers > 1) {
      removeTab(inputId = "plot_settings_tabs", target = paste("Layer", rv$num_layers, "Settings"))
      rv$num_layers <- rv$num_layers - 1
    }
    
    updateTabsetPanel(session, "plot_settings_tabs", selected = "Layer 1")
    rv$warnings <- character(0) # Clear warnings on reset
  })
  
  # Reactive expressions for column names
  numeric_cols <- reactive({
    names(df)[sapply(df, is.numeric)]
  })
  
  categorical_cols <- reactive({
    names(df)[sapply(df, is.factor) | sapply(df, is.character)]
  })
  
  # Helper to get aesthetic parameter
  get_aesthetic_value_simplified <- function(input_obj, id_suffix, var_input_id, fixed_val_input_id = NULL) {
    var_val <- input_obj[[paste0(var_input_id, "_", id_suffix)]]
    if (!is.null(var_val) && var_val != "" && var_val != "None") {
      return(list(aesthetic = sym(var_val), fixed = NULL))
    } else if (!is.null(fixed_val_input_id)) {
      fixed_val <- input_obj[[paste0(fixed_val_input_id, "_", id_suffix)]]
      if (!is.null(fixed_val) && !is.na(fixed_val)) {
        return(list(aesthetic = NULL, fixed = fixed_val))
      }
    }
    return(list(aesthetic = NULL, fixed = NULL))
  }
  
  # Observe button click to add a new layer tab
  observeEvent(input$add_layer_btn, {
    new_layer_id <- rv$num_layers + 1
    rv$num_layers <- new_layer_id
    
    layer1_geom <- input$geom_type_1
    layer1_x_var <- input$x_var_1
    layer1_y_var <- input$y_var_1
    layer1_color <- input$color_var_1
    layer1_fill <- input$fill_var_1
    layer1_title <- input$layer_title_1
    layer1_subtitle <- input$layer_subtitle_1
    layer1_caption <- input$layer_caption_1
    layer1_x_label <- input$x_label_1
    layer1_y_label <- input$y_label_1
    layer1_show_axis_scale <- input$show_axis_scale_1
    layer1_show_labels_title <- input$show_labels_title_1
    layer1_group <- input$group_var_1
    layer1_show_aes <- input$show_aes_1
    
    appendTab(inputId = "plot_settings_tabs",
              tabPanel(paste("Layer", new_layer_id, "Settings"),
                       layer_controls_ui(as.character(new_layer_id), df, numeric_cols(), categorical_cols())
              ))
    
    updateSelectInput(session, paste0("geom_type_", new_layer_id), selected = layer1_geom)
    updateSelectInput(session, paste0("x_var_", new_layer_id), selected = layer1_x_var)
    if (!is.null(layer1_y_var) && layer1_y_var != "") {
      updateSelectInput(session, paste0("y_var_", new_layer_id), selected = layer1_y_var)
    }
    updateSelectInput(session, paste0("color_var_", new_layer_id), selected = layer1_color)
    updateSelectInput(session, paste0("fill_var_", new_layer_id), selected = layer1_fill)
    updateTextInput(session, paste0("layer_title_", new_layer_id), value = layer1_title)
    updateTextInput(session, paste0("layer_subtitle_", new_layer_id), value = layer1_subtitle)
    updateTextInput(session, paste0("layer_caption_", new_layer_id), value = layer1_caption)
    updateTextInput(session, paste0("x_label_", new_layer_id), value = layer1_x_label)
    updateTextInput(session, paste0("y_label_", new_layer_id), value = layer1_y_label)
    updateCheckboxInput(session, paste0("show_axis_scale_", new_layer_id), value = layer1_show_axis_scale)
    updateCheckboxInput(session, paste0("show_labels_title_", new_layer_id), value = layer1_show_labels_title)
    updateSelectInput(session, paste0("group_var_", new_layer_id), selected = layer1_group)
    updateCheckboxInput(session, paste0("show_aes_", new_layer_id), value = layer1_show_aes)
    
    updateTabsetPanel(session, "plot_settings_tabs", selected = paste("Layer", new_layer_id, "Settings"))
  })
  
  # Observe button click to remove the last layer tab
  observeEvent(input$remove_layer_btn, {
    if (rv$num_layers > 1) {
      removeTab(inputId = "plot_settings_tabs", target = paste("Layer", rv$num_layers, "Settings"))
      rv$num_layers <- rv$num_layers - 1
    }
  })
  
  # Dynamic UI elements for variable selection
  observe({
    lapply(1:rv$num_layers, function(i) {
      id_suffix <- as.character(i)
      
      output[[paste0("x_var_ui_", id_suffix)]] <- renderUI({
        req(input[[paste0("geom_type_", id_suffix)]])
        geom <- input[[paste0("geom_type_", id_suffix)]]
        num_cols <- numeric_cols()
        cat_cols <- categorical_cols()
        
        choices_x <- if (geom == "boxplot" || geom == "violin") {
          c(cat_cols, num_cols)
        } else {
          c(num_cols, cat_cols)
        }
        
        selected_x <- if (length(choices_x) > 0 && !is.null(input[[paste0("x_var_", id_suffix)]]) && input[[paste0("x_var_", id_suffix)]] %in% choices_x) {
          input[[paste0("x_var_", id_suffix)]]
        } else {
          choices_x[1]
        }
        selectInput(paste0("x_var_", id_suffix), "X variable:", choices = choices_x, selected = selected_x)
      })
      
      output[[paste0("y_var_ui_", id_suffix)]] <- renderUI({
        req(input[[paste0("geom_type_", id_suffix)]])
        geom <- input[[paste0("geom_type_", id_suffix)]]
        num_cols <- numeric_cols()
        
        choices_y <- if (geom %in% c("point", "line", "bar", "col", "area", "tile", "smooth", "bin_2d", "contour", "contour_filled", "count", "density_2d", "density_2d_filled", "dotplot", "errorbar", "errorbarh", "hex", "jitter", "crossbar", "linerange", "pointrange", "map", "path", "polygon", "quantile", "ribbon", "segment", "curve", "spoke", "label", "text", "raster", "rect", "sf", "sf_label", "sf_text", "boxplot", "violin")) {
          num_cols
        } else {
          NULL
        }
        
        selected_y <- if (!is.null(input[[paste0("y_var_", id_suffix)]]) && input[[paste0("y_var_", id_suffix)]] %in% choices_y) {
          input[[paste0("y_var_", id_suffix)]]
        } else {
          if (geom %in% c("boxplot", "violin") && length(choices_y) > 0) choices_y[1] else NULL
        }
        selectInput(paste0("y_var_", id_suffix), "Y variable:", choices = choices_y, selected = selected_y)
      })
      
      output[[paste0("color_var_ui_", id_suffix)]] <- renderUI({
        all_cols <- c(numeric_cols(), categorical_cols())
        choices <- c("None", if(length(all_cols) > 0) all_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("color_var_", id_suffix)]]) && input[[paste0("color_var_", id_suffix)]] %in% choices) input[[paste0("color_var_", id_suffix)]] else "None"
        selectInput(paste0("color_var_", id_suffix), "Variable for color:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("fill_var_ui_", id_suffix)]] <- renderUI({
        all_cols <- c(numeric_cols(), categorical_cols())
        choices <- c("None", if(length(all_cols) > 0) all_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("fill_var_", id_suffix)]]) && input[[paste0("fill_var_", id_suffix)]] %in% choices) input[[paste0("fill_var_", id_suffix)]] else "None"
        selectInput(paste0("fill_var_", id_suffix), "Variable for fill:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("size_var_ui_", id_suffix)]] <- renderUI({
        num_cols <- numeric_cols()
        choices <- c("None", if(length(num_cols) > 0) num_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("size_var_", id_suffix)]]) && input[[paste0("size_var_", id_suffix)]] %in% choices) input[[paste0("size_var_", id_suffix)]] else "None"
        selectInput(paste0("size_var_", id_suffix), "Variable for size:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("alpha_var_ui_", id_suffix)]] <- renderUI({
        num_cols <- numeric_cols()
        choices <- c("None", if(length(num_cols) > 0) num_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("alpha_var_", id_suffix)]]) && input[[paste0("alpha_var_", id_suffix)]] %in% choices) input[[paste0("alpha_var_", id_suffix)]] else "None"
        selectInput(paste0("alpha_var_", id_suffix), "Variable for alpha:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("shape_var_ui_", id_suffix)]] <- renderUI({
        cat_cols <- categorical_cols()
        choices <- c("None", if(length(cat_cols) > 0) cat_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("shape_var_", id_suffix)]]) && input[[paste0("shape_var_", id_suffix)]] %in% choices) input[[paste0("shape_var_", id_suffix)]] else "None"
        selectInput(paste0("shape_var_", id_suffix), "Variable for shape:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("linetype_var_ui_", id_suffix)]] <- renderUI({
        cat_cols <- categorical_cols()
        choices <- c("None", if(length(cat_cols) > 0) cat_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("linetype_var_", id_suffix)]]) && input[[paste0("linetype_var_", id_suffix)]] %in% choices) input[[paste0("linetype_var_", id_suffix)]] else "None"
        selectInput(paste0("linetype_var_", id_suffix), "Variable for linetype:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("label_var_ui_", id_suffix)]] <- renderUI({
        all_cols <- c(numeric_cols(), names(df)[sapply(df, is.character)])
        choices <- c("None", if(length(all_cols) > 0) all_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("label_var_", id_suffix)]]) && input[[paste0("label_var_", id_suffix)]] %in% choices) input[[paste0("label_var_", id_suffix)]] else "None"
        selectInput(paste0("label_var_", id_suffix), "Variable for label:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("group_var_ui_", id_suffix)]] <- renderUI({
        all_cols <- c(numeric_cols(), categorical_cols())
        choices <- c("None", if(length(all_cols) > 0) all_cols else character(0))
        selected_val <- if (!is.null(input[[paste0("group_var_", id_suffix)]]) && input[[paste0("group_var_", id_suffix)]] %in% choices) input[[paste0("group_var_", id_suffix)]] else "None"
        selectInput(paste0("group_var_", id_suffix), "Variable for group:", choices = choices, selected = selected_val)
      })
      
      output[[paste0("x_label_ui_", id_suffix)]] <- renderUI({
        current_x_var <- input[[paste0("x_var_", id_suffix)]]
        label_value <- if (!is.null(current_x_var) && current_x_var != "") current_x_var else ""
        textInput(paste0("x_label_", id_suffix), "X-axis Label:", value = label_value)
      })
      
      output[[paste0("y_label_ui_", id_suffix)]] <- renderUI({
        req(input[[paste0("geom_type_", id_suffix)]])
        geom <- input[[paste0("geom_type_", id_suffix)]]
        current_y_var <- input[[paste0("y_var_", id_suffix)]]
        default_y_label <- if (geom %in% c("histogram", "density", "freqpoly", "violin") || is.null(current_y_var) || current_y_var == "") "Count" else current_y_var
        textInput(paste0("y_label_", id_suffix), "Y-axis Label:", value = default_y_label)
      })
    })
  })
  
  # Dynamic UI elements for Faceting
  output$facet_row_ui <- renderUI({
    choices <- c("None", categorical_cols())
    selectInput("facet_row", "Facet by Row (optional):", choices = choices, selected = "None")
  })
  
  output$facet_col_ui <- renderUI({
    choices <- c("None", categorical_cols())
    selectInput("facet_col", "Facet by Column (optional):", choices = choices, selected = "None")
  })
  
  # Generate a single geom layer
  generate_geom_layer <- function(layer_idx, input_obj, df_data) {
    id_suffix <- as.character(layer_idx)
    
    req(input_obj[[paste0("geom_type_", id_suffix)]])
    geom_type <- input_obj[[paste0("geom_type_", id_suffix)]]
    
    x_var_input <- input_obj[[paste0("x_var_", id_suffix)]]
    x_var <- if (!is.null(x_var_input) && x_var_input != "") sym(x_var_input) else NULL
    
    y_var_input <- input_obj[[paste0("y_var_", id_suffix)]]
    y_var <- if (!is.null(y_var_input) && y_var_input != "") sym(y_var_input) else NULL
    
    # For boxplot and violin, ensure y_var is numeric
    if (geom_type %in% c("boxplot", "violin")) {
      if (is.null(y_var_input) || y_var_input == "" || y_var_input == "None" || !is.numeric(df_data[[y_var_input]])) {
        rv$warnings <- c(rv$warnings, paste("Layer", id_suffix, ": For geom_", geom_type, ", y-variable must be numeric. Skipping this layer.", sep = ""))
        return(NULL)
      }
    }
    
    aes_list <- list()
    
    if (!geom_type %in% c("hline", "vline", "abline")) {
      if (!is.null(x_var)) {
        if (geom_type %in% c("boxplot", "violin") && !is.factor(df_data[[x_var_input]])) {
          aes_list$x <- expr(factor(!!x_var))
        } else {
          aes_list$x <- x_var
        }
      }
      if (!is.null(y_var)) aes_list$y <- y_var
    }
    
    color_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "color_var")
    if (!is.null(color_res$aesthetic)) aes_list$color <- color_res$aesthetic
    
    fill_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "fill_var")
    if (!is.null(fill_res$aesthetic)) aes_list$fill <- fill_res$aesthetic
    
    size_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "size_var", "size_val")
    if (!is.null(size_res$aesthetic)) aes_list$size <- size_res$aesthetic
    
    alpha_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "alpha_var", "alpha_val")
    if (!is.null(alpha_res$aesthetic)) aes_list$alpha <- alpha_res$aesthetic
    
    shape_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "shape_var", "shape_val")
    if (!is.null(shape_res$aesthetic)) aes_list$shape <- shape_res$aesthetic
    
    if (geom_type %in% c("line", "smooth", "abline", "hline", "vline", "freqpoly", "function", "path", "segment", "curve", "spoke", "ribbon")) {
      linetype_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "linetype_var", "linetype_val")
      if (!is.null(linetype_res$aesthetic)) aes_list$linetype <- linetype_res$aesthetic
    }
    
    label_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "label_var")
    if (!is.null(label_res$aesthetic)) aes_list$label <- label_res$aesthetic
    
    group_res <- get_aesthetic_value_simplified(input_obj, id_suffix, "group_var")
    if (!is.null(group_res$aesthetic)) {
      aes_list$group <- group_res$aesthetic
    } else if (geom_type %in% c("line", "path", "smooth") && !is.null(x_var)) {
      aes_list$group <- 1
    } else if (geom_type %in% c("boxplot", "violin") && !is.null(x_var)) {
      aes_list$group <- x_var
    }
    
    geom_args <- list()
    if (length(aes_list) > 0) {
      geom_args$mapping <- aes(!!!aes_list[!sapply(aes_list, is.null)])
    }
    
    if (!is.null(size_res$fixed)) geom_args$size <- size_res$fixed
    if (!is.null(alpha_res$fixed)) geom_args$alpha <- alpha_res$fixed
    if (!is.null(shape_res$fixed)) geom_args$shape <- shape_res$fixed
    
    if (geom_type %in% c("line", "smooth", "abline", "hline", "vline", "freqpoly", "function", "path", "segment", "curve", "spoke", "ribbon")) {
      if (!is.null(linetype_res$fixed)) geom_args$linetype <- linetype_res$fixed
    }
    
    if (geom_type == "abline") {
      geom_args$intercept <- input_obj[[paste0("abline_intercept_", id_suffix)]]
      geom_args$slope <- input_obj[[paste0("abline_slope_", id_suffix)]]
      geom_args$intercept <- if(is.null(geom_args$intercept) || is.na(geom_args$intercept)) 0 else geom_args$intercept
      geom_args$slope <- if(is.null(geom_args$slope) || is.na(geom_args$slope)) 1 else geom_args$slope
    } else if (geom_type == "hline") {
      geom_args$yintercept <- input_obj[[paste0("hline_yintercept_", id_suffix)]]
      geom_args$yintercept <- if(is.null(geom_args$yintercept) || is.na(geom_args$yintercept)) 0 else geom_args$yintercept
    } else if (geom_type == "vline") {
      geom_args$xintercept <- input_obj[[paste0("vline_xintercept_", id_suffix)]]
      geom_args$xintercept <- if(is.null(geom_args$xintercept) || is.na(geom_args$xintercept)) 0 else geom_args$xintercept
    } else if (geom_type == "function") {
      fun_str <- input_obj[[paste0("function_fun_", id_suffix)]]
      if (!is.null(fun_str) && nchar(fun_str) > 0) {
        geom_args$fun <- rlang::parse_expr(fun_str)
      }
    }
    
    geom_layer <- tryCatch({
      switch(geom_type,
             "point" = do.call(geom_point, geom_args),
             "line" = do.call(geom_line, geom_args),
             "bar" = {
               if (!is.null(y_var) && y_var != "") {
                 geom_args$stat <- "identity"
                 geom_args$position <- "dodge"
               } else {
                 geom_args$stat <- "count"
                 geom_args$position <- "dodge"
               }
               do.call(geom_bar, geom_args)
             },
             "boxplot" = do.call(geom_boxplot, geom_args),
             "col" = {
               geom_args$stat = "identity"
               do.call(geom_col, geom_args)
             },
             "histogram" = {
               if (!is.null(x_var_input) && x_var_input %in% names(df_data) && is.numeric(df_data[[x_var_input]])) {
                 geom_args$binwidth <- diff(range(df_data[[x_var_input]], na.rm = TRUE)) / 30
               } else {
                 geom_args$binwidth <- 1
               }
               do.call(geom_histogram, geom_args)
             },
             "freqpoly" = {
               if (!is.null(x_var_input) && x_var_input %in% names(df_data) && is.numeric(df_data[[x_var_input]])) {
                 geom_args$binwidth <- diff(range(df_data[[x_var_input]], na.rm = TRUE)) / 30
               } else {
                 geom_args$binwidth <- 1
               }
               geom_args$geom <- "freqpoly"
               do.call(geom_histogram, geom_args)
             },
             "density" = do.call(geom_density, geom_args),
             "violin" = do.call(geom_violin, geom_args),
             "area" = do.call(geom_area, geom_args),
             "tile" = do.call(geom_tile, geom_args),
             "smooth" = do.call(geom_smooth, geom_args),
             "abline" = do.call(geom_abline, geom_args),
             "hline" = do.call(geom_hline, geom_args),
             "vline" = do.call(geom_vline, geom_args),
             "bin_2d" = do.call(geom_bin2d, geom_args),
             "blank" = geom_blank(),
             "contour" = do.call(geom_contour, geom_args),
             "contour_filled" = do.call(geom_contour_filled, geom_args),
             "count" = do.call(geom_count, geom_args),
             "density_2d" = do.call(geom_density_2d, geom_args),
             "density_2d_filled" = do.call(geom_density_2d_filled, geom_args),
             "dotplot" = do.call(geom_dotplot, geom_args),
             "errorbar" = do.call(geom_errorbar, geom_args),
             "errorbarh" = do.call(geom_errorbarh, geom_args),
             "function" = do.call(geom_function, geom_args),
             "hex" = do.call(geom_hex, geom_args),
             "jitter" = do.call(geom_jitter, geom_args),
             "crossbar" = do.call(geom_crossbar, geom_args),
             "linerange" = do.call(geom_linerange, geom_args),
             "pointrange" = do.call(geom_pointrange, geom_args),
             "map" = do.call(geom_map, geom_args),
             "path" = do.call(geom_path, geom_args),
             "polygon" = do.call(geom_polygon, geom_args),
             "qq_line" = do.call(geom_qq_line, geom_args),
             "qq" = do.call(geom_qq, geom_args),
             "quantile" = do.call(geom_quantile, geom_args),
             "ribbon" = do.call(geom_ribbon, geom_args),
             "rug" = do.call(geom_rug, geom_args),
             "segment" = do.call(geom_segment, geom_args),
             "curve" = do.call(geom_curve, geom_args),
             "spoke" = do.call(geom_spoke, geom_args),
             "label" = do.call(geom_label, geom_args),
             "text" = do.call(geom_text, geom_args),
             "raster" = do.call(geom_raster, geom_args),
             "rect" = do.call(geom_rect, geom_args),
             "sf" = do.call(geom_sf, geom_args),
             "sf_label" = do.call(geom_sf_label, geom_args),
             "sf_text" = do.call(geom_sf_text, geom_args)
      )
    }, error = function(e) {
      rv$warnings <- c(rv$warnings, paste("Layer", id_suffix, ": Error creating geom layer:", e$message))
      NULL
    })
    
    list(aes_list = aes_list, geom_layer = geom_layer)
  }
  
  # Reactive expression for statistical tests
  stat_test_results <- reactive({
    if (!isTRUE(input$add_stat_tests) || !input$geom_type_1 %in% c("boxplot", "violin")) {
      return(NULL)
    }
    
    x_var <- input$x_var_1
    y_var <- input$y_var_1
    test_type <- input$stat_test_type
    alpha <- input$stat_test_alpha
    
    if (is.null(x_var) || is.null(y_var) || x_var == "" || y_var == "" || y_var == "None") {
      rv$warnings <- c(rv$warnings, "Statistical test: X and Y variables must be selected.")
      return(NULL)
    }
    
    if (!is.numeric(df[[y_var]])) {
      rv$warnings <- c(rv$warnings, "Statistical test: Y-variable must be numeric.")
      return(NULL)
    }
    
    if (!is.factor(df[[x_var]]) || nlevels(df[[x_var]]) != 2) {
      rv$warnings <- c(rv$warnings, "Statistical test: X-variable must be a categorical variable with exactly two levels.")
      return(NULL)
    }
    
    group1 <- df[df[[x_var]] == levels(df[[x_var]])[1], y_var]
    group2 <- df[df[[x_var]] == levels(df[[x_var]])[2], y_var]
    
    if (length(group1) < 2 || length(group2) < 2 || any(is.na(group1)) || any(is.na(group2))) {
      rv$warnings <- c(rv$warnings, "Statistical test: Each group must have at least two non-NA observations.")
      return(NULL)
    }
    
    tryCatch({
      if (test_type == "t-test") {
        test_result <- t.test(group1, group2, conf.level = 1 - alpha)
        result <- list(
          test_name = "t-test",
          statistic = test_result$statistic,
          p_value = test_result$p.value,
          significant = test_result$p.value < alpha,
          groups = paste(levels(df[[x_var]])[1], "vs", levels(df[[x_var]])[2])
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
          groups = paste(levels(df[[x_var]])[1], "vs", levels(df[[x_var]])[2])
        )
      }
      return(result)
    }, error = function(e) {
      rv$warnings <- c(rv$warnings, paste("Statistical test error:", e$message))
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
    sprintf("%s results (%s): Statistic = %.3f, p-value = %.4f (%s at alpha = %.2f)",
            result$test_name, result$groups, result$statistic, result$p_value, sig_text, input$stat_test_alpha)
  })
  
  # Reactive expression to generate the full ggplot object
  ggplot_plot <- reactive({
    req(input$x_var_1)
    
    layer1_aes_list <- list(x = sym(input$x_var_1))
    if (!is.null(input$y_var_1) && input$y_var_1 != "" && input$y_var_1 != "None" && (!input$geom_type_1 %in% c("boxplot", "violin") || is.numeric(df[[input$y_var_1]]))) {
      layer1_aes_list$y <- sym(input$y_var_1)
    }
    
    color_res <- get_aesthetic_value_simplified(input, "1", "color_var")
    if (!is.null(color_res$aesthetic)) layer1_aes_list$color <- color_res$aesthetic
    
    fill_res <- get_aesthetic_value_simplified(input, "1", "fill_var")
    if (!is.null(fill_res$aesthetic)) layer1_aes_list$fill <- fill_res$aesthetic
    
    p <- ggplot(data = df, mapping = aes(!!!layer1_aes_list[!sapply(layer1_aes_list, is.null)]))
    
    for (i in 1:rv$num_layers) {
      geom_type_id <- paste0("geom_type_", i)
      x_var_id <- paste0("x_var_", i)
      if (is.null(input[[geom_type_id]]) || is.null(input[[x_var_id]])) {
        next
      }
      
      layer_result <- generate_geom_layer(i, input, df)
      if (!is.null(layer_result$geom_layer)) {
        p <- p + layer_result$geom_layer
      }
      
      if (!is.null(input[[paste0("layer_title_", i)]]) && input[[paste0("layer_title_", i)]] != "") {
        p <- p + labs(title = input[[paste0("layer_title_", i)]])
      }
      if (!is.null(input[[paste0("layer_subtitle_", i)]]) && input[[paste0("layer_subtitle_", i)]] != "") {
        p <- p + labs(subtitle = input[[paste0("layer_subtitle_", i)]])
      }
      if (!is.null(input[[paste0("layer_caption_", i)]]) && input[[paste0("layer_caption_", i)]] != "") {
        p <- p + labs(caption = input[[paste0("layer_caption_", i)]])
      }
      if (!is.null(input[[paste0("x_label_", i)]]) && input[[paste0("x_label_", i)]] != "") {
        p <- p + labs(x = input[[paste0("x_label_", i)]])
      }
      if (!is.null(input[[paste0("y_label_", i)]]) && input[[paste0("y_label_", i)]] != "") {
        p <- p + labs(y = input[[paste0("y_label_", i)]])
      }
      
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
      
      xlim <- c(input[[paste0("xmin_", i)]], input[[paste0("xmax_", i)]])
      ylim <- c(input[[paste0("ymin_", i)]], input[[paste0("ymax_", i)]])
      
      if (any(!is.na(xlim)) || any(!is.na(ylim))) {
        p <- p + coord_cartesian(
          xlim = if (any(!is.na(xlim))) xlim,
          ylim = if (any(!is.na(ylim))) ylim,
          default = TRUE
        )
      }
    }
    
    if (isTRUE(input$add_stat_summary) && !is.null(input$x_var_1) && !is.null(input$y_var_1) && input$y_var_1 != "" && input$y_var_1 != "None" && is.numeric(df[[input$y_var_1]])) {
      if (input$stat_summary_fun == "mean") {
        p <- p + stat_summary(fun = mean, geom = input$stat_summary_geom, width = input$stat_summary_width, colour = "red")
      } else if (input$stat_summary_fun == "median") {
        p <- p + stat_summary(fun = median, geom = input$stat_summary_geom, width = input$stat_summary_width, colour = "blue")
      }
    }
    
    if (!is.null(input$facet_row) && input$facet_row != "None" || !is.null(input$facet_col) && input$facet_col != "None") {
      facet_formula_str <- paste(input$facet_row, "~", input$facet_col)
      facet_formula_str <- gsub("None ~ ", "~ ", facet_formula_str)
      facet_formula_str <- gsub(" ~ None", " ~ ", facet_formula_str)
      facet_formula_str <- gsub("None", ".", facet_formula_str)
      p <- p + facet_grid(as.formula(facet_formula_str), scales = input$facet_scales)
    }
    
    if (!is.null(input$plot_theme)) {
      p <- p + do.call(input$plot_theme, list())
    }
    
    # Apply custom theme settings
    custom_theme <- theme_settings()
    if (!is.null(custom_theme)) {
      p <- p + do.call(theme, custom_theme)
    }
    
    return(p)
  })
  
  # Render warnings
  output$warnings <- renderText({
    paste(rv$warnings, collapse = "\n")
  })
  
  # Render the plot using plotly
  output$plot <- renderPlotly({
    p <- ggplot_plot()
    if (!is.null(p)) {
      ggplotly(p)
    } else {
      NULL
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)