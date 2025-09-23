# ui.R
# Defines the user interface for the ADaM Generator Shiny App

# Required R packages for the app to run
# Make sure to install them before launching:
# install.packages(c("shiny", "shinythemes", "DT", "haven", "arrow"))

library(shiny)
library(shinythemes)

# Define the user interface using a fluidPage layout
shinyUI(fluidPage(
    # Use the "Sandstone" theme for a clean, professional look
    theme = shinytheme("sandstone"),

    # Application title panel
    titlePanel(
        windowTitle = "ADaM Generator",
        title = div(
            img(src = "https://www.cdisc.org/sites/default/files/cdisc-logo-color.png", height = 50, style = "margin-right: 20px;"),
            "ADaM Dataset Generator from Specification"
        )
    ),

    # Sidebar layout with input and output definitions
    sidebarLayout(
        # Sidebar panel for user inputs
        sidebarPanel(
            h4("1. Upload Specification"),
            # Input for uploading the YAML specification file
            fileInput("spec_file", "Upload Mapping Specification (.yml or .yaml)",
                      accept = c(".yaml", ".yml"),
                      placeholder = "No spec file selected"),

            hr(),

            h4("2. Upload Source Data"),
            # Input for uploading multiple SDTM datasets
            fileInput("sdtm_files", "Upload SDTM Datasets (.csv, .xpt, .sas7bdat)",
                      multiple = TRUE,
                      accept = c(".csv", ".xpt", ".sas7bdat"),
                      placeholder = "No data files selected"),

            hr(),

            h4("3. Generate Datasets"),
            # Action button to trigger the ADaM generation process
            # Using a button prevents the app from re-calculating on every file change
            actionButton("generate_btn", "Generate ADaM Datasets",
                         icon = icon("cogs"),
                         class = "btn-primary btn-lg btn-block"),

            hr(),

            # Informational text to guide the user
            helpText("Instructions: Upload a YAML spec, then upload all necessary SDTM source datasets. Click 'Generate' to create the ADaM datasets.")
        ),

        # Main panel for displaying outputs
        mainPanel(
            # Organize outputs into tabs for clarity
            tabsetPanel(
                id = "main_tabs",
                # Tab 1: Welcome and instructions
                tabPanel("Overview",
                         icon = icon("info-circle"),
                         h3("Welcome to the ADaM Generator"),
                         p("This application automates the creation of ADaM datasets based on a formal specification file."),
                         tags$ol(
                             tags$li(strong("Upload Specification:"), " Provide the YAML file exported from the CDISC Drawing Board tool."),
                             tags$li(strong("Upload Source Data:"), " Upload all the SDTM datasets that are listed as sources in your specification."),
                             tags$li(strong("Generate Datasets:"), " Click the button in the sidebar to start the process."),
                             tags$li(strong("Review & Download:"), " The 'Generated ADaM Datasets' tab will populate with previews and download buttons for each successfully created dataset.")
                         )
                ),
                # Tab 2: Dynamically generated outputs for ADaM datasets
                tabPanel("Generated ADaM Datasets",
                         icon = icon("table"),
                         # This UI element will be populated by the server
                         uiOutput("adam_outputs")
                ),
                # Tab 3: Log viewer for status updates and error messages
                tabPanel("Processing Log",
                         icon = icon("terminal"),
                         h4("Live Log"),
                         # Shows a monospace text block with processing messages
                         verbatimTextOutput("log_output", placeholder = TRUE)
                )
            )
        )
    )
))

