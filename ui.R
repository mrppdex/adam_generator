# ui.R
# Defines the User Interface for the ADaM Generator Shiny App

# Required R packages
# install.packages("shiny", "shinythemes", "DT")

library(shiny)
library(shinythemes) # For professional-looking themes
library(DT)

shinyUI(fluidPage(
    # Apply a dark theme similar to the drawing board
    theme = shinytheme("cyborg"),
    
    # Custom CSS for a more polished look
    tags$head(
        tags$style(HTML("
            body {
                font-family: 'Inter', sans-serif;
            }
            .navbar-brand {
                font-weight: 600;
            }
            .well {
                background-color: #2a2a2a;
                border: 1px solid #444;
            }
            .btn-primary {
                background-color: #4f46e5;
                border-color: #4f46e5;
            }
            .btn-primary:hover {
                background-color: #4338ca;
                border-color: #4338ca;
            }
            .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
                background-color: #2a2a2a;
                border-color: #444;
            }
            #log_output {
                background-color: #1e1e1e;
                border: 1px solid #444;
                color: #d4d4d4;
                font-family: 'Fira Code', monospace;
                font-size: 0.85em;
                white-space: pre-wrap;
                word-wrap: break-word;
                height: 300px;
                padding: 10px;
            }
        "))
    ),

    # Application Title
    titlePanel("ADaM Dataset Generator"),

    # Sidebar layout similar to the drawing board
    sidebarLayout(
        # Sidebar Panel for Inputs
        sidebarPanel(
            width = 3,
            h4("1. Upload Specification"),
            fileInput("spec_file", "Upload YAML Spec File (.yml)",
                      accept = c(".yml", ".yaml")),
            
            hr(),
            
            h4("2. Upload SDTM Data"),
            fileInput("sdtm_files", "Upload SDTM Datasets (.csv, .xpt)",
                      multiple = TRUE,
                      accept = c(".csv", ".xpt", "text/csv", "application/x-sas-xport")),
            
            hr(),

            h4("3. Generate Datasets"),
            p("Once files are uploaded, click here to start processing.", class = "text-muted"),
            actionButton("generate_btn", "Generate ADaM Datasets", 
                         icon = icon("cogs"), class = "btn-primary btn-lg btn-block")
        ),

        # Main Panel for Outputs
        mainPanel(
            width = 9,
            tabsetPanel(
                id = "main_tabs",
                # Tab for the processing log
                tabPanel("Processing Log", 
                         h3("Log"),
                         verbatimTextOutput("log_output")
                ),
                # Tab for the generated ADaM datasets and download buttons
                tabPanel("Generated ADaM Datasets", 
                         h3("Results"),
                         uiOutput("adam_outputs")
                )
            )
        )
    )
))

