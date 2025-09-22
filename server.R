# server.R
# Contains the server-side logic for the ADaM Generator Shiny App

# Required R packages
# install.packages(c("shiny", "yaml", "dplyr", "purrr", "haven", "readr", "DT", "rlang", "tools"))

library(shiny)
library(yaml)
library(dplyr)
library(purrr)
library(haven)
library(readr)
library(DT)
library(rlang)
library(tools)

# Define the server logic
shinyServer(function(input, output, session) {

    # Use reactiveValues to store application state that can be safely shared
    # between reactive contexts.
    sdtm_data <- reactiveValues(datasets = list())
    adam_data <- reactiveValues(datasets = list())
    log_messages <- reactiveValues(log = c("Application log initialized."))

    # --- Helper Functions ---
    # A simple function to append messages to the log
    log_message <- function(message) {
        log_messages$log <- c(log_messages$log, paste(format(Sys.time(), "%H:%M:%S"), "-", message))
    }

    # --- Observers for File Inputs ---

    # This observer triggers whenever the user uploads SDTM files.
    # It reads each file and stores it in the sdtm_data reactive value.
    observeEvent(input$sdtm_files, {
        req(input$sdtm_files)
        log_message("New SDTM files detected. Reading data...")
        
        walk(1:nrow(input$sdtm_files), function(i) {
            file <- input$sdtm_files[i, ]
            # Get the dataset name by removing the file extension (e.g., "dm.csv" -> "DM")
            dataset_name <- toupper(file_path_sans_ext(file$name))
            
            tryCatch({
                df <- switch(file_ext(file$name),
                    "csv" = read_csv(file$datapath, show_col_types = FALSE),
                    "xpt" = read_xpt(file$datapath)
                )
                sdtm_data$datasets[[dataset_name]] <- df
                log_message(sprintf("Successfully loaded '%s' with %d rows and %d columns.",
                                    file$name, nrow(df), ncol(df)))
            }, error = function(e) {
                log_message(sprintf("ERROR: Failed to read '%s'. Details: %s", file$name, e$message))
            })
        })
    })

    # --- Main Processing Logic ---

    # This observer triggers when the "Generate ADaM Datasets" button is clicked.
    observeEvent(input$generate_btn, {
        # 1. Validation: Ensure required files are uploaded
        if (is.null(input$spec_file)) {
            showModal(modalDialog(title = "Error", "Please upload a YAML specification file.", easyClose = TRUE))
            return()
        }
        if (length(sdtm_data$datasets) == 0) {
            showModal(modalDialog(title = "Error", "Please upload at least one SDTM source dataset.", easyClose = TRUE))
            return()
        }

        # 2. Initialization
        log_message("--- Starting ADaM Generation Process ---")
        adam_data$datasets <- list() # Clear previous results
        
        # 3. Parse the YAML specification
        spec <- tryCatch({
            log_message(paste("Reading specification file:", input$spec_file$name))
            read_yaml(input$spec_file$datapath)
        }, error = function(e) {
            log_message(paste("ERROR: Invalid YAML format.", e$message))
            NULL
        })
        if (is.null(spec)) return()

        # 4. Filter for ADaM datasets in the spec
        adam_specs <- keep(spec$datasets, ~ .x$type == "ADaM")
        log_message(sprintf("Found %d ADaM dataset(s) to generate in the specification.", length(adam_specs)))
        
        # 5. Loop through each ADaM spec and generate the dataset
        walk(adam_specs, function(adam_spec) {
            tryCatch({
                log_message(sprintf("--- Generating: %s ---", adam_spec$name))

                # Identify all unique source datasets required for this ADaM dataset
                source_names <- adam_spec$columns %>%
                    map("derivation") %>%
                    map("sources") %>%
                    unlist() %>%
                    map_chr(~ strsplit(.x, "\\.")[[1]][1]) %>%
                    unique()

                log_message(paste("Required sources:", paste(source_names, collapse = ", ")))
                
                # Check if all required source datasets have been uploaded
                missing_sources <- setdiff(source_names, names(sdtm_data$datasets))
                if (length(missing_sources) > 0) {
                    stop(paste("Missing required source data for:", paste(missing_sources, collapse = ", ")))
                }
                
                # Heuristic to find the "base" dataset (most common source) to start the join
                # This is typically the subject-level dataset like DM or ADSL (if it were a source).
                base_dataset_name <- names(which.max(table(unlist(map(adam_spec$columns, ~.x$derivation$sources)))))
                base_dataset_name <- strsplit(base_dataset_name, "\\.")[[1]][1]
                
                log_message(paste("Using", base_dataset_name, "as the base for joining."))
                
                # Start with the base dataset
                merged_df <- sdtm_data$datasets[[base_dataset_name]]
                
                # Sequentially left_join the other required source datasets
                other_sources <- setdiff(source_names, base_dataset_name)
                if(length(other_sources > 0)) {
                  for (source in other_sources) {
                      log_message(paste("Joining with", source))
                      # A common-sense join by standard CDISC keys. This may need adjustment
                      # for complex, non-standard joins.
                      merged_df <- left_join(merged_df, sdtm_data$datasets[[source]], by = c("STUDYID", "USUBJID"))
                  }
                }

                log_message("All sources merged. Applying column derivations...")

                # Apply derivations column by column
                # This approach is robust to errors in one derivation.
                derived_df <- merged_df
                for (col in adam_spec$columns) {
                    logic <- col$derivation$logic
                    if (!is.null(logic) && logic != "") {
                        log_message(paste("Deriving column:", col$name))
                        tryCatch({
                            # Use dplyr::mutate with rlang's := and parse_expr for robust
                            # programmatic creation of columns from text expressions.
                            derived_df <- derived_df %>%
                                mutate(!!col$name := !!parse_expr(logic))
                        }, error = function(e) {
                             log_message(sprintf("--> WARNING: Failed to derive '%s'. Logic: '%s'. Error: %s",
                                                col$name, logic, e$message))
                        })
                    }
                }
                
                # Select only the columns specified for the final dataset
                final_adam_cols <- map_chr(adam_spec$columns, "name")
                final_df <- derived_df %>% select(any_of(final_adam_cols))
                
                adam_data$datasets[[adam_spec$name]] <- final_df
                log_message(sprintf("--- Successfully generated '%s' with %d rows and %d columns. ---",
                                    adam_spec$name, nrow(final_df), ncol(final_df)))

            }, error = function(e) {
                log_message(sprintf("FATAL ERROR generating '%s': %s", adam_spec$name, e$message))
            })
        })
        
        # Switch the view to the results tab
        updateTabsetPanel(session, "main_tabs", selected = "Generated ADaM Datasets")
    })

    # --- Dynamic UI and Output Rendering ---

    # Render the processing log
    output$log_output <- renderText({
        paste(log_messages$log, collapse = "\n")
    })

    # Dynamically create UI elements (previews and download buttons) for each
    # generated ADaM dataset.
    output$adam_outputs <- renderUI({
        req(length(adam_data$datasets) > 0)

        # Create a UI block for each dataset
        map(names(adam_data$datasets), function(name) {
            fluidRow(
                column(12,
                    h3(name),
                    # Each download button and table needs a unique ID
                    downloadButton(paste0("download_", name), "Download as CSV"),
                    downloadButton(paste0("download_xpt_", name), "Download as XPT"),
                    hr(),
                    DTOutput(paste0("table_", name)),
                    hr()
                )
            )
        })
    })

    # Dynamically create the server-side logic for the download handlers and
    # table previews for each generated dataset.
    observe({
        req(length(adam_data$datasets) > 0)

        walk(names(adam_data$datasets), function(name) {
            # Logic for CSV download
            output[[paste0("download_", name)]] <- downloadHandler(
                filename = function() { paste0(tolower(name), ".csv") },
                content = function(file) {
                    write_csv(adam_data$datasets[[name]], file)
                }
            )
            # Logic for XPT download
            output[[paste0("download_xpt_", name)]] <- downloadHandler(
                filename = function() { paste0(tolower(name), ".xpt") },
                content = function(file) {
                    write_xpt(adam_data$datasets[[name]], file)
                }
            )
            # Logic for rendering the preview table
            output[[paste0("table_", name)]] <- renderDT({
                datatable(adam_data$datasets[[name]],
                          options = list(scrollX = TRUE, pageLength = 5),
                          rownames = FALSE)
            })
        })
    })
})
