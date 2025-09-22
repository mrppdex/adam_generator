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
                current_adam_name <- adam_spec$name
                log_message(sprintf("--- Generating: %s ---", current_adam_name))

                # Identify all unique source domains from the spec
                all_source_domains <- adam_spec$columns %>%
                    map("derivation") %>%
                    map("sources") %>%
                    unlist() %>%
                    map_chr(~ strsplit(.x, "\\.")[[1]][1]) %>%
                    unique()

                # Distinguish between external SDTM sources and internal ADaM sources (itself)
                external_source_names <- setdiff(all_source_domains, current_adam_name)
                
                log_message(paste("Required external sources:", paste(external_source_names, collapse = ", ")))
                
                # Check if all required *external* source datasets have been uploaded
                missing_sources <- setdiff(external_source_names, names(sdtm_data$datasets))
                if (length(missing_sources) > 0) {
                    stop(paste("Missing required source data for:", paste(missing_sources, collapse = ", ")))
                }
                
                if (length(external_source_names) == 0) {
                    stop(paste("Specification for", current_adam_name, "does not list any external SDTM sources."))
                }

                # Find the most frequently mentioned external source to use as the base for the join.
                all_external_sources_list <- adam_spec$columns %>%
                    map("derivation") %>% map("sources") %>% unlist() %>%
                    map_chr(~ strsplit(.x, "\\.")[[1]][1]) %>%
                    keep(~ .x %in% external_source_names)

                if (length(all_external_sources_list) == 0) {
                   base_dataset_name <- external_source_names[1]
                } else {
                   base_dataset_name <- names(which.max(table(all_external_sources_list)))
                }

                log_message(paste("Using", base_dataset_name, "as the base for joining."))
                
                merged_df <- sdtm_data$datasets[[base_dataset_name]]
                
                # Sequentially left_join the other required source datasets
                other_sources <- setdiff(external_source_names, base_dataset_name)
                if(length(other_sources > 0)) {
                  for (source in other_sources) {
                      join_keys_spec <- adam_spec$join_keys
                      
                      if (is.null(join_keys_spec) || length(join_keys_spec) == 0) {
                          log_message("WARNING: 'join_keys' not specified in YAML. Defaulting to common CDISC keys.")
                          default_keys <- c("STUDYID", "USUBJID", "SUBJID", "SITEID")
                          keys_to_use <- intersect(default_keys, intersect(names(merged_df), names(sdtm_data$datasets[[source]])))
                      } else {
                          keys_to_use <- intersect(join_keys_spec, intersect(names(merged_df), names(sdtm_data$datasets[[source]])))
                      }

                      if (length(keys_to_use) == 0) {
                          stop(sprintf("Cannot join '%s' with '%s'. No common keys found based on spec or defaults.",
                                       base_dataset_name, source))
                      }
                      
                      log_message(sprintf("Joining with '%s' using keys: [%s]", source, paste(keys_to_use, collapse=", ")))
                      merged_df <- left_join(merged_df, sdtm_data$datasets[[source]], by = keys_to_use)
                  }
                }

                log_message("All sources merged. Applying column derivations...")

                derived_df <- merged_df
                for (col in adam_spec$columns) {
                    logic <- col$derivation$logic
                    if (!is.null(logic) && logic != "") {
                        log_message(paste("Deriving column:", col$name))
                        tryCatch({
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

                # **FIX**: Handle one-row-per-subject requirement
                if (!is.null(adam_spec$one_row_per_subject) && adam_spec$one_row_per_subject) {
                    log_message("Dataset is one-row-per-subject. Removing duplicate subject entries.")
                    if ("USUBJID" %in% names(final_df)) {
                        final_df <- final_df %>%
                            distinct(USUBJID, .keep_all = TRUE)
                    } else {
                        log_message("--> WARNING: 'USUBJID' not found in the final dataset. Cannot enforce one-row-per-subject rule.")
                    }
                }
                
                adam_data$datasets[[adam_spec$name]] <- final_df
                log_message(sprintf("--- Successfully generated '%s' with %d rows and %d columns. ---",
                                    adam_spec$name, nrow(final_df), ncol(final_df)))

            }, error = function(e) {
                log_message(sprintf("FATAL ERROR generating '%s': %s", adam_spec$name, e$message))
            })
        })
        
        updateTabsetPanel(session, "main_tabs", selected = "Generated ADaM Datasets")
    })

    # --- Dynamic UI and Output Rendering ---
    output$log_output <- renderText({ paste(log_messages$log, collapse = "\n") })

    output$adam_outputs <- renderUI({
        req(length(adam_data$datasets) > 0)
        map(names(adam_data$datasets), function(name) {
            fluidRow(column(12, h3(name),
                downloadButton(paste0("download_", name), "Download as CSV"),
                downloadButton(paste0("download_xpt_", name), "Download as XPT"),
                hr(), DTOutput(paste0("table_", name)), hr()
            ))
        })
    })

    observe({
        req(length(adam_data$datasets) > 0)
        walk(names(adam_data$datasets), function(name) {
            output[[paste0("download_", name)]] <- downloadHandler(
                filename = function() { paste0(tolower(name), ".csv") },
                content = function(file) { write_csv(adam_data$datasets[[name]], file) }
            )
            output[[paste0("download_xpt_", name)]] <- downloadHandler(
                filename = function() { paste0(tolower(name), ".xpt") },
                content = function(file) { write_xpt(adam_data$datasets[[name]], file) }
            )
            output[[paste0("table_", name)]] <- renderDT({
                datatable(adam_data$datasets[[name]], options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
            })
        })
    })
})

