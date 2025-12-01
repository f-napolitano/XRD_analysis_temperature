# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

#' This is the main analysis function (orchestration) that is called by the runner. It takes a configuration file and an environment to proceed with the complete analysis pipeline.
#' @param config_file Character, name of the configuration file to be used
#' @param env Character, environment to be used (production == NULL, development, testing)
#' @param override_list List, a list that overrides the config_file in case to be needed
#' @importFrom utils modifyList
run_complete_analysis <- function(config_file = "analysis_config.yml",
                                  env = NULL,
                                  override_list = NULL) {
  # Load configuration
  config <- load_config(config_file, env)

  # Apply any command-line overrides
  if (!is.null(override_list)) {
    config <- utils::modifyList(config, override_list)
  }

  message("Starting analysis with configuration:")
  message("  Environment: ", ifelse(is.null(env), "base", env))
  message("  Sample: ", config$sample$name)
  message("  Measurement: ", config$sample$measurement)
  message("  Data path: ", config$data_input$data_path)

  # Create output directory
  output_dir <- file.path(
    config$output$output_path,
    paste0("run_", config$computed$timestamp)
  )

  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # save configuration file for reproducibility
  save_config_with_results(config, output_dir)

  # ========== MAIN ANALYSIS PIPELINE ==========
  message("\n[1/5] Loading data files...")

  patterns <- import_all_data(config)

  if (config$parallel$enabled) {
    # Step 2 (2a, 2b, 2c, 2d): Process all files (using the function from R/analysis_core.R)
    message("\n[2/5] Processing files through parallel analysis pipeline...")

    # flattening input (WARNING imap_dfr is superseeded, need to be changed to map + list_rbind approach in a future revision)
    result_df <- purrr::imap_dfr(patterns, function(direction_data, direction_name) {
      purrr::imap_dfr(direction_data, function(inner_list, index) {
        data.frame(
          name = inner_list$fileName,
          temperature = inner_list$Temperature,
          direction = direction_name,
          inner_list$pattern,
          stringsAsFactors = FALSE
          )
        })
      })

    data_by_file <- split(result_df, result_df$name)

    processed_results <- future.apply::future_lapply(data_by_file, function(file_data) {
      # Each worker gets all rows for one file
      area <- area_function(
        file_data,
        xmin = config$area_calculation$peak_min,
        xmax = config$area_calculation$peak_max,
        alpha_beta = config$area_calculation$alpha_beta,
        config = config
        )
      return(area)
      }, future.seed = TRUE)

  }
  else
  {
    # Step 2 (2a, 2b, 2c, 2d): Process all files (using the function from R/analysis_core.R)
    message("\n[2/5] Processing files through sequential analysis pipeline...")

    count_dataframes <- function(x) {
      if (is.data.frame(x)) return(1)
      if (!is.list(x)) return(0)

      sum(purrr::map_dbl(x, count_dataframes)) }

    pb <- progress::progress_bar$new(total = count_dataframes(patterns),
                                     format = "[:bar] :percent :eta")

    processed_results <- purrr::map_depth(patterns, 2, ~ {
      pb$tick()
      .x$pattern <- area_function(.x$pattern,
                                  config$area_calculation$peak_min,
                                  config$area_calculation$peak_max,
                                  config$area_calculation$alpha_beta,
                                  config)
      .x
    })
  }


  # Step 3: Results Aggregation
  message("\n[3/5] Aggregating results...")
  results_lst_short <- extract_results_function(processed_results, config$parallel$enabled)

  # Step 4: Exporting results to files
  message("\n[4/5] Exporting results to files")
  if (config$parallel$enabled) {
    export_parallel_csv(results_lst_short,
                        output_dir,
                        config$sample$name,
                        config$sample$measurement)
  } else {
    export_sequential_csv(results_lst_short,
                          output_dir,
                          config$sample$name,
                          config$sample$measurement)
  }

  # Step 4: Plotting results and exporting to png file
  message("\n[5/5] Ploting results and exporting plot to file")
  plotgg <- single_plot_function(results_lst_short,
                                 parallel = config$parallel$enabled,
                                 output_dir,
                                 config$output$plot[1])

  message("\nAnalysis complete!")
  message("Results saved to: ", output_dir)
  message("Files processed: ", sum(lengths(patterns)))

  return(list(
    results_calculations = processed_results,
    results_for_plot = results_lst_short,
    config = config,
    output_dir = output_dir,
    plot = plotgg))

}


#results <- microbenchmark::microbenchmark(
#  traditional = results_lst(patterns),
#  parallel = processed_files(data_by_file),
#  times = 30  # Number of repetitions
#)

#print(results)
#autoplot(results)  # Visual comparison
