# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

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

  patterns <- lapply(config$data_input$direction, function(dir) {
    filenames <- list.files(paste0(config$data_input$data_path, "/", dir, "/"),
                            pattern = config$data_input$file_pattern,
                            full.names = TRUE)
    pb <- progress::progress_bar$new(total = length(filenames),
                                     format = "[:bar] :percent :eta")
    lapply(filenames, function(file) {
      pb$tick()
      import_xrdml(file)
    })
  })

  names(patterns) <- config$data_input$direction

  # Step 2 (2a, 2b, 2c, 2d): Process all files (using the function from R/analysis_core.R)
  message("\n[2/5] Processing files through analysis pipeline...")

  count_dataframes <- function(x) {
    if (is.data.frame(x)) return(1)
    if (!is.list(x)) return(0)

    sum(purrr::map_dbl(x, count_dataframes))
  }

  pb <- progress::progress_bar$new(total = count_dataframes(patterns),
                                   format = "[:bar] :percent :eta")

  results_lst <- purrr::map_depth(patterns, 2, ~ {
    pb$tick()
    .x$pattern <- area_function(.x$pattern,
                                config$area_calculation$peak_min,
                                config$area_calculation$peak_max,
                                config$area_calculation$alpha_beta)
    .x
  })

  # Step 3: Results Aggregation
  message("\n[3/5] Aggregating results...")
  results_lst_short <- extract_results_function(results_lst)

  # Step 4: Exporting results to files
  message("\n[4/5] Exporting results to files")
  purrr::iwalk(results_lst_short,
               ~ readr::write_csv(.x,
                                  file = paste0(output_dir, "/",
                                                config$sample$name, "_",
                                                config$sample$measurement, "_",
                                                .y, ".csv")))

  # Step 4: Plotting results and exporting to png file
  message("\n[5/5] Ploting results and exporting plot to file")
  plotgg <- single_plot_function(results_lst_short, method = "list", output_dir)

  message("\nAnalysis complete!")
  message("Results saved to: ", output_dir)
  message("Files processed: ", sum(lengths(patterns)))

  return(list(
    raw_result = results_lst,
    results = results_lst_short,
    config = config,
    output_dir = output_dir,
    plot = plotgg))
}
