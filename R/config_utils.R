# ==============================================================================
# CONFIGURATION MANAGEMENT FUNCTIONS
# ==============================================================================
#' Load analysis configuration
#'
#' @param config_file Path to YAML config file (default: "analysis_config.yml")
#' @param env Environment to load (default: NULL). If provided, overrides base config.
#' @return List with configuration parameters
#' @export
load_config <- function(config_file = "analysis_config.yml", env = NULL) {
  # Load base configuration
  base_config <- yaml::read_yaml(
    file.path("config", config_file)
  )

  # Load environment-specific overrides if provided
  if (!is.null(env)) {
    env_file <- file.path("config", paste0(env, ".yml"))
    if (file.exists(env_file)) {
      env_config <- yaml::read_yaml(env_file)
      base_config <- utils::modifyList(base_config, env_config)
    } else {
      warning("Environment config file not found: ", env_file)
    }
  }

  # Set additional derived parameters
  base_config$computed <- list(
    full_data_path = normalizePath(base_config$data_input$data_path, mustWork = FALSE),
    timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  # Validate critical configuration
  validate_config(base_config)

  return(base_config)
}

#' Validate configuration parameters
#' @keywords internal
validate_config <- function(config) {
  # Check required fields
  required <- c("data_input", "output", "analysis")
  missing <- setdiff(required, names(config))
  if (length(missing) > 0) {
    stop("Missing required configuration sections: ", paste(missing, collapse = ", "))
  }

  # Validate data path exists (if we're not in development mode)
  if (!grepl("test|dev", config$data_input$data_path, ignore.case = TRUE)) {
    if (!dir.exists(config$data_input$data_path)) {
      stop("Data directory does not exist: ", config$data_input$data_path)
    }
  }

  # Validate numeric parameters
  if (config$analysis$n_cores > parallel::detectCores()) {
    warning("Requested cores (", config$analysis$n_cores,
            ") exceeds available cores (", parallel::detectCores(), ")")
  }

  invisible(TRUE)
}

#' Save configuration with results for reproducibility
#' @param config configuration file to be saved
#' @param output_dir path where configuration file is going to be saved to
#' @importFrom yaml write_yaml
#' @export
save_config_with_results <- function(config, output_dir) {
  config_copy <- config
  config_copy$computed <- NULL  # Remove computed fields

  output_file <- file.path(
    output_dir,
    paste0("analysis_config_", config$computed$timestamp, ".yml")
  )

  yaml::write_yaml(config_copy, output_file)
  message("Configuration saved to: ", output_file)
}

#' Create a new configuration template
#' @param output_file file name and path of the new configuration file
#' @importFrom yaml write_yaml
#' @export
create_config_template <- function(output_file = "config/my_analysis.yml") {
  template <- list(
    data_input = list(
      data_path = "data/raw",
      file_pattern = "\\.csv$",
      recursive_search = TRUE,
      expected_file_count = NA
    ),
    background = list(
      method = "interpolated_baseline",
      interpolation_points = c(50, 150, 250, 350),
      polynomial_degree = 2
    ),
    area_calculation = list(
      integration_method = "spline",
      x_range = c(100, 400)
    ),
    output = list(
      output_path = "results",
      save_intermediate = TRUE
    )
  )

  yaml::write_yaml(template, output_file)
  message("Configuration template created: ", output_file)
}
