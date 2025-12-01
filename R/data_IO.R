#' Load a difraction pattern from a panalytical file (.xrdml) measured in a non-ambient setup,
#' extract the proper diffraction pattern (2theta, intensity) and store it in a data frame,
#' extract the temperature and store it an double. Return a list with (pattern, temperature).
#'
#' @param file_name Complete path and file name of the .xrdml file wanted to be imported
#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_text
#' @return A list with a data frame and a double.
#'         Data frame is the XRD pattern (2 columns: 2theta, intensity)
#'         Double is the temperature where pattern was acquired
#' @export
#'
#' @examples
#' \dontrun{
#' difractogram_data <- import_xrdml("/data/sample/xrd_pattern.xrdml")
#' }

import_xrdml <- function(file_name) {

  tryCatch( {
    pattern <- xml2::read_xml(file_name)
  }, error = function(e) {
    warning("failed to read file: ", file_name)
    return(NULL)
  })

  intensity <- pattern |>
    xml2::xml_ns_strip() |>
    xml2::xml_find_all("//counts") |>
    xml2::xml_text() |>
    strsplit(" ")
  intensity <- as.numeric(intensity[[1]])

  temperature <- pattern |>
    xml2::xml_ns_strip() |>
    xml2::xml_find_all("//nonAmbientValues") |>
    xml2::xml_text() |>
    strsplit(" ")
  temperature <- as.numeric(temperature[[1]]) |>
    mean() |>
    round(2)

  twoTheta_start <- pattern |>
    xml2::xml_ns_strip() |>
    xml2::xml_find_all("//startPosition") |>
    xml2::xml_text() |>
    strsplit(" ")
  twoTheta_start <- as.numeric(twoTheta_start[[1]])

  twoTheta_end <- pattern |>
    xml2::xml_ns_strip() |>
    xml2::xml_find_all("//endPosition") |>
    xml2::xml_text() |>
    strsplit(" ")
  twoTheta_end <- as.numeric(twoTheta_end[[1]])

  twoTheta <- seq(from = twoTheta_start,
                  to = twoTheta_end,
                  by = (twoTheta_end - twoTheta_start) / (length(intensity) - 1 ))

  pattern <- data.frame(twoTheta = twoTheta,
                        intensity = intensity)

  list(pattern = pattern,
       Temperature = temperature,
       fileName = file_name)
}

#' Get file paths of all files of a given directory that follows a given pattern
#'
#' @param data_path Character, base data path
#' @param dir Character, subdirectory name (e.g. "up", "down", directions)
#' @param file_pattern Character, regex pattern for files (e.g. "\\.csv$", "\\.xrdml$")
#' @return Character vector of full file paths
#' @noRd
get_directory_files <- function(data_path, dir, file_pattern) {
  target_dir <- file.path(data_path, dir)

  if (!dir.exists(target_dir)) {
    warning("Directory does not exist: ", target_dir, call. = FALSE)
    return(character(0))
  }

  list.files(path = target_dir,
             pattern = file_pattern,
             full.names = TRUE)
}

#' Import files with progress reporting
#'
#' @param files Character vector of file paths
#' @param dir_name Character, directory name for progress messaging
#' @importFrom progress progress_bar
#' @importFrom purrr map
#' @return List of imported data
#' @noRd

import_file_with_progress <- function(files, dir_name = "") {
  if (length(files) == 0) {
    return(list())
  }

  # initialize progress bar
  pb <- progress::progress_bar$new(
    total = length(files),
    format = paste("Importing from", dir_name, "[:bar] : percent :eta")
  )

  result <- purrr::map(files, function(file) {
    pb$tick()
    import_xrdml(file)
  })

  return(result)
}

#' Main import function
#' @param config List, containing data input configuration (config$data_input$...)
#' @importFrom purrr set_names map imap
#' @return Nested list of imported data by subdirectory (e.g. by "up", "down")
#' @export
import_all_data <- function(config) {
  # input validation
  if (!all(c("data_path", "direction", "file_pattern") %in% names(config$data_input))) {
    stop("Invalid config structure: missing required data input elements", call. = FALSE)
  }

  # main pipeline for data import
  patterns <- config$data_input$direction |>
    purrr::set_names() |>
    purrr::map( ~ get_directory_files(data_path = config$data_input$data_path,
                                      dir = .x,
                                      file_pattern = config$data_input$file_pattern)) |>
    purrr::imap(import_file_with_progress)

  return(patterns)
}

#' Exporting results to csv file (parallel processing version)
#'
#' @param data_to_export List or tibble, data to export to csv file
#' @param output_directory Character, path to destination directory
#' @param sample_name Character, name of the measured sample (to construct file name)
#' @param measurement Character, measurement identifier (to construct file name)
# @export
export_parallel_csv <- function(data_to_export, output_directory, sample_name, measurement) {
  #message("Data structure:")
  #print(str(data_to_export))

  file_path <- file.path(output_directory, paste0(sample_name, "_", measurement, ".csv"))
  readr::write_csv(data_to_export, file_path)
  message("Exported to: ", file_path)
  invisible(TRUE)
}

#' Exporting results to csv file (sequential processing version)
#'
#' @param data_list List or tibble, data to export to csv file
#' @param output_directory Character, path to destination directory
#' @param sample_name Character, name of the measured sample (to construct file name)
#' @param measurement Character, measurement identifier (to construct file name)
#' @importFrom purrr iwalk
#' @importFrom readr write_csv
#' @export
export_sequential_csv <- function(data_list, output_directory, sample_name, measurement) {
  base_path <- file.path(output_directory, paste0(sample_name, "_", measurement, ".csv"))

  purrr::iwalk(data_list, function(df, suffix) {
    file_path <- add_suffix_to_filename(base_path, paste0("_", suffix))
    readr::write_csv(df, file_path)
    message("Exported to: ", file_path)
  })

  invisible(TRUE)
}

#' A helper function that takes a file path and add a suffix to it
#' @param file_path Character, file path to be transformed with the suffix
#' @param suffix Character, the suffix to be added to the file path
#' @export
add_suffix_to_filename <- function(file_path, suffix) {
  parts <- strsplit(file_path, "\\.")[[1]]
  if (length(parts) == 1) {
    return(paste0(file_path, suffix))
  } else {
    base <- paste(parts[-length(parts)], collapse = ".")
    ext <- parts[length(parts)]
    return(paste0(base, suffix, ".", ext))
  }
}

#' Plotting results using ggplot2 library. It has 2 possible working modes controlled by "parallel" argument. If data is coming from a parallel processing pipeline then parallel = TRUE and filepointer must be a tibble with columns at least c("Temperature", "beta", "maximum", "direction"), then function produces the ggplot of "beta" or "maximum" (choosen by "yaxis" value) as a function of "Temperature" and class given by "direction". If data is coming from a sequential processing pipeline (parallel = FALSE) then pipeline must be a nested list where first level are the possible "direction" and at theirs second level must be a tibble with the data to be plotted, this tibble should have at least c("id", "Temperature", "beta", "maximum") columns.
#'
#' @param filepointer Data frame (parallel = TRUE) or list (parallel = FALSE) where the data to be plotted is
#' @param parallel Logical, flag to control what the input is coming from, a parallel processing pipeline (parallel == TRUE) or a sequential pipeline (parallel == FALSe).
#' @param saving_dir Character, path to output directory where graph file is saved into
#' @param yaxis Character, name of the data column that is going to be plotted in the y axis ("beta" or "maximum")
#' @importFrom rlang .data
#' @return return a ggplot2 object with the plot, additionally it saves it in output directory as a png file
#' @export
#' @examples
#' \dontrun{
#'   single_plot_function(path_to_csv_files, method = "csv", path_to_output_dir)
#'   single_plot_function(list_name, method = "list", path_to_output_dir)
#'}
#'
single_plot_function <- function(filepointer, parallel, saving_dir, yaxis = c("beta", "maximum")) {
  # Validate yaxis and method inputs
  yaxis <- match.arg(yaxis)

  if (!is.logical(parallel)) {
    stop(paste("parallel argument must be a single logical (TRUE or FALSE)"))
  }

  if (parallel) {
    data_to_plot <- filepointer
  } else {
    data_to_plot <- dplyr::bind_rows(filepointer, .id = "direction")
  }

  pg <- ggplot2::ggplot(data_to_plot, ggplot2::aes(x = Temperature,
                                                  y = .data[[yaxis]],
                                                  color = .data$direction)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(title = paste(yaxis, " vs temperature"), x = "Temperature (K)", y = yaxis) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(paste0(saving_dir, "/", "result_plot.png"),
                  pg, width = 8, height = 6, dpi = 300)

  return(pg)

}

