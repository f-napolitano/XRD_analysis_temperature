#' Load a difraction pattern from a panalytical file (.xrdml) measured in a non-ambient setup,
#' extract the proper diffraction pattern (2theta, intensity) and store it in a data frame,
#' extract the temperature and store it an double. Return a list with (pattern, temperature).
#'
#' @param file_name Complete path and file name of the .xrdml file wanted to be imported
#' @return A list with a data frame and a double.
#'         Data frame is the XRD pattern (2 columns: 2theta, intensity)
#'         Double is the temperature where pattern was acquired
#' @export
#'
#' @examples
#' \dontrun{
#' difractogram_data <- import_xrdml("/data/sample/xrd_pattern.xrdml")
#' }

# function: import xrdml files and store it in a list
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

#' Plotting results using ggplot2 library. It is expected to work with 2 csv files ("up" and "down" directions) or with a list of 2 data frames ("up" and "down" directions)
#'
#' @param filepointer path where the csv files intended for plot are
#' @param method flag to control what is the input, csv files (method == "csv") or a list of data frames (method == "list").
#' @param saving_dir path to output directory where graph file is saved
#' @return return a ggplot2 object with the plot, additionally it saves it in output directory as a png file
#' @export
#' @examples
#' \dontrun{
#'   single_plot_function(path_to_csv_files, method = "csv", path_to_output_dir)
#'   single_plot_function(list_name, method = "list", path_to_output_dir)
#'}
#'
single_plot_function <- function(filepointer, method = c("csv", "list"), saving_dir) {
  method <- match.arg(method)

  if (method == "csv") {
    filenames <- list.files(filepointer, pattern = ".csv", full.names = TRUE)
    data_to_plot <- lapply(filenames, function(n) utils::read.csv(file = n))
    names(data_to_plot) <- stringr::str_extract(basename(filenames), "[^._]+(?=\\.csv$)")
  } else if (method == "list") {
    data_to_plot <- filepointer
  }


  combined_data <- dplyr::bind_rows(data_to_plot, .id = "dataset_id")
  #data_to_plot <- dplyr::arrange(data_to_plot, Temperature)

#  p <- plotly::plot_ly() |>
#    plotly::add_trace(data_to_plot[[1]], x = ~data_to_plot[[1]]$Temperature, y = ~data_to_plot[[1]]$beta, type = "scatter", mode = "lines+markers", name = "beta vs temp") |>
#    plotly::add_trace(data_to_plot[[2]], x = ~data_to_plot[[1]]$Temperature, y = ~data_to_plot[[1]]$beta, type = "scatter", mode = "lines+markers", name = "beta vs temp") |>
#    plotly::layout(title = "beta vs temperature", xaxis = list(title = "Temperature (K)"), yaxis = list(title = "beta (deg)"))

  pg <- ggplot2::ggplot(combined_data, ggplot2::aes(x = Temperature,
                                                    y = beta,
                                                    color = dataset_id)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(title = "beta vs temperature", x = "Temperature (K)", y = "beta (deg)") +
    ggplot2::theme_minimal()

  ggplot2::ggsave(paste0(saving_dir, "/", "result_plot.png"),
                  pg, width = 8, height = 6, dpi = 300)

  return(pg)

}

