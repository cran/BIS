if (getRversion() >= "2.15.1") utils::globalVariables(c("obs_value"))

# Clean names
.clean_names <- function(x) {
  x <- make.unique(tolower(trimws(gsub("[[:space:]]", "_", x))))

  return(x)
}

# Download a file
.download_file <- function(url, ...) {
  # Save user options
  old_options <- options()

  # Restore user options on function exit
  on.exit(options(old_options))

  # Force minimum timeout of 300 for file download
  options(timeout = max(300, getOption("timeout")))

  path <- tryCatch({
    # Prepare temp file
    tmp_file <- tempfile(fileext = ".zip")

    # Download data and store in temp file
    utils::download.file(url, tmp_file, mode = "wb", ...)

    # Return path to temp tile
    path <- tmp_file

    path
  },
  error = function(x) {
    message(paste("Unable to download file:", url))
    message("The resource is unavailable or has changed.")
    message("To download large files, try increasing the download timeout:")
    message("options(timeout = 600)")
    message("Original error message:")
    message(x)
    return(NA)
  },
  warning = function(x) {
    message(paste("Unable to download file:", url))
    message("The resource is unavailable or has changed.")
    message("To download large files, try increasing the download timeout:")
    message("options(timeout = 600)")
    message("Original warning message:")
    message(x)
    return(NA)
  }
  )

  return(path)
}

# Unzip a file
.unzip_file <- function(path) {
  # Prepare temp dir
  tmp_dir <- tempdir()

  # Unpack zip file
  filename <- utils::unzip(path, list = TRUE)
  utils::unzip(path, exdir = tmp_dir)

  # Get path(s) to csv file(s)
  path <- file.path(tmp_dir, filename$Name)

  return(path)
}

#' Convert a BIS data set to long format
#'
#' @param tbl Tibble. A tibble data frame containing a BIS data set (usually
#' obtained via \code{get_bis(url, auto_pivot = FALSE)}).
#'
#' @return A tibble data frame.
#' @export
#'
#' @examples
#' \donttest{
#' ds    <- get_datasets()
#' rates <- get_bis(ds$url[ds$id == "full_cbpol_m_csv"], auto_pivot = FALSE)
#' rates <- subset(rates, ref_area %in% c("US", "DE", "JP"))
#' rates <- pivot_longer_bis(rates)
#' }
pivot_longer_bis <- function(tbl) {
  excl_cols <- grep("^[0-9]", names(tbl), invert = TRUE, value = TRUE)
  tbl <- tidyr::pivot_longer(data = tbl, cols = -tidyselect::all_of(excl_cols),
                             names_to = "date", values_to = "obs_value")
  tbl <- dplyr::mutate(tbl, obs_value = as.numeric(obs_value))

  return(tbl)
}

# Parse a BIS data set
.parse_bis <- function(path, url, auto_pivot = TRUE) {
  # Get file name
  file_name <- tools::file_path_sans_ext(basename(url))

  # Read data into a list of tibble data frames
  tbl <- list()
  i   <- 0

  # One tibble data frame per file
  while (i < length(path)) {
    i <- i + 1

    # Read data into tibble data frame
    tbl[[i]] <- readr::read_csv(path[[i]], col_names = FALSE,
                                show_col_types = FALSE,
                                na = c("", "NA", "NaN"),
                                col_types = readr::cols(.default = "c"))

    # Transpose daily data
    if (is.element(file_name, c("full_xru_d_csv_row", "full_cbpol_d_csv_row",
                                "full_eer_d_csv_row"))) {
      tbl[[i]] <- as.data.frame(t(tbl[[i]]))
      tbl[[i]] <- dplyr::as_tibble(tbl[[i]])
    }

    # Set column names
    nms <- as.character(tbl[[i]][1, ])

    # Fix non-unique names
    if (file_name == "full_bis_rb_csv") {
      if (any(grep("^sts.*", nms))) {
        sts_cols      <- grep("^sts.*", nms)
        nms[sts_cols] <- paste0("sts", nms[sts_cols - 1])
      }
    }

    names(tbl[[i]]) <- .clean_names(nms)
    tbl[[i]]        <- tbl[[i]][-1, ]

    # Pivot data from wide to long
    if (auto_pivot) {
      tbl[[i]] <- pivot_longer_bis(tbl[[i]])
    }

    # Add name to list item
    names(tbl)[[i]] <- tools::file_path_sans_ext(basename(path))[[i]]

    # Check for successful parsing
    if (nrow(tbl[[i]]) == 0) {
      message(paste("Unable to parse file:", file_name))
    }
  }

  # If there is only one tibble data frame, return as single object
  if (length(tbl) < 2) {
    tbl <- tbl[[1]]
  }

  return(tbl)
}

#' Download and parse a list of available BIS data sets
#'
#' @param base_url Character. URL of the BIS's homepage listing single file data
#' sets for download (optional).
#'
#' @return A tibble data frame.
#' @export
#'
#' @examples
#' \donttest{
#' ds <- get_datasets()
#' }
get_datasets <- function(
    base_url = "https://www.bis.org/statistics/full_data_sets.htm") {
  tbl <- tryCatch({
    # Download webpage
    page  <- xml2::read_html(base_url)
    nodes <- rvest::html_nodes(page, xpath = "//a[contains(@href, 'zip')]")

    # Parse homepage: Get name, id, url
    item_name <- rvest::html_text(nodes)
    item_id   <- tools::file_path_sans_ext(
      basename(rvest::html_attr(nodes, "href")))
    item_url  <- xml2::url_absolute(rvest::html_attr(nodes, "href"), base_url)

    # Omit items with SDMX data
    sdmx_items <- which(item_id == "full_bis_rb_sdmx_ml21")
    item_name  <- item_name[-sdmx_items]
    item_id    <- item_id[-sdmx_items]
    item_url   <- item_url[-sdmx_items]

    # Return tibble data frame
    tbl <- dplyr::tibble(name = item_name,
                         id   = item_id,
                         url  = item_url)

    if (nrow(tbl) == 0) {
      message(paste("Unable to download and parse homepage:", base_url))
      message("The resource is unavailable or has changed.")
    }

    tbl
  },
  error = function(x) {
    message(paste("Unable to download and parse homepage:", base_url))
    message("The resource is unavailable or has changed.")
    message("Original error message:")
    message(x)
    return(NA)
  },
  warning = function(x) {
    message(paste("Unable to download and parse homepage:", base_url))
    message("The resource is unavailable or has changed.")
    message("Original warning message:")
    message(x)
    return(NA)
  }
  )

  return(tbl)
}

#' Download and parse a BIS data set
#'
#' @param url Character. URL of the data set to be imported (usually obtained
#' through \code{get_datasets()}).
#' @param auto_pivot Logical. Controls whether source data set is converted to
#' long format. Set this to \code{FALSE} to disable conversion (default: TRUE).
#' @param ... Arguments passed to \code{download.file()} (e.g.
#' \code{quiet = TRUE}).
#'
#' @return A tibble data frame, or a list of tibble data frames in cases where
#' the source zip file contains multiple csv files.
#' @export
#'
#' @details Large data sets may cause \code{get_bis()} to fail if the amount of
#' available memory is insufficient for executing a required pivot operation. As
#' a workaround, users may wish to set \code{auto_pivot = FALSE} when calling
#' \code{get_bis()}, then subset the data and run \code{pivot_longer_bis()}
#' manually. See the vignette for detail.
#'
#' @examples
#' \donttest{
#' ds <- get_datasets()
#' df <- get_bis(ds$url[2])
#' }
get_bis <- function(url, auto_pivot = TRUE, ...) {
  try(zip_file_path <- .download_file(url, ...), TRUE)
  try(csv_file_path <- .unzip_file(zip_file_path), TRUE)
  try(return(.parse_bis(csv_file_path,
                        url = url,
                        auto_pivot = auto_pivot)),
      TRUE)
}
