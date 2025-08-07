#' Pivoting qualcoder data into one columne per code and category `(theme)`
#'
#' @param filepath
#'
#' @return A data.frame with one column per code and category. Each row represent one participant, and each code and category are coded with 1 and 0.
#' @export
#'
#' @examples
#'
#'
wide_format <- function(filepath) {
  read_data <- function(path, sheet = NULL) {
    if (!file.exists(path)) stop("File does not exist: ", path)

    ext <- tools::file_ext(path)

    if (ext == "csv") {
      read.csv(path)
    } else if (ext == "xlsx") {
      readxl::read_excel(path, sheet = sheet)
    } else {
      stop("Unsupported file type: ", ext)
    }
  }
  data <- read_data(filepath) %>%
    select(1,2,5,6) %>% # Select relevant column
    rename("ID" = 1) # Rename participant ID

  # pivot codes
  codes <- data %>%
    distinct(ID, Codename) %>% # Keep only one row per Codename BY ID.
    # Long format, one column per Codename
    pivot_wider(
      id_cols = ID,
      names_from = Codename,
      values_from = Codename,
      values_fn = length
    )

  # pivot category (themes)
  category <- data %>%
    distinct(ID, Category) %>% # Keep only one row per Codename BY ID.
    # Long format, one column per Codename
    pivot_wider(
      id_cols = ID,
      names_from = Category,
      values_from = Category,
      values_fn = length
    )

  # Output
  full_join(codes, category)

}
