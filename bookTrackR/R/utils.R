#' Function to read in book data and return a validate data.frame
#'
#' @param book_table_path Path to the book table data file
#' @param sep The separator used in the book table data file.
#' @param ... Additional arguments to be passed to \code{\link{read.table}}
#' Default = '\\t'.
#'
#' @export
read_book_table <- function(book_table_path, sep = "\t", ...) {
  book_df <- utils::read.table(book_table_path, sep = sep, header = TRUE, stringsAsFactors = FALSE, ...)

  validate_book_df(book_df = book_df)

  format_book_df(book_df = book_df)
}

#' Function to validate book `data.frame` is formatted correctly
#'
#' @param book_df `data.frame` to be validated.
#' Required columns are listed in \code{\link{BOOKTRACKR_REQUIRED_COLUMNS}}
#'
#' @export
validate_book_df <- function(book_df) {
  assertthat::assert_that(class(book_df) == "data.frame", msg = "book_df must be of class `data.frame`")

  assertable::assert_colnames(book_df, BOOKTRACKR_REQUIRED_COLUMNS(), only_colnames = FALSE)

  assertthat::assert_that(
    all(unique(book_df[[getOption("bookTrackR.format.column")]]) %in% names(getOption("bookTrackR.format.colors"))),
    msg = paste0(
      "The '",
      getOption("bookTrackR.format.column"),
      "' column may only contain values: ",
      paste(names(getOption("bookTrackR.format.colors")), collapse = ", ")
    )
  )
}

#' Function to format book `data.frame` for bookTrackR functions
#'
#' @param book_df `data.frame` to be formatted
#'
format_book_df <- function(book_df) {
  book_df
}

#' The columns required in `data.frame` to be used with bookTrackR functions
BOOKTRACKR_REQUIRED_COLUMNS <- function() {
  unlist(options()[grep("bookTrackR\\.[a-z]+\\.column", names(options()))])
}
