#' Function to read in book data from a file and return a validated `data.frame`
#'
#' @description Function to read in book data, validate the table, and return a formatted \code{data.frame}.
#' The validation is done by \code{\link{validate_book_df}}.
#' The formatting is done by \code{\link{format_book_df}}.
#'
#' @param book_table_path Path to the book table data file
#' @param sep The separator used in the book table data file.
#' @param ... Additional arguments to be passed to \code{\link{read.table}}
#' Default = '\\t'.
#' 
#' @return Returns a \code{data.frame} formatted for easy use with visualization functions.
#' 
#' @examples
#' # Generate example data
#' minimal_book_df <-  data.frame(
#'   "Title"  = c("Cannery Row"),
#'   "Author" = c("John Steinbeck"),
#'   "Format" = c(names(getOption("bookTrackR.format.colors"))[1])
#' )
#' 
#' # Save example data
#' tmp_txt_path <- tempfile(pattern = "", fileext = ".txt")
#' tmp_csv_path <- tempfile(pattern = "", fileext = ".csv")
#' 
#' write.table(minimal_book_df, tmp_txt_path, quote = FALSE, sep = "\t", row.names = FALSE)
#' write.table(minimal_book_df, tmp_csv_path, quote = FALSE, sep = ",", row.names = FALSE)
#' 
#' # Read example data
#' minimal_txt_book_df <- read_book_table(tmp_txt_path)
#' minimal_csv_book_df <- read_book_table(tmp_csv_path, sep = ",")
#' 
#' # Cleanup
#' unlink(c(tmp_txt_path, tmp_csv_path))
#'
#' @export
read_book_table <- function(book_table_path, sep = "\t", ...) {
  book_df <- utils::read.table(book_table_path, sep = sep, header = TRUE, stringsAsFactors = FALSE, ...)

  format_book_df(book_df = book_df)
}

#' Function to validate book `data.frame` is formatted correctly
#' 
#' @description function to validate a book \code{data.frame} is formatted correctly for use with \code{bookTrackR} functions.
#' A valid \code{data.frame} will contain all column names returned by \code{\link{BOOKTRACKR_REQUIRED_COLUMNS}}.
#' A valid \code{data.frame} will contain a column named \code{getOption("bookTrackR.format.column")} with only values from
#' \code{names(getOption("bookTrackR.format.colors"))}
#'
#' @param book_df `data.frame` to be validated.
#' 
#' @return \code{TRUE} if \code{data.frame} is valid else throws an error
#' 
#' @examples
#' 
#' minimal_book_df <-  data.frame(
#'   "Title"  = c("Cannery Row"),
#'   "Author" = c("John Steinbeck"),
#'   "Format" = c(names(getOption("bookTrackR.format.colors"))[1])
#' )
#' 
#' # Valid data.frame
#' validate_book_df(minimal_book_df)
#' 
#' \dontrun{
#' # Does not include required columns
#' validate_book_df(data.frame())
#' 
#' # Has invalid values in getOption("bookTrackR.format.column")
#' validate_book_df(rbind(minimal_book_df, c("Ye", "Kanye West", "CD")))
#' }
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
#' @description Function to format book `data.frame` for use with other \code{bookTrackR} functions.
#' The \code{book_df} is validated with \code{\link{validate_book_df}} before formatting.
#' Current formatting includes:
#' setting factor levels for the \code{getOption("bookTrackR.format.column")}
#'
#' @param book_df \code{data.frame} to be formatted
#' 
#' @return \code{data.frame} formatted for use with other \code{bookTrackR} functions.
#'
#' @examples
#' 
#' minimal_book_df <-  data.frame(
#'   "Title"  = c("Cannery Row"),
#'   "Author" = c("John Steinbeck"),
#'   "Format" = c(names(getOption("bookTrackR.format.colors"))[1])
#' )
#' 
#' format_book_df(minimal_book_df) 
#' 
#' @export
format_book_df <- function(book_df) {
  validate_book_df(book_df = book_df)
  
  book_df[[getOption("bookTrackR.format.column")]] <- factor(
    book_df[[getOption("bookTrackR.format.column")]],
    levels = names(getOption("bookTrackR.format.colors"))
  )
  
  book_df
}

#' The columns required in `data.frame` to be used with bookTrackR functions
#' 
#' @export
BOOKTRACKR_REQUIRED_COLUMNS <- function() {
  unlist(options()[grep("bookTrackR\\.[a-z]+\\.column", names(options()))])
}
