library(bookTrackR)

# Generate example data
minimal_book_df <- data.frame(
  "Title"  = c("Cannery Row"),
  "Author" = c("John Steinbeck"),
  "Format" = c(names(getOption("bookTrackR.format.colors"))[1]),
  stringsAsFactors = FALSE
)

tmp_txt_path <- tempfile(pattern = "", fileext = ".txt")
tmp_csv_path <- tempfile(pattern = "", fileext = ".csv")

write.table(minimal_book_df, tmp_txt_path, quote = FALSE, sep = "\t", row.names = FALSE)
write.table(minimal_book_df, tmp_csv_path, quote = FALSE, sep = ",", row.names = FALSE)

test_that(
  "A valid book_df is validated",
  {
    expect_true(validate_book_df(minimal_book_df))
    
    expect_true(
      validate_book_df(
        data.frame("Title" = character(0), "Author" = character(0), "Format" = character(0))
      )
    )
  }
)

test_that(
  "An invalid book_df generates an error",
  {
    expect_error(validate_book_df(2), "book_df must be of class `data.frame`")
    
    expect_error(validate_book_df(data.frame()), "These columns exist in colnames but not in your dataframe:")
    
    expect_error(validate_book_df(data.frame(column1 = c(1))), "These columns exist in colnames but not in your dataframe:")
    
    expect_error(
      validate_book_df(rbind(minimal_book_df, c("Ye", "Kanye West", "CD"))),
      paste0(
        "The '", 
        getOption("bookTrackR.format.column"),
        "' column may only contain values: ",
        paste(names(getOption("bookTrackR.format.colors")), collapse = ", ")
      )
    )
  }
)

test_that(
  "A book_df is correctly formatted when processed with format_book_df",
  {
    expect_equal(
      format_book_df(minimal_book_df),
      data.frame(
        "Title"  = c("Cannery Row"),
        "Author" = c("John Steinbeck"),
        "Format" = factor(c(names(getOption("bookTrackR.format.colors"))[1]), levels = c(names(getOption("bookTrackR.format.colors"))))
      )
    )
  }
)


# Cleanup
unlink(c(tmp_txt_path, tmp_csv_path))