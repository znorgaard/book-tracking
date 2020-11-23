library(bookTrackR)

minimal_book_df <- data.frame(
  "Title"  = c("Cannery Row"),
  "Author" = c("John Steinbeck"),
  "Format" = c("Digital"),
  stringsAsFactors = FALSE
)

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
    
    expect_error(validate_book_df(data.frame("Some_Column" = 1)), "These columns exist in colnames but not in your dataframe:")
    
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