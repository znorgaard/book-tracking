.onLoad  <- function(libname, pkgname) {
  op.bookTrackR <- list(
    bookTrackR.format.colors = c("Physical" = "#1f78b4", "Digital" = "#a6cee3", "Audio" = "#b2df8a"),
    bookTrackR.title.column  = "Title",
    bookTrackR.author.column = "Author",
    bookTrackR.format.column = "Format"
  )
  
  options(op.bookTrackR)
}