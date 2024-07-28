#' Replace NA from col type is character
#'
#' @param data a data frame
#'
#' @return a data frame with NA's
#' @export
#'
#' @examples
#' data <- data.frame(
#' a = c("hello", "NA", "", "world"),
#' b = c(1, 2, 3, 4),
#' c = c("foo", "bar", "", "NA"),
#' stringsAsFactors = FALSE)
#' replace_na_strings(data)
replace_na <- function(data) {
  data[] <- lapply(data, function(col) {
    if (is.character(col)) {
      col[col == "NA" | col == ""] <- NA
    }
    return(col)
  })
  return(data)
}
