#' Transforming a categorical column into numeric
#'
#' @param x A data frame
#'
#' @return A Data Frame.
#'
#' @examples
#' cat <- data.frame(col1 = c("cat1", "cat2", "cat3", "cat2", "cat1", NA), col2 = c(1, 2, 3, 4, 5, 6))
#' asnum(cat)

asnum <- function(x="Data frame") {
  cha <- charname(x)
  le <- length(cha)
  x2 <- x
  for (i in 1:le) {
    x2[, cha[i]] <- tonum(x2, cha[i])
  }
  x2
}
