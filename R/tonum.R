#' Transforming a categorical column into numeric
#'
#' @param data A data frame
#' @param col column name to be transformed
#'
#' @return A Data Frame.
#'
#' @examples
#' cat <- data.frame(col1 = c("cat1", "cat2", "cat3", "cat2", "cat1", NA), col2 = c(1, 2, 3, 4, 5, 6))
#' tonum(cat, "col1")
tonum <- function(data, col) {
  nas <- sum(is.na(data[, col]))

  if (nas == 0) {
    data2 <- as.data.frame(data[, c(col)])
    colnames(data2) <- col
    features <- data2[, col][!duplicated(data2[, col])]
    le <- length(features)
    feature_num <- c(1:le)
    for (i in 1:le) {
      data2[, col] <- ifelse(data2[, col] == features[i], feature_num[i],
        data2[, col]
      )
    }
    data2[, col] <- as.numeric(data2[, col])
  } else {
    data2 <- as.data.frame(data[, c(col)])
    colnames(data2) <- col
    dim <- dim(data2)
    data2[, col][is.na(data2[, col])] <- "NANo"
    features <- data2[, col][!duplicated(data2[, col])]
    le <- length(features)
    feature_num <- c(1:le)
    for (i in 1:le) {
      data2[, col] <- ifelse(data2[, col] == features[i],
        feature_num[i], data2[, col]
      )
    }
    data2[, col] <- as.numeric(data2[, col])
    data2[, col][data2[, col] == le] <- NA
  }
  return(data2[, col])
}
