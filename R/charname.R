#' Categoric column name
#'
#' @param x A data frame
#'
#' @return A character vector.
#'
#' @examples
#' cat<-data.frame(col1=c("cat1","cat2","cat3","cat2","cat1"),col2=c(1,2,3,4,5))
#' charname(cat)

charname<-function(
    x="Data frame") {
  di<-dim(x)
  char_name<-c()
  for (i in 1:di[2])
  { if (class(x[1, i])=="character"){
      char_name<-c(char_name,colnames(x[i]))
  }
    }
  char_name
  }
