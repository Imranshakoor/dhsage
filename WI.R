#' @export

heaping <- function(x, a, b) {
  w_index <- function(x) {
    x <- x[x >= a & x <= b]
    n <- length(x)
    re <- x%%5
    WI <- sum(re == 0)/n
    w <- WI * 500
    return(w)
  }
  m1 <- round(w_index(x), 4)
  data_quality <- function(x) {
    if (m1 <= 105) {
      mess <- "highly accurate"
    } else if (105 < m1 && m1 < 110) {
      mess <- "accurate"
    } else if (110 < m1 && m1 < 125) {
      mess <- "approximate"
    } else if (125 < m1 && m1 < 175) {
      mess <- "rough"
    } else mess <- "very rough"
    return(mess)
  }
  m2 <- data_quality(x)
  aux <- cbind(m1)
  colnames(aux) = c("")
  aux1 <- cbind(m2)
  colnames(aux1) = c("")

  list(`Whipple index` = aux, `data quality` = aux1)
}


