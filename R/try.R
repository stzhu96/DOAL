#' try formula
#'
#' @param formula formula.
#' @param data data.
#' @param a x.
#' @param b b.
#'
#' @return y.
#' @export
#'
#' @examples
#' a<-c(1,2,3,1,5)
#' b<-c(2,2,3,2,5)
#' data <- data.frame(a,b)
#' c <-try(y~a+b,data=data)
try<-function(formula,data = data,y=FALSE,a=TRUE,b=TRUE){
  formula <- as.formula(formula)
  data <- na.omit(data)
  y~a+b
  return(y)
}
