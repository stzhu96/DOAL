#' ATE_est for esating average effects.
#'
#' @param fY data's outcomes.
#' @param fw data's weight.
#' @param fA data"s explore.
#'
#' @return Average effects.
#' @export
#'
#' @examples
ATE_est = function(fY,fw,fA){
  dataset<- data.frame(fY,fA,fw)
  design.b<-svydesign(ids= ~1, weights = ~fw,data= dataset)
  tt_ATE <-svyglm(fY~fA,design = design.b)
  tt_ATE1<-list(Trt_coef=summary(tt_ATE)$coefficients,ATE_wreg=tt_ATE)
  return(tt_ATE1)
}
