#' wAMD_function for easting total effects.
#'
#' @param DataM data which is uesd to match.
#' @param varlist variables list.
#' @param trt.var treatment list.
#' @param wgt weight .
#' @param beta coffectient.
#'
#' @return ret which is ...
#' @export
#'
#' @examples
wAMD_function <- function(DataM,varlist,trt.var,wgt,beta){
  diff_vec <- rep(NA,length(beta))
  names(diff_vec) <- varlist
  for(jj in 1:length(varlist)){
    diff_vec[jj]<-abs(weightedCorr(DataM[,trt.var],DataM[,varlist[jj]],
                                   method="Pearson",weights=DataM[,wgt]))
  }
  wdiff_vec = diff_vec * abs(beta)
  wAMD = c(sum(wdiff_vec))
  ret = list( diff_vec = diff_vec, wdiff_vec = wdiff_vec, wAMD = wAMD )
  return(ret)
}
