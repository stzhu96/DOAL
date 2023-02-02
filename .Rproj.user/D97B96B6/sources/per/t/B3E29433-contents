#' GOAL is our main function for high
#'
#' @param data our data.
#' @param var.list  variable list.
#' @param covar covar.
#' @param Trt tratments.
#' @param out outcome.
#' @param lambda_vec lambda_vec.
#' @param gamma_convergence gamma_convergence.
#'
#' @return results.
#' @export
#'
#' @examples
GOAL<-function (data,var.list,covar,Trt="Trt",out="Y",lambda_vec,gamma_convergence=10) {
  Data<-data
  n<-dim(Data)[1]
  #??׼??
  temp.mean <- colMeans(Data[,var.list])
  Temp.mean <- matrix(temp.mean,ncol=length(var.list),nrow=nrow(Data),byrow=TRUE)
  Data[,var.list] <- Data[,var.list] - Temp.mean
  temp.sd <- apply(Data[var.list],FUN=sd,MARGIN=2)
  Temp.sd <- matrix(temp.sd,ncol=length(var.list),nrow=nrow(Data),byrow=TRUE)
  Data[var.list] <- Data[,var.list] / Temp.sd
  rm(list=c("temp.mean","Temp.mean","temp.sd","Temp.sd"))
  #outcome model
  y.form <- formula(paste(out,"~",paste(c(Trt,covar,var.list),collapse="+")))
  lm.Y <- lm(y.form,data=Data)
  betaXY <- coef(lm.Y)[var.list]
  lambda_vec <- lambda_vec
  names(lambda_vec) <- as.character(lambda_vec)
  gamma_convergence_factor <- gamma_convergence
  gamma_vals <- 2*( gamma_convergence_factor - lambda_vec + 1 )
  names(gamma_vals) <- names(lambda_vec)

  wAMD_vec=rep(NA, length(lambda_vec))
  ATE_lambda=vector(mode = "list", length(lambda_vec))
  names(ATE_lambda)=names(wAMD_vec)=names(lambda_vec)
  coeff_XA <- as.data.frame(matrix(NA,nrow=length(var.list),
                                   ncol=length(lambda_vec)))
  names(coeff_XA) = names(lambda_vec)
  rownames(coeff_XA) <- var.list
  CBPS.var<-list()
  w.full.form <- formula(paste(Trt,"~",paste(c(covar,var.list),collapse="+")))
  for(lil in names(lambda_vec)){
    il = lambda_vec[lil]
    ig = gamma_vals[lil]
    oal_pen <- adaptive.lasso(lambda=n^(il),al.weights = c(rep(0,length(covar)),abs(betaXY)^(-ig)))
    logit_oal <- lqa(w.full.form, data=Data, penalty=oal_pen,
                     family=gaussian())
    coeff_XA[var.list,lil] <- coef(logit_oal)[var.list]
    CBPS.var[[lil]]<-c(covar,rownames(coeff_XA[var.list,])[which(round(coeff_XA[var.list,lil],5)!=0)])
    if (length(CBPS.var)!=0) {
      w.model<-formula(paste(Trt,"~",paste(CBPS.var[[lil]],collapse="+")))
      CBPS_fit<-npCBPS(w.model,data=Data,corprior=.1/n, print.level=1)
      Data[,paste("w",lil,sep="")]<-CBPS_fit$weights
    } else {
      Data[,paste("w",lil,sep="")]<-1
    }
    wAMD_vec[lil] <- wAMD_function(DataM=Data,varlist=c(covar,var.list),trt.var=Trt,
                                   wgt=paste("w",lil,sep=""),beta=coef(lm.Y)[c(covar,var.list)])$wAMD
    ATE_lambda[[lil]] <- ATE_est(fY=Data[,out],fw=Data[,paste("w",lil,sep="")],
                                 fA=Data[,Trt])
    #$Trt_coef["fA","Estimate"]
  }
  ATE<-ATE_lambda[[which.min(wAMD_vec)]]
  Svar<-CBPS.var[[which.min(wAMD_vec)]]
  lambda<-names(wAMD_vec)[which.min(wAMD_vec)]
  GOAL_results<-list(
    ATE=ATE,
    selectedVar=Svar,
    lambda=lambda
  )
  return (GOAL_results)
}
