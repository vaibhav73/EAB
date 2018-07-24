#'Load a list of correlated variables
#'
#'It loads the function to find the best variables 
#'
#' @param  Correlated_variables list of correlated variables
#' @return best variable value
#' @export
Correlation_DJV= function(Correlated_variables,data){
  use=names(data)[names(data) %in% Correlated_variables ]
  data1=data[,use]
  p_val=list()
  R_sq=list()
  for(i in 1:(ncol(data1)-1)){
    x=chisq.test(data1[,i],data1[,ncol(data1)])
    p_val[names(data1)[i]]=x$p.value 
    
    
    if (p_val[names(data1)[i]]<0.05)
    {
      y=lm(data1[,ncol(data1)]~data1[,i],data=data1)
      R_sq[names(data1)[i]]=summary(y)$r.squared
    } else 
    {
      R_sq[names(data1)[i]]=0 }
  }
  
  R_sq=unlist(R_sq)
  R_sq=sort(R_sq,decreasing = TRUE)
  R_sq=sort(R_sq,decreasing = TRUE)
  
  
  Best=names(R_sq[1])
  
  return(Best)
  
}


#' Loads a dataframe for binning
#' 
#' This function is used for binning 
#' Bins the variables and retuns a data frame
#' 
#' @param  data Input dataframe
#' @return A dataframe 
#' @export
Binning_DJV=function(data,bin=5){
  
  newd=bin.rpart(formula = Applications ~ Distance, data = data, 
                 rcontrol = rpart.control(minbucket = .1 * nrow(data2)),n.group = bin)
  
  data$Distance=newd$bins
  
  return(data)
}


