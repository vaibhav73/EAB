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
#' This function is used for binning
#' Bins the variables and retuns a data frame
#'
#' @param  data Input dataframe
#' @return A dataframe
#' @export
Feeder_High_School_Function= function(Model_data)
{
  Selection_Table <- data.frame(Cutoff_Inquiries=numeric(), Cutoff_Submissionrate=numeric(),Information_value=numeric(),Feeder_Percentage=numeric())
  Model_data$Inquiry=1
  k=1
  for(i in 10:25)
  {

    for(j in 15:30)
    {

      Feeder_School=summaryBy(Applications+Inquiry~Ceeb.Cd,data = Model_data,FUN = sum)
      Feeder_School$Submission_Rate= (Feeder_School$Applications.sum/Feeder_School$Inquiry.sum)*100
      FeederHSInd= ifelse(((Feeder_School$Submission_Rate>i) & (Feeder_School$Inquiry.sum>j)),"FeederHS","Non-FeederHS")
      Feeder_School$FeederHSInd=FeederHSInd
      h=summaryBy(Applications.sum+Inquiry.sum ~ FeederHSInd,data = Feeder_School,FUN = sum)
      Apps_FH=h[1,2]
      Inq_FH=h[1,3]
      Apps_NFH=h[2,2]
      Inq_NFH=h[2,3]
      Event_FH=h[1,2]/(h[1,2]+h[2,2])
      NonEvent_FH=h[1,3]/(h[1,3]+h[2,3])
      Event_NFH=h[2,2]/(h[1,2]+h[2,2])
      NonEvent_NFH=h[2,3]/(h[1,3]+h[2,3])
      log(NonEvent_FH/Event_FH)
      log(NonEvent_NFH/Event_NFH)
      IV=(log(NonEvent_FH/Event_FH)*(NonEvent_FH-Event_FH))+(log(NonEvent_NFH/Event_NFH)*(NonEvent_NFH-Event_NFH))
      Feed_Percent=(Inq_FH/(Inq_FH+Inq_NFH))*100

      Selection_Table[k,]=list(j,i,IV,Feed_Percent)
      k=k+1
    }

  }
  return(Selection_Table)
}


