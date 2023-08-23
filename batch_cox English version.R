batch_cox <- function(data, subname = NULL, save_csv = TRUE){
  Coxoutput=data.frame()
  for(i in colnames(data[,3:ncol(data)])){
    cox <- survival::coxph(survival::Surv(OS.time, OS) ~ data[,i], data = data)
    coxSummary = summary(cox)
    Coxoutput=rbind(Coxoutput,cbind(OTU=i,HR=coxSummary$coefficients[,"exp(coef)"],
                                    z=coxSummary$coefficients[,"z"],
                                    pvalue=coxSummary$coefficients[,"Pr(>|z|)"],
                                    lower=coxSummary$conf.int[,3],
                                    upper=coxSummary$conf.int[,4]))
  }
  if(nrow(Coxoutput)>0 & save_csv == TRUE){
    for(i in c(2:6)){
      Coxoutput[,i] <- as.numeric(as.vector(Coxoutput[,i]))
    }
    Coxoutput <- arrange(Coxoutput,pvalue)  %>% #按照p值排序
      filter(pvalue < 0.05) 
    if (is.null(subname)) {
      filename <- "cox_output.csv"
    } else {
      filename <- paste0(subname, "_cox_output.csv")
    }
    cat("note: ",nrow(Coxoutput),"prognostic items were selected.\n")
    
    #save as csv
    write.csv(Coxoutput, filename, row.names = FALSE)
  }else{
    cat(paste(subname,"No items were selected! \n"))
  }
}