#############################################################
#                                                           #
#           Explort GAM outputs as csv tables               #
#                                                           #
#############################################################

# Retrieved from: https://drive.google.com/file/d/0BzcFB9Tm4gh_Vlg3cUVkUXBQeVU/view?resourcekey=0-tAMdQyi_eKhl0h1TfaZaHw
# http://mksuwal.blogspot.com/2017/02/export-glm-and-lm-to-csv-file.html

gamOut <- function(res, file="test.csv", ndigit=5, writecsv=T) {
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  co <- res$p.table     # result table
  nvar <- nrow(co)      # No. row as in summary()$coefficients
  ncoll <- ncol(co)     # No. col as in summary()$coefficients
  
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  nstats <- 4          # sets the number of rows to record the coefficients           
  
  # s table
  StartRow<- nstats+ nvar +2 # starting row of for second table
  st <- res$s.table
  ncol2 <- ncol(st)
  nrow2 <- nrow(st)
  # create a matrix
  G <- matrix("", nrow=(nvar+nstats+StartRow), ncol=(ncoll+1))       # storing data for output
  # equation, deviance and R square
  G[1,1] <- toString(res$formula)
  G[1,2] <- "Deviance explained"  # save AIC value
  G[2,2] <- res$dev.expl
  G[1,3] <- "R-sq. (adj)"
  G[2,3] <- res$r.sq
  # P table
  G[(nstats+1):(nvar+nstats), 1] <- rownames(co) # save rownames and colnames
  G[nstats,  2:(ncoll+1)] <- colnames(co)
  G[(nstats+1):(nvar+nstats), 2:(ncoll+1)] <- formatter(co)  # save coefficients
  # S table 
  G[(StartRow+1):(StartRow+nrow2), 1] <- rownames(st)
  G[(StartRow), 2: (ncol2+1)]         <- colnames(st)
  G[(StartRow+1):(StartRow+nrow2), 2:(ncol2+1)] <- formatter(st)
  
  # for output
  print(G)
  write.csv(G, file=file, row.names=F)
}
