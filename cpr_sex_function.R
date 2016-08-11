cpr_sex<-function(x){ 
  #input is vector of DK cpr-numbers ie "ddmmyy-xxxx", returns sex according to cpr
  
  last<-as.integer(substr(x,start = 11,stop = 11))
  sex<-ifelse(last %% 2 == 0,"female","male")
  return(sex)
}
#test
#cpr_sex("121090-1899")
