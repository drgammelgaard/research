col_fact<-function(string,data){
## Defining factors for columns containing string, based on dplyr

  require(dplyr)
  d<-data
  n<-names(select(d,contains(string)))
  
  for(i in 1:length(n)) {
    d[,n[i]]<-factor(d[,n[i]])
  }
  return(d)
}
