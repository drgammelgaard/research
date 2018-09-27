col_fact<-function(string,data){
## Defining factors for columns containing string (can be vector of multiple strings), based on dplyr

  require(dplyr)
  d<-data
  s<-string
  n<-c()
  
  for(i in 1:length(s)){
  n<-c(n,names(select(d,contains(s[i])))
  
  
  for(i in 1:length(n)) {
    d[,n[i]]<-factor(d[,n[i]])
  }}
  return(d)
}
