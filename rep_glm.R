rep_glm<-function(x,y,output="coef"){
## x is data.frame, y is vector
## output can be "coef" or "or_ci"
## The confint() function is rather slow, causing the whole function to hang when including many predictors.
  
  require(broom)

  
   if (output=="coef"){
     
       df<-data.frame(matrix(ncol = 3))
  names(df)<-c("pred","b","pv")
     
  for(i in 1:ncol(x)){
     m<-glm(y~x[,i],family = binomial())
     
     b<-round(coef(m)[-1],3)
     
     pv<-round(tidy(m)$p.value[-1],3)
     pv<-ifelse(pv<0.001,"<0.001",pv)
     pv <- ifelse(pv<=0.05|pv=="<0.001",paste0("*",pv),pv)
    pv <- ifelse(pv>0.05&pv<=0.1,paste0(".",pv),pv)
     
     v<-x[,i]
     
     if (is.factor(v)){
       pred<-paste(names(x)[i],levels(v)[-1],sep = "_")}
       
       else {pred<-names(x)[i]}
     
     df<-rbind(df,cbind(pred,b,pv))}
     }
   
    if (output=="or_ci"){
      
             df<-data.frame(matrix(ncol = 3))
  names(df)<-c("pred","or_ci","pv")
      
      for(i in 1:ncol(x)){
     m<-glm(y~x[,i],family = binomial())
     
     l<-suppressMessages(round(exp(confint(m))[-1,1],2))
     u<-suppressMessages(round(exp(confint(m))[-1,2],2))
     or<-round(exp(coef(m))[-1],2)
     
     (or_ci<-paste0(or," (",l," to ",u,")"))
     
     pv<-round(tidy(m)$p.value[-1],3)
     pv<-ifelse(pv<0.001,"<0.001",pv)
     pv <- ifelse(pv<=0.05|pv=="<0.001",paste0("*",pv),pv)
    pv <- ifelse(pv>0.05&pv<=0.1,paste0(".",pv),pv)
     
    v<-x[,i]
     
    if (is.factor(v)){
       pred<-paste(names(x)[i],levels(v)[-1],sep = "_")}
       
       else {pred<-names(x)[i]}
      
      df<-rbind(df,cbind(pred,or_ci,pv))
    }}
    
      else {df="Some kind of error message would be nice"}
     
  return(df)
}
