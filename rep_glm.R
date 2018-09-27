rep_glm<-function(x,y){
## Function to return results from multiple bivariate analyses based on the data.frame x and vector y.

  require(broom)
  df<-data.frame(matrix(ncol = 3))
  names(df)<-c("pred","b","pv")
  
  for(i in 1:ncol(x)){
     m<-glm(y~x[,i],family = binomial())
     
     b<-round(coef(m)[-1],3)
     
     pv<-round(tidy(m)$p.value[-1],3)
     
     pred<-names(x)[i]
     
     df<-rbind(df,cbind(pred,b,pv))
     }
  return(df)
}
