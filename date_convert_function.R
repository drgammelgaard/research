date_convert<-function(x)
  ## Input format as dd/mm/yyyy, output is standard yyyy-mm-dd  
  {
      result<-as.Date(x, format="%d/%m/%Y")
print(result)  
}