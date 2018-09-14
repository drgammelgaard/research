hwe_geno_p<-function(mm,mn,nn,mo,no,oo,alleles=2)
{
  ## x is the number of common homozygote, y the heterozygote and z the rare homozygote.
  
  if (alleles==2){
    ## Biallelic has three degrees of freedom
    df=1
    
    a<-sum(mm,mn,nn)*2
    b<-sum(mm,mn,nn)
    p<-(2*mm+mn)/a
    q<-(2*nn+mn)/a
    
    p_p<-round(p^2*b,3)
    p_q<-round(2*p*q*b,3)
    q_q<-round(q^2*b,3)
    
    obs=rbind(mm,mn,nn)
    exp=rbind(p_p, p_q, q_q)
    
    hwe<-data.frame(obs,exp)
    
    hwe$dev<-hwe$obs-hwe$exp
    hwe$chi<-hwe$dev^2/hwe$exp

    p_v<-pchisq(sum(hwe$chi,na.rm = TRUE), df=df, lower.tail=FALSE)
  }
  if(alleles==3){
    ## Triallelic has three degrees of freedom
    df=3
    
    a<-sum(mm,mn,nn,mo,no,oo)*2
    b<-sum(mm,mn,nn,mo,no,oo)
    p<-(2*mm+mn+mo)/a
    q<-(2*nn+mn+no)/a
    r<-(2*oo+no+mo)/a
    
    p_p<-round(p^2*b,3)
    p_q<-round(2*p*q*b,3)
    q_q<-round(q^2*b,3)
    p_r<-round(2*r*p*b,3)
    q_r<-round(2*q*r*b,3)
    r_r<-round(r^2*b,3)
    
    obs=rbind(mm,mn,nn,mo,no,oo)
    exp=rbind(p_p, p_q, q_q, p_r, q_r, r_r)
    
    hwe<-data.frame(obs,exp)
    
    hwe$dev<-hwe$obs-hwe$exp
    hwe$chi<-hwe$dev^2/hwe$exp

    p_v<-pchisq(sum(hwe$chi,na.rm = TRUE), df=df, lower.tail=FALSE)    
  }

  return(p_v)
}
