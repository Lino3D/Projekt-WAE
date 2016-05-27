
CreateBoard<-function(x)
{
  a = matrix(nrow=x,ncol=x)
  return(a)
}



fillMatrix<-function(x)
{
  for(i in 1:dim(x)[1])  # for each row
  {
    for(j in 1:dim(x)[2]) # for each column
    {
      x[i,j] = 3     
    }
  }
  return(x)
}