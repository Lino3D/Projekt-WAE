Generate.Particles <- function(nPart=10, nrow = 20, ncol = 20)
{
  myList <- list()
  for( i in 1:10)
  {
    board = gen.board("random",nrow ,ncol)
    myList[[i]] <- board
  }
  myList
}


CalculateDifference<-function(final, particle)
{
  diff = 0
  for(i in 1:dim(final)[1])  # for each row
  {
    for(j in 1:dim(particle)[2]) # for each column
    {
      if( final[i,j] != particle[i,j])
      {
        diff = diff + 1
      }
    }
  }
  return(diff)
}



tmpf<-function(p1,p2)
{
  lst <- list()
  er = CalculateDifference(p1,p2)
  
  
  tmp <-list(a = p1,b = er, c = "dupa")
  name <- paste('item:',1,sep='')
  lst[[1]] <- tmp
  
  
  name <- paste('item:',2,sep='')
  tmp<-list(a = p1,b = er, c = "dup3a3")
  lst[[2]] <- tmp
 
  lst 
}


tmpf2<-function()
{
  mybiglist <- list()
  for(i in 1:5){
    a <- runif(10)
    b <- rnorm(16)
    c <- rbinom(8, 5, i/10)
    name <- paste('item:',i,sep='')
    tmp <- list(uniform=a, normal=b, binomial=c)
    mybiglist[[name]] <- tmp
  }
  mybiglist
}


