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