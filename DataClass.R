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