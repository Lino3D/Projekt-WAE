
fillMatrix<-function(x)
{
  for(i in 1:dim(x)[1])  # for each row
  {
    for(j in 1:dim(x)[2]) # for each column
    {
      x[i,j] = 0     
    }
  }
  return(x)
}


CreateBoard<-function(x)
{
  a = matrix(nrow=x,ncol=x)
  fillMatrix(a)
  return(a)
}

# Generates a new board - either a random one, sample blinker or gliders, or user specified.
gen.board <- function(type="random", nrow=3, ncol=3, seeds=NULL)
{
  if(type=="random")
  {
    return(matrix(runif(nrow*ncol) > 0.5, nrow=nrow, ncol=ncol))
  } 
  else if(type=="blinker")
  {
    seeds <- list(c(2,1),c(2,2),c(2,3))
  } 
  else if(type=="glider")
  {
    seeds <- list(c(1,2),c(2,3),c(3,1), c(3,2), c(3,3))
  }
    board <- matrix(FALSE, nrow=nrow, ncol=ncol) 
  for(k in seq_along(seeds))
  {
    board[seeds[[k]][1],seeds[[k]][2]] <- TRUE
  }
  return(board)
}


# Returns the number of living neighbours to a location
count.neighbours <- function(x,i,j) 
{   
 return (sum(x[max(1,i-1):min(nrow(x),i+1),max(1,j-1):min(ncol(x),j+1)]) - x[i,j])
}

# Implements the rulebase
determine.new.state <- function(board, i, j)
{
  N <- count.neighbours(board,i,j)
  return(N == 3 || (N ==2 && board[i,j]))
}

# Generates the next interation of the board from the existing one
evolve <- function(board)
{ 
  newboard <- board
  for(i in seq_len(nrow(board)))
  {
    for(j in seq_len(ncol(board)))
    {
      newboard[i,j] <- determine.new.state(board,i,j)         
    }   
  }
 return(newboard)
}

checkdimentions = function(x, y)
{
  if(dim(x)[1] != dim(y)[1] || dim(x)[2] != dim(y)[2])
  {
    return(FALSE)
  }
  else
    return(TRUE)
  
}


# Plays the game.  By default, the board is shown in a plot window, though output to the console if possible.
game.of.life <- function(board, finalboard = gen.board("random",10,10),minsteps =20,maxsteps=50, timebetweensteps=0.25, graphicaloutput=TRUE)
{
  library("lattice")
  nr <- nrow(board)
  
  counter =0
  
 # if(checkdimentions(board, finalboard)==FALSE)
 # {
#    message("Dimensions of boards don't match")
#    invisible(board)
 # }
  
  
  for(i in seq_len(maxsteps) )
  {
    if(graphicaloutput) 
    {
      print(levelplot(t(board[nr:1,]), colorkey=FALSE)) 
    } else print(board)  
    
    Sys.sleep(timebetweensteps)
    
    newboard <- evolve(board)
    counter = counter + 1
    
   # if(identical(board, finalboard))
  #  {
   #   message("Beta board reached")
   #   break;
   # }
    
    if(all(newboard==board))
    {
      message("board is static")
      break
    } 
    else if(sum(newboard) < 1)
    {
      message("everything is dead")
      break
    } 
    else if(counter == maxsteps)
    {
      message("Max steps reached")
      break;
    }
    else  
      board <- newboard
   
    
  }   
  return(board)
}
#game.of.life(gen.board("glider", 18, 20))




