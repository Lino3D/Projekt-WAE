
InitializeTest<-function(Betamap = gen.board("random", dimx, dimy), Particles = Generate.Particles(ParticlesNumber,dimx,dimy), ParticlesNumber=10, MinSteps=15, MaxSteps=20, dimx=10, dimy=10)
{
  Main(Betamap, Particles, ParticlesNumber, MinSteps, MaxSteps , dimx,dimy)
}

FindMin<-function(lst)
{
  minError = 100000
  index = 0
  for( i in 1:length(lst))
    if( lst[[i]]$error < minError)
    {
      index = i
      minError = lst[[i]]$error
    }
  index
}

Main<-function(BetaMap, Particles, ParticlesNumber, MinSteps = 15, MaxSteps = 20, dimx = 10, dimy = 10)
{
  if( MinSteps > MaxSteps)
  { 
    message("MinSteps is greater than MaxSteps")
    return() 
  }
  if( dim(BetaMap)[1]!= dimx || dim(BetaMap)[2] != dimy)
  {
    message("Dimensions of boards don't match")
    return()
  }
  NextStep = Particles
  
  minError = 1000
  iteration = 0
  
  while( minError > 5 && iteration < 500)
  {
  
  
  # Loop for calculating Minimums for Particles
  for( i in 1: MaxSteps)
  {
    PreviousStep = NextStep
    # For each particle
    NextStep = CalculateNextStep(PreviousStep,ParticlesNumber)
    if( i == MinSteps){
      MinimumParticles = CreateMinimumList(NextStep,ParticlesNumber,BetaMap,Particles)
    }
    if( i > MinSteps ){
      MinimumParticles = CheckForMinimum(NextStep, MinimumParticles,ParticlesNumber, BetaMap) }
  }
  
  
    
  # Loop for Evolution Algorithm
  
    
    MinimumParticles = EvolutionAlgorithm(MinimumParticles)
    
    NextStep = GetBoard(MinimumParticles)
    
    
    minError = MinimumParticles[[FindMin(MinimumParticles)]]$error
    if( minError < 40){
    print(minError)}
  
  
  
  
  iteration = iteration + 1
  }  

  MinimumParticles
}

EvolutionAlgorithm<-function(Mins, n = 5, k = 5, t = 4)
{
  Mins = BubbleSortList(Mins)
  
  length = length(Mins)
  
  for( i in floor(length/2):length)
  {
   # Mins[[i]] = Mins[[i+1-(floor(length/2))]]
    
  }
  
  
  for( i in 1:length(Mins))
    {
      coef = floor(Mins[[i]]$error / t)
    
      Mins[[i]]$board = GenerateRandomChanges(Mins[[i]]$board, coef)
    }
  
  Mins
}

GetBoard<-function(lst)
{
  Board <- list()
  for( i in 1:length(lst))
  {
    Board[[i]] <- lst[[i]]$board
  }
  Board
  
}
BubbleSortList = function(array)
{
  count = 0
  while(1) {
    count_swaps = 0
    for (j in 1 : (length(array) -1 - count)) 
      {
      if (array[[j]]$error > array[[j + 1]]$error) 
        {
          s = array[[j]]
          array[[j]] = array[[j+1]]
          array[[j+1]] = s
          count_swaps = count_swaps + 1
        }
    }
    count = count + 1
    if(count_swaps == 0) break
  }

  return(array)
}



GenerateRandomChanges<-function(Board,ChangeNum)
{
  if(ChangeNum==0)
    return(Board)
  for( i in 1:ChangeNum)
  {
    r1 = sample(1:dim(Board)[1],1)
    r2 = sample(1:dim(Board)[2],1)
    
    Board[r1,r2] = !Board[r1,r2]
  }
  return(Board)
  
}






# board in MinimumList is the best board for the current particle
CreateMinimumList<-function(CurrentBoards, ParticlesNumber, final, InitalBoards)
{
  mybiglist <- list()
  for(i in 1:ParticlesNumber){
    
    a <- InitalBoards[[i]]
    b <- CalculateDifference(final,CurrentBoards[[i]]) 
    
    tmp <- list(board = a, error = b)
    mybiglist[[i]] <- tmp
  }
  mybiglist
}

CheckForMinimum<-function(NextStep, MinimumParticles,ParticlesNumber, BetaMap)
{
  for( i in 1: ParticlesNumber)
  {
    error = CalculateDifference(NextStep[[i]],BetaMap)
    
    if( error < MinimumParticles[[i]]$error)
    {
      MinimumParticles[[i]]$error = error
     # MinimumParticles[[i]]$board = NextStep[[i]]
    }
  }
  MinimumParticles
}

CalculateNextStep<-function(Particles,ParticlesNumber)
{
  NextStepParticles <- list()
  for( i in 1: ParticlesNumber)
  {
    board = evolve(Particles[[i]])
    NextStepParticles[[i]] <- board
  }
  NextStepParticles
}