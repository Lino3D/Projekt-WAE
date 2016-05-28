Main<-function(BetaMap, MinSteps = 15, MaxSteps = 20, ParticlesNumber = 5, dimx = 10, dimy = 10)
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
  
  Particles = Generate.Particles(ParticlesNumber,dimx,dimy)
  
  NextStep = Particles
  PreviousStep = Particles
  
  
  
  
  # Loop for calculating Particles
  for( i in 1: MaxSteps)
  {
    PreviousStep = NextStep
    # For each particle
    NextStep = CalculateNextStep(PreviousStep,ParticlesNumber)
    if( i == MinSteps){
      MinimumParticles = CreateMinimumList(NextStep, ParticlesNumber,BetaMap)
    }
    if( i > MinSteps ){
      MinimumParticles = CheckForMinimum(NextStep, MinimumParticles,ParticlesNumber, BetaMap) }
  }

  MinimumParticles
}
# board in MinimumList is the best board for the current particle
CreateMinimumList<-function(BoardsList, ParticlesNumber, final)
{
  mybiglist <- list()
  for(i in 1:ParticlesNumber){
    
    a <- BoardsList[[i]]
    b <- CalculateDifference(final,BoardsList[[i]]) 
    
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
      MinimumParticles[[i]]$board = NextStep[[i]]
    }
  }
  return(MinimumParticles)
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