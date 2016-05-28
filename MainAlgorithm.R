Main<-function(MinSteps = 100, MaxSteps = 120, ParticlesNumber = 30, dimx = 50, dimy = 50)
{
  if( MinSteps > MaxSteps)
  { return() }
  
  Particles = Generate.Particles(ParticlesNumber,dimx,dimy)
  
  NextStep = Particles
  PreviousStep = Particles
  MinimumParticles
  
  
  
  # Main Program Loop
  for( i in 1: MaxSteps)
  {
    PreviousStep = NextStep
    # For each particle
    NextStep = CalculateNextStep(PreviousStep,ParticlesNumber)
    if( i > MinSteps ){
      MinimumParticles = CheckForMinimum(NextStep, PreviousStep) }
    
  }
  
  
}
CheckForMinimum<-function(NextStep, PreviousStep)
{
  for( i in 1: ParticlesNumber)
  {
    board = evolve(Particles[[i]])
    NextStepParticles[[i]] <- board
  }
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