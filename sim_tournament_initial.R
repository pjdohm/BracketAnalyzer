library(rlang)

simGame <- function(teams, matchups) {
  # function to simulate a game
  
  # INPUT:
  # teams is a vector of two teams (indices) that are "playing'
  # matchups is the matchup/probability matrix
  
  # OUTPUT:
  # returns winner of game
  
  team1 <- teams[1]
  team2 <- teams[2]
  matchup <- matchups[team1, team2]
  
  if (runif(1) < matchup) {
    return(team1)
  } else {
    return(team2)
  }
}

simRound <- function(bracket, matchups) {
  # function to simulate a round of a tournament given a bracket and probability matrix
  
  # INPUT:
  # bracket is the bracket structure
  # matchups is the matchup/probability matrix
  
  # OUTPUT:
  # returns winners of the round
  
  nGames <- length(bracket)/2
  outBracket <- c()
  for (game in 1:nGames) {
    teams <- bracket[(2*game - 1):(2*game)]
    winner <- simGame(teams, matchups)
    outBracket <- c(outBracket, winner)
  }
  
  return(outBracket)
}

simTournament <- function(bracket, matchups) {
  # function to simulate a tournament given a bracket and probability matrix
  
  # INPUT:
  # bracket is the bracket structure
  # matchups is the matchup/probability matrix
  
  # OUTPUT:
  # returns winner of tournament
  
  nTeams <- length(bracket)
  newBracket <- duplicated(bracket)
  nRounds <- log(nTeams, base=2)
  
  for (round in 1:nRounds) {
    newBracket <- simRound(newBracket, matchups)
  }
  
  return(newBracket)
}

n <- 8 # number of teams
t <- 1:n # teams
M <- matrix(0L, nrow=n, ncol=n) # matchup probability matrix

# randomly generate a matchup probability matrix
for (i in 1:n) {
  for (j in i:n) {
    m <- runif(1) * (i != j)
    M[j,i] <- 1-m
    M[i,j] <- m
  }
}
