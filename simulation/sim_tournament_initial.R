library(rlang) # for duplicate function

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

# want to capture number of wins each team has per tournament
# list with the winning team
# "winner = team #
# "num wins" = c(how many wins each team has gotten)

simTournament <- function(bracket, matchups) {
  # function to simulate a tournament given a bracket and probability matrix
  
  # INPUT:
  # bracket is the bracket structure
  # matchups is the matchup/probability matrix
  
  # OUTPUT:
  # returns winner of tournament
  
  nTeams <- length(bracket)
  newBracket <- bracket
  nRounds <- log(nTeams, base=2)
  
  results = c("Winner"=-1,"WinsPerTeam"=list(rep(0, nTeams)))
  
  for (round in 1:nRounds) {
    #winsPrev = results["WinsPerTeam"]
    #print(newBracket)
    newBracket <- simRound(newBracket, matchups)
    for(i in newBracket){
      results$WinsPerTeam[i] = results$WinsPerTeam[i]+1
    }
  }
  
  results["Winner"] = newBracket
  return(results)
}



########### TEST SIMULATION ###########

n <- 16 # number of teams
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

nSim <- 1000 # number of tournaments to simulate

#### GET M AND SEEDING STRUCTURE t FROM OTHER FILES ####

# t should be bracket structure, and M is probability matrix
simResults <- replicate(nSim, simTournament(t, M), simplify=FALSE)
# access results of the simulation as following syntax:
# simResults[[1]]$WinsPerTeam
