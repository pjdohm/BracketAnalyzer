#install.packages("hash")
#library(hash)

# IDEA: each team plays each other, the teams are ranked by number of wins,
# and if two teams are tied, then their head to head result decides who gets
# the higher seed

generateMatchups <- function(nTeams, randFun=runif) {
  # returns an n x n matchup matrix
  
  matchups = matrix(0L, nrow=nTeams, ncol=nTeams)
  for (i in 1:nTeams) {for (j in 1:nTeams) {
    matchup <- randFun(1) * (i != j)
    matchups[j,i] <- 1 - matchup
    matchups[i,j] <- matchup
  }}
  
  return(matchups)
}

simGame <- function(teams, matchups) {
  # returns winner of game
  
  team1 <- teams[1]
  team2 <- teams[2]
  matchup <- matchups[team1, team2]
  result <- runif(1) < matchup
  return(teams[(!result) + 1])
}

# this is just one seeding method
seedTeams <- function(teams, matchups) {
  # returns seeds of teams as a vector
  
  nTeams <- length(teams)
  winMatrix <- matrix(0L, nrow=nTeams, ncol=nTeams)
  
  for (i in 1:nTeams) {for (j in i:nTeams) {
    result <- simGame(c(i,j), matchups) * (i != j)
    winMatrix[j,i] <- !result
    winMatrix[i,j] <- result
  }}
  
  wins <- rowSums(winMatrix)
  seeds <- order(wins, decreasing=TRUE)
  return(seeds)
}

seedsToBracket <- function(teams, seeds) {
  # returns bracket (vector) based on seeds of teams
  
  nTeams <- length(teams)
  bracket <- numeric(nTeams)
  
  for (i in 1:(nTeams/2)) {
    bracket
  }
}

nTeams <- 4


teams <- 1:nTeams
matchups <- generateMatchups(nTeams)



