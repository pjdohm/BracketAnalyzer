

##########
# HELPERS
##########


# assume simGame, simRound, and simTournament functions
# assume seeding functions


# get win distribution of a bracket (via simulation)
getWinDistribution <- function(bracket, matchups, nSim=10000) {
    winners <- replicate(nSim, simTournament(bracket, matchups))
    out <- data.frame(table(winners))
    colnames(out) <- c("teams", "freq")
    out$teams <- as.numeric(out$teams)
    return(out)
}


# get prior and posterior win distributions of an old and new bracket
getPriorAndPosterior <- function(oldBracket, newBracket, matchups, nSim=10000) {
    prior <- getWinDistribution(oldBracket, matchups, nSim)
    posterior <- getWinDistribution(newBracket, matchups, nSim)
    
    out <- prior
    colnames(out) <- c("teams", "priorFreq")
    
    nTeams <- length(oldBracket)
    for (team in 1:nTeams) {
        if (!(any(newBracket == team))) {
            tempDF <- data.frame(t(c(team, 0)))
            colnames(tempDF) <- c("teams", "freq")
            posterior <- rbind(posterior, tempDF)
        }
    }
    
    posterior <- posterior[order(posterior$teams),]
    out$postFreq <- posterior$freq
    
    return(out)
}


##########
# TESTING
##########


nTeams <- 8
bracket <- tournament_seeding(nTeams)
matchups <- matrix(runif(nTeams^2), nrow=nTeams)

newBracket <- simRound(bracket, matchups)

getPriorAndPosterior(bracket, newBracket, matchups)
=======



##########
# HELPERS
##########


# assume simGame, simRound, and simTournament functions
# assume seeding functions


# get team given seed
getTeamFromSeed <- function(seed, teams, seeds)


# get win distribution of a bracket (via simulation)
getWinDistribution <- function(bracket, matchups, nSim=10000) {
    winners <- replicate(nSim, simTournament(bracket, matchups))
    out <- data.frame(table(winners))
    colnames(out) <- c("teams", "freq")
    out$teams <- as.numeric(out$teams)
    return(out)
}


# get prior and posterior win distributions of an old and new bracket
getPriorAndPosterior <- function(oldBracket, newBracket, matchups, nSim=10000) {
    prior <- getWinDistribution(oldBracket, matchups, nSim)
    posterior <- getWinDistribution(newBracket, matchups, nSim)
    
    out <- prior
    colnames(out) <- c("teams", "priorFreq")
    
    nTeams <- length(oldBracket)
    for (team in 1:nTeams) {
        if (!(any(newBracket == team))) {
            tempDF <- data.frame(t(c(team, 0)))
            colnames(tempDF) <- c("teams", "freq")
            posterior <- rbind(posterior, tempDF)
        }
    }
    
    posterior <- posterior[order(posterior$teams),]
    out$postFreq <- posterior$freq
    
    return(out)
}


##########
# TESTING
##########


nTeams <- 8
bracket <- tournament_seeding(nTeams)
matchups <- matrix(runif(nTeams^2), nrow=nTeams)

newBracket <- simRound(bracket, matchups)

getPriorAndPosterior(bracket, newBracket, matchups)
>>>>>>> 5e4ffb2372f301f6087b5a15d39dc1d070883522
