

##########
# HELPERS
##########


# assume simGame, simRound, and simTournament functions
# assume seeding functions


# get win distribution of a bracket (via simulation)
getWinDistribution <- function(bracket, matchups, nSim=10000) {
    simResults <- replicate(nSim, simTournament(bracket, matchups), simplify='array')
    winners <- numeric(nSim)
    for(i in 1:nSim) winners[i] = simResults[,i]$Winner
    out <- data.frame(table(winners))
    colnames(out) <- c("teams", "freq")
    out$teams <- as.numeric(out$teams)
    return(out)
}


# get prior and posterior win distributions of an old and new bracket
getPriorAndPosteriorWins <- function(oldBracket, newBracket, matchups, nSim=10000) {
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


getExpectedWins <- function(bracket, matchups, nSim=10000) {
    simResults <- replicate(nSim, simTournament(bracket, matchups), simplify='array')
    
    winsPerTeam <- c()
    for(i in 1:nSim) winsPerTeam = rbind(winsPerTeam, simResults[,i]$WinsPerTeam)
    expWins <- apply(winsPerTeam, MARGIN=2, FUN=mean)
    sdWins <- apply(winsPerTeam, MARGIN=2, FUN=sd)
    
    teams <- 1:ncol(matchups)
    out <- data.frame(teams, expWins, sdWins)
    out$teams <- as.numeric(out$teams)
    return(out)
}


getPriorAndPosteriorExpectedWins <- function(oldBracket, newBracket, matchups, nSim=10000) {
    prior <- getExpectedWins(oldBracket, matchups, nSim)
    posterior <- getExpectedWins(newBracket, matchups, nSim)
    
    out <- prior
    colnames(out) <- c("teams", "priorExp", "priorSD")
    
    totTeams <- ncol(matchups)
    oldTeams <- length(oldBracket)
    newTeams <- length(newBracket)
    
    oldRounds <- log2(totTeams) - log2(oldTeams)
    newRounds <- log2(totTeams) - log2(newTeams)
    
    out$priorExp <- out$priorExp
    out$postExp <- posterior$expWins
    out$postSD <- posterior$sdWins
    
    for (team in 1:totTeams) {
        if (any(newBracket == team)) out$postExp[team] = out$postExp[team] + newRounds
        if (any(oldBracket == team)) out$priorExp[team] = out$priorExp[team] + oldRounds
    }
    
    return(out)
}


##########
# TESTING
##########


n <- 8
B <- tournament_seeding(n)
m <- matrix(runif(n^2), nrow=n)

newBracket <- simRound(B, m)

getPriorAndPosteriorWins(B, newBracket, m)

getExpectedWins(B, m)
getExpectedWins(newBracket, m)
