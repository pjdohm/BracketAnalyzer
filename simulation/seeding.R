#' Creates a seeding arrangement that is left skewed.
#'
#' @param size is the number of seeds to generate. This needs
#' to be a power of two.
#'
left_skewed_seeding = function(size) {
    
    # Stop if not a power of two
    if (sum(as.numeric(intToBits(size))) != 1) {
        stop("Size must be a power of two")
    }
    
    # Special case when size is 2
    if (size == 2) {
        return(c(1, 2))
    }
    
    # Create seeds and left/right branches
    seeds = 1:size
    left_seeds = numeric(size / 2)
    right_seeds = numeric(size / 2)
    
    # Indexes for the left/right branches
    left_idx = 1
    right_idx = 1
    
    # Keep track of which branch will get the next seed
    left_turn = TRUE
    
    # For each iteration add the first and last seed to the
    # current branch
    for (i in 1:(size / 2)) {
        if (left_turn) {
            left_seeds[left_idx] = seeds[i]
            left_seeds[left_idx + 1] = seeds[(size + 1) - i]
            left_idx = left_idx + 2
            left_turn = FALSE
        } else {
            right_seeds[right_idx] = seeds[i]
            right_seeds[right_idx + 1] = seeds[(size + 1) - i]
            right_idx = right_idx + 2
            left_turn = TRUE
        }
    }
    
    # Combine branches
    c(left_seeds, right_seeds)
}

#' Creates a traditional tournament style seeding arrangement. 
#'
#' @param size is the number of seeds to generate. This needs
#' to be a power of two.
#'
tournament_seeding = function(size) {
    
    #' Generates the next round of seeding based off the passed in round.
    #'
    #' @param seeds is the previous round of seeding to generate for.
    #'
    nextRound = function(seeds) {
        
        # Next round has twice the size
        nextSize = length(seeds) * 2
        out = numeric(nextSize)
        idx = 1
        
        # Make sure to match up best seed with worst
        for (seed in seeds) {
            out[idx] = seed
            out[idx + 1] = (nextSize + 1) - seed
            idx = idx + 2
        }
        
        return(out)
    }
    
    # Stop if not a power of two
    if (sum(as.numeric(intToBits(size))) != 1) {
        stop("Size must be a power of two")
    }
    
    # Special case when size is 2
    if (size == 2) {
        return(c(1, 2))
    }
    
    # Calculate number of rounds
    rounds = log2(size) - 1
    
    # Start with first two seeds
    seeds = c(1, 2)
    
    # Iteratively generate next seeds
    for (i in 1:rounds) {
        seeds = nextRound(seeds)
    }
    
    return(seeds)
}

#' Creates a random seeding arrangement. 
#'
#' @param size is the number of seeds to generate. This needs
#' to be a power of two.
#'
random_seeding = function(size) {
    # Stop if not a power of two
    if (sum(as.numeric(intToBits(size))) != 1) {
        stop("Size must be a power of two")
    }
    
    return(sample(1:size, size))
}

