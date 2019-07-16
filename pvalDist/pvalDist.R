# SAMPLE DATA
x <- data.frame(case = c("case1", "case2", "case3", "case4", "case5", "case6", "case7"),
                prob = c(0.85, 0.2345, 0.0555, 0.001, 0.35, 0.16, 0.68),
                reve = c(15000, 10000, 5000, 5000, 7000, 2000, 3000),
                stringsAsFactors = FALSE)



# Vector of All Examining Probabilities
p <- x$prob

# Minimum Length (for 10 Success-Failures binomial condition) ----
min.tail <- min(p, 1 - p) # min value of all range of both heads and tails
min.len  <- ceiling(10 / min.tail) # min length to get 10 binom successes

# mean proportion of binomial distribution upon single success P and given len
pDeviate1 <- function(p., len = min.len) {
        
        stopifnot(length(p.) == 1L)
        
        smp <- sample(x       = c(0, 1),
                      size    = len,
                      replace = TRUE,
                      prob    = c(1 - p., p.))
        return(mean(smp))
}


# list of binomial means for given P vector - p distibutions of given length
pMeanDist <- function(pvec, len = 10000) {
        
        distrs <- lapply(seq(length(pvec)), function(n) {
                p <- pvec[n] # single nth probability value (vectorised)
                prop.dist <- replicate(len, pDeviate1(p. = p))
                return(prop.dist)
        })
        
        stopifnot(length(distrs) == length(pvec))
        names(distrs) <- pvec
        return(distrs)
}

# names(pdists) <- x$prob # 


# VECTOR OF SIMULATED BINARY OUTCOMES UPON SINGLE RANDOM SUCCESS PROBABILITY
# FROM EACH DISTRIBUTION OF PROBABILITIES 
binarySim <- function(distr.p) {
        
        # take by 1 random value from each distribution (for each probability)
        rand.each1 <- vapply(distr.p, function(x) sample(x, 1), numeric(1))
        
        # 1 sample from each dist as "success probability" for binary simulation
        bin.sim <- vapply(rand.each1, function(b) {
                sample(x       = c(0, 1),
                       size    = 1,
                       replace = TRUE,
                       prob    = c(1 - b, b))
                }, 
                numeric(1))
        
        stopifnot(length(distr.p) == length(bin.sim))
        return(bin.sim)
}


# SIMULATED TOTAL REVENUE DISTRIBUTION
pvalDist <- function(pvec, valvec = x$reve, rep = 10000) {
        
        pmeandist <- pMeanDist(pvec = pvec)
        stopifnot(identical(length(pmeandist), length(valvec), length(pvec)))
        
        rsim <- replicate(rep, {
                # Apply each binary outcome to related revenue
                rev <- sum(valvec * binarySim(pmeandist))
                return(rev)
        })
        
        stopifnot(length(rsim) == rep)
        return(rsim)
}



simdist <- pvalDist(pvec = x$prob, valvec = x$reve, rep = 10000)
hist(simdist)