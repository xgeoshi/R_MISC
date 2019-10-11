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