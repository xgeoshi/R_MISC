# SAMPLE DATA
x <- data.frame(case = c("case1", "case2", "case3", "case4", "case5", "case6", "case7", "case8", "case9"),
                prob = c(0.85, 0.2345, 0.0555, 0.001, 0.35, 0.16, 0.68, 0.4, 0.12),
                reve = c(15000, 10000, 5000, 5000, 7000, 2000, 3000, 4000, 1000),
                stringsAsFactors = FALSE)



# Vector of All Examining Probabilities
p <- x$prob

# Minimum Length (for 10 Success-Failures binomial condition) ----
min.tail <- min(p, 1 - p) # min value of all range of both heads and tails
min.len  <- ceiling(10 / min.tail) # min length to get 10 binom successes


bernulliTrial <- function(success_p, length) {
        sample(x       = c(0, 1),
               size    = length,
               replace = TRUE,
               prob    = c(1 - success_p, success_p))
        
}

# trial1 <- bernulliTrial(success_p = 0.30, length = 25)

# mean proportion of binomial distribution upon single success P and given len

bernulliMean <- function(p., length) {
        stopifnot(length(p.) == 1L)
        mean(bernulliTrial(success_p = p., length = length))
}


# list of binomial means for given P vector - p distibutions of given length
pMeanDist <- function(pvec, rep = 10000, bern_len = min.len) {
        
        distrs <- lapply(seq(length(pvec)), function(n) {
                p <- pvec[n] # single nth probability value (vectorised)
                prop.dist <- replicate(rep, bernulliMean(p. = p, length = bern_len))
                return(prop.dist)
        })
        
        stopifnot(length(distrs) == length(pvec))
        names(distrs) <- pvec
        return(distrs)
}

# names(pdists) <- x$prob # 

require(gridExtra)
require(ggplot2)
require(data.table)
require(forcats)
plot.data <- list()

for (r in seq(length(pmeandist))) {
        plot.data[[r]] <- data.table(p = pmeandist[[r]], cat = names(pmeandist[r]))
}

# plotbind <- rbindlist(plot.data)

ggplot(data = rbindlist(plot.data), aes(x = p)) +
        geom_histogram() +
        facet_wrap(~ as_factor(cat), scales = "free_x")


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