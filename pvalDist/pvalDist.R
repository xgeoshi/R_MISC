# SAMPLE DATA
x <- data.frame(case = c("case1", "case2", "case3", "case4", "case5", "case6", "case7", "case8", "case9"),
                prob = c(0.85, 0.2345, 0.0555, 0.001, 0.35, 0.16, 0.68, 0.4, 0.12),
                reve = c(15000, 10000, 5000, 5000, 7000, 2000, 3000, 4000, 1000),
                stringsAsFactors = FALSE)



# Vector of All Examining Probabilities
p <- x$prob

# Minimum Length (for 10 Success-Failures binomial condition) ----
pMinLen <- function(p) {
        
        # check for probability condition
        stopifnot(p >= 0 & p <= 1)
        
        # indexing zeroes an ones (0% & 100% probability)
        p01 <- which(p == 0 | p == 1)
        
        # return NA if input consist of only zeroes and/or ones
        if (length(p01) == length(p)) return(NA)
        
        # condition in case some zeroes and ones present
        if (length(p01) > 0) {
                # vector without zeroes & ones + meassge
                non01_p <- p[-p01]
                message("<pMinLen>: ", length(p01),
                        " zeroes & ones are removed / total ", length(p), " p")
        }
        
        # condition if there are no zeroes & ones
        if (length(p01) == 0) non01_p <- p
        
        # min value of all range of both heads and tails of non zero & ones
        min.p <- min(non01_p, 1 - non01_p)
        # proportion to evaluate min length to get 10 binom successes
        min.len  <- ceiling(10 / min.p)
        
        stopifnot(length(min.len) == 1L && min.len > 0)
        message(min.p, " minimum length for bernulli success-failure cond: ",
                min.len)
        return(min.len)
}

# Return vector of Zero & Ones with given success (1) probability & given length
bernulli <- function(success_p, length) {
        
        stopifnot(length(success_p) == 1L)
        if (success_p == 0 | success_p == 1) return(rep(success_p, length))
        
        sample(x       = c(0, 1),
               size    = length,
               replace = TRUE,
               prob    = c(1 - success_p, success_p))
}

# mean proportion of binomial distribution upon single success P and given len

# bernulliMean <- function(success_p, length) {
#         
#         stopifnot(length(success_p) == 1L)
#         
#         if (success_p == 0 | success_p == 1) return(success_p)
#         
#         if (success_p > 0 & success_p < 1) {
#                 return(mean(bernulli(success_p = success_p, length = length)))}
# }


# list of binomial means for given P vector - p distibutions of given length
bernulliMeansDist <- function(pvec, rep = 10000, each, export = "n") {
        
        mlen <- pMinLen(pvec)
        # stopifnot(mlen > 0)
        
        nthMean <- function(n) {

                p <- pvec[n] # single nth probability value (vectorised)

                if (p == 0) p.dist <- numeric(length = rep)
                if (p == 1) p.dist <- rep(1, length = rep)

                if (p > 0 & p < 1) {
                        p.dist <- replicate(rep,
                                            mean(bernulli(success_p = p,
                                                          length = mlen)))
                }

                return(p.dist)
        }
        
        if (each) {
                dist.ls <- foreach(n = seq(length(pvec)),
                                   .export = export) %dopar% nthMean(n)
                
        } else {
                
                dist.ls <- lapply(seq(length(pvec)), function(n) nthMean(n))
        }
        
        
        stopifnot(length(unique(vapply(dist.ls, length, numeric(1)))) == 1L)
        stopifnot(length(dist.ls) == length(pvec))
        
        names(dist.ls) <- paste0(seq_along(pvec), "_", pvec)
        
        return(dist.ls)
}


require(ggplot2)
require(data.table)
require(forcats)

# pmeandist <- bernulliMeansDist(p)

plotMeansDist <- function(pmeandist) {
        plot.data <- list()
        
        for (r in seq(length(pmeandist))) {
                plot.data[[r]] <- data.table(p = pmeandist[[r]],
                                             cat = names(pmeandist[r]))
        }
        
        # plotbind <- rbindlist(plot.data)
        
        ggplot(data = rbindlist(plot.data), aes(x = p)) +
                geom_histogram() +
                facet_wrap(~ as_factor(cat), scales = "free_x")
}


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

# Apply each binary outcome to related revenue
bernulliTimesVal <- function(values, distr) {
        sum(values * binarySim(distr.p = distr))
}


# SIMULATED TOTAL REVENUE DISTRIBUTION
pvalDist <- function(pvec, valvec, rep = 10000, parallel) {
        
        repl.args <- list(n = rep)
        
        if (parallel) {
                require(future)
                require(doFuture)
                registerDoFuture()
                plan(multisession)
                
                source("parallelRep.R", local = TRUE)
                message("<pvalDist> parallel")
                repl.args[["each"]] <- TRUE
                repl.args[["export"]] <- c("bernulliTimesVal", "valvec",
                                           "pmeandist","binarySim", "actual")
                replX <- parallelRep
                
        } else {
                replX <- replicate
        }
        
        # Creating list of berbulli means distribution
        pmeandist <- bernulliMeansDist(pvec = pvec, each = parallel)
        stopifnot(identical(length(pmeandist), length(valvec), length(pvec)))
        
        # Adding argument
        repl.args[["expr"]] <- quote(bernulliTimesVal(values = valvec,
                                                      distr = pmeandist))
        
        # CREATE OUTER FUNCTION FROM BELOW CODE
        rsim <- do.call(replX, repl.args)
        
        stopifnot(length(rsim) == rep)
        return(rsim)
}



simdist <- pvalDist(pvec = actual$`Probability %`,
                    valvec = actual$`Amount USD`,
                    rep = 100,
                    parallel = TRUE)
hist(simdist)
