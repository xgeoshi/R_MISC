# Function to create deviation distribution of given length for each p
# list of binomial means for given P vector - p distibutions of given length

bernulliMeansDist <- function(pvec, rep = 10000, each, export) {
        require(foreach)
        
        source("_pMinLen.R", local = TRUE)
        source("_bernulli.R", local = TRUE)
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
                                   .export = export) %dopar% nthMean(n = n)
                
        } else {
                
                dist.ls <- lapply(seq(length(pvec)), function(n) nthMean(n))
        }
        
        
        stopifnot(length(unique(vapply(dist.ls, length, numeric(1)))) == 1L)
        stopifnot(length(dist.ls) == length(pvec))
        
        names(dist.ls) <- paste0("case", seq_along(pvec), "_p", pvec)
        
        return(dist.ls)
}
