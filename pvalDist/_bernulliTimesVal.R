
# Apply each binary outcome to related revenue
bernulliTimesVal <- function(values, distr, coefs = NULL) {
        
        bin.sim <- binarySim(distr.p = distr)
        
        if (!is.null(coefs) && any(bin.sim == 1)) {
                bin.ones.index <- which(bin.sim == 1)
                len.ones <- length(values[bin.ones.index])
                
                smp.coef <- sample(coefs, len.ones, replace = TRUE)
                v <- values[bin.ones.index]
                stopifnot(length(v) == length(smp.coef))
                values[bin.ones.index] <- v - (v * smp.coef)
        }
        
        return(sum(values * bin.sim))
        
}
