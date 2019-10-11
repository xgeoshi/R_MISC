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