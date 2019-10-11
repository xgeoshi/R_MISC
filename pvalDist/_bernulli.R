
# Return vector of Zero & Ones with given success (1) probability & given length
bernulli <- function(success_p, length) {
        
        stopifnot(length(success_p) == 1L)
        if (success_p == 0 | success_p == 1) return(rep(success_p, length))
        
        sample(x       = c(0, 1),
               size    = length,
               replace = TRUE,
               prob    = c(1 - success_p, success_p))
        
}