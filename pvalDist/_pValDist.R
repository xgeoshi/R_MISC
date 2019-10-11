pvalDist <- function(pvec,
                     valvec,
                     rep = 10000,
                     parallel,
                     export = c("bernulliTimesVal", "valvec",
                                "pmeandist", "binarySim", "actual", "coefs"),
                     coefs = NULL) {
        
        repl.args <- list(n = rep)
        
        if (parallel) {
                require(future)
                require(doFuture)
                registerDoFuture()
                plan(multisession)
                
                source("_parallelRep.R", local = TRUE)
                message("<pvalDist> parallel")
                repl.args[["each"]] <- TRUE
                repl.args[["export"]] <- export
                replX <- parallelRep
                
        } else {
                replX <- replicate
        }
        
        # Creating list of berbulli means distribution
        source("_bernulliMeansDist.R", local = TRUE)
        pmeandist <- bernulliMeansDist(pvec = pvec, each = parallel,
                                       export = export)
        stopifnot(identical(length(pmeandist), length(valvec), length(pvec)))
        
        # Adding argument
        source("_bernulliTimesVal.R")
        repl.args[["expr"]] <- quote(bernulliTimesVal(values = valvec,
                                                      distr = pmeandist,
                                                      coefs = coefs))
        
        # revenue simulations
        rsim <- do.call(replX, repl.args)
        
        stopifnot(length(rsim) == rep)
        return(rsim)
}