# Same as replicate but in parallel
# require(parallel)
require(foreach)
require(doFuture)

parallelRep <- function(n,
                        expr,
                        each,
                        export) {
        
        source("_binarySim.R", local = TRUE)    
    
        # print("parallel execute")
        # 
        # require(doParallel)
        # 
        # ls <- foreach(n = seq(rep), .combine = c) %do% eval(sub.fun)
        # ls <- foreach(n = seq(rep), .combine = c) %dopar% eval(sub.fun)
        # ls <- parLapply(cl = clus, X = seq(rep), function(x) eval(sub.fun))
        # plan(sequential)
        # replfun <- future_lapply
        
        # res <- unlist(ls)
        # return(res)
    
    #COMMON
        sub.expr <- substitute(expr)
    
    # FOREACH w/DoFuture

        
        # registerDoFuture()
        # plan(multisession)
        # plan(multisession, workers = 6)
        if (each) {
                par.out <- foreach(n = numeric(n), .export = export) %dopar% 
                    eval(sub.expr)
        } else {
                par.out <- lapply(seq(n), function(x) eval(sub.expr))
        }
        
    # FOREACH w/doParallel
        # require(doParallel)
        # cores <- detectCores(logical = FALSE)
        # use.core <- max(c(1, cores - 1L))
        # cl <- makeCluster(use.core)
        # c.env <- environment()
        # parallel::clusterExport(cl = cl, varlist = "sub.fun", envir = c.env)
        # glb.vars <- ls(envir = globalenv())
        # parallel::clusterExport(cl = cl, varlist = glb.vars, envir = globalenv())
        # 
        # registerDoParallel(cl)
        # 
        # par.out <- foreach(n = numeric(rep)) %dopar% eval(sub.fun)
        # stopCluster(cl)
        
    # FUTURE APPLY # 967 on 100000
        # plan(multicore)
        # par.out <- future_lapply(seq(rep), function(x) eval(sub.fun))
        
    #NEW parallel # 400 on 100000
        # cores <- detectCores(logical = FALSE)
        # use.core <- max(c(1, cores - 1L))
        # message("Cores used in cluster: ", use.core)
        # cl <- makeCluster(use.core)
        # on.exit(stopCluster(cl))
        # c.env <- environment()
        # parallel::clusterExport(cl = cl, varlist = "sub.fun", envir = c.env)
        # glb.vars <- ls(envir = globalenv())
        # parallel::clusterExport(cl = cl, varlist = glb.vars, envir = globalenv())
        # 
        # 
        # par.out <- clusterApplyLB(cl, seq(rep), function(x) eval(sub.fun))
    
    #COMMON OUT
        unlist(par.out)
}
