retro <- function(stk, idxs, fit, retro=2, ...){
  args <- list(...)
  lst0 <- split(0:retro, 0:retro)
  lst0 <- lapply(lst0, function(x){
    yr <- range(stk)["maxyear"] - x
    args$stock <- window(stk, end=yr)
    args$indices <- FLIndices(window(idxs, end=yr))
    args$fmodel <- fit@pars@stkmodel@fMod
    args$srmodel <- fit@pars@stkmodel@srMod
    args$qmodel <- list(fit@pars@qmodel[[1]]@formula)
    fit <- do.call("sca", args)
    args$stock + fit
  })
  FLStocks(lst0)
}

