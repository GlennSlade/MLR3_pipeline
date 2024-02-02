benchmark_lrnrs <- function(..., .task, .resamp, .workers= future::availableCores()-2){
  .resamp$instantiate(.task)
  
  .lrn_list <- list(...)
  
  .design <- data.table::data.table(task=lapply(1:length(.lrn_list), function(x) return(.task)),
                                    learner=.lrn_list,
                                    resampling=lapply(1:length(.lrn_list), function(x) return(.resamp)))
  
  future::plan("multisession", workers =.workers)
  bench <- progressr::with_progress(expr = {
    mlr3::benchmark(.design)
  })
  future:::ClusterRegistry("stop")
  
  return(bench)
}