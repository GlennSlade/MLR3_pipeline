ml_resample <- function(.task, .learner,
                        spcv_plan = mlr3::rsmp("repeated_spcv_coords", folds = 10, repeats=10),
                        .folds = 10, .repeats=10,
                        .verbose=FALSE,
                        .workers = future::availableCores()-2,
                        .seed=5446){
  set.seed(.seed)
  
  if (isFALSE(.verbose)){
    lgr::get_logger("mlr3")$set_threshold("warn")
    lgr::get_logger("bbotk")$set_threshold("info")
  }
  #========= now let's check the accuracy wih spatial resampling ===================
  
  resample_wrap <- function(.tsk, .lrn, .rsmp){
    
    resampled_model <- progressr::with_progress(expr = {
      mlr3::resample(
        task = .tsk,
        learner = .lrn,
        resampling = .rsmp, #rsmp("cv", folds=10) try this resampling and you'll see why we need spatial resampling.
        store_models = FALSE,
        encapsulate = "evaluate"
      )
    })
    
    return(resampled_model)
  }
  # future::plan(list(
  #   "sequential",
  #   future::tweak("multisession", workers = .workers)
  # ))
  future::plan("multisession", workers = .workers)
  resamp.run <- resample_wrap(.task, .learner, spcv_plan)
  future:::ClusterRegistry("stop")
  return(resamp.run)
}