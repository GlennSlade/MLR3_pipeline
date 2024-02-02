#' Tune an mlr3 leaner which has tune tokens set.
#'
#' @param .task 
#' @param .lrnr 
#' @param .resamp 
#' @param .measure 
#' @param .n_evals 
#' @param sub.sample 
#' @param .tune_method 
#' @param .test.scale 
#' @param .test.pca 
#' @param .workers 
#' @param .verbose 
#' @param .seed 
#'
#' @return
#' @export
#'
#' @examples
tune_lrnr <- function(.task, .lrnr, .resamp, .measure, .n_evals, sub.sample= NULL,
                         .tune_method=c("random_sample", "mbo"), 
                         .test.scale = FALSE,
                         .test.pca = FALSE,
                         .workers=future::availableCores()-2,
                         .verbose=FALSE,
                         .seed=5446){
  set.seed(.seed)
  
  if (isFALSE(.verbose)){
    lgr::get_logger("mlr3")$set_threshold("warn")
    lgr::get_logger("bbotk")$set_threshold("warn")
  }

  
  # ============== Define graph leaner pipeline ======================
  # browser()
  p0.scale <- ppl("branch", list(
    "scale" = po("scale"),
    "nothing" = po("nop", id="no.scale")
  ), prefix_branchops="scale_")
  
  # preprocessing - PCA or nothing
  p1.prepro <- ppl("branch", list(
    "pca" = po("pca"),
    "nothing" = po("nop")
  ), prefix_branchops="pre_")
  # filter method - here using ANOVA or importance for RF.
  
  # ==============Define hyper params =======================
  # 
  # param_set <- paradox::ps(
  #   scale_branch.selection = p_fct(levels=c("scale", "nothing")))
  
  #====== Stack the pipeline components ==============
  
  if (isTRUE(.test.scale) | isTRUE(.test.pca)){
    if (isTRUE(.test.scale)){
      # browser()
      gr <- p0.scale
      gr$param_set$values$scale_branch.selection <- to_tune(levels=c("scale", "nothing"))
      
    }
    
    if (isTRUE(.test.pca)){
      
      gr <- gr %>>%
        p1.prepro
      gr$param_set$values$pre_branch.selection <- to_tune(levels=c("pca", "nothing"))
    }
    
    gr <- gr %>>%
      .lrnr
    
  } else {
    gr <-
      po("nop")%>>%
      .lrnr
  }
  
  if (!is.null(sub.sample)){
    gr <- po("subsample", frac=sub.sample, id="sample_head")%>>%
      gr
    
  }
  
  # browser()
  glrn <- as_learner(gr)
  
  
  # ========== Run the tuning =====================
  if (.tune_method[1] == "random_search"){
    .tuner <- tnr("random_search")
  } else if (.tune_method[1] == "mbo") {
    .tuner = tnr("mbo")
  }
  
  
  tune_result <- tune_model(.task=.task,
                            .learner = glrn,
                            .resamp = .resamp,
                            .tuner = .tuner,
                            .measure = .measure,
                            term_evals = .n_evals,
                            .workers=.workers)
  
  
  return(list(graph = gr,
              tun_inst = tune_result))
}

#' A wrapper function to tune a model with some defaults
#'
#' @param .task an mlr3 task (which defines the model structure)
#' @param .learner an mlr3 learner (jargon for model) This should include 
#' params for hypertuning
#' @param term_evals integer value - ho many hyperparameter runs to do - more is 
#' better but takes longer
#'
#' @return An mlr3 autotune object from which you can extract the new leaner 
#' with the new hyperparameters
#' 
tune_model <- function(.task, .learner, .resamp, .tuner, .measure, term_evals, .workers){
  at = auto_tuner(
    method = .tuner, 
    learner = .learner,
    resampling = .resamp,
    measure = .measure,
    term_evals = term_evals) 
  
  future::plan("multisession", workers = .workers)
  progressr::with_progress(expr = {
    at$train(.task)
  })
  future:::ClusterRegistry("stop")
  return(at)
}