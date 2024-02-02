auto_ml_tune <- function(.task, .resamp, .measure, .n_evals, 
                         .tune_method, sub.sample= NULL,
                         .test.pca = FALSE,
                         .workers=future::availableCores()-2,
                         .verbose=FALSE,
                         .seed=5446){
  set.seed(.seed)
  
  if (isFALSE(.verbose)){
    lgr::get_logger("mlr3")$set_threshold("warn")
    lgr::get_logger("bbotk")$set_threshold("info")
  }
  # leaners to test
  # ================== xgboost with hypertuning 
  xgb.lrn.tune <- mlr3::lrn("classif.xgboost", id="xgb")
  # ================== random forest with hypertuning 
  rf.lrn.tune <- mlr3::lrn("classif.ranger", id="rf")
  # ================== random forest with hypertuning 
  svm.lrn.tune <- mlr3::lrn("classif.svm", id="svm", type = "C-classification")
  # =================== Define the learner pipeline 
  
  ens.lrn.tune <- gunion(list(po("learner_cv", xgb.lrn.tune, id="ens_xgb"),
                              po("learner_cv", rf.lrn.tune, id="ens_rf"),
                              po("learner_cv", svm.lrn.tune, id="ens_svm"),
                              po("nop", "ens_nop"))) %>>%
    po("featureunion", id="ens_union") %>>% 
    lrn("classif.ranger", id="master_rf")
  
  # ============== Define graph leaner pipeline ======================
  
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
  
  # p2.filter <- ppl("branch", list(
  #   "anova" = flt("anova"),
  #   "xgb_importance" = flt("importance",
  #                      learner = lrn("classif.xgboost"),
  #                      id="importance_xgb"),
  #   "rf_importance" = flt("importance", 
  #                      learner = lrn("classif.ranger", importance = "impurity"),
  #                      id="importance_rf")
  # ), prefix_branchops="flt_")
  
  
  p3.lrnopts <- ppl("branch", list(
    "xgb"= po("filter", flt("importance",
                            learner = lrn("classif.xgboost"),
                            id="importance_xgb")) %>>%  
      xgb.lrn.tune,
    
    "rf" = po("filter", flt("importance", 
                            learner = lrn("classif.ranger", importance = "impurity"),
                            id="importance_rf")) %>>%
      rf.lrn.tune,
    
    "svm" = po("filter", flt("anova", id="anova_lvm")) %>>% 
                 svm.lrn.tune,
    
    "ens" = po("filter", flt("anova", id="anova_ens")) %>>%
      ens.lrn.tune
    
  ), prefix_branchops="lrn_")
  
  
  # ==============Define hyper params =======================
  
  param_set <- paradox::ps(
    scale_branch.selection = p_fct(levels=c("scale", "nothing"))
    
    # , flt_branch.selection= p_fct(levels =c("anova", "rf_importance"))
    , anova_lvm.filter.frac = p_dbl(lower=0.1, upper = 1)
    , importance_xgb.filter.frac = p_dbl(lower=0.1, upper = 1)
    , importance_rf.filter.frac = p_dbl(lower=0.1, upper = 1)
    , anova_ens.filter.frac = p_dbl(lower=0.1, upper = 1)
    , lrn_branch.selection = p_fct(levels = c("xgb", "rf", "svm", "ens"))
    
    , xgb.eta = p_dbl( lower = 1e-04, upper=1, logscale=TRUE)
    , xgb.nrounds = p_int(lower=1, upper=100)
    , xgb.max_depth = p_int(lower=1, upper=20) 
    , xgb.colsample_bytree = p_dbl(lower = 0.1, upper=1)
    , xgb.colsample_bylevel = p_dbl(lower = 0.1, upper=1)
    , xgb.lambda = p_dbl(lower = 1e-3, upper=1000, logscale=TRUE)
    , xgb.alpha = p_dbl(lower = 1e-3, upper=1000, logscale=TRUE)
    , xgb.subsample = p_dbl(lower = 0.1, upper=1)
    
    
    , rf.mtry.ratio = p_dbl(lower=0, upper=1)
    , rf.replace = p_lgl()
    , rf.num.trees = p_int(lower=5, upper=100)
    , rf.sample.fraction = p_dbl(lower = 0.1, upper=1)
    
    , svm.cost = p_dbl(lower = 1e-04, upper=10000, logscale=TRUE)
    , svm.gamma =p_dbl( lower = 1e-4, upper=1e3, logscale=TRUE)
    , svm.degree =p_int( lower = 2, upper=5)
    , svm.kernel= p_fct(levels=c("polynomial", "radial", "sigmoid", "linear"))
  )
  
  
  param_set$add_dep("importance_xgb.filter.frac", "lrn_branch.selection", CondEqual$new("xgb"))
  param_set$add_dep("importance_rf.filter.frac", "lrn_branch.selection", CondEqual$new("rf"))
  param_set$add_dep("anova_lvm.filter.frac", "lrn_branch.selection", CondEqual$new("svm"))
  param_set$add_dep("anova_ens.filter.frac", "lrn_branch.selection", CondEqual$new("ens"))
  
  param_set$add_dep("xgb.eta", "lrn_branch.selection", CondAnyOf$new(c("xgb", "ens")))
  param_set$add_dep("xgb.nrounds", "lrn_branch.selection", CondAnyOf$new(c("xgb", "ens")))
  param_set$add_dep("xgb.max_depth", "lrn_branch.selection", CondAnyOf$new(c("xgb", "ens")))
  
  param_set$add_dep("xgb.colsample_bytree", "lrn_branch.selection", CondAnyOf$new(c("xgb", "ens")))
  param_set$add_dep("xgb.colsample_bylevel", "lrn_branch.selection", CondAnyOf$new(c("xgb", "ens")))
  param_set$add_dep("xgb.lambda", "lrn_branch.selection", CondAnyOf$new(c("xgb", "ens")))
  param_set$add_dep("xgb.alpha", "lrn_branch.selection", CondAnyOf$new(c("xgb", "ens")))
  param_set$add_dep("xgb.subsample", "lrn_branch.selection", CondAnyOf$new(c("xgb", "ens")))
  
  param_set$add_dep("rf.mtry.ratio", "lrn_branch.selection", CondAnyOf$new(c("rf", "ens")))
  param_set$add_dep("rf.replace", "lrn_branch.selection", CondAnyOf$new(c("rf", "ens")))
  param_set$add_dep("rf.num.trees", "lrn_branch.selection", CondAnyOf$new(c("rf", "ens")))
  param_set$add_dep("rf.sample.fraction", "lrn_branch.selection", CondAnyOf$new(c("rf", "ens")))
  
  param_set$add_dep("svm.cost", "lrn_branch.selection", CondAnyOf$new(c("svm", "ens")))
  param_set$add_dep("svm.gamma", "lrn_branch.selection", CondAnyOf$new(c("svm", "ens")))
  param_set$add_dep("svm.degree", "lrn_branch.selection", CondAnyOf$new(c("svm", "ens")))
  param_set$add_dep("svm.kernel", "lrn_branch.selection", CondAnyOf$new(c("svm", "ens")))
  param_set$add_dep("svm.degree", "svm.kernel", CondEqual$new("polynomial"))
  param_set$add_dep("svm.gamma", "svm.kernel", CondAnyOf$new(c("polynomial", "radial", "sigmoid")))
  
  
  #====== Stack the pipeline components ==============
  gr <- p0.scale
  
  if (isTRUE(.test.pca)){
    param_set$add(paradox::ps(pre_branch.selection = p_fct(levels=c("pca", "nothing"))))
    gr <- gr %>>%
      p1.prepro
  }
  
  
  if (.tune_method == "random_search"){
    gr <- gr %>>%
      p3.lrnopts
  } else if (.tune_method == "hyperband") {
    gr <- gr %>>%
      po("subsample") %>>%
      p3.lrnopts
    
    param_set$add(paradox::ps(subsample.frac = p_dbl(lower = 0.1, upper = 1, tags = "budget")))
  }
  
  
  if (!is.null(sub.sample)){
    gr <- po("subsample", frac=sub.sample, id="sample_head")%>>%
      gr
  
  }
  
  # browser()
  glrn <- GraphLearner$new(gr)
  

  # ========== Run the tuning =====================

  inst <- TuningInstanceSingleCrit$new(task= .task,
                                       learner = glrn,
                                       resampling = .resamp,
                                       measure = .measure,
                                       search_space = param_set,
                                       terminator = trm("evals", n_evals=.n_evals)
  )
  
  if (.tune_method == "random_search"){
    tuner <- tnr("random_search")
  } else if (.tune_method == "hyperband") {
    tuner = tnr("hyperband", eta = 3L)
  }
  
  # future::plan(list(
  #   "sequential",
  #   future::tweak("multisession", workers = .workers)
  # ))
  future::plan("multisession", workers = .workers)
  progressr::with_progress(expr = {
    tuner$optimize(inst)
  })
  future:::ClusterRegistry("stop")
  
  
  lrner_tuned <- as_learner(glrn)
  
  lrner_tuned$param_set$set_values(.values = inst$result_learner_param_vals)
  
  return(list(lrnr = lrner_tuned$train(.task),
              tun_inst = inst))
}