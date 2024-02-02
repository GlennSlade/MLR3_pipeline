model_params <- function(auto_ml_result){
  
  
  auto_ml_out$tun_inst$result_learner_param_vals
  
  tu.res <- auto_ml_out$tun_inst$result |> 
    as.data.frame()
  tu.res <- tu.res[,colSums(is.na(tu.res))<nrow(tu.res)]
  # class(tu.res)
  
  # scale.type <- tu.res$scale_branch.selection
  # 
  # prepro.type <- tu.res$pre_branch.selection
  
  # filt.type <- tu.res$flt_branch.selection
  
  # filt.frac <- tu.res[, grep(filt.type, colnames(tu.res), value=TRUE)]
  
  lrnr.type <- tu.res$lrn_branch.selection
  
  # lrnr.params <- tu.res[, grep(lrnr.type, colnames(tu.res), value=TRUE)]
  
  
  if (lrnr.type=="xgb"){
    filt.scores <- auto_ml_out$lrnr$model$importance_xgb$scores
    filt.feats <- auto_ml_out$lrnr$model$importance_xgb$features
  } else if (lrnr.type=="rf"){
    filt.scores <- auto_ml_out$lrnr$model$importance_rf$scores
    filt.feats <- auto_ml_out$lrnr$model$importance_rf$features
  } else if (lrnr.type=="lvm"){
    filt.scores <- auto_ml_out$lrnr$model$anova_lvm$scores
    filt.feats <- auto_ml_out$lrnr$model$anova_lvm$features
  } else if (lrnr.type=="ens"){
    filt.scores <- auto_ml_out$lrnr$model$anova_ens$scores
    filt.feats <- auto_ml_out$lrnr$model$anova_ens$features
  }
  
  tibble::tibble(tu.res,
         filt.feats = I(list(filt.feats)),
         filt.scores = I(list(filt.scores))
         )
}


