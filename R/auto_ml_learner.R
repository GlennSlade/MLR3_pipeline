
#' Auto ML leaner
#' 
#' Creates a trained learner using an auto-ml frame work. Tunes across pre 
#' processing (PCA or none), feature selection (anova or RF importance), fraction
#' of features retained, and then model selection (xgb, rf and svm), and finally
#' across a selecion of hyper parameters of these models.
#'
#' @param df the input data frame
#' @param site_name the site name/prefix
#' @param drop_cols columns to drop - should include "Class" as we don't want 
#' this for the ML. could also include other covariates that are unwanted.
#' @param .folds default 10. he number of folds to use for spatial cross validation
#' @param .repeats default 5. The number of repeat resamplings.
#' @param .n_evals number of evaluations to use for the tuning - the more the 
#' better but takes longer.
#' @param sub.sample default NULL - if testing and you just want to use a subsample of the data
#' set this to a sensible smaller number e.g. 100.
#' 
auto_ml_learner <- function(df, site_name, drop_cols = c("Class"), 
                           .folds = 10, .repeats=5, 
                           .tune_method = c("random_search", "hyperband"),
                           .n_evals= 100,
                           sub.sample=1,
                           test.pca = FALSE){
  
  full_df <- readRDS(df) |> 
    select(!all_of(drop_cols)) 
  
  # if (!is.null(sub.sample)){
  #   full_df <- dplyr::slice_sample(full_df, n=sub.sample) # testing only!!!
  # }
  
  
  # define the mlr3 task
  task = mlr3spatiotempcv::TaskClassifST$new(
    id = site_name,
    backend = full_df, 
    target = "Type",
    coordinate_names = c("x", "y"),
    coords_as_features = FALSE,
    crs = attributes(full_df)$CRS)
  
  
  #=============Define the spatial resampling for the auto-ml step ===============
  
  spcv_plan <- mlr3::rsmp("repeated_spcv_coords", 
                          folds = .folds, 
                          repeats=.repeats)
  
  
  # =========== Model tuning/filtering/selection ====================
  # run the auto ml/hyperparam tuning and return a new graph learner.
  auto_lrn <- auto_ml_tune(.task = task, 
                           .resamp = spcv_plan, 
                           .measure = msr("classif.acc"),
                           .tune_method = .tune_method[1],
                           .n_evals = .n_evals, 
                           sub.sample = sub.sample,
                           .test.pca = test.pca)
  
  return(list(
    task = task,
    lrnr = auto_lrn$lrnr,
    spcv = spcv_plan,
    tun_inst = auto_lrn$tun_inst
  ))
  
}




