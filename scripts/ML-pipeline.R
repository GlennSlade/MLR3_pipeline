# This way is more manual which has some advantages but may need adjustment for
# multiple sites as you have.

source("R/load_deps.R")

set.seed(5446)


# load and clean the pixel level data
full_df <- readRDS("data_out/Bokspits_1/Bokspits_1ML_in_grid_level.rds")|> 
  select(!all_of("Class")) 


# define the mlr3 task
task = mlr3spatiotempcv::TaskClassifST$new(
  id = "Bokspits_1",
  backend = full_df, #point_df
  target = "Type",
  coordinate_names = c("x", "y"),
  coords_as_features = FALSE,
  crs = attributes(full_df)$CRS)

# check out available learners
# mlr3extralearners::list_mlr3learners(filter=list(class="classif"))

#=============Basic just fit and predict==================
# This is the simplest approach no hyper parameter tuning or resampling.
# This may be fine but you will get more performance and better accuracy 
# assessment with the steps below this.

# # Define the leaner to be used
# .lrn <- mlr3::lrn("classif.xgboost", predict_type = "response")
# # .lrn <- mlr3::lrn("classif.ranger", predict_type = "response")
# 
# .lrn$help()
# .fullMod <- .lrn$train(task)
# 
# 
# mod.pred <- predict_spatial( .fullMod, full_stack)
# 
# par(mfrow=c(1,2))
# plot(mod.pred, col=rev(brewer.pal(6, name="Dark2")))
# terra::plotRGB(stack_res[[c(3,2,1)]], scale=0.5, smooth=TRUE, axes=TRUE)


#============= Filtering example ==============================

# Not actually filtering as such but generating some scores to filter variables
# with. using importance for xgb and rf but "performance" with svm. Apply
# these as you think best further on.


#' Filter the variables of a mlr3 model by importance
#' 
#' Note it only works for models that suppor "importance"
#'
#' @param .task an mlr3 task object
#' @param .learner an mlr3 leaner object
#'
#' @return A data table showing the column names and the score
#'
importance_filter <- function(.task, .learner) {
  filter = flt("importance", learner = .learner)
  filter$calculate(.task)

  as.data.table(filter) #> 
    # na.omit()
}

xgb.filt.scores <- importance_filter(task, .learner = lrn("classif.xgboost") )
xgb.filt.scores
rf.filt.scores <- importance_filter(task, .learner = lrn("classif.ranger", importance = "impurity") )
rf.filt.scores

#' Filter the variables of a mlr3 model by performance
#'
#' @param .task an mlr3 task object
#' @param .learner an mlr3 leaner object
#'
#' @return A data table showing the column names and the score
#'
perfrmance_filter <- function(.task, .learner){
  flt.perf <- FilterPerformance$new(
    learner = .learner,
    resampling = mlr3::rsmp("cv"),
    measure = msr("classif.acc")
  )
  flt.perf$calculate(.task)
  as.data.table(flt.perf)#|> 
    # na.omit()
}

svm.filt.scores <- perfrmance_filter(flt_task, mlr3::lrn("classif.svm"))
svm.filt.scores

# then filter columns like so (for example):
# task$select(svm.filt.scores$feature[1:5]) # this selects the top 5 vars from the svm performance filter




# ================== xgboost with hypertuning =================================
xgb.lrn.tune <- mlr3::lrn("classif.xgboost", predict_type = "response"
                       , nrounds = to_tune(10, 20)
                       , eta = to_tune(1e-3, 0.3, logscale = TRUE) 
                       , max_depth = to_tune(lower = 3, upper = 20, logscale = TRUE)
                       , min_child_weight= to_tune(lower = 0, upper = 10)
                       , max_delta_step= to_tune(lower = 1, upper = 10)
                       , lambda = to_tune(lower = 1e-3, upper = 10)
                       , gamma = to_tune(lower = 0, upper = 10)
                       , subsample = to_tune(lower = 0.5, upper = 1)
                       , colsample_bytree  = to_tune(lower = 0.6,  upper = 1)
                       , colsample_bylevel = to_tune(lower = 0.6,  upper = 1)
                       , colsample_bynode = to_tune(lower = 0.6,  upper = 1))

# ================== random forest with hypertuning ============================
rf.lrn.tune <- mlr3::lrn("classif.ranger", predict_type = "response"
                         , max.depth=to_tune(5, 30)
                         # , mtry = to_tune(1, 10)
                         # , min.node.size = to_tune(1, 10)
                         , num.trees=to_tune(500, 2000))


# ================== random forest with hypertuning ============================
svm.lrn.tune <- mlr3::lrn("classif.svm", predict_type = "response"
                          , type = "C-classification"
                          , cost= to_tune(1e-2, 1e3)
                          , gamma = to_tune(1e-4, 100, logscale=TRUE)
                          , kernel= to_tune(levels =c("linear", "polynomial", "radial")))


# ========== Run the auto tuning =====================
# set the resampling plan - not yet used properly
spcv_plan <- mlr3::rsmp("repeated_spcv_coords", folds = 10, repeats=5)
autoplot(spcv_plan, task=task, 1:3)


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
tune_model <- function(.task, .learner, .resamp, term_evals=5, .workers= future::availableCores()-2){
  at = auto_tuner(
    method = tnr("random_search"),
    learner = .learner,
    resampling = .resamp,
    measure = msr("classif.acc"),
    term_evals = term_evals) 
  
  future::plan("multisession", workers = .workers)
    progressr::with_progress(expr = {
      at$train(.task)
    })
    future:::ClusterRegistry("stop")
  return(at)
}

# run the tuning for each model - could iterate this but it's only 3 and helps
# with the clarity - if you want to test lots more models, consider using purr::map
xgb.tuned <- tune_model(task, xgb.lrn.tune, spcv_plan)
rf.tuned <- tune_model(task, rf.lrn.tune, spcv_plan)
svm.tuned <- tune_model(task, svm.lrn.tune, spcv_plan)

# ============ Benchmark these models ========================

benchmark_wrap <- function(..., .task, .resamp, .workers= future::availableCores()-2){
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

bench.mark <- benchmark_wrap(xgb.tuned$learner, 
                             rf.tuned$learner, 
                             lrn("classif.ranger", id="ranger.untuned"), #untuned random forest
                             svm.tuned$learner,
                             .task = task, 
                             .resamp=spcv_plan)


bench.mark$aggregate(c(msr("classif.acc"), msr("classif.bacc"))) # svm is the best!
autoplot(bench.mark, measure=msr("classif.acc"))
# apparently svm is he best a pixel level ~86%

#train the best model.
tuned.mod <-  svm.tuned$learner$train(task)

tuned.mod.preds <- tuned.mod$predict(task)
tuned.mod.preds$confusion # confusion matrix for full model (not that useful?)

# stack_res <- ezwarp::ezwarp(full_stack,full_stack, res=0.25, engine="sf") # resampled becuase my laptop sucks

# make predictions across raster with full model returns a SpaRaster
tune.mod.pred <- terra::predict(full_stack, tuned.mod, cores=1, na.rm=TRUE)
plot(tune.mod.pred, col=rev(brewer.pal(6, name="Dark2")))

writeRaster(tune.mod.pred, "data/svm_pixel_level_v01.tif",
            wopt= list(gdal=c("COMPRESS=DEFLATE")),
            overwrite=TRUE)


#========= now let's check the accuracy wih spatial resampling ===================
spcv_plan_res <- mlr3::rsmp("repeated_spcv_coords", folds = 10, repeats=10) 

resample_wrap <- function(.task, .learn, .resamp){
  future::plan("multisession", workers = future::availableCores()-5)
  
  resampled_model <- progressr::with_progress(expr = {
    mlr3::resample(
      task = .task,
      learner = .learn,
      resampling = .resamp, #rsmp("cv", folds=10) try this resampling and you'll see why we need spatial resampling.
      store_models = FALSE,
      encapsulate = "evaluate"
    )
  })
  
  future:::ClusterRegistry("stop")
  return(resampled_model)
}

svm.resamp <- resample_wrap(task, svm.tuned$learner, spcv_plan_res)


svm.resamp$aggregate(msr("classif.acc")) # 84% for the pixel level - not bad!
                                               # 82% for the plot level - not bad!
                                              # 89% with the tuned svm!
# create sum of confusion matrices from the resampling iterations.
res.preds <-
  svm.resamp$predictions() |>
  purrr::map(function(x = .x) x$confusion) |> 
  Reduce("+", x = _)
print(res.preds)

as.data.frame(res.preds)


autoplot(svm.resamp, measure=msr("classif.acc"))

