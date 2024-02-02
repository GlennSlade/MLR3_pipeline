
df <- "data_out/Bokspits_1/Bokspits_1ML_in_grid_level.rds"
full_df <- readRDS(df) |> 
  select(!all_of("Class")) 

# if (!is.null(sub.sample)){
#   full_df <- dplyr::slice_sample(full_df, n=sub.sample) # testing only!!!
# }


# define the mlr3 task
task = mlr3spatiotempcv::TaskClassifST$new(
  id = "TEST1",
  backend = full_df, 
  target = "Type",
  coordinate_names = c("x", "y"),
  coords_as_features = FALSE,
  crs = attributes(full_df)$CRS)


#=============Define the spatial resampling for the auto-ml step ===============

spcv_plan <- mlr3::rsmp("repeated_spcv_coords", 
                        folds = 5, 
                        repeats=2)

# get default tuning space of rpart learner

# leaners to test
# ================== xgboost with hypertuning 
xgb.lrn.tune <- mlr3::lrn("classif.xgboost", id="xgb")
xgb.lrn.tune$param_set$values = lts("classif.xgboost.default")$values
# ================== random forest with hypertuning 
rf.lrn.tune <- mlr3::lrn("classif.ranger", id="rf", importance = "impurity")
rf.lrn.tune$param_set$values = lts("classif.ranger.default")$values
# ================== random forest with hypertuning 
svm.lrn.tune <- mlr3::lrn("classif.svm", id="svm")
svm.lrn.tune$param_set$values = lts("classif.svm.default")$values
# =================== Define the learner pipeline 

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


gr.base <- p0.scale %>>%
  p1.prepro


gr.xgb <- gr.base %>>%
  po("filter", filter = flt("importance", learner = lrn("classif.xgboost")),
     filter.frac=to_tune(0, 0.5))%>>%
  xgb.lrn.tune

glrn <- GraphLearner$new(gr.xgb) 

# param_set <- paradox::ps(
# # scale_branch.selection = p_fct(levels=c("scale", "nothing"))
# # , pre_branch.selection = p_fct(levels=c("pca", "nothing"))
#  importance.filter.frac = p_dbl(lower=0.1, upper = 1))

# glrn$param_set$params$scale_branch.selection <- paradox::ps(
#   scale_branch.selection = p_fct(levels=c("scale", "nothing")))

instance = tune(
  method = "random_search",
  task = task,
  learner = glrn,
  resampling = spcv_plan,
  measure = msr("classif.acc"),
  term_evals = 10)




