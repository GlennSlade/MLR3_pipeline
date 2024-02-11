
# run script

invisible(lapply(list.files("R", full.names = TRUE, recursive = TRUE), source))

library(stringr)

library(openxlsx)

site_name = "Dinaka"
df_type = "point" # either "point" or "grid"
# point takes the value from the centre point of the polygon - use this if the training polygon size
#is smaller or similar to the resolution of your image data - uses exact_extract for polygons
# grid takes all the raster cells within the polygon and treats each grid cell as a training cell
#use this if your polygons are larger than you raster cell size
folds = 5 
# if you have a small number of training points (ie running the points df_type)
#care needs to be taken to ensure that the number of folds and n_evals doesnt exceed the number of 
# training points to iterate over
n_evals = 10 # number of evaluaations (iterations)
tune_method = "random_search" # or mbo 
tile_split =  "yes" # yes or no. Use tile if you have large raster images and 


#improvements pending -script to check folds and n_evals against size of training classes


#============ Data Prep =====================


x <- rast(paste0("data_in/",site_name,"/",site_name,"_stack.tif")) #import stacked image

v <- build_ml_df(cube = x, site_name = "Dinaka", df_type = "point")

#v <- paste0("data_out/",site_name,"/",site_name,"_ML_in_grid_level.rds")
v <- paste0("data_out/",site_name,"/",site_name,"_ML_in_point_level.rds")

#================ ML pipeline ================
# Auto ML ------------------
task <- build_task(v, "Dinaka")

tune_spcv <- mlr3::rsmp("spcv_coords", folds = folds)

# #inspect the spatial cv
autoplot(tune_spcv, task=task, 1:3)

# ================ model tuning. =================
# xgboost first.
tic()
xgb.lrn <- lts('classif.xgboost.rbv2')$get_learner()

# here we can add the option for filtering based on importance as a hyper parameter.
xgb.lrn.filt <- po("filter", filter=flt("importance"), filter.frac=to_tune(0.1, 1))%>>%
  xgb.lrn 

xgb_tune <- tune_lrnr(
  .task = task,
  .lrnr = xgb.lrn.filt,
  .resamp = tune_spcv,
  .measure = msr("classif.acc"),
  .n_evals = n_evals,
#  sub.sample = 0.1,
#  .tune_method = tune_method,
.tune_method = "random_search",
  .test.scale = TRUE,
  .test.pca = TRUE,
#.test.pca = TRUE,
  .workers = future::availableCores() - 2,
  .verbose = FALSE,
  .seed = 5446
)

plot(xgb_tune$graph)
xgb_tune$tun_inst$tuning_result
xgb_tune$tun_inst$learner$model$importance$features # view selected features if filter is used.
xgb_tune$tun_inst$learner$model$importance$scores

toc()
# now for svm

tic()
svm.lrn <- lts('classif.svm.rbv2', type=to_tune("C-classification"))$get_learner()

svm_tune <- tune_lrnr(
  .task = task,
  .lrnr = svm.lrn,
  .resamp = tune_spcv,
  .measure = msr("classif.acc"),
#  sub.sample = 0.1,
  .n_evals = n_evals,
#  .tune_method = tune_method,
.tune_method = "random_search",
  .test.scale = TRUE,
#.test.pca = TRUE
  .test.pca = TRUE
)
plot(svm_tune$graph)
svm_tune$tun_inst$tuning_result

toc()

# ranger random forest.
tic()
rf.lrn <- lts('classif.ranger.rbv2')$get_learner()

rf.lrn.filt <- po("filter", filter=flt("importance"), 
                  filter.frac=to_tune(0.1, 1)) %>>%
  rf.lrn 

rf_tune <- tune_lrnr(
  .task = task,
  .lrnr = rf.lrn.filt,
  .resamp = tune_spcv,
  .measure = msr("classif.acc"),
#  sub.sample = 0.1,
  .n_evals = n_evals,
.tune_method = "random_search",
#  .tune_method = tune_method,
  .test.scale = TRUE,
  .test.pca = TRUE
#.test.pca = TRUE

)

toc()

plot(rf_tune$graph)
rf_tune$tun_inst$tuning_result
rf_tune$tun_inst$learner$model$importance$scores
# ensemble example
tic()

ens.lrn.tune <- gunion(list(
  po("learner_cv", xgb.lrn, id = "ens_xgb"),
  po("learner_cv", svm.lrn, id = "ens_rf"),
  po("learner_cv", rf.lrn, id = "ens_svm"),
  po("nop", "ens_nop")
)) %>>%
  po("featureunion", id = "ens_union") %>>%
  lrn("classif.ranger", id = "master_rf")


ens_tune <- tune_lrnr(
  .task = task,
  .lrnr = ens.lrn.tune,
  .resamp = tune_spcv,
  .measure = msr("classif.acc"),
#  sub.sample = 0.1,
  .n_evals = n_evals,
 # .tune_method = "random_search", 
.tune_method = tune_method, # definitely use "mbo" for this!
  .test.scale = TRUE,
  .test.pca = TRUE
)

# You can just remove the PO("filter"..) or copy it from the above xfboost filter bit 

plot(ens_tune$graph)
ens_tune$tun_inst$tuning_result

toc()


# =================== benchmarking =================================
tic()
bench.mark <- benchmark_lrnrs(xgb_tune$tun_inst$learner, 
                              svm_tune$tun_inst$learner, 
                              rf_tune$tun_inst$learner, 
                              ens_tune$tun_inst$learner, 
                             lrn("classif.ranger", id="ranger.untuned"), #untuned random forest
                             .task = task, 
                             .resamp=tune_spcv)

bench.mark$aggregate(c(msr("classif.ce"),msr("classif.acc")))
bench.mark$aggregate(msr("classif.acc"))

toc()

p2 <- autoplot(bench.mark, measure=msr("classif.acc"))+
  scale_x_discrete(label = function(x) str_wrap(str_trunc(x, 60), width = 30)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p2



# decide which is the best...

# Resampling ------------------------------
# now proper resampling...here I'm using svm as it looks the best just switch the learner as needed.
#resamps <- ml_resample(.task = task,
 #           .learner = svm_tune$tun_inst$learner)

resamps <- ml_resample(.task = task,
                       .learner =  ens_tune$tun_inst$learner)

resamps$aggregate(c(msr("classif.acc"),
                    msr("classif.ce"))) # get aggregate scores
res.preds <-
  resamps$predictions() |>
  purrr::map(function(x = .x) x$confusion) |> 
  Reduce("+", x = _)
print(res.preds)


p3 <- autoplot(resamps, measure=msr("classif.acc"))
p3
 
# Prediction ----------------------------------

# tic()
# mod.pred <- predict_terra_tile(x,
#                                mod= rf_tune$tun_inst$learner,
#                                site_name= site_name,
#                                .workers=1, tile=TRUE, tile_dims = 5)
# toc()


tic()
mod.pred <- predict_terra(x, mod= rf_tune$tun_inst$learner,
                               site_name= site_name,
                               .workers=1)
toc()




# quick n dirt map.
# par(mfrow=c(1,2))
# plot(mod.pred, col=rev(brewer.pal(6, name="Dark2")), axes=FALSE)
# terra::plotRGB(x[[c(3,2,1)]], scale=0.6, smooth=TRUE, axes=FALSE)
# par(mfrow=c(1,1))

v.vals = unique(readRDS(v)$Type)
.m <- matrix(c(v.vals, as.numeric(levels(v.vals))), ncol=2)
mod.pred.fac <- terra::classify(mod.pred, .m)

# par(mfrow=c(1,2))
# plot(mod.pred.fac, col=rev(brewer.pal(6, name="Dark2")), axes=FALSE)
# terra::plotRGB(x[[c(3,2,1)]], scale=0.6, smooth=TRUE, axes=FALSE)
# par(mfrow=c(1,1))

writeRaster(mod.pred.fac, filename = paste0("data_out/",site_name,"/",site_name,"_Predict_Map.tif"), overwrite=TRUE)


### Exporting Data ------
# Exporting confusion matrix
as.data.frame(res.preds)
write.xlsx(res.preds, paste0("data_out/",site_name,"/Confusion/res.preds_svm_",site_name,".xlsx"), rowNames=FALSE)

#Exporting benchmark results
bench <-bench.mark$aggregate(msr("classif.acc"))
r <- bench$classif.acc
s <- bench$learner_id
b<- dplyr::bind_cols(r,s)
names(b) <- c('classif.acc','learner')

write.xlsx(b, paste0("data_out/",site_name,"/Bench/MLR_bench_results_",site_name,".xlsx"), rowNames=FALSE)


#Exporting benchmark plot
ggsave(
  p2,
  filename = paste0("data_out/",site_name,"/Figures/benchmark_",site_name,"_model_classif_ac.png"),
  width =16,
  height = 8,
  units = "cm"
)
#Exporting final classification accuracy
ggsave(
  p3,
  filename = paste0("data_out/",site_name,"/Figures/Final_",site_name,"_model_classif_ac.png"),
  width =16,
  height = 8,
  units = "cm"
)


