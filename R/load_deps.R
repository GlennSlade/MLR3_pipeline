# This file will check all files are installed on source and install them if needed
library(rlang)
library(remotes)
library(dplyr)
suppressMessages(rlang::local_use_cli())
package_check <- function(packages){
   
    v <- lapply(packages, function(x) {
      if (
        suppressMessages(
          suppressWarnings(!require(x, character.only = TRUE)))) {
        return(x)
      } 
    }) 
    v <- unlist(v[!sapply(v,is.null)])
    if (length(v) > 0){
      rlang::abort(
        cli::format_error(c(
          "The following packages must be installed to run this workflow: \n",
        "i" = "{v}")))
    }
    message(cli::format_message(c(
      "The following packages have been loaded:",
      "i" = "{packages}")))
} 

pkg_list <- c("remotes", "tictoc", "terra", "sf", "ggplot2", "readxl", "dplyr", "purrr", 
              "exactextractr", "RColorBrewer", "mlr3", "mlr3spatiotempcv",
              "mlr3extralearners", "mlr3tuning", "mlr3viz", "mlr3filters",
              "mlr3learners", "mlr3pipelines", "mlr3hyperband", "mlr3mbo", "mlr3tuningspaces",
              "progressr", "igraph", "patchwork")

package_check(pkg_list)


# if (!dir.exists("data")) dir.create("data")

