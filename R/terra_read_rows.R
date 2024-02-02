
#' Read SpatRaster by row chunks
#' 
#' required when reading a massive raster as a dataframe where there are many 
#' NA values. without NA values it fits in memory, with it blos things up. 
#' So read it in by chunks and ge read of the NA values on the fly.
#'
#' @param stack a SpatRaser
#' @param .prop default 0.2. The proportion of rows to read in each chunk.
terra_read_rows <- function(stack, .prop=0.2){
  
  terra::readStart(stack)
  on.exit({
    terra::readStop(stack)
    gc()
  })
  
  # determine rows to read
  .nrow <- round(nrow(stack)*.prop)
  
  v <- seq.int(1, nrow(stack), by=.nrow)
  v <- v[-length(v)]
  v2 <- c(v[-1], nrow(stack)) - v
  
  read_chunks <- function(.row, .nrows) {
    
    .x <- xFromCol(stack, 1:ncol(stack))
    .y <- yFromRow(stack, .row:(.row + (.nrows-1)))
    # browser()
    
    .coords <- expand.grid(x = .x,
                           y = .y)

    # browser()
    terra::readValues(stack,
                      row = .row,
                      nrows = .nrows,
                      dataframe = TRUE) |>
      cbind(.coords) |> 
      na.omit()
  }
  
  # purrr is a fair bit faster here
  purrr::map2(v, v2, ~read_chunks(.x, .y),
              .progress = TRUE) |>
    purrr::list_rbind()
  
  # base R alternative if needed.
  # df <- mapply(read_chunks, .row=v, .nrows=v2,SIMPLIFY = FALSE)
  # do.call("rbind", df)
}
