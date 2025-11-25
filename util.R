#' This might be bad code, but it is fun code.
cache <- function(expression, cache_file, use_cache = TRUE) {
  if (use_cache && file.exists(cache_file)) {
    print("Loading from cache...")
    readRDS(file = cache_file)
  } else {
    print("Recomputing")
    return_value <- expression
    saveRDS(return_value, file = cache_file)
    return_value
  }
}

#' Pull the variance out of a 
get_variance <- function(model) {
  (model %>% VarCorr %>% as_tibble %>% select(grp, sdcor) %>% deframe()) ** 2
}