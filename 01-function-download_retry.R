# téléchargement avec pause + retry si warning et arrêt complet au bout de 10 retry successifs
# d'parès https://rdrr.io/bioc/recount/src/R/download_retry.R
download_retry <- function(url, destfile = basename(url), mode = "wb",
                           N.TRIES = 10L, ...) {
  ## Based on http://bioconductor.org/developers/how-to/web-query/
  
  N.TRIES <- as.integer(N.TRIES)
  stopifnot(length(N.TRIES) == 1L, !is.na(N.TRIES))
  stopifnot(N.TRIES > 0L)
  
  while (N.TRIES > 0L) {
    result <- tryCatch(download.file(
      url = url, destfile = destfile, mode = mode, ...
    ), error = identity)
    if (!inherits(result, "error")) {
      break
    }
    ## Wait between 0 and 2 seconds between retries
    Sys.sleep(runif(n = 1, min = 2, max = 5))
    N.TRIES <- N.TRIES - 1L
  }
  
  if (N.TRIES == 0L) {
    stop(
      "'download_retry()' failed:",
      "\n  URL: ", url,
      "\n  error: ", conditionMessage(result)
    )
  }
  
  invisible(result)
}