#' SS_diags
#'
#' \code{SS_diags} wrapper function for running all or a subset of diagnostics
#' @param summaryoutput an object generated via SS_output
#' @param tests which numbered tests should be run
#' @param plot logical, should plots be generated?
#' @seealso \code{\link[r4ss]}


SS_diags <- function(summaryoutput,
         tests = 1:4,
         plot = T){
  
  if(1 %in% tests){
    produceRuns(smabase)
  }
  
}