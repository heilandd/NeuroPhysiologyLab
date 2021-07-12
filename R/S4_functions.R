#' @include S4_object.R
NULL


# Generics ----------------------------------------------------------------

#' @title Initicate NeuoPhyhysiology object
#'
#' @param Data.character A valid Data.character list
#'
#' @return NeuoPhyhysiology object
#' @export

setMethod("initialize", signature = "NeuoPhyhysiology", definition = function(.Object){
  return(.Object)
})


#' @export
setMethod(f = "show", signature = "NeuoPhyhysiology", definition = function(object){
  
  num_samples <- object@Traces %>% base::nrow()
  base::print(glue::glue("An object of class 'NeuoPhyhysiology' that contains {num_samples} cells."))
  
  #base::print(glue::glue("An object of class 'NeuoPhyhysiology'"))
  
})


