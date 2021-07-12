#' @include S4_object.R
NULL


# Generics ----------------------------------------------------------------

#' @title Initicate NeuoPhyhysiology object
#'
#' @param Data.character A valid Data.character list
#'
#' @return NeuoPhyhysiology object
#' @export

setMethod("initialize",signature = "NeuoPhyhysiology", definition = function(.Object,Data.character ){
  .Object@Data.character=Data.character
  return(.Object)
})


#' @export
setMethod(f = "show", signature = "NeuoPhyhysiology", definition = function(object){
  
  #num_samples <- base::length(samples(object))
  #samples <- stringr::str_c( samples(object), collapse = "', '")
  #sample_ref <- base::ifelse(num_samples > 1, "samples", "sample")
  #base::print(glue::glue("An object of class 'NeuoPhyhysiology' that contains {num_samples} {sample_ref} named '{samples}'."))
  
  base::print(glue::glue("An object of class 'NeuoPhyhysiology'"))
  
})


