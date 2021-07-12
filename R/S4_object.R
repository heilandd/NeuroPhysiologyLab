
# S4 Object of name NeuoPhyhysiology ----------------------------------------------------

#' data_counts object
#'
#'
#' @return S4 object
#' @export
#'

NeuoPhyhysiology <- setClass("NeuoPhyhysiology", slots = 
                               c(Data.character="list",
                                 sample_desc="data.frame",
                                 Image = "matrix",
                                 Image_real = "matrix", 
                                 Spikes="list",
                                 Traces="matrix",
                                 Connections="list",
                                 Rasterplotfile="matrix",
                                 Framerate="numeric",
                                 Frames="numeric",
                                 modules="matrix",
                                 scDisctiption="data.frame"
                               ))


