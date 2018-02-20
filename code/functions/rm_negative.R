#' Remove negative symbols from a column
#' 
#' @param var1 vector or column containing negative symbols to remove ("-")
#'   
#' @return The original vector without negative symbols
#' @export
#' 
#' @examples
#' a <- -20:20
#' b <- rm_negative(a)
rm_negative <- function(var1){
  var1 <- as.character(var1)
  var1 <- gsub("-", "", var1)
  var1 <- as.numeric( var1)
  return(var1)
}