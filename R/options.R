# function flatten.list from pkgutils
#
flatten <- function (object, use.names = TRUE, ...) 
{
  while (any(is.a.list <- vapply(object, is.list, NA))) {
    object[!is.a.list] <- lapply(object[!is.a.list], list)
    object <- unlist(object, FALSE, use.names)
  }
  object
}
# l <- list(a=12, b=13)
# flatten(l)
# flatten(list(l))


# init mat2tex default settings when package is loaded
#
mat2tex_options_init <- function() 
{
  l <- list(digits = 2,
            round=TRUE,
            mtype="pmatrix",
            mathenvir = 1,
            transpose.sign = "^{T}",
            na = "")  
  options(mat2tex=l)   
}
#mat2tex_options_init()


#' get and set mat2tex default options
#' @param ... Enter the options you want to change as \code{name = value} 
#'            pairs separated by commas.
#' @export
#' @examples \dontrun{
#'  
#'  # show all mat2tex options
#'  mat2tex_options()
#'  
#'  # change default for number of digits to round to
#'  mat2tex_options(digits=3)
#'  
#'  # change default for matrix type and digits
#'  mat2tex_options(digits=3, mtype="bmatrix")
#'  
#'  # change default tranpose sign (e.g. used in xmt)
#'  mat2tex_options(transpose.sign="^\\intercal")
#'  
#' }
#' 
mat2tex_options <- function(...) 
{
  dots <- flatten(list(...))
  if ( length(dots)  == 0)
    options()$mat2tex
  else {
    old.opt <- mat2tex_options()   # get current options
    l <- modifyList(old.opt, dots) # modify
    options(mat2tex=l)             # overwrite options  
    invisible(old.opt)             # return old opts
  }  
}
#mat2tex_options()
#mat2tex_options(digits=2, round=T)


# remove options on detaching package
mat2tex_remove_options <- function() {
  options(mat2tex=NULL)  
}
#mat2tex_remove_options()