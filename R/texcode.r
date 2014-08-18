# Goal: to put matrices into LaTex via Sweave.
# The matrix options for LaTex.
#
# \begin{matrix} x & y \\ z & v \end{matrix}  
# \bigl( \begin{smallmatrix} a&b \\ c&d \end{smallmatrix} \bigr)  
# \begin{pmatrix} x & y \\ z & v \end{pmatrix}  
# \begin{bmatrix} 0 & 1\\ 2 & 3 \end{bmatrix} 
# \begin{Bmatrix} x & y \\ z & v \end{Bmatrix}  
# \begin{vmatrix} x & y \\ z & v \end{vmatrix}  
# \begin{Vmatrix} x & y \\ z & v \end{Vmatrix}
#
# the idea is to output latex matrix code
# and allow to inject LaTeX code fragments.
# So, LaTeX code fragments and matrix code can be easily
# combined.


#' Inject LaTeX code.
#' Converts LaTeX code into texcode object.
#' @param   x     Some arbitrary LaTeX code.
#' @return  Object of class texcode.
#' @author  Mark Heckmann
#' @export
#'
xc <- function(x) {
  x <- as.texcode(x)
  #res <- list(x)
  #class(res) <- "texcode"
  x
}


#' Inject LaTeX code.
#' Converts LaTeX code into texcode object.
#' @param   x     Some arbitrary LaTeX code.
#' @return  Object of class texcode.
#' @author  Mark Heckmann
#' @keywords internal
#' @export
#'
lex <- function(x) {
  res <- list(x)
  class(res) <- "texcode"
  res
}


#' Convert a matrix to LaTeX code.
#'
#' @param   x       A matrix object.
#' @param   mtype   LaTeX matrix type, i.e. round braces, curly braces etc.
#'                  Available types are \code{"matrix", "pmatrix", "bmatrix", 
#'                  "Bmatrix", "vmatrix", "Vmatrix"}.
#' @param   digits  Number of digits to display. Only trimmin, no rounding.
#' @return  Object of class texcode.
#' @rdname  xm
#' @author  Mark Heckmann
#' @export
#' @examples 
#' 
#' m <- matrix(1:9, ncol=3)
#' xm(m)
#'
#' # change digits
#' 
#' "$" %% xm(m) %% "$" 
#' 
#' "$" + xm(m) + "$" 
#'
xm <- function(x, digits=NA, mtype=NA, round=NA)
{
  opts <- mat2tex_options()
  if (is.na(digits))
    digits <- opts$digits
  if (is.na(mtype))
    mtype <- opts$mtype
  if (is.na(round))
    round <- opts$round

  mtypes <- c("matrix", "pmatrix", "bmatrix", 
              "Bmatrix", "vmatrix", "Vmatrix")
  mtype <- match.arg(mtype, mtypes)
  
  # formatting if no rounding occurs, the values are simply trimmed
  if (round)
    x <- round(x, digits=digits)
  x <- as.matrix(x)
  xc <- formatC(x, digits=digits, format="f")

  # LaTex output
  l.env.begin <- paste0("\\begin{", mtype, "}\n")
  l.env.end <- paste0("\\end{", mtype, "}\n")
  l.matrix <- apply(xc, 1, function(x) 
                    paste(paste(x, collapse=" & "), "\\\\ \n"))
  res <- list(l.env.begin, l.matrix, l.env.end)
  class(res) <- "texcode"
  res
}

#' @rdname  xm
#' @export
#' 
xmt <- function(x, digits=2, mtype="pmatrix", round=TRUE) {
  tc <- xm(x=x, digits=digits, mtype=mtype, round=round)
  tc %_% "^T"
}

#' Print method for objects of class \code{texcode}.
#' 
#' @param x    texcode object.
#' @export 
#' @keywords internal
#' @method print texcode
#'
print.texcode <- function(x, autoenv=TRUE, ...) {
  
  tex.string <- do.call(c, x)
  cat(tex.string)
}
# TODO: auto-add standard envir if 

math_env_code <- function(e=1, begin=TRUE, label=NULL) 
{
  es <- c("$$", "$")
  es.be <- c("equation", "equation*", "align", "align*")
  if (!is.null(label) & begin)
    label <- paste0("\\label{", label, "}")
  es.b <- c(paste0(es, "\n"), 
            paste0("\\begin{", es.be, "} ", label, "\n"))
  es.e <- c(paste0(es, "\n"), 
            paste0("\\end{", es.be, "}\n"))
  
  math <- ifelse (begin, es.b[e], es.e[e])
  as.texcode(math)
}
# math_env_code(1)
# math_env_code(3, F)

wrap_in_math_envir <- function(x, e=2, label=NULL) 
{
  math_env_code(e=e, begin=TRUE, label=label) %_% 
    x %_% math_env_code(e=e, begin=FALSE)
}

check_for_math_envir <- function() {
  
}


#' Concatenate \code{texcode} chunks.
#' @param   ... \code{texcode} objects or objects that can ce converted
#'              into texcode objects.
#' @return  Object of class \code{texcode}.
#' @author  Mark Heckmann
#' @export
#'
xx <- function(..., e=1, label=NULL) {
  dots <- list(...)
  texcodes <- Reduce("%_%", dots) 
  if (!is.na(e)) 
    texcodes <- wrap_in_math_envir(texcodes, e = e, label=label) 
  texcodes
}


#' Overloading "+" and "\%\%" operator to allow for easy combination of texcode objects.
#' 
#' @param e1    texcode object
#' @param e2    texcode object
#' @export
#' @keywords internal
#' @method Ops texcode
#'
Ops.texcode <- function(e1, e2) {
  e1 <- as.texcode(e1)
  e2 <- as.texcode(e2)
  if (.Generic %in% c("+", "%%") 
	    & class(e1) == "texcode" 
	    & class(e2) == "texcode") {
	} else {
    NextMethod(.Generic)		
	} 
	res <- c(e1,e2)
	class(res) <- class(e1)
	res
}


#' Convert to \code{texcode} object.
#' 
#' @param x    Some R object.
#' @export
#' @keywords   internal
#' @details    Currently only character vectors can be converted.
#'
as.texcode <- function(x) 
{
  if (class(x) == "matrix")
    x <- xm(x)
  if (class(x) == "character")
    x <- lex(x)
  if (is.vector(x) & length(x) == 1)
    x <- lex(paste(x, collapse=" "))
  x
}
#ax <- as.texcode

#' Check if object is a \code{texcode} object.
#' 
#' @param x    R object.
#' @export
#' @keywords   internal
#'
is.texcode <- function(x) {
  class(x) == "texcode"
}


#' Begin or end LaTeX math environment.
#' 
#' \code{xb} will begin, \code{xb} will end a math environment.
#' 
#' @param   e     Numeric indicating the type of math environment to begin or end
#'                (\code{1 = equation, 2= equation*, 3=$, 4=align, 5=align*}).
#'                Default is \code{1 = 'equation'}. 
#' @param label   Optional LaTeX equation label               
#' @return  Object of class texcode.
#' @author  Mark Heckmann
#' @rdname xbe
#' @export
#' 
xb <- function(e=NA, label=NULL) {
  if (is.na(e))
    e <- mat2tex_options()$mathenv
  math_env_code(e=e, begin=TRUE, label=label)
}


##'  @rdname xbe
##'  @export
xe <- function(e=1) {
  if (is.na(e))
    e <- mat2tex_options()$mathenv
  math_env_code(e=e, begin=FALSE)
}


#' "\%\_\%" operator to allow for easy combination of texcode objects 
#' 
#' @param x     \code{texcode} object, string or matrix.
#' @param y    \code{texcode} object, string or matrix.
#' @rdname grapes-_-grapes.Rd
#' @export
#' @keywords internal
#'
`%_%` <-  function(x,y) {
  # make sure function does not overwrite matrix %% matrix case
  as.texcode(x) + as.texcode(y)
}

#' @rdname grapes-_-grapes.Rd
#' @export
#' 
`%__%` <-  function(x,y) {
  # make sure function does not overwrite matrix %% matrix case
  as.texcode(x) %_% as.texcode("\\;") %_% as.texcode(y)
}


#' Left and right parenthesis
#' 
#' @rdname parenthesis.Rd
#' @export
#'
pl <- function() {
  as.texcode("\\left(")
}

#' @rdname parenthesis.Rd
#' @export
#' 
pr <- function() {
  as.texcode("\\right)")
}

# Hack to allow the case where no texcode object is used at all.
# This may be the case if the user does "$" + "$".
# "+" is not redefined as it appears too error prone to me.
#
`+` <- function(x,y) {
  if ( ( (is.character(x) | is.matrix(x)) & (is.texcode(y) | is.character(y)) ) |
         ( (is.texcode(x) | is.character(x)) & ( is.character(y) | is.matrix(y) ) ) ) 
  {
    return( x %_% y )  
  } else {
    .Primitive("+")(x,y)
  }
}

#' Convenience function for \code{\\mathbf\{\}}
#' 
#' @param x     \code{texcode} object or string.
#' @export
#'
mb <- function(x) {
  x <- paste("\\mathbf{", x, "}")
  as.texcode(x)
}


#' Convenience function for linebreak, i.e. \code{\\\\}.
#' 
#' @export
#' 
lb <- function() {
  as.texcode("\\\\")
}


#### EXAMPLES ###

# A <- matrix(runif(4), 2)
# "\\mathbf{A} = " + xm(2 * A) + "=" + xm(A) + "\\mathbf{A} = "
# "\\mathbf{A} = " %_% (2 * A) %_% "=" %_% A %_% "\\mathbf{A} = "
# "$$" + A + 2*A + A  +"$$"

# xb(2) %% A %% "=" %% A %% xe(2)
# class("aas" %% A %% "aasss")

# d <- 3
# "$$" %_% d %__% "=" %__% A %_% "$$"
#  
# xb(2, l="eq:123") %% A %% xe(2)
# 
# xb(label="eq:123") %% 
#   xm(anscombe, 2,"m") %% "=" %% xm(A) %% "+" %% xm(A) %%
# xe()
# 
# xb(label="eq:123") + 
#   xm(anscombe) + "=" + xm(A) + "+" + xm(A) +
# xe()
# 
# xc("$$") + xc(A) + xc("$$")
# "$$" + A + "$$"
# "$$" %_% A %_% "$$"



