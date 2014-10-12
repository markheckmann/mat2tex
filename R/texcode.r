# REMINDER: The gap question is addressed in print.texcode !!


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


#' Create LaTeX code for several matrix types defined in the amsmath package.
#' 
#' @param   x       A matrix object.
#' @param   mtype   LaTeX matrix type.
#' @return  List with LaTeX code fragments.
#' @keywords internal
#' 
latexcode_matrix <- function(x, mtype) 
{
  l.env.begin <- paste0("\\begin{", mtype, "}\n")
  l.env.end <- paste0("\\end{", mtype, "}")
  l.matrix <- apply(x, 1, function(x) 
    paste(paste(x, collapse=" & "), "\\\\ \n"))
  list(l.env.begin, l.matrix, l.env.end)
}


#' Create LaTeX code for bordermatrix.
#' 
#' @param   x       A matrix object with column and/or rownames.
#' @param   corner  Entry for upper left corner, usually empty.
#' @return  List with LaTeX code fragments.
#' @keywords internal
#' 
latexcode_bordermatrix <- function(x, corner="") 
{
  nr <- nrow(x)
  nc <- ncol(x)
  rnames <- rownames(x)
  cnames <- colnames(x)
  if (is.null(rnames))
    rnames <- rep("", nr)
  if (is.null(cnames))
    cnames <- rep("", nc) 
  x.ext <- cbind(c(corner, rnames), rbind(cnames, x))  
  l.matrix <- apply(x.ext, 1, function(x) 
    paste(paste(x, collapse=" & "), "\\cr \n"))
  list("\\bordermatrix{\n", l.matrix, "}\n")
}


#' Convert a matrix to LaTeX code.
#'
#' @param   x       A matrix object.
#' @param   mtype   LaTeX matrix type, i.e. round braces, curly braces etc.
#'                  Available types are \code{"matrix", "pmatrix", "bmatrix", 
#'                  "Bmatrix", "vmatrix", "Vmatrix", "bordermatrix"}.
#' @param   digits  Number of digits to display (if matrix is numeric). 
#' @param   round   Logical. Round numbers? If not numbers are trimmed only.
#' @param   na      How to replace NA values? (default is \code{""}).
#' @return  Object of class \code{texcode}.
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
#' # matrix types
#' x <- matrix(1:4, 2)
#' xm(x, mtype="matrix")
#' xm(x, mtype="bmatrix")
#' 
#' rownames(x) <- letters[1:2]
#' colnames(x) <- LETTERS[1:2]
#' xm(x, mtype="bordermatrix")
#' 
xm <- function(x, digits=NA, mtype=NA, round=NA, na=NA)
{
  opts <- mat2tex_options()
  if (is.na(digits))
    digits <- opts$digits
  if (is.na(mtype))
    mtype <- opts$mtype
  if (is.na(round))
    round <- opts$round
  if (is.na(na))                         
    na <- mat2tex_options()$na       # get string to replace NAs with

  mtypes <- c("matrix", "pmatrix", "bmatrix", 
              "Bmatrix", "vmatrix", "Vmatrix")
  more.types <- "bordermatrix"
  mtype <- match.arg(mtype, c(mtypes, more.types))
  
  # trim numeric values (after rounding if requested)
  x <- as.matrix(x)
  nas <- is.na(x)                             # save NA positions
  if (is.numeric(x)) {
    if (round)
      x <- round(x, digits=digits) 
    x <- formatC(x, digits=digits, format="f")  # format first
  }
  x[nas] <- na                                # replace NAs
  
  # LaTeX output
  if (mtype %in% mtypes)
    res <- latexcode_matrix(x, mtype)
  else if (mtype %in% more.types)
    res <- latexcode_bordermatrix(x)
  class(res) <- "texcode"
  res
}


#' @rdname  xm
#' @export
#' 
xmt <- function(x, digits=NA, mtype=NA, round=NA, na=NA) {
  # use defaults
  opts <- mat2tex_options()
  if (is.na(digits))
    digits <- opts$digits
  if (is.na(mtype))
    mtype <- opts$mtype
  if (is.na(round))
    round <- opts$round  
  tc <- xm(x=x, digits=digits, mtype=mtype, round=round)
  tc %_% opts$transpose.sign
}



# print.texcode <- function(x, autoenv=TRUE, ...) 
# {
#   tex.string <- do.call(c, x)
#   cat(tex.string)
# }
# TODO: auto-add standard envir if 


# get list element by index
# if index does not exist return empty character
#
get_list_element <- function(l, i) {
  tryCatch( l[[i]], error = function(e) "")
}

# TODO :
# See issues on github


#' Add whitespace after single tex chunk.
#' 
#' The LaTeX commands need to be seperated by whitespaces or linebreaks.
#' But after the $ evironment delimiter they are not allowed.
#' The function:
#' - adds a whitespace at the end of every chunk except
#'  after chunks beginning or ending with $ 
#' - adds no whitespace at the end of chunks followed by a chunk onyl 
#' containing $
#' 
#' A cleaner approach would use objects that come along with the
#' space to insert but that would mean reprogramming everything.
#' 
#' @param x texcode object.
#' @export 
#' @keywords internal
#' @return A textcode object with whitespace added where needed.
#' @examples 
#' 
#'  x <- "$" + lex(10) + lex("a") + "$\\n"
#' 
add_whitespaces_to_seperate_chunks <- function(x) {
  # add whitespace at end of all non $ chunks
  # but not to a chunk followed by $ chunk 
  for (i in seq_along(x) ) {
    l0 <- get_list_element(x, i)
    l1 <- get_list_element(x, i + 1)   
    l0 <- paste(l0, collapse=" ")                 # collapse if a list elements holds a vector with more than one element
    l1 <- paste(l1, collapse=" ")
    l0.crit <- !grepl("^[ ]*[$][ ]*$", l0)        # no $ at beginning of string (can be preceeded by whitespaces) 
                                                  # and not followed by non-whitespaces
    l1.crit <- !grepl("^\\s*[$][ ]*[\n]*$", l1)  # no $ or $\n at end of string and no non-whitespaces before
    if (l0.crit & l1.crit ) {     
      x[[i]] <- paste0(x[[i]], " ")
    }   
  }
  x
}

## regex tests

# regex0 <- "^[ ]*[$][ ]*$"
# grepl(regex0, "$")      # be TRUE
# grepl(regex0, " $")     # be TRUE
# grepl(regex0, "$as")    # be FALSE

# regex1 <- "^\\s*[$][ ]*[\n]*$"
# grepl(regex1, "$")        # be TRUE
# grepl(regex1, "$\n")     # be TRUE
# grepl(regex1, "} $\n")   # be FALSE
# grepl(regex1, "$  \n")   # be TRUE
# grepl(regex1, "} $  \n") # be FALSE

## add white space tests

# opt <- mat2tex_options(digits=0)  
# A <- matrix(c(0,1,-1,-1,1,-0), by=TRUE, 3)
# x <- xx("\\B{A} = ", A, e=2)
# str(x)
# x <- "$" + xm(A) + "$"
# add_whitespaces_to_seperate_chunks(x)

# l <- math_env_code(e=2, begin=TRUE) %_% 
#   A %_% math_env_code(e=2, begin=FALSE)
# l <- add_whitespaces_to_seperate_chunks(l)
# str(l)
# l <- list("$", "1 + 1= 2", "$")
# l <- list("$", c("4+6", "2+2"), "1 + 1= 2", "$")
# add_whitespaces_to_seperate_chunks(l)
# xx(M, e=2)



#' Print method for objects of class \code{texcode}.
#' 
#' @param x    texcode object.
#' @export 
#' @keywords internal
#' @method print texcode
#' @details
#' Function has been rewritten so no space is introduced by default
#' between the single texcode chunks. This is necessary for 
#' $ environment, which allows no whitespaces after and before 
#' the environment delimiter $ in markdown.
#' 
print.texcode <- function(x, autoenv=TRUE, ...) 
{
  x <- add_whitespaces_to_seperate_chunks(x)  # add whitespace to seperate texcode chunk lines
  tex.string <- do.call(c, x)   # convert list to string
  cat(tex.string, sep="")       # no default seperator, required by $ environment
}


math_env_code <- function(e=1, begin=TRUE, label=NULL) 
{
  es <- c("$$", "$")      
  es.be <- c("equation", "equation*", "align", "align*",     # envirs w \begin \end wrapper
             "gather", "gather*", "multline", "multline*", "split")
  all.es <- c(es, es.be)
  if (is.character(e))      # convert e to number of given as character
    e <- which(match.arg(e, all.es) == all.es)
  if (!is.null(label) & begin)
    label <- paste0("\\label{", label, "}")
  es.b <- c(c("$$\n", "$"),                     # inline $ must not follow any whitespace
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
# g <- wrap_in_math_envir(lex("1 + 1 = 2"))


# TODO
autocheck_for_math_envir <- function() {
  
}


#' Concatenate \code{texcode} chunks.
#' 
#' @param   ... \code{texcode} objects or objects that can ce converted
#'              into texcode objects. Enter any number of chunks 
#'              seperated by commas.
#' @param e Math environment to use (numeric or string). 
#'          If \code{NA} the default 
#'          is used as defined in \code{mat2tex_options()$mathenv}.
#'          If set to \code{NULL}, no environment is added, just the plain
#'          math code is returned. See details.
#' @param   label A label for the equation in case an environment is used 
#'          that supports labels, e.g. \code{equation}. 
#'          Only applicable to \code{Rnw} documents.
#' @inheritParams xm         
#' @details The available math environments are
#'           \code{1=$$, 2=$, 3=equation, 4=equation*, 5=align, 6=align*, 
#'          7=gather, 8=gather*, 9=multiline, 10=multiline*, 11=split}. 
#'          You can either supply the corresponding numeric or the name of 
#'          the math environment in argument \code{e}. 
#'          For \code{Rmd} file only math environment (argument \code{e}) 1 and 2 are relevant.
#'          When using \code{Rnw} files make sure to include the \code{amsmath} package 
#'          the document preamble as most environments are defined in it.
#' @return  Object of class \code{texcode}.
#' @author  Mark Heckmann
#' @export
#'
xx <- function(..., e=NA, label=NULL, digits=NA, mtype=NA, round=NA, na=NA) 
{
  # temporarily change defaults within xx
  opts <- mat2tex_options()
  if (!is.na(digits))
    mat2tex_options(digits=digits)
  if (!is.na(mtype))
    mat2tex_options(mtype=mtype)
  if (!is.na(round))
    mat2tex_options(round=round)
  if (!is.na(na))
    mat2tex_options(na=na)
  
  # get default values 
  if (is.na(e))
    e <- mat2tex_options()$mathenv
  dots <- list(...)
  texcodes <- Reduce("%_%", dots) 
  if (!is.null(e)) 
    texcodes <- wrap_in_math_envir(texcodes, e = e, label=label) 
  mat2tex_options(opts) # restore old option pars
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
Ops.texcode <- function(e1, e2) 
{
  e1 <- as.texcode(e1)
  e2 <- as.texcode(e2)
  if (.Generic %in% c("+", "%%") 
	    & class(e1) == "texcode" 
	    & class(e2) == "texcode") {
	} else {
    NextMethod(.Generic)		
	} 
	res <- c(e1, e2)
	class(res) <- class(e1)
	res
}


#' Convert to \code{texcode} object.
#' 
#' @param x    Some R object.
#' @export
#' @keywords   internal
#' @details    Converts matrices, dataframes, strings and vectors.
#'             Vectors of length 1 are converted to strings, e.g. to represent
#'             scalars.
#'
as.texcode <- function(x) 
{
  if (class(x) == "matrix")
    x <- xm(x)
  if (class(x) == "data.frame")
    x <- xm(as.matrix(x))
  if (class(x) == "character" & length(x) == 1)  # only single string
    x <- lex(x)
  if (is.vector(x) & length(x) > 1)   # any vector length > 1
    x <- xm(as.matrix(x))
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
#' @inheritParams xx
#' @details The available math environments are
#'           \code{1=$$, 2=$, 3=equation, 4=equation*, 5=align, 6=align*, 
#'          7=gather, 8=gather*, 9=multiline, 10=multiline*, 11=split}. 
#'          You can either supply the corresponding numeric or the name of 
#'          the math environment in argument \code{e}. 
#'          For \code{Rmd} file only math environment (argument \code{e}) 1 and 2 are relevant.
#'          When using \code{Rnw} files make sure to include the \code{amsmath} package 
#'          the document preamble as most environments are defined in it.      
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


#' Operators to allow for easy combination of texcode objects 
#' 
#' @param x,y     \code{texcode} object, string or matrix.
#' @rdname grapes-_-grapes.Rd
#' @export
#' @keywords internal
#'
`%_%` <-  function(x,y) {
  # no spaces between chunks
  as.texcode(x) + as.texcode(y)
}


#' @rdname grapes-_-grapes.Rd
#' @export
#' 
`%__%` <-  function(x,y) {
  # small space between chunks
  as.texcode(x) %_% as.texcode("\\;") %_% as.texcode(y)
}


#' @rdname grapes-_-grapes.Rd
#' @export
#' 
`%_0%` <-  function(x,y) {
  # small negative space
  as.texcode(x) %_% as.texcode("\\!") %_% as.texcode(y)
}


#' @rdname grapes-_-grapes.Rd
#' @export
#' 
`%_1%` <-  function(x,y) {
  # thin space between chunks
  as.texcode(x) %_% as.texcode("\\,") %_% as.texcode(y)
}


#' @rdname grapes-_-grapes.Rd
#' @export
#' 
`%_2%` <-  function(x,y) {
  # medium space between chunks 
  as.texcode(x) %_% as.texcode("\\:") %_% as.texcode(y)
}


#' @rdname grapes-_-grapes.Rd
#' @export
#' 
`%_3%` <-  function(x,y) {
  # a thick space between chunks 
  as.texcode(x) %_% as.texcode("\\;") %_% as.texcode(y)
}


#' @rdname grapes-_-grapes.Rd
#' @export
#' 
`%_4%` <-  function(x,y) {
  # space of width of letter M
  as.texcode(x) %_% as.texcode("\\quad") %_% as.texcode(y)
}


#' @rdname grapes-_-grapes.Rd
#' @export
#' 
`%_5%` <-  function(x,y) {
  # two times space of widths of letter M
  as.texcode(x) %_% as.texcode("\\qquad") %_% as.texcode(y)
}


#' Insert horizontal spaces in formula
#' 
#' The function is a wrapper around the
#' LaTeX \code{\\mkern} command. It will produce
#' horizintal spaces with a width given as multiples of the 
#' letter \code{M}. E.g. \code{s(2)} equates the LaTeX code
#' \code{\\mkern2em}.
#' 
#' @param em    Numeric. Width of inserted space in multiples 
#'              of letter \code{M}. The default is \code{0}.
#'              Negative spaces are allowed.
#' @export
#' @examples
#' 
#'  "$ 1 + 1 =" %_% s(2) %_% "2$"
#'  xx("1+1=", s(2), "2")
#'  
s <- function(em=0)
{
  s.code <- paste0("\\mkern", em, "em")
  as.texcode(s.code)
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



