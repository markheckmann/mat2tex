## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(mat2tex)
#opts_chunk$set(out.extra='style="display:block; margin: auto"', fig.align="center")

## ----echo=FALSE----------------------------------------------------------
# reset default values. build_vignettes() seems to use current environment
mat2tex:::mat2tex_options_init()

## ----eval=FALSE----------------------------------------------------------
#  library(devtools)
#  install_github("mat2tex", "markheckmann")

## ----results='hide'------------------------------------------------------
set.seed(1)
A <- matrix(runif(4), 2)

## ----echo=TRUE, results='asis'-------------------------------------------
"$$ A = " %_% A %_% "$$"

## ----echo=TRUE, results='asis'-------------------------------------------
xx("A =", A)

## ----echo=TRUE, results='asis'-------------------------------------------
xb(e=1) %_% xm(A, digits=3) %_% xe(e=1)

## ----echo=TRUE, results='asis'-------------------------------------------
xc("$$") %_% xm(A, 3) %_% xc("$$")

## ----echo=TRUE, results='asis'-------------------------------------------
"$$" %_% xm(A, 2) %_% "$$"

## ----echo=TRUE, results='asis'-------------------------------------------
"$$" %_% A %_% "$$"

## ----echo=TRUE, results='asis'-------------------------------------------
xx(xm(A, 2), e=1)

## ----echo=TRUE, results='asis'-------------------------------------------
xx(A)

## ----echo=TRUE, results='asis'-------------------------------------------
"$$" %_% xm(A, mtype = "b") %_% "$$"

## ----echo=TRUE, results='asis'-------------------------------------------
xx( xm(A, m="m"), xm(A, m="p"), xm(A, m="b"), 
    xm(A, m="B"), xm(A, m="v"), xm(A, m="V"))   

## ----echo=TRUE, results='asis'-------------------------------------------
d <- svd(A)
"$$ A =" %_% A %_% "= UDV^T =" %_% d$u %_% diag(d$d) %_% d$v %_% "^T" %_% "$$"

## ----echo=TRUE, results='asis'-------------------------------------------
xx("A =", A, "= UDV^T =", d$u, diag(d$d), xmt(d$v), e=1)

## ----echo=TRUE, results='asis'-------------------------------------------
g <- rep(1, 2)
I <- diag(2)
"$$" %_% 
  pl() %_% "I - 11^T" %_% pr() %_% "A" %_% lb() %_%
  "=" %_% pl() %_% xm(I, 0) %_% "-" %_% xm(g, 0) %_% xm(t(g), 0) %_% pr() %_% A %_% lb() %_% 
  "=" %_% xm(I - g %*% t(g), 0) %_% A %_% lb() %_% 
  "=" %_% xm((I - g %*% t(g)) %*%A) %_% 
"$$" 

## ----echo=TRUE, results='asis'-------------------------------------------
xx(pl(), "I - 11^T", pr(), "A", lb(),
  "=", pl(), xm(I, 0), "-", xm(g, 0), xm(t(g), 0), pr(), A, lb(), 
  "=", xm(I - g %*% t(g), 0), A, lb(),
  "=", xm((I - g %*% t(g)) %*%A), e=1)

## ------------------------------------------------------------------------
mat2tex_options()

## ----echo=TRUE, results='asis'-------------------------------------------
xx(xm(A, digits=4, mtype="bmatrix"))

## ------------------------------------------------------------------------
opts <- mat2tex_options(digits=4, mtype="bmatrix")

## ----echo=TRUE, results='asis'-------------------------------------------
xx(A)

## ----echo=TRUE, results='asis'-------------------------------------------
mat2tex_options(opts)
xx(A)

## ----eval=FALSE----------------------------------------------------------
#  mat2tex_options(mathenvir=1)

## ----eval=FALSE----------------------------------------------------------
#  mat2tex_options(mathenvir=3)

