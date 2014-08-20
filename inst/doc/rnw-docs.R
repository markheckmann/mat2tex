## ----eval=FALSE----------------------------------------------------------
#  library(devtools)
#  install_github("mat2tex", "markheckmann")

## ------------------------------------------------------------------------
library(mat2tex)

## ----echo=FALSE----------------------------------------------------------
# reset default values. build_vignettes() seems to use current environment
mat2tex:::mat2tex_options_init()

## ------------------------------------------------------------------------
set.seed(1)
A <- matrix(runif(4), 2)

## ----echo=TRUE, results='asis'-------------------------------------------
"$$" %_% xm(A) %_% "$$"

## ----echo=TRUE, results='asis'-------------------------------------------
xx(A)

## ------------------------------------------------------------------------
mat2tex_options(mathenvir=3)

## ----echo=TRUE, results='asis'-------------------------------------------
xx(A)

## ----echo=TRUE, results='asis'-------------------------------------------
xx(A, label="mylabel")

## ------------------------------------------------------------------------
mat2tex_options(mtype="bmatrix", digits=3)

## ----results='asis'------------------------------------------------------
d <- svd(A)
xx("A = UDV^T =", d$u, diag(d$d), xmt(d$v))

