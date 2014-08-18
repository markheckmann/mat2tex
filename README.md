<!--
%\VignetteEngine{knitr::rmarkdown}
%\VignetteIndexEntry{Inject R matrices into LaTeX formulas}
-->




Inject R matrices into LaTeX formulas
=======================================

The **mat2tex** package was written to facilitate the combination of matrices and $\LaTeX$ code.

### Installation

To install the **mat2tex** package from github enter


```r
library(devtools)
install_github("mat2tex", "markheckmann")
```

into the console.

### Introduction

**mat2tex** is a mini-language with several operators and functions to allow to combine LaTeX code and R objects. Suppose you have the matrix `A`.


```r
set.seed(1)
A <- matrix(runif(4), 2)
```

Know we want to create a LaTeX chunk inside an `Sweave` or a `Rmarkdown` file that contains an equation like this one:

$$ 
A =  
\begin{pmatrix}
 0.27 & 0.58 \\ 
 0.37 & 0.91 \\ 
\end{pmatrix}
$$

Using **mat2tex** you can achieve this very easily, by inserting the following code into a `knitr` chunk with the options `echo=FALSE` and `results='asis'`


```r
"$$ A = " %_% A %_% "$$"
```

$$ A =  \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 $$

which is equivalent to


```r
xx("A =", A)
```

$$
 A = \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 $$


In the example above we used a string containing plain LaTeX code `"$$ \\mathbf{A} = "` and concatated it with the matrix `A` and another plain LaTeX chunk using the `%_%` operator. The operator can be used to concatenate chunks. Alternatively, you can use the `xx` function which will concatenate all the chunks given as arguments and seperated by comma automatically. The math environment we added by hand above (`$$`) is added automatically here.


### Creating chunks using Functions and operators

**mat2tex** is a mini-language consisting of functions to generate code chunks and several operators to combine the chunks. Most of the function names are acronymes starting with an `x` which stands for `TeX` or `LaTeX`.

function  | description
------------- | -------------
`xc`    | LaTeX code. Convert R object into chunk that can be concatenated.
`xm`    | Convert to LaTeX matrix
`xmt`   | Convert to LaTeX matrix plus transpose sign
`xb`    | Begin LaTeX math environment
`xe`    | End LaTeX math environment
`xx`    | A function that allows to enter all code chunks seperated by comma.
`lp`    | Create left parenthesis
`rp`    | Create right parenthesis


operator  | description
------------- | -------------
`%_%`   | Concatenate LaTeX chunks.
`%__%`  | Same as `%_%` but adds extra horizontal space.
`+`     | Same as `%_%` but only applicabale if unambigous. Better use `%_%` to be on the safe side.

### Examples

The following codes begins a new math environment (`xb()`) and ends it again (`xb()`).
The `e` argument specifies the type of environment we want. In the case we use the `$$` environment (`e=1`), which is the default. Inside we use the function `xm` to convert the matrix `A` into LaTeX codem in this case with `3` digits.


```r
xb(e=1) %_% xm(A, digits=3) %_% xe(e=1)
```

$$
 \begin{pmatrix}
 0.266 & 0.573 \\ 
 0.372 & 0.908 \\ 
 \end{pmatrix}
 $$

The following code is identical to the one above, using plain text to specify the type of math environment. In order to convert the text into a chunk that can be concatenated, the function `xc` is used.


```r
xc("$$") %_% xm(A, 3) %_% xc("$$")
```

$$ \begin{pmatrix}
 0.266 & 0.573 \\ 
 0.372 & 0.908 \\ 
 \end{pmatrix}
 $$

In most cases you can simply omit the `xc` function when using the `%_%` operator. The chunks are then automatically converted. The following code is identical to the one before.


```r
"$$" %_% xm(A, 2) %_% "$$"
```

$$ \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 $$

You can even omit the `xm` function. To convert the matrix into a LaTex code chunk the function `xm` is invoked. As no arguments are specified in this case, the default values (see section **defaults**) are used. 


```r
"$$" %_% A %_% "$$"
```

$$ \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 $$

An alternative formulation which is equivalent is to use the `xx` function to concatenate the code chunks. This function allows to enter the chunks seperated by commas. The nice feature is, that it automatically adds a math environment specified via the argument `e`.


```r
xx(xm(A, 2), e=1)
```

$$
 \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 $$

As `e=1` and `digits=2` is the default anyway, we can reduce this to


```r
xx(A)
```

$$
 \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 $$


Let's explore more arguments. We can change the type of brackets that are produced using the `mtype` argument.


```r
"$$" %_% xm(A, mtype = "b") %_% "$$"
```

$$ \begin{bmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{bmatrix}
 $$

The available bracket types (this time using the `xx` function) are


```r
xx( xm(A, m="m"), xm(A, m="p"), xm(A, m="b"), 
    xm(A, m="B"), xm(A, m="v"), xm(A, m="V"))   
```

$$
 \begin{matrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{matrix}
 \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 \begin{bmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{bmatrix}
 \begin{Bmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{Bmatrix}
 \begin{vmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{vmatrix}
 \begin{Vmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{Vmatrix}
 $$


### More examples

To construct a LaTeX expressions you can combine plain LaTeX text chunks, matrices and the output from several functions shown above to create more complex output. 

The SVD of $A$ is


```r
d <- svd(A)
"$$ A =" %_% A %_% "= UDV^T =" %_% d$u %_% diag(d$d) %_% d$v %_% "^T" %_% "$$"
```

$$ A = \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 = UDV^T = \begin{pmatrix}
 -0.54 & -0.84 \\ 
 -0.84 & 0.54 \\ 
 \end{pmatrix}
 \begin{pmatrix}
 1.17 & 0.00 \\ 
 0.00 & 0.02 \\ 
 \end{pmatrix}
 \begin{pmatrix}
 -0.39 & -0.92 \\ 
 -0.92 & 0.39 \\ 
 \end{pmatrix}
 ^T $$
    
or using the `xx` function


```r
xx("A =", A, "= UDV^T =", d$u, diag(d$d), xmt(d$v), e=1)
```

$$
 A = \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 = UDV^T = \begin{pmatrix}
 -0.54 & -0.84 \\ 
 -0.84 & 0.54 \\ 
 \end{pmatrix}
 \begin{pmatrix}
 1.17 & 0.00 \\ 
 0.00 & 0.02 \\ 
 \end{pmatrix}
 \begin{pmatrix}
 -0.39 & -0.92 \\ 
 -0.92 & 0.39 \\ 
 \end{pmatrix}
 ^T $$

Let's create a **multiple rows** example using the line break function `lb` to insert linebreaks (if or not this works depends on your math environment).


```r
g <- rep(1, 2)
I <- diag(2)
"$$" %_% 
  pl() %_% "I - 11^T" %_% pr() %_% "A" %_% lb() %_%
  "=" %_% pl() %_% xm(I, 0) %_% "-" %_% xm(g, 0) %_% xm(t(g), 0) %_% pr() %_% A %_% lb() %_% 
  "=" %_% xm(I - g %*% t(g), 0) %_% A %_% lb() %_% 
  "=" %_% xm((I - g %*% t(g)) %*%A) %_% 
"$$" 
```

$$ \left( I - 11^T \right) A \\ = \left( \begin{pmatrix}
 1 & 0 \\ 
 0 & 1 \\ 
 \end{pmatrix}
 - \begin{pmatrix}
 1 \\ 
 1 \\ 
 \end{pmatrix}
 \begin{pmatrix}
 1 & 1 \\ 
 \end{pmatrix}
 \right) \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 \\ = \begin{pmatrix}
 0 & -1 \\ 
 -1 & 0 \\ 
 \end{pmatrix}
 \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 \\ = \begin{pmatrix}
 -0.37 & -0.91 \\ 
 -0.27 & -0.57 \\ 
 \end{pmatrix}
 $$

or


```r
xx(pl(), "I - 11^T", pr(), "A", lb(),
  "=", pl(), xm(I, 0), "-", xm(g, 0), xm(t(g), 0), pr(), A, lb(), 
  "=", xm(I - g %*% t(g), 0), A, lb(),
  "=", xm((I - g %*% t(g)) %*%A), e=1)
```

$$
 \left( I - 11^T \right) A \\ = \left( \begin{pmatrix}
 1 & 0 \\ 
 0 & 1 \\ 
 \end{pmatrix}
 - \begin{pmatrix}
 1 \\ 
 1 \\ 
 \end{pmatrix}
 \begin{pmatrix}
 1 & 1 \\ 
 \end{pmatrix}
 \right) \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 \\ = \begin{pmatrix}
 0 & -1 \\ 
 -1 & 0 \\ 
 \end{pmatrix}
 \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 \\ = \begin{pmatrix}
 -0.37 & -0.91 \\ 
 -0.27 & -0.57 \\ 
 \end{pmatrix}
 $$


### Changing the default settings

You can change several options that will affect the output.
As default e.g. numeric values are rounded to two digits and round brackets 
are used to display the matrices and the `$$` math environment is used. 
You can retrieve the current default settings by typing.


```r
mat2tex_options()
```

```
## $digits
## [1] 2
## 
## $round
## [1] TRUE
## 
## $mtype
## [1] "pmatrix"
## 
## $mathenvir
## [1] 1
```

Suppose we want to have rectangular brackets and values rounded to four digits.
We can achieve that by setting the `xm` arguments accrodingly.


```r
xx(xm(A, digits=4, mtype="bmatrix"))
```

$$
 \begin{bmatrix}
 0.2655 & 0.5729 \\ 
 0.3721 & 0.9082 \\ 
 \end{bmatrix}
 $$

If we want to use these settings as the default and apply these settings to all matrices can instead change the default values. To change the settings enter `name = value` pairs seperated by a comma. To change the default number of digits to `4` and the type of matrix to `bmatrix` ), i.e. rectangular, type  


```r
opts <- mat2tex_options(digits=4, mtype="bmatrix")
```

As a result the matrix $A$ will know look like this


```r
xx(A)
```

$$
 \begin{bmatrix}
 0.2655 & 0.5729 \\ 
 0.3721 & 0.9082 \\ 
 \end{bmatrix}
 $$

To recreate the former settings again (note that the old settings were saved in `opt`) we can supply them as the argument to `mat2tex_options` and we get the old default back.


```r
mat2tex_options(opts)
xx(A)
```

$$
 \begin{pmatrix}
 0.27 & 0.57 \\ 
 0.37 & 0.91 \\ 
 \end{pmatrix}
 $$


#### Working with **.Rmd** and **.Rnw** files

It makes sense to change the default settings according to 
file default math environment type you work with.
Working with `.Rmd` files using `mathenviron=1` i.e. `$$` (which is the default) 
is suitable. 


```r
mat2tex_options(mathenvir=1)
```

For `.Rnw` you may want to change it to `mathenviron=3`, i.e. the `equation` environment.


```r
mat2tex_options(mathenvir=3)
```




 

