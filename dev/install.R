# build vignette devtools::build_vignettes()
# make sure to include in DESCRIPTION file:
# 
# VignetteBuilder: knitr
# Suggests:
#     knitr,
#     VignetteBuilder
# 
# and in .Rmd file
# 
# <!--
# %\VignetteEngine{knitr::rmarkdown}
# %\VignetteIndexEntry{Inject R matrices into LaTeX formulas}
# -->
  
# create .md file for github

require(knitr) # required for knitting from rmd to md
require(markdown) # required for md to html 
knit('vignettes/mat2tex.Rmd', 'README.md') # creates md file
