# misctools
A very messy R package compiling miscellaneous tools for general use.

## Install and update
Install and update using [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html):
```R
devtools::install_github("jgmv/misctools")
```

## Build package
Build new package versions after modifying with the build script:
```R
source("data-raw/build_misctools.R")
update_misctools()
```
