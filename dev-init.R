if (!require(pak)) {
  install.packages("pak")
}

#' Create a lockfile and install all required packages
pak::lockfile_create(
  pkg = c(
    "dplyr",
    "Greater-London-Authority/gglaplot",
    "ggplot2",
    "Greater-London-Authority/gsscoder",
    "kableExtra",
    "knitr",
    "magrittr",
    "minpack.lm",
    "ropensci/nomisr",
    "readr",
    "readxl",
    "rmarkdown",
    "stringr",
    "tidyr",
    "zoo"
  ),
  dependencies = NA,
  upgrade = TRUE,
  lockfile = "pkg.lock"
)

# Create a temp library for this session
tmp_lib <- tempfile("r_lib_")
dir.create(tmp_lib)

# Put it first in the path
.libPaths(c(tmp_lib, .libPaths()))

pak::lockfile_install(lockfile = "pkg.lock", lib = .libPaths()[1])
