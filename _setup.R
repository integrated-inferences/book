# Replication controls
#
# To replicate figures for chapters 13 and 14 execute rep/strategy_replication.Rmd
# This analysis is slow (multiple hours for 13, multiple days for 14)
# The replication file has switches to allow figure replication only or
# complete simulation replication

# All other tables and figures are replicated on the fly within chapters
# Note: Chapter 16 loo and p value analyses are slow
# To replicate all these analyses set run to TRUE
run <-  FALSE

# Packages

# Check if BiocManager is installed
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Check and install Rgraphviz
if (!requireNamespace("Rgraphviz", quietly = TRUE)) {
  BiocManager::install("Rgraphviz")
}

# Check and install RBGL
if (!requireNamespace("RBGL", quietly = TRUE)) {
  BiocManager::install("RBGL")
}

# Load packages, and install from CRAN when not already installed:

library("pacman")
pacman::p_load(
  DT,
  ggdag,
  dagitty,
  ggtext,
  bookdown,
  CausalQueries,
  cowplot,
  DeclareDesign,
  GGally,
  ggh4x,
  ggstance,
  gtools,
  igraph,
  latex2exp,
  partitions,
  plotrix,
  pcalg,
  rstan,
  kableExtra,
  knitr,
  reshape2,
  dagitty,
  RBGL,
  Rgraphviz,
  rstan,
  stargazer,#,
   haven,
   magrittr,
   labelled,
   repr,
   sjmisc,
 tidyverse
)


# Functions
source("helpers/_tools.R")

# Package options:
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
knitr::opts_chunk$set(fig.align = "center")

options(knitr.kable.NA = '')
