# Packages

run <-  FALSE

# devtools::install_github("benmarwick/wordcountaddin", type = "source", dependencies = TRUE)

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


# Functions from CQtools
source("helpers/_tools.R")


#####
# Programming and default package options:

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
knitr::opts_chunk$set(fig.align = "center")

options(knitr.kable.NA = '')


kabble <- function(x, ...) {

  if (knitr::is_latex_output()) {
    format <- "latex"
  } else {
    format <- "pandoc"
  }


  latex_options = c("striped")

  a <- list(...)


  a$x <- x

  # a$escape <- FALSE

  argnames <- names(a)

  if(!("digits" %in% argnames)) a$digits <- 3
  if(!("booktabs" %in% argnames)) a$booktabs <- TRUE
  if(!("format" %in% argnames)) a$format <- format

  X <- do.call(kable, a)
  if(!is.null(a$note)) X <- footnote(X, general = a$note)

  if(format == "latex") X <- kable_styling(X, latex_options = latex_options)

  X

  }

# kabble(data.frame(pi = pi))

#######

## Functions to extract key inferences given different data strategies

I1D1_names <- c("Mongolia (1990)", "Paraguay (1989)", "Sierra Leone (1996)", "Malawi (1994)")
I0D1_names <- c("Mexico (2000)", "Taiwan (1996)", "Albania (1991)", "Nicaragua (1984)")

Y0R0_names <-  c("Suriname", "Pakistan", "Nigeria", "Vietnam")
Y0R1_names <-  c("Ethiopia", "India", "Tanzania", "Senegal")
Y1R0_names <-  c("Columbia", "Algeria", "Indonesia", "(No example)")
Y1R1_names  = c("Malaysia", "Brazil", "Papua\nNew Guinea", "Dominican\nRepublic")

possible_inferences <- function(m, p, results, V1 = "M", V2= "P"){
  c(m, p,
    dplyr::filter(results, is.na(!!as.symbol(V1)) & is.na(!!as.symbol(V2)))$posterior,
    dplyr::filter(results, !!as.symbol(V1) == m   & is.na(!!as.symbol(V2)))$posterior,
    dplyr::filter(results, is.na(!!as.symbol(V1)) & !!as.symbol(V2) == p)$posterior,
    dplyr::filter(results, !!as.symbol(V1) == m   & !!as.symbol(V2) == p)$posterior)}


cases_table <- function(results, case_names, digits = 3, V1 = "M", V2= "P", ...) {

  cnames = c("Case", V1, V2, "No clues", paste0(V1, " only"), paste0(V2, " only"), paste0(V1, " and ", V2))

  out <- rbind(
    possible_inferences(0,0, results, V1, V2, ...),
    possible_inferences(0,1, results, V1, V2, ...),
    possible_inferences(1,0, results, V1, V2, ...),
    possible_inferences(1,1, results, V1, V2, ...))

  out <- round(out, digits)

  x <- data.frame(cbind(case_names, out))

  colnames(x) <- cnames
  x
}



cases_MP_ID <- function(model, using = "parameters")

list(
  # MP given ID
  "I = 0, D = 1:\nIs democratization due to equality?" = cases_table(
    conditional_inferences(
      model, using = using,
      query = "D[I=1] < D[I=0]",
      given = "D==1 & I==0"),
    case_names  = I0D1_names
  ),

  "I = 1, D = 1:\nIs democratization due to inequality?" =  cases_table(
    conditional_inferences(
      model, using = using,
      query = "D[I=1] > D[I=0]",
      given = "D==1 & I==1"),
    case_names  = I1D1_names
  ),

  "I = 0, D = 0:\nDid equality prevent democratization? " = cases_table(
    conditional_inferences(
      model, using = using,
      query = "D[I=1] > D[I=0]",
      given = "D==0 & I==0"),
    case_names  = rep("NA", 4)
  ),

  "I = 1, D = 0:\nDid inequality prevent democratization?" =  cases_table(
    conditional_inferences(
      model, using = using,
      query = "D[I=1] < D[I=0]",
      given = "D==0 & I==1"),
    case_names  = rep("NA", 4)
  ))


# MD given YR
cases_RST <- function(model, using = "parameters")

  list(
    "Y = 1, R = 1:\nIs strong growth due to strong institutions?" =
      cases_table(
        conditional_inferences(
          model,
          using = using,
          query = "Y[R=1] > Y[R=0]",
          given = "Y==1 & R==1"),
        case_names  = Y1R1_names,
        V1 = "M", V2= "D"
      ),

    "Y = 0, R = 0:\nIs poor growth due to weak institutions?" =
      cases_table(
        conditional_inferences(
          model,
          using = using,
          query = "Y[R=1] > Y[R=0]",
          given = "Y==0 & R==0"),
        case_names  = Y0R0_names,
        V1 = "M", V2= "D"
      ),

    "Y = 0, R = 1:\nIs poor growth due to strong institutions?" =
      cases_table(
        conditional_inferences(
          model,
          using = using,
          query = "Y[R=1] < Y[R=0]",
          given = "Y==0 & R==1"),
        case_names  = Y0R1_names,
        V1 = "M", V2= "D"),

    "Y = 1, R = 0:\nIs strong growth due to weak institutions?" =
      cases_table(
        conditional_inferences(
          model,
          using = using,
          query = "Y[R=1] < Y[R=0]",
          given = "Y==1 & R==0"),
        case_names  = Y1R0_names,
        V1 = "M", V2= "D"
      )

  )


#####
  # Draw DAG

hj_ggdag <- function(x = NULL,
                     y = NULL,
                     names = NULL,
                     arcs = NULL,
                     statement = NULL,
                     model = NULL, #accepts causal model objects and returns ggdag
                     title = "",
                     padding = .1, # padding around box if labels = T
                     labels = FALSE,
                     textcol = 'black', # text colour (not label)
                     textsize = 3.88, #text size (not label)
                     force = 0, #repelling force between labels
                     obscure=NULL, # obscure arrows of the form X->Y
                     shape = 16,
                     nodecol = 'lightgrey',
                     nodesize = 16,
                     labelsize = 3.88,
                     labelparse = TRUE,
                     ...) { # other arguments passed to ggdag and geom_dag_label_repel, e.g. force_pull, node = T/F

  # Checks
  if(is.null(model) & is.null(names))
    stop("Names should be provided directly or via model argument")
  if(is.null(statement) & is.null(model) & is.null(arcs))
    stop("Model statement should be provided directly or via model or arcs argument")

  # Get names
  nodes <- if (is.null(names)) model$nodes else LETTERS[1:length(names)]

  # Get statement
  if(!is.null(model)) statement <- model$statement
  if(!is.null(arcs))
    statement <-  paste(nodes[arcs[,1]], " -> ", nodes[arcs[,2]], collapse = "; ")
  dagitty_statement <-  paste("dag{", statement, "}") %>% dagitty


  # Add coordinates if provided (otherwise generated)

  if(!is.null(x)){
    names(x) <- nodes
    names(y) <- nodes

    coordinates(dagitty_statement) <-
      list(x = x , y = y) %>%
      coords2df() %>% coords2list()
  }

  # Make the df
  df <- dagitty_statement %>% tidy_dagitty()
  df$data <- df$data %>% mutate(
    label = if(is.null(names)) name else
      names %>% as.character %>% .[match(df$data$name,LETTERS)],
    end = if(is.null(names)) to else
      names %>% as.character %>% .[match(df$data$to,LETTERS)],
    update=paste0(label,end),
    pos=match(label,names)) %>%
    arrange(-desc(pos))

  #matching bit is necessary because the dataframe doesn't always list all names in the order you first specify

  # remove any arrows to be obscured
  if (!is.null(obscure)) {obscoords<-data.frame(update = lapply(obscure %>%
                                                                  str_split('->'),paste,collapse='') %>%
                                                  unlist())
  df$data$direction[match(obscoords$update,df$data$update)]<-NA}


  # Step 2: Format and export
  p <- df %>%
    ggplot(aes(x=x,y=y,xend=xend,yend=yend)) +
    geom_dag_point(colour=nodecol, shape=shape, size=nodesize) +
    theme_dag() +
    labs(title = TeX(title %>% str_remove_all('\\"')))

  if (labels==TRUE){
    parse <- ifelse(class(names)=='expression',TRUE,FALSE)

    p +
      geom_dag_label_repel(aes(label = label), #, fill = 'label'),
                           show.legend = TRUE,
                           parse = labelparse,
                           box.padding = padding,
                           hjust = 0,
                           segment.color = 'grey',
                           segment.size = 0.5,
                           min.segment.length=0.5,
                           size = labelsize,
                           force = force+2,
                           ...) +
      geom_dag_edges()

  } else {
    # Labels centered on nodes
    p +
      geom_dag_text(aes(label = label),
                          show.legend = FALSE,
                          parse = labelparse,
                          color=textcol,
                          size=textsize# ,
#                          box.padding = 0,
#                          force = force
      ) + geom_dag_edges()
  }
}


#### Simpler:

plot_fig_data <- function(model, fig.daata, ...) {
  fig.data <- fig.data[match(model$nodes, fig.data$nodes), ]
  plot_model(model, x_coord = fig.data$x , y_coord = fig.data$y,
             labels = fig.data$names, nodecol = "lightgrey", textcol = "black", nodesize = 10, ...)
}


####

perm_bb <- function(v) {
  sapply(1:length(v), function(x) {
    rep( rep(1:v[x], each=prod(v[x:length(v)]) / v[x]),
         length.out=prod(v))
  } ) - 1
}


#####
  # Find replace

file_find_replace <- function(filepath, pattern, replacement) {
  file_contents <- readLines(filepath)
  updated_contents <- gsub(x = file_contents, pattern = pattern, replacement = replacement)
  cat(updated_contents, file = filepath, sep = "\n")
}
my_rmd_scripts <- list.files(pattern = "(Rmd)$")

 # for (r_script in my_rmd_scripts ) file_find_replace(r_script, "lower-level", "lower level")


multiple_inferences <- function(model,
         query,
         parameters=NULL,
         given = NULL,
         using = "parameters"){


  if(!is.null(parameters) & using != "parameters") stop("parameters arguments is not consistent with using argument")

  if(is.null(parameters)) parameters <- get_parameters(model)

  vars <- model$nodes

  # Possible data
  vals <-  all_data_types(model, given = given) %>% select(-event)

  # Conditions
  conds <- t(apply(vals, 1, function(j) paste(vars, j, sep = "==")))
  conds[is.na(vals)] <- NA
  given <- apply(conds, 1, function(j) paste(j[!is.na(j)], collapse = " & ")) %>%
    as.list
  given[given==""] <- paste0(model$nodes[1], ">-1") # Guaranteed true

  impossible <- lapply(given, function(s) all(!(CausalQueries:::map_query_to_causal_type(model, s)$types))) %>% unlist

  if(all(impossible)) return(data.frame(vars, posterior = NA, prob = NA))

  vals   <- vals[!impossible, ]
  given  <- given[!impossible]


  # Calculate estimands
  if(using == "parameters")
    estimands <- query_model(
      model   = model,
      parameters  = parameters,
      using = "parameters",
      queries = query,
      given = given)$mean

  if(using != "parameters")
    estimands <- query_model(
      model   = model,
      using = using,
      queries = query,
      given = given,
      case_level = TRUE)$mean

  # Calculate data probabilities
  probs <- unlist(get_data_probs(model, data = vals))
  probs <- probs[rownames(vals)]


  out <- data.frame(cbind(vals, estimands, probs))

  names(out) <- c(vars, "posterior", "prob")

  data.frame(out)
}


colorize <- function(x, color= "orange") {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
            x)
  } else x
}

