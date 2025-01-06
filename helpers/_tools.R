
#' Conditional inferences
#'
#' Calculate estimands conditional on observed data (currently, for single-case process tracing) together with data realization probabilities
#' Realization probabilities are the probability of the observed data given data is sought on observed node
#'
#' Function is designed for process tracing; it can be applied to a posterior distribution however though currently data probabilities using parameters only.
#'
#'
#' @export
#' @examples
#' model <- make_model("X->Y")
#' conditional_inferences(model, query = "Y[X=1]>Y[X=0]")
#'
#' # Example of posteriors given monotonic X -> M -> Y model
#' model <- make_model("X-> M -> Y")  %>%
#'   set_restrictions(labels = list(M = "10", Y = "10"))
#' conditional_inferences(model, query = "Y[X=1]>Y[X=0]", given = "Y==1")
#'
#' # Example on posteriors given monotonic X -> M -> Y model
#' model <- make_model("X-> M -> Y")  %>%
#'   set_restrictions(labels = list(M = "10", Y = "10")) %>%
#'   update_model(data.frame(X=0:1, Y = 0:1))
#' conditional_inferences(model, query = "Y[X=1]>Y[X=0]", given = "Y==1",
#' using = "posteriors")
#'
#' # Running example
#' model <- make_model("S -> C -> Y <- R <- X; X -> C -> R") %>%
#'    set_restrictions(labels =
#'    list(C = "1110", R = "0001", Y = "0001"), keep = TRUE)
#' conditional_inferences(model, query = list(COE = "(Y[S=0] > Y[S=1])"),
#' given = "Y==1 & S==0")

conditional_inferences <- function(model,
                                   query,
                                   parameters=NULL,
                                   given = NULL,
                                   using = "parameters",
                                   case_level = FALSE){


  if(!is.null(parameters) & using != "parameters")
    stop("parameters arguments is not consistent with using argument")

  if(is.null(parameters)) parameters <- grab(model, "parameters")

  vars <- model$nodes

  # Possible data
  vals <-  CausalQueries:::get_all_data_types(model, given = given) %>% select(-event)

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
      parameters  = list(parameters),
      using = "parameters",
      queries = query,
      given = given)$mean

  if(using != "parameters")
    estimands <- query_model(
      model   = model,
      using = using,
      queries = query,
      given = given,
      case_level = case_level)$mean

  # Calculate data probabilities
  probs <- unlist(get_data_probs(model, data = vals))
  probs <- probs[rownames(vals)]


  out <- data.frame(cbind(vals, estimands, probs))

  names(out) <- c(vars, "posterior", "prob")

  data.frame(out)
}

#' Make data for multi-step strategy
#'
#' Creates a database of possible data from a data strategy.
#' Users can gather additional data on node specified via \code{vars} for any possible cases in the model ("any"). Or they can
#' gather data in all cases within an observed dataset ("within"). Or they can specify  the subset of cases for which within-case data should be collected (e.g. "Y == 1").
#'
#' @param N An integer. Number of node to seek.
#' @param withins A list of logicals. Whether to seek node within existing data. Defaults to TRUE.
#' @param conditions  A list of character strings indicating for which cases data should be gathered. Options are: (i) to gather additional data on node specified via \code{vars} for any possible cases in the model ("any"), (ii) to gather data in all cases within an observed dataset ("within"), or (iii) to specify the subset of cases for which within-case data should be collected (e.g. "Y == 1").
#' @param vars A character vector. Variables to be sought or NA. If NA \code{make_possible_data} gathers data on all node containing NA for the specified data strategy.
#' @param prefix for columns of output; useful if multiple dataframes are later merged
#' @param unique = TRUE  If same data is gathered via different routes it still only gets represented once
#' @export
#' @return A dataset with columns: event, strategy, plus possibly multiple cases profiles
#' @examples
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(c("Y[M=1]<Y[M=0]"), "(M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#' df <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#' observed <- collapse_data(df, model)[, -2]
#'
#' # Complex *not within*
#' make_possible_data(model, N=list(1,2), withins = FALSE, vars = list(c("X", "Y"), c("X")), conditions = TRUE)
#'
#' # Complex, sequential: not within, then within
#' make_possible_data(model, N=list(1,1), observed = observed,
#'              withins = c(FALSE, TRUE),
#'              vars = list(c("X", "Y"), c("M")),
#'              conditions = TRUE)
#'
#' # Look for data on M for all possible cases in the observed data
#' make_possible_data(model, N = 0, within = FALSE)
#' make_possible_data(model, N = 2, within = FALSE)
#' make_possible_data(model, observed = observed, vars = "M", N = 2, conditions = c("X==Y"))
#' make_possible_data(model, observed = observed, vars = "M", N = list(1,1), conditions = list("X==Y", "X==Y"))
#'
#' # Not possible:
#' make_possible_data(model, observed, vars = "M", within = TRUE, N = 7)
#'
#' # Partly possible: only one step completed
#' model2 <- make_model("A -> B -> C -> D")
#' observed2 <- data.frame(A = c(0,0,0,1,1,1), B = NA, C = NA, D = c(0,0,1,0,1,1)) %>%
#'   collapse_data(model2, drop_family = TRUE)
#' make_possible_data(model2, observed2, vars = list("B", "C"), within = TRUE, N = list(1,1), conditions = list("A==D", "A==D & B==1"))
#'
#' # Within conditions
#' make_possible_data(model, observed, within = TRUE, N = 2, conditions = "X==1 & Y==1", vars = "M")
#'
#' # Look for data on K but not M
#'
#'
#' # From book
#'
#' model <- make_model("X->M->Y")  %>%
#'  set_restrictions(c("(Y[M=1]<Y[M=0])", "(M[X=1]<M[X=0])"))
#'
#' 	observed <-  collapse_data(data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1)),
#' 													model, drop_family = TRUE)
#' 	make_possible_data(model,
#'  	observed = observed,
#'  	vars = list("M", "M", "M", "M"),
#'  	withins = TRUE,
#'  	N = list(1,1,1,1),
#'  	conditions = list("X==0 & Y==0", "X==1 & Y==0", "X==0 & Y==1", "X==1 & Y==1"))

make_possible_data <- function(
    model,
    observed = NULL,
    N = list(1),
    withins = TRUE,
    conditions = list(TRUE),
    vars = NULL,
    prefix = NULL,
    unique = TRUE) {

  if(is.null(vars)) vars <- list(model$node)

  if(is.null(observed) & withins[1]) {message("No datan observed; 'withins' changed to FALSE"); withins[1] <- FALSE}

  if(!is.null(observed)) if(!identical(names(observed), c("event", "count"))){
    stop("'observed' df should have two columns: event and count")}

  if(is.null(observed)) observed <- CausalQueries:::minimal_event_data(model)

  if("strategy" %in% names(observed)) observed <- dplyr::select(observed, - strategy)

  if(length(vars)==1 & (length(N)>1)) vars <- rep(vars, length(N))
  if(length(withins)==1 & (length(N)>1)) withins <- rep(withins, length(N))
  if(length(conditions)==1 & (length(N)>1)) conditions <- rep(conditions, length(N))
  if(!identical(length(conditions), length(N)) )
    stop("N, and conditions  must have the same length")



  if(!identical(length(vars), length(N)) )
    stop("Vars should be of length 1 or else have the same length as conditions  and N")

  g_df <- make_possible_data_single(
    model,
    observed = observed,
    withins = withins[[1]],
    N = N[[1]],
    conditions = conditions[[1]],
    vars = vars[[1]] )


  if(length(N) == 1){
    attr(g_df, "possible_data_args") <- list(N = N,withins = withins, conditions = conditions, vars = vars)
    names(g_df)[1:2] <- c("event", "count")
    g_df <- (check_event_data(g_df, model))
    colnames(g_df)[-c(1:2)] <- 1:(ncol(g_df)-2)

    return(g_df)
  }


  for (i in 2:length(N)){

    out <- lapply(2:ncol(g_df), function(s) {
      use_df <-  g_df[,c(1,s)]
      names(use_df)  <- c("event", "count")
      data_single <- make_possible_data_single(model,
                                               observed = 	use_df,
                                               withins = withins[[i]],
                                               N     = N[[i]],
                                               conditions = conditions[[i]],
                                               vars  = vars[[i]])

      colnames(data_single)[2:ncol(data_single)] <- paste0(s-1, "-", colnames(data_single)[2:ncol(data_single)])
      data_single
    })

    out   <- Reduce(function(x, y) merge(x, y,  by = c("event"), all = TRUE), 	out)
    g_df  <- dplyr:::mutate_if(out, is.numeric, ~replace(., is.na(.), 0))

  }

  if(!is.null(prefix)) names(g_df)[-1] <- paste0(prefix, "_", names(g_df)[-1])

  # Flag
  names(g_df)[1:2] <- c("event", "count")
  g_df <- check_event_data(g_df, model)

  g_df[,!duplicated(t(g_df))]

  colnames(g_df)[-c(1:2)] <- 1:(ncol(g_df)-2)

  if(unique) g_df <- g_df[, !duplicated(t(g_df))]

  g_df
}




#' Make possible data for a single strategy step
#'
#' Creates a database of possible data from a data strategy.
#' Users can gather additional data on node specified via \code{vars} for any possible cases in the model ("any"). Or they can
#' gather data in all cases within an observed dataset ("withins"). Or they can specify  the subset of cases for which withins-case data should be collected (e.g. "Y == 1").
#' @keywords internal
#' @param N Number of node to seek
#' @param withins logical Whether to seek node within existing data
#' @param conditions  A list of character strings indicating for which cases data should be gathered. Options are: (i) to gather additional data on node specified via \code{vars} for any possible cases in the model ("any"), (ii) to gather data in all cases within an observed dataset ("within"), or (iii) to specify the subset of cases for which within-case data should be collected (e.g. "Y == 1").
#' @param vars Variables to be sought or NA. If NA \code{make_possible_data} gathers data on all node containing NA for the specified data strategy.
#' @export
#' @return A dataset
#' @examples
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(c("Y[M=1]<Y[M=0]", "M[X=1]<M[X=0]")) %>%
#'    set_parameter_matrix()
#' df <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#' observed <- collapse_data(df, model, drop_family = TRUE)
#'
#' make_possible_data_single(model, observed = observed, vars = "M", withins = TRUE, N = 2)

make_possible_data_single <- function(model,
                                      observed = NULL,
                                      N = 1,
                                      withins = FALSE,
                                      conditions = TRUE,
                                      vars = NULL) {

  if(is.null(vars) & withins) stop("Please specify vars to be examined")

  if(withins & is.null(observed)) stop("If 'withins' is specified 'observed' must be provided")

  if(is.null(observed)) observed <- CausalQueries:::minimal_event_data(model)[,-2]

  if(N==0 & withins) return(observed)

  # If not withins simply select possible data
  if(!withins) return(
    complex_combine(list(
      observed,
      new = all_possible(model, N, vars) %>% dplyr::select(-count))))


  # Otherwise its more complicated: select from *within* available data
  if(is.null(observed)) stop("observed not provided, but 'withins' requested")

  # all_event_types <- collapse_data(all_data_types(model), model, drop_family = TRUE)

  possible <- CausalQueries:::get_all_data_types(model) %>%
    filter(eval(parse(text = conditions))) %>%
    mutate(event = as.character(event))

  # This part to allow searching in cases where there is space to seek listed vars
  possible <- possible[apply(possible[vars], 1, function(j) all(is.na(j))),]

  all_buckets <- left_join(dplyr::select(possible, event), observed, by = "event")[c("event", "count")]
  all_buckets$count[is.na(all_buckets$count)] <-0
  all_buckets <- mutate(all_buckets, capacity = count)

  if(sum(all_buckets$capacity) < N) {message("Not enough units to allocate N.
																						 Perhaps you are seeking data within cases in which data is already observed?"); return(observed)}

  strategies <- as.matrix(partitions::blockparts(all_buckets$capacity, N))
  colnames(strategies) <- 1:ncol(strategies)
  all_buckets <- cbind(all_buckets, strategies)

  # This function goes through a bucket strategy and generates all possible datasets
  # that could be produced by the strategy
  ##################################################################################
  get_results_from_strategy <- function(strategy){
    buckets          <- all_buckets[all_buckets$capacity>0 ,]
    buckets          <- buckets[buckets[,strategy]>0, c(1:3, strategy)]
    data_list        <- lapply(1:nrow(buckets), function(j)   fill_bucket(model, buckets, vars, row = j, column = 4))

    # If cases have been drawn from within set, remove these now
    if(withins) data_list[["remove_bucket"]] <- data.frame(event = buckets$event, x = -(buckets$capacity))

    data_list[["observed"]] <- observed

    out <- complex_combine(data_list)

    names(out)[-1] <-paste0(strategy-3, ".",  1:(ncol(out)-1))
    out

  }

  # Run over all strategies
  all_strategies <- sapply(4:ncol(all_buckets), function(s) get_results_from_strategy(s),
                           simplify = FALSE)
  out <- Reduce(function(x, y) merge(x, y,  by = "event", all = TRUE), all_strategies) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))

  out$event <- as.character(out$event)

  out

}


#' Complex combine
#'
#' Used to combine permutations of rows of dataframes in a list
#' @param data_list list of dataframes. All dataframes should contain event column but have unique event elements
#' @examples
#' data_list <- list(
#' data.frame(event = c("a", "b"), w = 1:2, x = 3:4),
#' data.frame(event = c("c", "d", "e"), y = 5:7, z = 8:10, q = 11:13))
#' complex_combine(data_list)
#' data_list <- list(
#' data.frame(event = c("a", "b"), w = 1:2, x = 3:4),
#' data.frame(event = c("c", "d", "b"), y = 5:7, z = 8:10, q = 11:13))
#' complex_combine(data_list)


complex_combine <- function(data_list) {

  locations <- CausalQueries:::perm(unlist(lapply(data_list, ncol)) - 2)

  dfs <- lapply(1:nrow(locations), function(i) {
    parts <-  lapply(1:length(data_list), function(j) {
      df <- data_list[j][[1]][, c(1, locations[i,j][[1]]+2)]
      names(df) <- c("event", "cases")
      df
    })
    out <- do.call("rbind", parts)
    out <- aggregate(cases ~ event, data = out, sum)
    names(out) <- c("event", i)
    out
  })
  Reduce(function(x, y) merge(x, y,  by = "event", all = TRUE), dfs)
}


#' Get data probabilities
#'
#' Takes in a matrix of possible (single case) observations and returns the probability of each.
#' FLAG: Some redundancy with make_data_probabilities
#'
#' @param data Data in long format
#' @export
#' @examples
#' model <- make_model("X->Y")
#' data <- simulate_data(model, n = 4)
#' get_data_probs(model, data)
get_data_probs <- function(model, data, parameters = NULL){

  if(is.null(parameters)) parameters <- grab(model, "parameters")

  events  <- collapse_data(data = data, model = model)$event
  A_w     <- CausalQueries:::get_data_families(model, mapping_only = TRUE, drop_all_NA = FALSE)
  probs   <- A_w %*% get_event_probabilities(model, parameters = parameters)
  np      <- rownames(probs)
  unlist(sapply(events, function(j) probs[np==j]))
}


check_event_data <-
function(df, model) {
  if(!(names(df)[[1]] == "event")) stop("event_data must include an initial `event` column")
  if("strategy" %in% names(df)) df <- dplyr::select(df, - strategy)
  structure <- collapse_data(expand_data(df[, 1:2], model), model)[, 1:2]
  out <- dplyr::left_join(structure, df, by = "event")
  out[is.na(out)] <- 0
  out
}

complex_combine <-

  function(data_list) {

  locations <- CausalQueries:::perm(unlist(lapply(data_list, ncol)) - 2)

  dfs <- lapply(1:nrow(locations), function(i) {
    parts <-  lapply(1:length(data_list), function(j) {
      df <- data_list[j][[1]][, c(1, locations[i,j][[1]]+2)]
      names(df) <- c("event", "cases")
      df
    })
    out <- do.call("rbind", parts)
    out <- aggregate(cases ~ event, data = out, sum)
    names(out) <- c("event", i)
    out
  })
  Reduce(function(x, y) merge(x, y,  by = "event", all = TRUE), dfs)
  }


all_possible <-

function(model, N, vars = NULL, condition = TRUE, possible_data = TRUE, complete_data = TRUE){

  if(is.null(vars)) vars <- model$node

  df <- CausalQueries:::get_all_data_types(model, possible_data = possible_data, complete_data = complete_data, given = condition)

  if(!all(is.na(vars))) df[, !names(df) %in% vars] <- NA

  df  <- collapse_data(df, model, drop_family = TRUE)

  possible_data <- allocations(N, n = sum(df$count>0))

  out <- matrix(0, nrow(df), ncol(possible_data))
  out[df$count > 0,] <- as.matrix(possible_data)
  cbind(df, out)
}

allocations <-
function(N, n) {
  x <- partitions::compositions(N,n)
  x <- data.frame(as.matrix(x))
  colnames(x) <- 1:ncol(x)
  x
}


#' Strategy evaluation
#'
#' @inheritParams CQtools_internal_inherit_params
#' @param strategies  s x 3 matrix of strstegies. Each strategy is a triple that indicates (a) first node sought (b) action if first node is 0 (c) action if first node = 1
#' @param prices A vector of prices of length length(model$nodes)
#'
#' This function calculates the expected posterior variance from each strategy and the expected number of clues sought
#'
#' @export
#' @examples
#' model <-
#'   make_model("S -> C -> Y <- R <- X; X -> C -> R") %>%
#'      set_restrictions(
#'      labels = list(C = c("1110", "1111"),
#'                    R = c("0001", "0000"),
#'                    Y = c("0001")), keep = TRUE)
#'
#' # Evaluation of single strategy
#' strategy_evaluation(model,
#' 		strategies  = c("S", NA, NA),  given = "Y==0",
#' 		query = "(Y[S=1] != Y[S=0])")
#'
#' strategy_evaluation(model,
#' 		strategies  = c("S", "Y", "Y"),  given = "Y==0",
#' 		query = "(Y[S=1] != Y[S=0])")
#'
#' # The cheaper strategy is to look for S only,
#' # the lower variance strategy is look for X if S=0
#' # Any strategy that loos for  X if S = 1 is dominated
#'
#' strategy_evaluation(model,
#' 		strategies  = rbind(
#'   		c("S", NA, NA),
#'  		c("S",  "X", NA),
#'   		c("S", NA, "X"),
#'  		c("S",  "X", "X")),
#' 		given = "Y==0",
#' 		query = "(Y[S=1] != Y[S=0])")
#'
#' strategy_evaluation(model,
#' 		strategies  = c("S", NA, NA),  given = "Y==0",
#' 		query = "(Y[S=1] != Y[S=0])")
#'
#' # R is no help
#' strategy_evaluation(model,
#'    strategies  = c("R", NA, "S"),
#'    query = "(Y[S=1] != Y[S=0])",
#'    given = "Y==0")
#'
#'\dontrun{
#' strategy_evaluation(model,
#' 		strategies  = c("X", NA, NA),  given = "Y==0 & R==1",
#' 		query = "(Y[S=1] != Y[S=0])")
#'}
#'
#' # Evaluation of many strategies (a little slow)
#'
#' strategies <- two_step_strategies(model)[1:5,]
#'
#' result <-  strategy_evaluation(model,
#'   										strategies  = strategies,
#'   										query = "(Y[S=1] != Y[S=0])",
#'   										given = "Y==0 & X==1",
#'   										prices = c(1, 1.5, 1.2, .5, .7))
#' result

strategy_evaluation <- function(model,
                                strategies,
                                query,
                                given = NULL,
                                prices = NULL) {

  prior_mean <- query_model(model = model, queries = query, given = given, using = "parameters")$mean
  prior_variance <- prior_mean*(1-prior_mean)
  if(is.na(prior_mean)) stop("Prior not defined. Check for impossible conditions.")

  prior = c(expected_n = 0, expected_variance = prior_variance, expected_cost = 0)

  vars <- model$nodes
  if(is.null(prices)) prices <- rep(1, length(vars))

  if(!is.null(given)) {
    given_vars <- stringr::str_extract_all(given, stringr::boundary("word"))[[1]]
    given_vars <- given_vars[(given_vars %in% vars)]
  } else {given_vars <- NULL}

  if(is.vector(strategies)) if(length(strategies) !=3){stop("A single strategy should be of length 3")
  } else {strategies <- matrix(strategies, 1, 3)}

  x <- apply(strategies, 1, function(j) {
    strategy_evaluation_single(model,
                                         strategy  = j,
                                         query = query,
                                         given = given,
                                         prices = prices,
                                         vars = vars,
                                         given_vars = given_vars)}) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE)

  # Export
  #	labels <- c("priors", apply(strategies, 1, paste, collapse = "-"))
  #	labels <- c("priors", apply(strategies, 1, paste, collapse = "-"))

  strategies2 <- strategies
  strategies2[is.na(strategies2)] <- "."
  labs <- c("priors", paste0("(",strategies2[,1],"),(", strategies2[,2], ",", strategies2[,3], ")"))
  cbind(labs,
        rbind(prior, x))
}


#' Internal function for evaluating a single strategy
#' @keywords internal
#' @inheritParams CQtools_internal_inherit_params

strategy_evaluation_single <- function(model,
                                       strategy,
                                       query,
                                       given = NULL,
                                       prices = NULL,
                                       vars = model$nodes,
                                       given_vars = NULL  #Names of vars in "given"
) {

  # Strategy 1

  s1 <- strategy[[1]]

  if(is.na(s1)) stop("First strategy should not be NA")

  na_vars <- vars[!(vars %in% unlist(c(s1, given_vars)))]

  givens  <- paste(given, paste0("is.na(", na_vars, ")", collapse = " & "), sep = " & ")

  # Conditional inferences
  ci  <- conditional_inferences(model, query = query, given = givens)

  # Step 1 conclusions depending on findings on strategy 1
  p0 <- filter(ci, ci[s1]==0)[1, c("posterior", "prob")]
  p1 <- filter(ci, ci[s1]==1)[1, c("posterior", "prob")]
  p0[is.na(p0)] <- 0  # NAs arise if no possible events
  p1[is.na(p1)] <- 0  # NAs arise if no possible events

  # Probability of finding 0/1 in step 1 (normalized)
  probs  <- c(p0$prob, p1$prob)/(p0$prob + p1$prob)

  # Gather posteriors
  posts <- c(p0$posterior, p1$posterior)

  evs <-
    sapply(0:1, function(j){
      s <- strategy[[2+j]]
      ifelse(is.na(s),
             (posts[1+j])*(1-posts[1+j]),
             ifelse(probs[1+j] == 0, -1,  # - 1 is placeholder for posteriors on zero probability event
                    expected_learning(model,
                                      query = query,
                                      strategy = paste(s),
                                      given = paste(given, "&", strategy[[1]], "==", j),
                                      parameters = NULL)$E_post_var)
      )
    })

  # Flag: Fill in NAs if probs = 0
  # evs[probs == 0]  <- 0
  # strategy[2:3][probs == 0]  <- "NONE"

  # Expected Number of clues sought: 1 plus 1 if additional steps sought
  expected_n        <- 1 + probs%*%as.vector((!is.na(strategy[2:3])))

  # Expected variance given conditional strategies
  expected_variance <- probs%*%evs

  # Costs
  costs <- c(sum(prices[vars %in% factor(strategy[-3])]),
             sum(prices[vars %in% factor(strategy[-2])]))
  expected_costs <- probs%*%costs

  c(expected_n =        expected_n,
    expected_variance = expected_variance,
    expected_costs =    expected_costs)
}



#' Make set of two step strategies
#'
#' @inheritParams CQtools_internal_inherit_params
#' @param vars Variables to
#' @export
#' @examples
#' two_step_strategies(make_model("X->M->Y"))
two_step_strategies <- function(model, vars = model$nodes){

  x <- sapply(1:length(vars), function(j)
    expand.grid(one = vars[j],
                two_0 = c(NA, vars[-j]),
                two_1 = c(NA, vars[-j]),
                stringsAsFactors = FALSE),
    simplify = FALSE)

  do.call("rbind", x) %>%
    data.frame(stringsAsFactors = FALSE)
}



#' Expected learning
#'
#' Expected reduction in variance from one step data collection strategy
#' @inheritParams CQtools_internal_inherit_params
#' @importFrom  stringr str_extract_all boundary
#' @importFrom dplyr mutate filter
#' @export
#' @examples
#' # Reduction in variance given monotonic X -> M1 -> M2 -> Y model
#'
#' model <- make_model("X -> M1 -> M2 -> Y") %>%
#'   set_restrictions(labels = list(M1 = "10", M2 = "10", Y = "10"))
#' el <- expected_learning(model, query = "Y[X=1]>Y[X=0]",
#'                   strategy = c("X", "M2"), given = "Y==1")
#' attr(el, "results_table")
#'
#' el2 <- expected_learning(model, query = "Y[X=1]>Y[X=0]",
#'                   strategy = c("M1"),
#'                   given = "Y==1 & X==1 & M2==1")
#' attr(el2, "results_table")
#'
#' # No strategy
#' expected_learning(model, query = "Y[X=1]>Y[X=0]")
#'
#' # No givens
#' expected_learning(model, query = "Y[X=1]>Y[X=0]",
#' strategy = c("M1"))
#' expected_learning(model, query = "Y[X=1]>Y[X=0]",
#' strategy = c("M1"), given = "Y==1")
#'
#' model <-  make_model("S -> C -> Y <- R <- X; X -> C -> R") %>%
#' set_restrictions(labels =  list(C = "1110", R = "0001", Y = "0001"),  keep = TRUE)
#'
#' expected_learning(model,
#' query = list(COE = "(Y[S=0] > Y[S=1])"),
#' strategy = "C", given = "Y==1 & S==0")
#'
#' expected_learning(model,
#' query = list(COE = "(Y[X=1] > Y[X=0])"),
#' strategy = "S", given = "X==0 & Y==0")



expected_learning <- function(model, query, strategy = NULL, given = NULL, parameters = NULL){

  prior_estimand <- query_model(model, query = query, given = given, using = "parameters")$mean

  vars <- model$nodes
  given0 <- ifelse(is.null(given), " ", given)

  # Figure out which nodes are given
  given_vars <- NULL
  if(!is.null(given)) {
    given_vars <- stringr:::str_extract_all(given, boundary("word"))[[1]]
    given_vars <- given_vars[(given_vars %in% vars)]}

  # All strategy vars need to be seen
  if(!is.null(strategy)){
    vars_to_see <- paste0("!is.na(", strategy, ")", collapse = " & ")
    if(is.null(given)) { given <- vars_to_see
    } else {
      given <- paste(given, "&", vars_to_see)}
  }

  # Augment "given" to examine cases with NA in all other vars
  unseen_vars <- vars[!(vars %in% c(strategy, given_vars)) ] # na only for these vars
  if(length(unseen_vars) >0) {
    unseen <- paste0("is.na(", unseen_vars, ")", collapse = " & ")
    if(is.null(given)) {given <- unseen} else {given  <- paste(given, "&", unseen)}
  }

  ######################

  results_table <-
    conditional_inferences(model = model, query = query,
                           given = given, parameters = parameters)
  results_table <- filter(results_table, prob !=0)

  # Clean up
  results_table <- mutate(results_table,  prob = prob/sum(prob), var = posterior*(1-posterior))

  # Summarize
  out <- with(results_table,
              data.frame(
                strategy = paste(strategy, collapse = ", "),
                given = given0,
                #prior_estimand = prob%*%posterior,
                #prior_var  = (prob%*%posterior)*(1- prob%*%posterior),
                prior_estimand = prior_estimand,
                prior_var  = (prior_estimand)*(1- prior_estimand),

                E_post_var = (prob%*%var), stringsAsFactors = FALSE))

  #  print(query)
  #  print(out)

  attr(out, "results_table") <- results_table

  out
}



#' helper to fill buckets dataframe
#' @inheritParams CQtools_internal_inherit_params
#' @param buckets dataframe with columns event, count and capacity vars plus strategy allocation var
#' @param vars vars to be observed
#' @export
#' @examples
#' model <- make_model("X->M->Y")
#' buckets = data.frame(event = "X0Y0", count = 3, capacity = 3, strategy = 2)
#' # Find different data that might result from looking at "M" in 2 out of 3 X0Y0 data types
#' fill_bucket(model, buckets, vars = "M")
fill_bucket <- function(model, buckets, vars, row = 1, column = 4){

  if(!(all(vars %in% model$node))) stop("Vars not in model$node")

  # Figure out set of possible finer units
  df <- expand_data(data_events = data.frame(
    event = buckets$event[row], count = 1), model)
  possible_findings <- CausalQueries:::perm(rep(1, length(vars)))
  df <- df %>% slice(rep(1:n(), each = nrow(possible_findings)))
  df[vars] <- possible_findings
  df <- collapse_data(df, model, drop_family = TRUE)
  # Assign n across new possible finer events
  new_events <- cbind(event = df[df$count ==1, "event"],
                      allocations(buckets[row, column], sum(df$count)))

  # tidy up
  remaining  <- data.frame(event = buckets[row, 1], matrix(buckets$count[row] - buckets[row, column], ncol = ncol(new_events)-1, nrow = 1))
  names(remaining) <- names(new_events)
  rbind(new_events,remaining)
}




#' helper for ways to allocate N units into n data types: tidies partition::composition output
#'
#' @param N Number of observations to be distributed
#' @param n Number of possible values observations could take
#' @export
#' @examples
#' allocations(4,2)
allocations <- function(N, n) {
	x <- partitions::compositions(N,n)
	x <- data.frame(as.matrix(x))
	colnames(x) <- 1:ncol(x)
	x
}


#' Generates a database of results using updated model over possible data
#'
#' This function can run many models and can take a long time depending on the size of possible data.
#'
#' @inheritParams CQtools_internal_inherit_params
#' @param possible_data A data frame with an events column and possible data columns (if a strategy columns is included it is ignored)
#' @param queries list of statements for causal queries
#' @param expand_grid logical, If TRUE combinations of queries and given are expanded
#' @export
#' @return A list with query output dataframes for each data strategy
#' @examples
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(c("Y[M=1]<Y[M=0]", "M[X=1]<M[X=0]")) %>%
#'    set_parameter_matrix()
#'
#' observed <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1)) %>%
#'         collapse_data(model, drop_family = TRUE)
#'
#'
#' possible_data <- make_possible_data(model, observed, vars = "M", condition = "X==1 & Y==1")
#'
#' estimates_database <- make_estimates_database(
#'       model,
#'       observed = observed,
#'       possible_data = possible_data,
#'       queries = "Y[X=1]>Y[X=0]")
#'
#' estimates_database <- make_estimates_database(
#'       model,
#'       observed = observed,
#'       possible_data = possible_data,
#'       queries = c(ATE = "Y[X=1]-Y[X=0]", PC = "Y[X=1]>Y[X=0]"),
#'       given = c(TRUE, "Y==1 & X==1"))

make_estimates_database <- function(model,
                                    observed,
                                    possible_data = NULL,
                                    queries = "Y[X=1]>Y[X=0]",
                                    given = TRUE,
                                    expand_grid = FALSE,
                                    use_parameters = FALSE,
                                    iter = 4000,
                                    refresh = 0,
                                    chains = NULL,
                                    ...) {


  if(is.null(possible_data)) possible_data <- make_possible_data(model, observed, ...)
  if("strategy" %in% names(possible_data)) possible_data <- dplyr::select(possible_data, -strategy)

  if(use_parameters){
    return(
      lapply(2:ncol(possible_data), function(j) {

        data.frame(
          query_model(model,
                      queries = queries,
                      using = "parameters",
                      given = given,
                      expand_grid = expand_grid),
          data_pattern = j -1
        )})
    )
  }


  ## Update model for each possible data type and query updated model
  ## Note: 2 here only because of particular shape of possible data

  lapply(2:ncol(possible_data), function(j) {

    data_events <- possible_data[, c(1, j)]
    names(data_events) <- c("event", "count")

    data <- expand_data(data_events, model)

    updated <- CausalQueries::update_model(
      model = model,
      data = data,
      iter = iter,
      refresh = refresh,
      chains = chains)

    data.frame(
      query_model(updated,
                  queries = queries,
                  using = "posteriors",
                  given = given,
                  expand_grid = expand_grid),
      data_pattern = j -1
    )

  })

}



#' Generates a probability distribution over possible data outcomes
#'
#' NOTE: This needs to be checked for whether it is taking account of strategy probabilities properly
#' @inheritParams CQtools_internal_inherit_params
#' @param pars A parameter vector.
#' @param possible_data Possible events data
#' @param A_w Ambiguity matrix for data types, optional
#' @param strategy_set vector containing possible strategies, optional
#' @param normalize logical if TRUE probabilities are normalized to sum to 1
#' @export
#' @return A dataset
#' @examples
#'
#' model <- make_model("X->M->Y")
#' possible_data <- make_possible_data(model, N= 2, vars = list(model$node), within = FALSE)
#' make_data_probabilities(model, pars = get_parameters(model), possible_data)
#'
#' given <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1)) %>%
#'   collapse_data(model)
#' possible_data <- make_possible_data(model, observed = select(given, - strategy), condition = "X==1 & Y==1", vars = "M", within = TRUE )
#' make_data_probabilities(model, pars = get_parameters(model), possible_data)
#' make_data_probabilities(model, pars = get_parameters(model), given)
#'
make_data_probabilities <- function(
    model,
    pars,
    possible_data,
    A_w = NULL,
    strategy = NULL,
    strategy_set = NULL,
    normalize = FALSE,
    type_prob = NULL,
    event_probs = NULL,
    P = NULL
) {

  # Check data consistency
  if(!all(names(possible_data)[1:2] == c("event", "strategy")))
    stop("possible_data should lead with event and strategy columns")

  # If only one possible data, return 1
  if(normalize & ncol(possible_data %>% select(-event, -strategy))==1)
    return(1)


  if (is.null(event_probs)){

    event_probs <- CausalQueries:::get_event_probabilities(model, parameters = as.numeric(pars))

  }

  # Ambiguity matrix for data types
  if(is.null(A_w))
    A_w <- CausalQueries:::get_data_families(model, drop_impossible = TRUE, drop_all_NA = FALSE, mapping_only = TRUE)[possible_data$event, ]

  # Event probabilities
  w_full <- A_w %*% event_probs

  if(is.null(strategy))
    strategy <- possible_data$strategy

  if(is.null(strategy_set))
    strategy_set <- unique(strategy)

  # Probability of outcomes within each strategy set
  # Need to be sure about ordering of data
  x <-
    dplyr::select(possible_data, - c(event, strategy)) %>%
    apply(2, function(d)
      sapply(strategy_set, function(j) dmultinom(d[strategy==j], prob = w_full[strategy==j])
      ))

  # Take product of probabilities across strategies
  if(length(strategy_set)>1) x <- apply(x, 2, prod)

  # Normalization
  if(normalize) x <- x/sum(x)

  x

}




#' Get leave one out data likelihood
#'
#' Assesses data likelihood using leave out procedure
#'
#' @inheritParams CQtools_internal_inherit_params
#' @param data Data in long format
#' @param sims Number of simulations
#' @param ... arguments passed to update_model
#' @export
#' @examples
#' model <- make_model("X->Y")
#' data <- simulate_data(model, n = 4)
#' get_loo_likelihood(data, model)
#'
get_loo_likelihood <- function(data, model, sims=100, ...){

  short_data <- collapse_data(data, model)
  rows       <- 1:nrow(short_data)

  case_likelihood <-
    lapply(rows,
           function(j) {
             if (short_data$count[j] == 0) return(1)
             loo_data <-
               mutate(short_data, count = ifelse(count > 0 & rows == j, count - 1, count))
             loo_left_compact <-
               mutate(short_data, count = ifelse(count > 0 & rows == j, 1, 0))

             loo_post <-
               update_model(model, loo_data, data_type = "compact", keep_event_probabilities = TRUE,...)
             k <-
               min(sims, nrow(loo_post$posterior_distribution))
             loo_prob <-
               apply(loo_post$stan_objects$event_probabilities[1:k, ], 1, function(ep)
                 make_data_probabilities(model,
                                         possible_data = loo_left_compact, event_probs = ep))
             mean(loo_prob)
           })

  list(data = short_data, model = model, case_likelihood = case_likelihood,
       loo_likelihood = prod((case_likelihood %>% unlist)^(short_data$count)))
}
