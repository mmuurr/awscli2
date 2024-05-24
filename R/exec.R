## desired usage:
## awscli(commands("rds", "wait", "db-instance-ready"),

`%0%` <- vctrs::`%0%`
`%>%` <- magrittr::`%>%`
`%||%` <- rlang::`%||%`


DEFAULT_CONFIG <-
  list(
    output = "json"
  ) %>% base::list2env()


is_valid_json <- function(json) {
  jsonlite::validate(json)
}


from_json <- function(json) {
  jsonlite::fromJSON(json, simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}


#' @export
set_default_profile <- function(profile_name) {
  DEFAULT_CONFIG$profile <- profile_name
}


#' @export
set_default_region <- function(region) {
  DEFAULT_CONFIG$region <- region
}


#' @export
get_default_config <- function() {
  config <- as.list(DEFAULT_CONFIG)
  config[order(names(config))]  ## stable ordering for test expectations
}


## TODO: deprecate in favor of get_default_config(), i.e. use consistent naming.
#' @export
get_default_opts <- get_default_config


dots2list <- function(...) {
  rlang::dots_list(
    ...,
    .ignore_empty = "all",
    .preserve_empty = FALSE,
    .homonyms = "last"
  )
}


list_names <- function(l, missing = "") {
  names(l) %||% rep(missing, length(l))
}


aws_args <- function(commands, ..., .config = NULL) {

  config_as_is <- inherits(.config, "AsIs")

  .config <- as.list(.config)
  checkmate::assert_list(.config, names = "unique")

  .config <-
    if (config_as_is) {
      .config
    } else {
      purrr::list_modify(get_default_config(), !!!.config)
    }

  ## convert any (possibly named) character vectors to a list.
  commands <- as.list(as.character(commands))  ## as.character() strips names
  command_args <- dots2list(...)

  ## this must remain a list for a few steps so NULLs can be stored (for flags).
  args_list <- dots2list(!!!c(.config, commands, command_args))

  ## force all names to be strings; if missing then use empty string ("").
  ## prepend double-hyphen ("--") if named and not already starting with "--"
  arg_names <-
    list_names(args_list, "") %>%
    stringr::str_trim() %>%
    { dplyr::if_else(. == "" | stringr::str_detect(., "^--"), ., sprintf("--%s", .)) }
  
  arg_values <- 
    args_list %>%
    purrr::map(function(x) {
      (x %||% "") %>%
        as.character() %>%
        stringr::str_trim()
    })
  
  ## zip these together into a single character vector.
  ## in cases where the value is itself a character vector of length > 1, e.g.:
  ##   names  = n_a, n_b,
  ##   values = (v_a1, v_a2), v_b
  ##   zipped = n_a v_a1, v_a2, n_b, v_b
  ## discard any still empty strings.
  final_args <-
    purrr::map2(arg_names, arg_values, c) %>%
    purrr::flatten_chr() %>%
    purrr::discard(function(x) stringr::str_length(x) == 0)

  final_args
}


#' @export
awscli <- function(commands, ..., .config = NULL, .proc = FALSE, .echo_cmd = FALSE, .echo = FALSE, .timeout_sec = Inf) {
  proc <- processx::run(
    "aws",
    aws_args(commands, ..., .config = .config),
    echo_cmd = .echo_cmd,
    echo = .echo,
    timeout = .timeout_sec,
    error_on_status = FALSE
  )
  if (isTRUE(proc$status == 0)) {
    if (isTRUE(.proc)) return(proc)
    if (is_valid_json(proc$stdout)) return(from_json(proc$stdout)) else return(proc$stdout)
  } else {
    rlang::abort(sprintf("system command 'aws' failed:\n%s", proc$stderr))
  }
}


#' @export
get_version <- function() {
  awscli(c(), "version" = NULL)
}
