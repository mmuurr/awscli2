## desired usage:
## awscli(commands("rds", "wait", "db-instance-ready"),


DEFAULT_CONFIG <- list(
  output = "json"
)


is_valid_json <- function(json) {
  jsonlite::validate(json)
}


from_json <- function(json) {
  jsonlite::fromJSON(json, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
}


set_default_profile <- function(profile_name) {
  DEFAULT_CONFIG$profile <<- profile_name
}


dots2list <- function(...) {
  rlang::dots_list(
    ...,
    .ignore_empty = "all",
    .preserve_empty = FALSE,
    .homonyms = "last"
  )
}


#' @importFrom vctrs `%0%`
#' @importFrom magrittr `%>%`
aws_args <- function(commands, ..., .config = NULL) {
  ## convert any (possibly named) character vectors to a list.
  .config <- if (is.null(.config)) DEFAULT_CONFIG else as.list(.config)
  commands <- as.list(as.character(commands))  ## as.character() strips names
  command_args <- dots2list(...)

  ## this must remain a list for a few steps so NULLs can be stored (for flags).
  args_list <- dots2list(!!!c(.config, commands, command_args))

  ## force names, even if empty string for all.
  ## then prepend "--" where non-empty and not already starting with "--".
  arg_names <- 
    (names(args_list) %0% rep("", length(args_list))) %>%
    stringr::str_trim() %>%
    { dplyr::if_else(
      stringr::str_length(.) == 0 | stringr::str_detect(., "^--"),  ## if
      .,                                                            ## then
      sprintf("--%s", .)                                            ## else
    ) }

  arg_values <- 
    args_list %>%
    purrr::map(function(x) if (is.null(x)) "" else x) %>%  ## NULL -> ""
    purrr::map_chr(stringr::str_trim)  ## now we have character()

  ## zip these together into a single character vector.
  ## discard any still empty strings.
  final_args <-
    purrr::map2(arg_names, arg_values, c) %>%
    purrr::flatten_chr() %>%
    purrr::discard(function(x) stringr::str_length(x) == 0)

  final_args
}


#' @importFrom magrittr `%>%`
#' @export
awscli <- function(commands, ..., .config = NULL, .proc = FALSE, .echo_cmd = FALSE, .echo = FALSE) {
  proc <- processx::run(
    "aws",
    aws_args(commands, ..., .config = .config),
    echo_cmd = .echo_cmd,
    echo = .echo
  )
  if (isTRUE(.proc)) return(proc)
  if (is_valid_json(proc$stdout)) return(from_json(proc$stdout)) else return(proc$stdout)
}


#' @export
get_version <- function() {
  awscli(c(), "version" = NULL)
}
