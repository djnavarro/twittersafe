#' Get list of mutes for the authenticated user
#'
#' @param token Token
#' @param n Maximum number of user_ids to retrieve
#'
#' @return
#' @export
get_mutes <- function(token = NULL, n = 5000) {
  api <- "/1.1/mutes/users/ids"
  params <- list(
    include_entities = "false",
    skip_status = "true"
  )
  #params[[twitterapi::user_type(user)]] <- user
  resp <- twitterapi::TWIT_paginate_cursor(token, api, params, n = n)
  ids <- unlist(lapply(resp, function(x) x$ids))
  users <- tibble::tibble(user_id = as.character(ids))
  return(users)
}



# check API output to see if rate limit reached
rate_exceeded <- function(out) {
  if(is.null(out$error)) return(FALSE)
  if(grepl("limit exceeded", out$error$message)) return(TRUE)
  return(FALSE)
}

# error-safe version of rtweet::post_mute
safe_mute <- purrr::safely(rtweet::post_mute)

# apply safe_mute, pausing if limits exceeded, with verbose cli output
verbose_mute <- function(user) {

  withr::local_options(scipen = 14)
  cli::cli_process_start(paste("muting user id", user))

  repeat {
    out <- safe_mute(user)
    if(rate_exceeded(out)) {
      Sys.sleep(300)
    } else {
      break
    }
  }

  if(is.null(out$result)) {
    cli::cli_process_failed()
  } else{
    cli::cli_process_done()
  }
}


#' Mute multiple users
#'
#' @param users Tibble containing a user_ids column
#'
#' @return
#' @export
mute_users <- function(users) {
  purrr::walk(as.numeric(users$user_id), verbose_mute)
}
