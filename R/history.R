#' Get play history
#'
#'@export
tp_get_history <- function(view = FALSE, print = FALSE) {
  # get the path of user history file
  path_user_tp_history <- check_user_tp_history(create_new = FALSE)
  # get user's history
  tubeplay_history <- readRDS(path_user_tp_history)

  # output
  if (view) {
    View(tubeplay_history)
  }
  if (print) {
    print(tubeplay_history)
  }

  invisible(tubeplay_history)
}


#' Clear play history
#'
#' @export
tp_clear_history <- function() {
  path_user_tp_history <- check_user_tp_history(create_new = FALSE)
  message("Clear tubeplay history.")
  saveRDS(tp_history_zero, path_user_tp_history)
}


#' play from history
#'
#' @export
tp_replay <- function(backto = 1, add_history = FALSE) {
  tp_history <- tp_get_history()
  target_history_url <- tp_history[backto, "url"]

  tubeplay(target_history_url, add_history = add_history)
}

# zero history df
tp_history_zero <- data.frame(
  title = character(),
  type = character(),
  url = character(),
  timestamp = numeric()
)

# check tubeplay user's history
check_user_tp_history <- function(create_new = TRUE) {
  path_user_tp_history <- fs::path(check_user_tp_dir(create_new = FALSE), "_tubeplay_history.Rds")
  if (!fs::file_exists(path_user_tp_history)) {
    message('The history file of "tubeplayR" does not exist.')
    if (create_new) {
      message(
        paste('Create "', path_user_tp_history, '".')
      )
      tp_history <- tp_history_zero
      saveRDS(tp_history, path_user_tp_history)
    } else {
      stop(
        paste('Please check this path:',
              path_user_tp_history,
              sep = '\n')
      )
    }
  }
  return(path_user_tp_history)
}


# add history
add_tube_history <- function(tube_info) {

  # check is_tube_info
  if (!is_tube_info(tube_info)) {
    stop("Please check tube_info objects")
  }

  # check existing ".Rtubeplay" and get this path
  path_user_tp_dir <- check_user_tp_dir(create_new = TRUE)

  # check existing "_tubeplay_history.Rds" and get this path
  path_user_tp_history <- check_user_tp_history(create_new = TRUE)

  # read history and bind new history
  tp_history <- readRDS(path_user_tp_history)

  tp_history_add <- subset(tube_info, select = names(tp_history_zero))
  tp_history <- rbind(tp_history_add, tp_history)

  # save tp_history
  saveRDS(tp_history, path_user_tp_history)

  # for test
  return(tp_history)
}
