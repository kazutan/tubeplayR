#' Create new plist
#'
#' @export
tp_create_new_plist <- function(urls = NULL, tube_infos = NULL, label) {
  # get tube_info from urls
  df_plist <- purrr::map_dfr(urls, get_tube_info)

  # bind tube_infos
  if (is_tube_info(tube_infos)) {
    df_plist <- dplyr::bind_rows(df_plist, tube_infos)
  }

  # check n of df_plist
  if (nrow(df_plist) == 0) {
    stop("Please set urls/tube_infos.")
  }

  # add plist
  plist <- tp_get_plist(create_new = TRUE)
  plist <- c(plist, list(df_plist))
  names(plist)[length(plist)] <- label

  # save plist
  path_plist <- check_user_tp_plist(create_new = TRUE)
  saveRDS(plist, path_plist)

  # return
  invisible(plist)
}

#' play from plist
#'
#' @export
tp_start_plist <- function(plist_id = NULL, plist_label = NULL, shuffle = FALSE) {
  # check and get plist file
  path_plist <- chech_user_tp_plist(create_new = FALSE)
  plist_all <- tp_get_plist()

  # attach plist
  if (!is.null(plist_id) & length(plist_all) < plist_id) {
    target_plist <- plist_all[[plist_id]]
  } else if (!is.null(plist_label)) {
    target_plist <- plist_all[[plist_label]]
  } else {
    target_plist <- NULL
  }
  if (is.null(target_plist)) {
    stop(
      "Please confirm args."
    )
  }

  # set state plist
  set_state_plist(target_plist, shuffle)

  # play 1st movie
  tp_next_plist(1)
}

#' get state plist
#' @export
tp_get_staged_plist <- function(view = FALSE, print = FALSE) {
  # check whether state plist exists
  if (!fs::file_exists(get_path_state_plist())) {
    stop(
      "Any plist are not started. Please start plist."
    )
  }

  # get state plist
  state_plist <- readRDS(get_path_state_plist())

  # output
  if (view) {
    View(state_plist)
  }
  if (print) {
    print(state_plist)
  }

  invisible(state_plist)
}

#' remove plist
#' @export
tp_remove_plist <- function(plist_id = NULL, plist_label = NULL) {
  # check and get plist file path
  path_plist <- check_user_tp_plist(create_new = FALSE)
  plist_all <- tp_get_plist()

  # remove plist
  if (!is.null(plist_id)) {
    if (length(plist_all) <= plist_id) {
      res_plist <- plist_all[-plist_id]
    } else {
      stop("Please confirm args.")
    }
  } else if (!is.null(plist_label)) {
    res_plist <- plist_all
    res_plist[plist_label] <- NULL
  } else {
    stop("Please confirm args.")
  }

  # write plist Rds file
  saveRDS(res_plist, path_plist)

  # return
  invisible(res_plist)
}

#' play next movie
#' @export
tp_next_plist <- function(skip = 1) {
  # check whether state plist exists
  if (!fs::file_exists(get_path_state_plist())) {
    stop(
      "Any plist are not started. Please start plist."
    )
  }

  # get state plist
  state_plist <- readRDS(get_path_state_plist())

  # set tubeplay
  target_tube_info <- state_plist[skip,]
  tubeplay(state_plist[skip, "url"])

  # remove played movie from state plist
  state_plist <- state_plist[-skip,]
  saveRDS(state_plist, get_path_state_plist())

  # return
  invisible(target_tube_info)
}

#' get path of state plist Rds
get_path_state_plist <- function() {
  path_user_state_plist <- fs::path(check_user_tp_dir(create_new = FALSE), "_tubeplay_state.Rds")
  return(path_user_state_plist)
}

#' set state plist
set_state_plist <- function(df_plist, shuffle) {
  # shuffle
  if (shuffle) {
    df_plist <- sample_n(df_plist, nrow(df_plist))
  }

  # write to Rds file
  path_user_state_plist <- get_path_state_plist()
  saveRDS(df_plist, path_user_state_plist)

  # return path
  invisible(path_user_state_plist)
}


#' get plist
#'
#' @export
tp_get_plist <- function(label = NULL, view = FALSE, print = FALSE, create_new = FALSE) {
  # get the path fo plist file
  path_user_tp_plist <- check_user_tp_plist(create_new = create_new)
  # get user's plist
  tubeplay_plist <- readRDS(path_user_tp_plist)

  # filter plist
  if (!is.null(label)) {
    tubeplay_plist <- tubeplay_plist[label]
  }

  # output
  if (view) {
    View(tubeplay_plist)
  }
  if (print) {
    print(summary_plist(tubeplay_plist))
  }

  invisible(tubeplay_plist)
}

# summary of plist
summary_plist <- function(plist) {
  df_summary_plist <- data.frame(
    no = 1:length(plist),
    label = names(plist),
    length = purrr::map_int(plist, nrow),
    stringsAsFactors = FALSE
  )

  return(df_summary_plist)
}

# check tubeplay user's plist
check_user_tp_plist <- function(create_new = TRUE) {
  path_user_tp_plist <- fs::path(check_user_tp_dir(create_new = FALSE), "_tubeplay_plist.Rds")
  if (!fs::file_exists(path_user_tp_plist)) {
    message('The plist file of "tubeplayR" does not exist.')
    if (create_new) {
      message(
        paste('Create "', path_user_tp_plist, '".')
      )
      saveRDS(list(), path_user_tp_plist)
    } else {
      stop(
        paste('Please check this path:',
              path_user_tp_plist,
              sep = '\n')
      )
    }
  }
  return(path_user_tp_plist)
}
