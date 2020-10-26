# get user(home) dir
get_user_dir <- function() {
  env <- Sys.getenv("R_PROFILE_USER", unset = "")
  if (!identical(env, "")) {
    return(fs::path_expand(env))
  }
  return(fs::path(fs::path_home_r()))
}

# get user tp dir
get_user_tp_dir <- function() {
  fs::path(get_user_dir(), ".Rtubeplay")
}

# check user tp dir
check_user_tp_dir <- function(create_new = TRUE) {
  path_user_tp_dir <- get_user_tp_dir()
  if (!fs::dir_exists(path_user_tp_dir)) {
    message(
      paste('The directory for "tubeplayR" does not exist.')
    )
    if (create_new) {
      message(
        paste('Create "', path_user_tp_dir, '".')
      )
      fs::dir_create(path_user_tp_dir)
    } else {
      stop(
        paste('Please check this path: ',
              path_user_tp_dir,
              sep = '\n')
      )
    }
  }
  return(path_user_tp_dir)
}

#' Clean tubeplay user directory
#'
#' @export
tp_clean_user_dir <- function() {
  path_user_tp_dir <- check_user_tp_dir(create_new = FALSE)
  path_temp_backup <- fs::path(tempdir(), paste0("tp_backup_", paste(str_extract_all(Sys.time(), "\\d", simplify = TRUE), collapse = "")))

  fs::dir_copy(path_user_tp_dir, new_path = path_temp_backup)
  message("Temporary backup: ", path_temp_backup)

  fs::dir_delete(path_user_tp_dir)
  message("tubeplayR user directory was removed: ", path_user_tp_dir)

  invisible(path_temp_backup)
}


