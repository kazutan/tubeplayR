#' Play YouTube movie at RStudio
#'
#' @param url url that you want show YouTube movie
#' @param viewer set viewer. See ?htmltools::html_print.
#' @param add_history whether add to play history or not.
#'
#' @examples
#' tubeplay() # play default set movie
#' tubeplay("https://www.youtube.com/watch?v=0E00Zuayv9Q") # play "PPAP"
#' @export
tubeplay <- function(url = "https://www.youtube.com/watch?v=XSLhsjepelI",
                     viewer = getOption("viewer", utils::browseURL),
                     add_history = TRUE) {
  # create tube_info
  tube_info <- get_tube_info(url)

  # build ui
  ui <- build_ui_tubeplay(url)

  # send ui
  htmltools::html_print(ui)

  # add history
  if (add_history) {
    add_tube_history(tube_info)
  }

  # return
  invisible(tube_info)
}

# tubeplay builder
build_ui_tubeplay <- function(url) {
  # judge youtube single or list
  if(grepl("playlist?", url)) {
    # set target for list
    target <- gsub("^.*\\.com/", "", url)
    target <- gsub("playlist", "videoseries", target)
  }else if(grepl("^https://www.youtube.com/", url)){
    # seto target for single
    target <- gsub("^.*\\?v=", "", url)
    target <- gsub("\\&.*$", "", target)
  }

  # make_ui
  ui <- miniUI::miniPage(
    htmltools::tags$style(type = "text/css",
                          paste(sep = "\n",
                                "<!--",
                                "div.iframeWrap {",
                                htmltools::css(
                                  position = "relative",
                                  width = "100%",
                                  `padding-top` = "56.25%"
                                ),
                                "}",
                                "div.iframeWrap iframe {",
                                htmltools::css(
                                  position = "absolute",
                                  top = 0,
                                  left = 0,
                                  width = "100%",
                                  height = "100%"
                                ),
                                "}",
                                "-->"
                          )),
    if(grepl(".mp4$", url)) {
      wd_path <- getwd()
      file_path <- paste(wd_path, url, sep = "/")
      htmltools::tags$div(class = "mp4Wrap",
                          htmltools::HTML(paste0("<object data='",
                                                 file_path,
                                                 "' type='video/mp4' width='640' height='480'>\n",
                                                 "<param name='src' value='",
                                                 file_path,
                                                 "'>\n",
                                                 "<param name='autoplay' value='false'>\n",
                                                 "<param name='controller' value='true'>\n",
                                                 "</object>")))
    }else{
      htmltools::tags$div(class = "iframeWrap",
                          htmltools::tags$iframe(src = paste("https://www.youtube.com/embed/", target, sep = ""),
                                                 frameborder="0")
      )
    }
  )

  # return ui
  return(ui)
}


# get youtube page infomation
get_tube_info <- function(url) {
  get_tube_page <- xml2::read_html(url)

  get_tube_title <- rvest::html_node(get_tube_page, "title")
  get_tube_title <- rvest::html_text(get_tube_title)

  get_tube_type <- ifelse(grepl("playlist?", url), "playlist", "single")

  df_tube_info <- data.frame(
    title = get_tube_title,
    type = get_tube_type,
    url = url,
    timestamp = Sys.time(),
    stringsAsFactors = FALSE
  )

  return(df_tube_info)
}

# check tube_info
is_tube_info <- function(tube_info_df) {
  if (is.data.frame(tube_info_df)) {
    all(hasName(tube_info_df, names(tp_history_zero)))
    return(TRUE)
  } else {
    return(FALSE)
  }
}
