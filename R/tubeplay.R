#' Play YouTube moview at RStudio
#'
#' @param url url that you want show Toutube movie
#' @param viewer set viewer. See ?htmltools::html_print .
#'
#' @examples
#' tubeplay() # play default set movie
#' tubeplay("https://www.youtube.com/watch?v=0E00Zuayv9Q") # play "PPAP"
#' @export
tubeplay <- function(url = "https://www.youtube.com/watch?v=iOFZKwv_LfA",
                     viewer = getOption("viewer", utils::browseURL)) {

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
  htmltools::html_print(ui)
}

