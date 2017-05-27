#' Play YouTube moview at RStudioServer
#'
#' @param url url that you want show Toutube movie
#' @param width ignored.
#' @param height ignored.
#' @param background ignored.
#' @param viewer set viewer. See ?htmltools::html_print .
#'
#' @examples
#' tubeplay() # play default set movie
#' tubeplay("https://www.youtube.com/watch?v=0E00Zuayv9Q") # play "PPAP"
#' @export
tubeplay <- function(url = "https://www.youtube.com/watch?v=iOFZKwv_LfA", width = "100%", height = "100%",
                     background = "white", viewer = getOption("viewer", utils::browseURL)) {

  # judge single or list
  if(grepl("playlist?", url)) {
    # set target for list
    target <- gsub("^.*\\.com/", "", url)
    target <- gsub("playlist", "videoseries", target)
  }else{
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
    htmltools::tags$div(class = "iframeWrap",
                        htmltools::tags$iframe(width = width,
                                               height = height,
                                               src = paste("https://www.youtube.com/embed", target, sep = "/"),
                                               frameborder="0")
    )
  )
  htmltools::html_print(ui)
}

