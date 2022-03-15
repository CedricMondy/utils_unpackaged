#' Get app logs from a dropbox folder
#'
#' @param drop_path
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom purrr walk2
#' @importFrom rdrop2 drop_dir drop_download
#' @importFrom shinylogs read_json_logs
get_logs <- function(drop_path) {
    drop_logs <- rdrop2::drop_dir(drop_path)

    logs_temp <- tempfile()

    dir.create(logs_temp)

    purrr::walk2(
        .x = drop_logs$path_lower,
        .y = drop_logs$name,
        .f = function(x, y){
                rdrop2::drop_download(
                    path = x,
                    local_path = file.path(logs_temp, y)
                )
            }
        )

    logs <- shinylogs::read_json_logs(logs_temp)$session

    unlink(logs_temp, recursive = TRUE)

    logs %>%
        dplyr::mutate(
            connection_time = difftime(server_disconnected, server_connected),
            year = lubridate::year(browser_connected),
            month = lubridate::month(browser_connected)
        )
}

#' Plot the connection log
#'
#' @param logs
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point
plot_logs <- function(logs) {
    # difftime formatting: https://stackoverflow.com/a/53825040
    format_hm <- function(sec) stringr::str_sub(format(sec), end = -4L)

    logs %>%
        ggplot2::ggplot(
            mapping = ggplot2::aes(
                x = browser_connected,
                y = hms::as_hms(connection_time)
            )
        )  +
        ggplot2::geom_segment(
            mapping = ggplot2::aes(
                x = browser_connected,
                xend = browser_connected,
                y = hms::as_hms(0),
                yend = hms::as_hms(connection_time)
            )
        )+
        ggplot2::geom_point() +
        ggplot2::scale_y_time(labels = format_hm) +
        ggplot2::labs(
            x = "Date de connection",
            y = "Temps de connection (hh:mm)"
        )
}
