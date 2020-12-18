#' Parse timestamp from the Wayback Machine URLs
#'
#' @param url The Wayback Machine URL.
#' @param tz The timezone, passed to \code{format.POSIXct}.
parseTimestamp <- function(url, tz) {
  timestamp.str <-
    gsub("(http(s)?://web.archive.org/web/)([0-9]{14})(/.*)", "\\3", url)
  timestamp.posix <-
    formatTimestamp(timestamp.str, "GMT")
  attributes(timestamp.posix)$tzone <- tz
  return(timestamp.posix)
}

#' Format timestamp from the Internet Archive's Wayback Machine URLs
#'
#' @param timestamp.str A character vector, with 14 digits.
#' @param tz The timezone, passed to `format.POSIXct`.
formatTimestamp <- function(timestamp.str, tz) {
  timestamp.posix <-
    as.POSIXct(timestamp.str, format = "%Y%m%d%H%M%S", tz = "GMT")
  attributes(timestamp.posix)$tzone <- tz
  return(timestamp.posix)
}

#' Retrieve a series of pages from the Wayback Machine and parse their content.
#'
#' `retrieve()` saves a list containing the text contained in a selected number of tags.
#'
#' @param webpage The original address of the page to parse.
#' @param from A character indicating when to start the series, with format `YYYY-mm-dd HH:MM:SS`.
#' @param to A character indicating when to end the series, with format `YYYY-mm-dd HH:MM:SS`.
#' @param by A character indicating how to increment the sequence,
#'   one of `hour`, `day`, `DSTday`, `week`, `month`, `quarter` or `year`.
#' @param tz The timezone, passed to `format.POSIXct`.
#' @param archive The filaname (with extension) used to save the archive,
#'   it will overwrite without warning!.
#' @param archive An uncompleted archive list, not tested yet!
#' @param verbose Logical, do you want to print information about the requests to the Wayback Machine?
#' @return `retrieve()` doesn't return anything. It saves the archive list in the working directory.
#'   The list details about the requests (including the sequence) in `attr` and the parsed text in `res`.
#' @examples
#' retrieve(webpage = 'corriere.it',
#' from = "2010-01-01 00:00:00", to = "2010-01-05 00:00:00",
#' by = 'day', tz = 'CET', filename = 'corriere.RData')
retrieve <-
  function(webpage = NULL,
           from = NULL, to = NULL, by = NULL, tz = NULL,
           filename = NULL,
           archive = NULL,
           verbose = TRUE) {

    start_from <- 1

    if (is.null(archive)) {
      # webpage <- gsub("^http(s)?://www.", "", webpage)

      if (!by %in% c("hour", "day", "DSTday", "week", "month", "quarter", "year")) {
        stop('`By` must be a character string containing containing one of
           "hour", "day", "DSTday", "week", "month", "quarter" or "year"')
      }

      sequence <-
        seq(from = as.POSIXct(from, tz = tz),
            to = as.POSIXct(to, tz = tz),
            by = by)

      archive <- list()
      archive$attr <-
        list('webpage' = webpage, 'sequence' = sequence,
             'filename' = filename, 'start_from' = start_from)
      archive$res <- list()

    } else {
      webpage <- archive$attr$webpage
      sequence <- archive$attr$sequence
      filename <- archive$attr$filename
      start_from <- archive$attr$start_from
    }

    for (i in start_from:length(sequence)) {

      Sys.sleep(5)

      this_timestamp <- sequence[i]
      this_label <- paste0("Failed on ", this_timestamp)
      attributes(this_timestamp)$tzone <- "GMT"

      this_url <-
        sprintf("http://archive.org/wayback/available?url=%s&timestamp=%s",
                webpage,
                format(this_timestamp, "%Y%m%d%H%M%S"))

      if (verbose) {
        print(this_url)
      }

      repeat{
        this_res <-
          try({jsonlite::fromJSON(this_url)})
        if(length(this_res$archived_snapshots$closest$available) == 0) {
          if(verbose) {print("Wayback Machine API unresponsive. Retrying....")}
          Sys.sleep(5)
        } else {
          break
        }
      }

      if (this_res$archived_snapshots$closest$available) {
        print(paste0("Searched: ", formatTimestamp(this_res$timestamp, tz = "GMT")))
        print(paste0("Found:    ", parseTimestamp(this_res$archived_snapshots$closest$url, "GMT")))
        print("")

        this_label <- parseTimestamp(this_res$archived_snapshots$closest$url, "GMT")
        attributes(this_label)$tzone <- tz
        this_label <- format(this_label, "%Y%m%d%H%M%S")
        this_list <- list()

        if (this_res$archived_snapshots$closest$available) {

          repeat{
            this_list[['archive-url']] <-
              this_res$archived_snapshots$closest$url
            this_html <-
              try({xml2::read_html(this_list[['archive-url']], verbose = T)})

            if (class(this_html[1]) != 'try-error') {
              break
            } else {
              if (verbose) {print("Error reading html source. Retrying...")}
              Sys.sleep(5)
            }
          }

          # h[1-7]
          for(this_tag in c(paste0("h",1:6))) {
            this_list[[this_tag]] <-
              try({this_html %>%
                  rvest::html_nodes(this_tag) %>%
                  stringr::str_squish() %>%
                  stringi::stri_remove_empty_na()})
          }

          # p
          this_list[['p']] <-
            try({this_html %>%
                rvest::html_nodes('p') %>%
                stringr::str_squish() %>%
                stringi::stri_remove_empty_na()})

          # a
          this_list[['a']] <-
            try({this_html %>%
                rvest::html_nodes('a') %>%
                stringr::str_squish() %>%
                stringi::stri_remove_empty_na()})

          # figure
          this_list[['figure']] <-
            try({this_html %>%
                rvest::html_nodes('figure') %>%
                stringr::str_squish() %>%
                stringi::stri_remove_empty_na()})

          # figcaption
          this_list[['figcaption']] <-
            try({this_html %>%
                rvest::html_nodes('figcaption') %>%
                stringr::str_squish() %>%
                stringi::stri_remove_empty_na()})

          # img alt
          this_list[['img-alt']] <-
            try({this_html %>%
                rvest::html_nodes('img') %>%
                rvest::html_attr('alt') %>%
                stringr::str_squish() %>%
                stringi::stri_remove_empty_na()})

        }

      }

      archive$attr$start_from <- i
      archive$res[[this_label]] <- this_list

      save(archive, file = filename)

    }
  }
