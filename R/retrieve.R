parseTimestamp <- function(url, tz) {
  timestamp.str <-
    gsub("(http(s)?://web.archive.org/web/)([0-9]{14})(/.*)", "\\3", url)
  timestamp.posix <-
    formatTimestamp(timestamp.str, "GMT")
  attributes(timestamp.posix)$tzone <- tz
  return(timestamp.posix)
}

formatTimestamp <- function(timestamp.str, tz) {
  timestamp.posix <-
    as.POSIXct(timestamp.str, format = "%Y%m%d%H%M%S", tz = "GMT")
  attributes(timestamp.posix)$tzone <- tz
  return(timestamp.posix)
}


retrieve <-
  function(webpage = NULL, from = NULL, to = NULL, by = NULL, tz = NULL, filename = NULL,
           archive = NULL,
           verbose = TRUE) {

  start_from <- 1

  if (is.null(archive)) {
    webpage <- gsub("^http(s)?://www.", "", webpage)

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

    this_timestamp <- sequence[i]
    attributes(this_timestamp)$tzone <- "GMT"

    this_url <-
      sprintf("http://archive.org/wayback/available?url=%s&timestamp=%s",
              webpage,
              format(this_timestamp, "%Y%m%d%H%M%S"))

    if (verbose) {
      print(this_url)
    }

    this_res <-
      jsonlite::fromJSON(this_url)

    if (this_res$archived_snapshots$closest$available) {
      print(paste0("Searched: ", formatTimestamp(this_res$timestamp, tz = "GMT")))
      print(paste0("Found:    ", parseTimestamp(this_res$archived_snapshots$closest$url, "GMT")))
      print("")
    }

    if (this_res$archived_snapshots$closest$available) {
      this_res$html <-
        xml2::download_html(this_res$archived_snapshots$closest$url)
    } else {
      this_res$html <-
        NA
    }

    archive$res[[i]] <- this_res

    save(archive, file = filename)

  }
}
