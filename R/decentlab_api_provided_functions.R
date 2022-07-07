# From https://docs.decentlab.com/data-access-guide/v5/api/r-ref.html
# Slightly modified to drop some older conventions

# MIT License
#
# Copyright (c) 2016 Decentlab GmbH
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

.getProxy <- function(domain, apiKey, database = "main", type = "local") {
  baseUrl <- paste0("https://",
                    domain,
                    "/api/datasources/id/",
                    database)
  res <- httr::GET(baseUrl,
                   httr::add_headers(Authorization = paste0("Bearer ", apiKey)))
  json <- httr::content(res)
  json$id
}

.getTitle <- function(tags) {
  unit <- ifelse(is.null(tags["unit"]), "", paste0(" [",  tags["unit"], "]"))
  paste0(tags["sensor"], unit)
}


query <- function(domain,
                  apiKey,
                  timeFilter = "",
                  device = "//",
                  location = "//",
                  sensor = "//",
                  uqk = "//",
                  includeNetworkSensors = FALSE,
                  channel = "//",
                  aggFunc = "",
                  aggInterval = "",
                  doCast = TRUE,
                  castAggFunc = mean,
                  convertTimestamp = TRUE,
                  database = "main",
                  timezone = "UTC",
                  type = "local") {
  selectVar <- 'value'
  fill <- ''
  interval <- ''
  baseUrl <- paste0(
    "https://",
    domain,
    "/api/datasources/proxy/",
    .getProxy(domain, apiKey, database),
    "/query?db=",
    database,
    "&epoch=ms&q="
  )
  
  if (aggFunc != "") {
    selectVar <- paste0(aggFunc, '("value") as value')
    fill <- 'fill(null)'
  }
  
  if (aggInterval != "") {
    interval <- paste0(', time(', aggInterval, ')')
  }
  
  if (timeFilter != "") {
    timeFilter <- paste0(' AND ', timeFilter)
  }
  
  filter <- paste0(" location =~ ", location,
                   " AND node =~ ", device,
                   " AND sensor =~ ", sensor,
                   " AND uqk =~ ", uqk,
                   " AND ((channel =~ ", channel, " OR channel !~ /.+/)",
                   if (includeNetworkSensors) ")" else " AND channel !~ /^link-/)")
  
  q <- paste(
    'SELECT ',
    selectVar,
    ' FROM "measurements" ',
    ' WHERE ', filter, timeFilter,
    ' GROUP BY channel,node,sensor,unit,uqk ',
    interval,
    fill,
    sep = ' '
  )
  
  res <- httr::GET(paste0(baseUrl, utils::URLencode(q)),
                   httr::add_headers(Authorization = paste0("Bearer ", apiKey)))
  json <- httr::content(res)
  
  timeseries <- json$results[[1]]$series
  
  if (is.null(timeseries)) {
    stop("No series returned: ", json)
  }
  tags <- sapply(timeseries, function(s) {
    t <- list()
    t[s$tags$uqk] <- list(s$tags)
    return (t)
  })
  series_lists <- lapply(timeseries, function(s) {
    tbl <- do.call(rbind, lapply(s$values, rbind))
    cbind(tbl, rep(s$tags$uqk, nrow(tbl)))
  })
  
  mat <- do.call(rbind, series_lists)  # stack groups in rows
  # replace nulls with NAs, otherwise unlist will remove nulls
  mat[sapply(mat, is.null)] <- NA
  
  time <- unlist(mat[, 1])
  value <- unlist(mat[, 2])
  series <- unlist(mat[, 3])
  df <- cbind.data.frame(time, series, value)
  # if (doCast) {
  #   require(data.table)
  #   dt <- dcast.data.table(as.data.table(df),
  #                          time ~ series,
  #                          fun.aggregate = castAggFunc,
  #                          value.var = "value")
  #   df <- as.data.frame(dt)
  # }
  if (convertTimestamp) {
    df$time <- as.POSIXct(df$time / 1000,
                          origin = "1970-01-01",
                          tz = timezone)
  }
  attr(df, "tags") <- tags
  return(df)
}


getSeriesUqk <- function(domain,
                         apiKey,
                         timeFilter = "",
                         device = "//",
                         location = "//",
                         sensor = "//",
                         uqk = "//",
                         includeNetworkSensors = FALSE,
                         channel = "//",
                         database = "main") {
  baseUrl <- paste0(
    "https://",
    domain,
    "/api/datasources/proxy/",
    .getProxy(domain, apiKey, database),
    "/query?db=",
    database,
    "&epoch=ms&q="
  )
  
  filter <- paste0(" location =~ ", location,
                   " AND node =~ ", device,
                   " AND sensor =~ ", sensor,
                   " AND uqk =~ ", uqk,
                   " AND ((channel =~ ", channel, " OR channel !~ /.+/)",
                   if (includeNetworkSensors) ")" else " AND channel !~ /^link-/)")
  
  q <- paste(
    'SHOW TAG VALUES',
    'FROM "measurements"',
    'WITH KEY = uqk',
    'WHERE', filter,
    sep = ' '
  )
  
  res <- httr::GET(paste0(baseUrl, utils::URLencode(q)),
                   httr::add_headers(Authorization = paste0("Bearer ", apiKey)))
  json <- httr::content(res)
  
  timeseries <- json$results[[1]]$series
  
  if (is.null(timeseries)) {
    stop("No series returned: ", json)
  }
  series_lists <- lapply(timeseries, function(s) {
    tbl <- do.call(rbind, lapply(s$values, rbind))
    cbind(tbl, rep(s$tags$uqk, nrow(tbl)))
  })
  
  mat <- do.call(rbind, series_lists)  # stack groups in rows
  # replace nulls with NAs, otherwise unlist will remove nulls
  mat[sapply(mat, is.null)] <- NA
  
  series <- unlist(mat[, 1])
  value <- unlist(mat[, 2])
  df <- cbind.data.frame(series, value)
  return(df)
}


store <- function(domain,
                  apiKey,
                  dataFrame,
                  didCast = TRUE,
                  convertTimestamp = TRUE,
                  database = "processed",
                  deviceSuffix = "",
                  type = "local") {
  tags <- attr(dataFrame, "tags", exact = TRUE)
  if (is.null(tags)) {
    stop('`tags` attribute of dataFrame is not set. Set it from the original data frame using `attr(dataFrame, "tags") <- attr(originalDataFrame, "tags")`')
  }
  
  if (didCast) {
    # require(data.table)
    # dataFrame <- as.data.frame(melt.data.table(as.data.table(dataFrame),
    #                                            id.vars='time',
    #                                            variable.name='series'))
  }
  if (convertTimestamp) {
    dataFrame$time <- as.numeric(dataFrame$time) * 1000
  }
  dataFrame <- dataFrame[!is.na(dataFrame$value), ]
  
  taggings <-lapply(tags, function(serie) {
    tagz <- lapply(names(serie), function(tag) {
      if (tag != "uqk" && !is.null(serie[tag]) && serie[tag] != "") {
        value <- gsub("([,= \\])", "\\\\\\1", serie[tag])
        paste(tag, if (tag == "node") paste0(value, deviceSuffix) else value, sep = "=")
      }
    })
    tagz <- Filter(Negate(is.null), tagz)
    uqk <- paste0(serie["node"], deviceSuffix, ".",
                  paste(Filter(function(t) !is.null(t) && t != "", serie[c("sensor", "channel")]), collapse = "."))
    title <- .getTitle(serie)
    tagsStr <- paste0(paste(tagz, collapse = ","), ",uqk=", uqk, ",title=", gsub("([,= \\])", "\\\\\\1", title))
  })
  
  lines <- by(dataFrame, 1:nrow(dataFrame), function(r) {
    sprintf("measurements,%s value=%s %.0f000000", taggings[as.character(r$series)], r$value, r$time)
  })
  
  baseUrl <- paste0(
    "https://",
    domain,
    "/api/datasources/proxy/",
    .getProxy(domain, apiKey, database),
    "/write?db=",
    database
  )
  
  res <- httr::POST(url = baseUrl,
                    body = paste(lines, collapse = "\n"),
                    config = httr::add_headers(Authorization = paste0("Bearer ", apiKey),
                                               'Content-Type' = "text/plain"))
  if (httr::status_code(res) != 204) {
    stop("HTTP POST error: ", httr::content(res))
  }
  
}

getLast <- function(domain,
                    apiKey,
                    timeFilter = "",
                    device = "//",
                    location = "//",
                    sensor = "//",
                    includeNetworkSensors = FALSE,
                    channel = "//",
                    convertTimestamp = TRUE,
                    timezone = "UTC") {
  doCast <- FALSE
  aggFunc <- "last"
  selectVar <- 'value'
  fill <- ''
  interval <- ''
  baseUrl <- paste0(
    "https://",
    domain,
    "/api/datasources/proxy/1/query?db=main&epoch=ms&q="
  )
  
  if (timeFilter != "") {
    timeFilter <- paste0(' AND ', timeFilter)
  }
  
  if (aggFunc != "") {
    selectVar <- paste0(aggFunc, '("value") as value')
    fill <- 'fill(null)'
  }
  
  
  filter <- paste0(" location =~ ", location,
                   " AND node =~ ", device,
                   " AND sensor =~ ", sensor,
                   " AND ((channel =~ ", channel, " OR channel !~ /.+/)",
                   if (includeNetworkSensors) ")" else " AND channel !~ /^link-/)")
  
  q <- paste(
    'SELECT ',
    selectVar,
    ' FROM "measurements" ',
    ' WHERE ', filter, timeFilter,
    ' GROUP BY "uqk", "location", "sensor", "channel", "unit" ',
    interval,
    fill,
    sep = ' '
  )
  
  res <- httr::GET(paste0(baseUrl, utils::URLencode(q)),
                   httr::add_headers(Authorization = paste0("Bearer ", apiKey)))
  json <- httr::content(res)
  
  if (res$status_code != 200) {
    stop(json$message, json$error)
  }
  series <- json$results[[1]]$series
  
  if (is.null(series)) {
    stop("No series returned")
  }
  
  lists <- lapply(series, function(s) {
    tbl <- do.call(rbind, lapply(s$values, rbind))
    i <- 1
    for (tag in s$tags) {
      tagcol <- rep(tag, dim(tbl)[1])
      tbl <- cbind(tbl, tagcol)
      colnames(tbl)[dim(tbl)[2]] <- names(s$tags)[i]
      i <- i + 1
    }
    tbl
  })
  
  mat <- do.call(rbind, lists)  # stack groups in rows
  # replace nulls with NAs, otherwise unlist will remove nulls
  mat[sapply(mat, is.null)] <- NA
  df <- data.frame(time = unlist(mat[, 1]), value = unlist(mat[, 2]),
                   apply(mat[, 3:dim(mat)[2]], 2, unlist))
  
  if (convertTimestamp) {
    df$time <- as.POSIXct(df$time / 1000,
                          origin = "1970-01-01",
                          tz = timezone)
  }
  
  return(df)
  
}
