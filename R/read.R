#Date substing
parse_date <- function(date){
  return(gsub("-","",date))
}

#'@title retrieve a vector of sampled log files
#'@description Grab sampled log files to be piped into
#'\code{\link{read_sampled_log}}. By default this retrieves all
#'sampled log files; it can be used to retrieve a particular date range of
#'files through the "earliest" and "latest" arguments.
#'
#'@param earliest a "Date" object. Set to NULL by default, which triggers
#'the retrieval of all log file names.
#'
#'@param latest a "Date" object; set to NULL by default. In the event that
#'\code{earliest} is set but \code{latest} is not, the files retrieved
#'will span from \code{earliest} to the current date; in the event that
#'both arguments are set, the retrieved files will be those in that range.
#'
#'@return A vector of filenames that can be passed into \code{\link{read_sampled_log}}.
#'@export
get_files <- function(earliest = NULL, latest = NULL){
  files <- list.files("/a/squid/archive/sampled", full.names= TRUE, pattern = "gz$")
  if(!is.null(earliest)){
    file_dates <- as.numeric(substring(files,47,55))
    if(!is.null(latest)){
      files <- files[file_dates >= as.numeric(parse_date(earliest)) & file_dates <= as.numeric(parse_date(latest))]
    } else {
      files <- files[file_dates >= as.numeric(parse_date(earliest))]
    }
  }
  return(files)
}

#'@title read a sampled log file
#'@description read a sampled log file identified with \code{\link{get_files}}.
#'The sampled logs are returned as a data.frame with 16 columns - see
#'the "Value" documentation.
#'
#'@param file a filename, retrieved with \code{\link{get_files}}
#'
#'@return a data.frame containing 16 columns - "squid", "sequence_no", "timestamp",
#'"servicetime", "ip_address", "status_code", "reply_size", "request_method",
#'"url", "squid_status", "mime_type", "referer", "x_forwarded", "user_agent",
#'"lang" and "x_analytics".
#'
#'@seealso \code{\link{to_pageviews}}, for filtering the output
#'@importFrom urltools url_decode
#'@export
read_sampled_log <- function(file){
  output_file <- tempfile()
  system(paste("gunzip -c", file, ">", output_file))
  data <- read.delim(output_file, as.is = TRUE, quote = "",
                     col.names = c("squid","sequence_no",
                                   "timestamp", "servicetime",
                                   "ip_address", "status_code",
                                   "reply_size", "request_method",
                                   "url", "squid_status",
                                   "mime_type", "referer",
                                   "x_forwarded", "user_agent",
                                   "lang", "x_analytics"))
  data$url <- url_decode(data$url)
  data$referer <- url_decode(data$referer)
  file.remove(output_file)
  return(data)
}

#'@export
convert_timestamps <- function(x){
  x <- iconv(x, to = "UTF-8")
  x[nchar(x) > 19] <- substring(x[nchar(x) > 19],1,19)
  return(strptime(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}