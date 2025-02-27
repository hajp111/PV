my_check_date <- function(input_date) {
  if (inherits(input_date, "Date")) {
    # Input is already a Date object
    return(format(input_date, "%Y-%m-%d"))
  } else if (is.character(input_date)) {
    # Input is a character string, try to convert it
    tryCatch({
      date_obj <- lubridate::ymd(input_date)
      if (is.na(date_obj)) {
        stop(paste("Input is not a valid date or string in YYYY-MM-DD format"))
      } else {return(format(date_obj, "%Y-%m-%d"))}
    }, error = function(e) {
      stop(paste("Invalid date format:", input_date))
      return(NULL)
    })
  } else {
    # Input is neither a Date object nor a character string
    stop(paste("Input is not a valid date or string:", input_date))
    return(NULL)
  }
}#endfunction my_check_date

ConvertTextToDate <- function(input_date) {
  if (inherits(input_date, "Date")) {
    # Input is already a Date object
    return(input_date)
  } else if (is.character(input_date)) {
    # Input is a character string, try to convert it
    tryCatch({
      date_obj <- lubridate::ymd(input_date)
      if (is.na(date_obj)) {
        stop(paste("Input is not a valid date or string in YYYY-MM-DD format"))
      } else {return(date_obj)}
    }, error = function(e) {
      stop(paste("Invalid date format:", input_date))
      return(NULL)
    })
  } else {
    # Input is neither a Date object nor a character string
    stop(paste("Input is not a valid date or string:", input_date))
    return(NULL)
  }
}#endfunction ConvertTextToDate

my_ggsave <- function(filename, plot = last_plot()
                      , width = 160
                      , height = 80
                      , units = "mm"
                      , dpi = 300, bg = "white", ...) {
  ggsave(filename, plot = plot, width = width, height = height
         , units = units, dpi = dpi, bg = bg, device = "png", create.dir = TRUE, ...) # Device set to "png"
}#endfunction my_ggsave
