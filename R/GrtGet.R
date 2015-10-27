#' GrtGet
#' Returns real-time data for a view (profile).
#' 
#' @export
GrtGet <- function(oauth, ids, metrics = "ga:activeVisitors", dimensions = NULL, filters = NULL, max_results = NULL, sort_by = NULL) {
  response <- GoogleApiRequest(
    req_type = "GET",
    base_url = base_url,
    oauth = oauth,
    query = c(
      paste("ids", ids, sep = "="),
      paste("metrics", paste(metrics, collapse = ","), sep = "="),
      if(!is.null(dimensions)){
        paste("dimensions", paste(dimensions, collapse = ","), sep = "=")
      },
      if(!is.null(filters)){
        paste("filters", paste(filters, collapse = ","), sep = "=")
      },
      if(!is.null(max_results)){
        paste("max-results", max_results, sep = "=")
      },
      if(!is.null(sort_by)){
        paste("sort", paste(sort_by, collapse = ","), sep = "=")
      }
    )
  )
  response.df <- ldply(response$rows)
  col_names <- sapply(response$columnHeaders, function(column) {column$name})
  names(response.df) <- col_names[seq(1, length.out=ncol(response.df))]
  return(response.df)
}
