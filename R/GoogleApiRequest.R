#' GoogleApiRequest
#' 
#' Perform an Google API request and return the results
#' 
#' Construct and perform an HTTP request to the selected Google API. Transform
#' the JSON output into an R list.
#' 
#' @param req_type Character string specifying the type of http request to
#'   perform: "GET", "POST", "PUT", or "DELETE".
#' @param oauth The httr OAuth2.0 token reference class object to use for
#'   authentication.
#' @param body_list optional An R list object containing the name value pairs
#'   for the body of the request, where applicable. This list is coverted into
#'   JSON by the GpApiRequest function.
GoogleApiRequest <- function(req_type, base_url, oauth, rel_path = NULL, query = NULL, body_list = NULL) {
  stopifnot(
    req_type %in% c("GET", "POST", "PUT", "DELETE")
  )
  url <- paste(
    paste(
      c(base_url, rel_path),
      collapse = "/"
    ),
    paste(query, collapse = "&"),
    sep = "?"
  )
  config <- add_headers(
    Authorization = paste("Bearer", oauth$getAccessToken()$access_token),
    `Content-Type` = "application/json"
  )
  args <- list(url = url, config = config)
  if(is.null(body_list)) {
    body <- NULL
  } else {
    body <- toJSON(
      body_list,
      pretty = TRUE, asIs = TRUE
    )
    args <- c(args, list(body = body))
  }
  response <- do.call(req_type, args)
  content(response)
}
