\name{GoogleApiRequest}
\alias{GoogleApiRequest}
\title{GoogleApiRequest}
\usage{
  GoogleApiRequest(req_type, base_url, oauth,
    rel_path = NULL, query = NULL, body_list = NULL)
}
\arguments{
  \item{req_type}{Character string specifying the type of
  http request to perform: "GET", "POST", "PUT", or
  "DELETE".}

  \item{oauth}{The httr OAuth2.0 token reference class
  object to use for authentication.}

  \item{body_list}{optional An R list object containing the
  name value pairs for the body of the request, where
  applicable. This list is coverted into JSON by the
  GpApiRequest function.}
}
\description{
  Perform an Google API request and return the results
}
\details{
  Construct and perform an HTTP request to the selected
  Google API. Transform the JSON output into an R list.
}

