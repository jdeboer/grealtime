\name{GetAuthToken}
\alias{GetAuthToken}
\title{GetAuthToken}
\usage{
  GetAuthToken(appname, scope, file = NULL, key = NULL,
    secret = NULL)
}
\arguments{
  \item{appname}{name of the Google APIs project for
  authentication}

  \item{file}{optional path and filename where to save the
  OAuth2.0 token for reuse}

  \item{key}{optional OAuth key to use or NULL to use the
  key in the environment variables for appname}

  \item{secret}{optional OAuth secret to use or NULL to use
  the secret in the environment variables for appname}
}
\value{
  an OAuth2.0 reference class object to use for httr
  requests
}
\description{
  Get an OAuth2.0 authentication token for accessing the
  Google API
}

