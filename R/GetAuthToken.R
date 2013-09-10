#' GetAuthToken
#' 
#' Get an OAuth2.0 authentication token for accessing the Google API
#' 
#' @param appname name of the Google APIs project for authentication
#' @param file optional path and filename where to save the OAuth2.0 token for
#'   reuse
#' @param key optional OAuth key to use or NULL to use the key in the
#'   environment variables for appname
#' @param secret optional OAuth secret to use or NULL to use the secret in the
#'   environment variables for appname
#' @return an OAuth2.0 reference class object to use for httr requests
#' @export
GetAuthToken <- function(appname, scope, file = NULL, key = NULL, secret = NULL) {
  auth <- new_oauth(
    base_url = "https://accounts.google.com/o/oauth2",
    authorize = "auth",
    access = "token",
    scope = paste(scope, collapse = " "),
    appname = appname,
    key = key,
    secret = secret,
    file = file
  )
}
