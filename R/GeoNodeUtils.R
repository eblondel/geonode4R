#' GeoNode REST API Manager Utils
#'
#' @docType class
#' @export
#' @keywords geonode rest api
#' @return Object of \code{\link{R6Class}} with static util methods for communication
#' with the REST API of a GeoNode instance.
#' @format \code{\link{R6Class}} object.
#'
#' @section Static methods:
#' \describe{
#'  \item{\code{getUserAgent()}}{
#'    This method is used to get the user agent for performing GeoNode API requests.
#'    Here the user agent will be compound by geonode4R package name and version.
#'  }
#'  \item{\code{getUserToken(user, pwd)}}{
#'    This method is used to get the user authentication token for performing GeoNode
#'    API requests. Token is given a Base64 encoded string.
#'  }
#'  \item{\code{GET(url, user, pwd, path, verbose)}}{
#'    This method performs a GET request for a given \code{path} to GeoNode REST API
#'  }
#'  \item{\code{PUT(url, user, pwd, path, filename, contentType, verbose)}}{
#'    This method performs a PUT request for a given \code{path} to GeoNode REST API,
#'    to upload a file of name \code{filename} with given \code{contentType}
#'  }
#'  \item{\code{POST(url, user, pwd, path, content, contentType, verbose)}}{
#'    This method performs a POST request for a given \code{path} to GeoNode REST API,
#'    to post content of given \code{contentType}
#'  }
#'  \item{\code{DELETE(url, user, pwd, path, verbose)}}{
#'    This method performs a DELETE request for a given GeoServer resource identified
#'    by a \code{path} in GeoNode REST API
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GeoNodeUtils <- R6Class("GeoNodeUtils")

GeoNodeUtils$getUserAgent <- function(){
  return(paste("geonode4R", packageVersion("geonode4R"), sep="-"))
}

GeoNodeUtils$getUserToken <- function(user, pwd){
  token <- openssl::base64_encode(charToRaw(paste(user, pwd, sep=":")))
  return(token)
}

GeoNodeUtils$GET <- function(url, user, pwd, path = "", contentType = "text/xml", verbose = FALSE){
  if(verbose){
    req <- httr::with_verbose(GeoNodeUtils$GET(url, user, pwd, path))
  }else{
    if(!grepl("^/", path) && path != "") path = paste0("/", path)
    url <- paste0(url, path) 
    req <- httr::GET(
      url = url,
      httr::add_headers(
        "User-Agent" = GeoNodeUtils$getUserAgent(),
        "Authorization" = paste("Basic", GeoNodeUtils$getUserToken(user, pwd)),
        "Content-Type" = contentType
      )
    )
  }
  return(req)
}

GeoNodeUtils$PUT <- function(url, user, pwd, path,
                        content = NULL, filename = NULL,
                        contentType, verbose = FALSE){
  if(verbose){
    req <- httr::with_verbose(GeoNodeUtils$PUT(url, user, pwd, path, content, filename, contentType))
  }else{
    body <- NULL
    if(missing(content) | is.null(content)){
      if(missing(filename) | is.null(filename)){
        stop("The filename must be provided")
      }
      body <- httr::upload_file(filename)
    }else{
      body <- content
    }
    
    if(!grepl("^/", path)) path = paste0("/", path)
    url <- paste0(url, path)
    req <- httr::PUT(
      url = url,
      httr::add_headers(
        "User-Agent" = GeoNodeUtils$getUserAgent(),
        "Authorization" = paste("Basic", GeoNodeUtils$getUserToken(user, pwd)),
        "Content-Type" = contentType
      ),    
      body = body
    )
  }
  return(req)
}

GeoNodeUtils$POST <- function(url, user, pwd, path, content, contentType, verbose = FALSE){
  if(verbose){
    req <- httr::with_verbose(GeoNodeUtils$POST(url, user, pwd, path, content, contentType))
  }else{
    if(!grepl("^/", path)) path = paste0("/", path)
    url <- paste0(url, path)
    req <- httr::POST(
      url = url,
      httr::add_headers(
        "User-Agent" = GeoNodeUtils$getUserAgent(),
        "Authorization" = paste("Basic", GeoNodeUtils$getUserToken(user, pwd)),
        "Content-Type" = contentType
      ),    
      body = content
    )
  }
  return(req)
}

GeoNodeUtils$DELETE <- function(url, user, pwd, path, verbose = FALSE){
  if(verbose){
    req <- httr::with_verbose(GeoNodeUtils$DELETE(url, user, pwd, path))
  }else{
    if(!grepl("^/", path)) path = paste0("/", path)
    url <- paste0(url, path)
    req <- httr::DELETE(
      url = url,
      httr::add_headers(
        "User-Agent" = GeoNodeUtils$getUserAgent(),
        "Authorization" = paste("Basic", GeoNodeUtils$getUserToken(user, pwd))
      )
    )
  }
  return(req)
}
