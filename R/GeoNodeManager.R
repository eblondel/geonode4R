#' GeoNode REST API Manager
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom openssl base64_encode
#' @import httr
#' @import XML
#' @import keyring
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @export
#' @keywords geonode rest api
#' @return Object of \code{\link{R6Class}} with methods for communication with
#' the REST API of a GeoNode instance.
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#' \dontrun{
#'    GeoNodeManager$new("http://localhost:8080", "user", "password")
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
GeoNodeManager <- R6Class("GeoNodeManager",
   lock_objects = FALSE,
   
   private = list(
     keyring_backend = NULL,
     keyring_service = NULL,
     user = NA
   ),
   
   public = list(
     
     #' @field verbose.info if geonode4R logs have to be printed
     verbose.info = FALSE,
     #' @field verbose.debug if curl logs have to be printed
     verbose.debug = FALSE,
     #' @field loggerType the type of logger
     loggerType = NULL,
     
     #'@description Prints a log message
     #'@param type type of log, "INFO", "WARN", "ERROR"
     #'@param text text
     logger = function(type, text){
       if(self$verbose.info){
         cat(sprintf("[geonode4R][%s] %s \n", type, text))
       }
     },
     
     #'@description Prints an INFO log message
     #'@param text text
     INFO = function(text){self$logger("INFO", text)},
     
     #'@description Prints an WARN log message
     #'@param text text
     WARN = function(text){self$logger("WARN", text)},
     
     #'@description Prints an ERROR log message
     #'@param text text
     ERROR = function(text){self$logger("ERROR", text)},
     
     #' @field url the Base url of GeoNode
     url = NA,
     
     #'@description This method is used to instantiate a GeoNodeManager with the \code{url} of the
     #'    GeoNode and credentials to authenticate (\code{user}/\code{pwd}). 
     #'    
     #'    By default, the \code{logger} argument will be set to \code{NULL} (no logger). 
     #'    This argument accepts two possible values: \code{INFO}: to print only geosapi logs,
     #'    \code{DEBUG}: to print geosapi and CURL logs.
     #'    
     #'    The \code{keyring_backend} can be set to use a different backend for storing 
     #'    the GeoNode user password with \pkg{keyring} (Default value is 'env').
     #'@param url url
     #'@param user user
     #'@param pwd pwd
     #'@param logger logger
     #'@param keyring_backend keyring backend. Default is 'env'
     initialize = function(url, user, pwd, logger = NULL,
                           keyring_backend = 'env'){
       
       if(!keyring_backend %in% names(keyring:::known_backends)){
         errMsg <- sprintf("Backend '%s' is not a known keyring backend!", keyring_backend)
         self$ERROR(errMsg)
         stop(errMsg)
       }
       private$keyring_backend <- keyring:::known_backends[[keyring_backend]]$new()
       private$keyring_service <- paste0("geonode4R@", url)
       
       #logger
       if(!missing(logger)){
         if(!is.null(logger)) if(logger == "") logger = NULL
         if(!is.null(logger)){
           self$loggerType <- toupper(logger)
           if(!(self$loggerType %in% c("INFO","DEBUG"))){
             stop(sprintf("Unknown logger type '%s", logger))
           }
           if(self$loggerType == "INFO"){
             self$verbose.info = TRUE
           }else if(self$loggerType == "DEBUG"){
             self$verbose.info = TRUE
             self$verbose.debug = TRUE
           }
         }
       }
       
       #baseUrl
       if(missing(url)) stop("Please provide the GeoNode base URL")
       baseUrl = url
       if(!grepl("/api", baseUrl)){
         if(grepl("/$", baseUrl)){
           baseUrl = paste0(baseUrl, "api/v2")
         }else{
           baseUrl = paste(baseUrl, "api", "v2", sep = "/")
         }
       }
       self$url = baseUrl
       private$user = user
       private$keyring_backend$set_with_value(private$keyring_service, username = user, password = pwd)

       self$connect()
       
       self$version <- GeoNodeVersion$new(baseUrl, user, pwd)
        
       invisible(self)
       
     },
     
     #'@description Get URL
     #'@return the Geoserver URL
     getUrl = function(){
       return(self$url)
     },
     
     #'@description Connects to geoServer
     #'@return \code{TRUE} if connected, raises an error otherwise
     connect = function(){
       req <- GeoNodeUtils$GET(
         url = self$getUrl(), 
         user = private$user, 
         pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user), 
         path = "/",
         verbose = self$verbose.debug
       )
       if(status_code(req) == 401){
         err <- "Impossible to connect to GeoNode: Wrong credentials"
         self$ERROR(err)
         stop(err)
       }
       if(status_code(req) == 404){
         err <- "Impossible to connect to GeoNode: Incorrect URL or GeoNode temporarily unavailable"
         self$ERROR(err)
         stop(err)
       }
       if(status_code(req) != 200){
         err <- sprintf("Impossible to connect to GeoNode: Unexpected error (status code %s)", status_code(req))
         self$ERROR(err)
         stop(err)
       }else{
         self$INFO("Successfully connected to GeoNode!")
       }
       return(TRUE)
     },
     
     #'@description Get execution status
     #'@param execution_id the execution id
     #'@return the status of execution
     getExecutionStatus = function(execution_id){
       path = sprintf("resource-service/execution-status/%s", execution_id)
       req <- GeoNodeUtils$GET(
         url = self$getUrl(),
         user = private$user,
         pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user), 
         path = path,
         verbose = self$verbose.debug
       )
       if(status_code(req) != 200){
         err <- sprintf("Error while getting categories at '%s'", file.path(self$getUrl(), path))
         self$ERROR(err)
         stop(err)
       }
       resp <- httr::content(req)
       out <- resp
       return(out)
     },
     
     #CATEGORIES
     #-------------------------------------------------------------------------------------------------------------
     
     #'@description Get categories
     #'@param raw Controls the output. Default will return an object of class \link{data.frame}.
     #'@return an object of class \link{list}
     getCategories = function(raw = FALSE){
        path <- "categories"
        req <- GeoNodeUtils$GET(
           url = self$getUrl(),
           user = private$user,
           pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user), 
           path = path,
           verbose = self$verbose.debug
        )
        if(status_code(req) != 200){
           err <- sprintf("Error while getting categories at '%s'", file.path(self$getUrl(), path))
           self$ERROR(err)
           stop(err)
        }
        resp <- httr::content(req)
        out <- resp$categories
        if(!raw){
           out <- do.call("rbind", lapply(out, function(x){as.data.frame(x)}))
        }
        return(out)
     },
     
     #'@description Get category
     #'@param id category id
     #'@param raw Controls the output. Default will return an object of class \link{data.frame}.
     #'@return an object of class \link{list}
     getCategory = function(id, raw = FALSE){
        path <- sprintf("categories/%s", id)
        req <- GeoNodeUtils$GET(
           url = self$getUrl(),
           user = private$user,
           pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user), 
           path = path,
           verbose = self$verbose.debug
        )
        if(status_code(req) != 200){
           err <- sprintf("Error while getting category at '%s'", file.path(self$getUrl(), path))
           self$ERROR(err)
           stop(err)
        }
        resp <- httr::content(req)
        out <- resp$categories
        if(!raw){
           out <- do.call("rbind", lapply(out, function(x){as.data.frame(x)}))
        }
        return(out)
     },
     
     #RESOURCES
     #-------------------------------------------------------------------------------------------------------------
    
     #'@description Get resource by UUID
     #'@param uuid resource uuid (or semantic id if used in place of uuid)
     #'@return an object of class \link{list}
     getResourceByUUID = function(uuid){
       out <- NULL
       req = GeoNodeUtils$GET(
         url = self$getUrl(),
         user = private$user,
         pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user),
         path = sprintf("resources?page_size=1&page=1&filter{uuid.icontains}=%s", uuid),
         verbose = self$verbose.debug
       )
       outreq <- httr::content(req)
       if(length(outreq$resources)>0) out <- outreq$resources[[1]]
       return(out)
     },
     
     #'@description Get resource by Alternate
     #'@param alternate resource alternate
     #'@return an object of class \link{list}
     getResourceByAlternate = function(alternate){
       out <- NULL
       req = GeoNodeUtils$GET(
         url = self$getUrl(),
         user = private$user,
         pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user),
         path = sprintf("resources?page_size=1&page=1&filter{alternate}=geonode:%s", alternate),
         verbose = self$verbose.debug
       )
       outreq <- httr::content(req)
       if(length(outreq$resources)>0) out <- outreq$resources[[1]]
       return(out)
     },
          
     #'@description Get resource
     #'@param id resource id
     #'@return an object of class \link{list}
     getResource = function(id){
        req = GeoNodeUtils$GET(
           url = self$getUrl(),
           user = private$user,
           pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user),
           path = sprintf("resources/%s", id),
           verbose = self$verbose.debug
        )
        out <- httr::content(req)
        if(httr::status_code(req)==200){
           out <- out$resource
        }
        return(out)
     },
     
     #'@description Deletes a resource
     #'@param id resource (either a dataset or document) id
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     deleteResource = function(id){
        deleted = FALSE
        req = GeoNodeUtils$DELETE(
           url = self$getUrl(),
           path = sprintf("resources/%s/delete", id),
           user = private$user,
           pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user),
           verbose = self$verbose.debug
        )
        if(httr::status_code(req)==200){
           deleted <- TRUE
        }
        return(deleted)
     },
     
     #UPLOADS
     #-------------------------------------------------------------------------------------------------------------
     
     #'@description Uploads resource files
     #'@param files files
     #'@return an object of class \link{list} giving the upload status
     upload = function(files){
        
        contentList <- lapply(files, httr::upload_file)
        names(contentList) <- sapply(files, function(x){
           prefix = unlist(strsplit(basename(x),"\\."))[2]
           if(prefix %in% c("shp","csv","tif")) prefix = "base"
           nm = paste0(prefix,"_file")
           return(nm)
         })
        
        req <- GeoNodeUtils$POST(
           url = self$getUrl(),
           path = "uploads/upload",
           user = private$user,
           pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user),
           content = contentList,
           contentType = NULL,
           verbose = self$verbose.debug
        )
        
        if(httr::status_code(req)==500){
           err <- sprintf("Error while uploading resources: %", req$errors[[1]])
           self$ERROR(err)
           stop(err)
        }
        
        out <- NULL
        if(httr::status_code(req) %in% c(200,201)){
           out <- httr::content(req)
           async <- "execution_id" %in% names(out)
           if(async){
             exec <- self$getExecutionStatus(execution_id = out$execution_id)
             finished = !is.null(exec$finished)
             while(!finished){
               Sys.sleep(time = 1)
               exec <- self$getExecutionStatus(execution_id = out$execution_id)
               finished = !is.null(exec$finished)
             }
             out <- exec
             out$dataset <- as.integer(basename(exec$output_params$detail_url[[1]]))
           }else{
             out$dataset <- as.integer(basename(out$url))
           }
        }
        return(out)
        
     },
     
     #'@description Uploads ISO 19115 dataset metadata
     #'@param id dataset id
     #'@param file a metadata XML file following ISO 19115 specification
     #'@return an object
     uploadMetadata = function(id, file){
        file_content <- list(metadata_file = httr::upload_file(file))
        req <- GeoNodeUtils$PUT(
           url = self$getUrl(),
           user = private$user,
           pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user),
           path = sprintf("datasets/%s/metadata", id),
           content = file_content, contentType = NULL,
           verbose = self$verbose.debug
        )
        if(httr::status_code(req)==404){
           err <- "Ups, it seems your GeoNode version does not support yet the dataset metadata upload"
           self$ERROR(err)
           stop(err)
        }
        out <- FALSE
        if(httr::status_code(req) %in% c(200,201)){
           out <- TRUE
        }
        return(out)
     },
     
     #DATASETS
     #-------------------------------------------------------------------------------------------------------------
     
     #'@description Get dataset standardized metadata
     #'@param id dataset id
     #'@return an object of class \link{list}
     getDataset = function(id){
        req = GeoNodeUtils$GET(
           url = self$getUrl(),
           user = private$user,
           pwd = private$keyring_backend$get(service = private$keyring_service, username = private$user),
           path = sprintf("datasets/%s", id),
           verbose = self$verbose.debug
        )
        out <- httr::content(req)
        if(httr::status_code(req)==200){
           out <- out$dataset
        }
        return(out)
     }
     
     #DOCUMENTS
     #-------------------------------------------------------------------------------------------------------------
     #TODOS

     
   )
   
)
