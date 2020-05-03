pacman::p_load(httr,jsonlite,getPass,tcltk,curl,RCurl,stringr)

# --- get.linkedin.access.token ---
# Linked in learning API keys are valid for several months at a time, 
# so if we have one we don't need get a new one
#
# If a file path is not passed we prompt for a file or the secrets
# The secrets file is a R file that wil be sourced that contains a named character vector like:
#apiClient <- c(
#	ID = "some...",
#	Secret = "value..."
#)
# or an .RDS file containing just that vector created with
# saveRDS(apiClient,apiClientIdSecretPath)  
#
# If you use a secrets file or include the apiKeyRdsPath a API Key file will be added to the 
# parent folder for later re-use of the valid API key by this function

get.linkedin.access.token <- function(
	apiClientIdSecretPath="",
	apiKeyRdsPath=""
){
  .getApiKeyIfValid <- function(apiKeyRdsPath){
	if(
		file.exists(apiKeyRdsPath) && 
		tolower(tools::file_ext(apiKeyRdsPath)) == "rds"
 	){
		token <- readRDS(apiKeyRdsPath)
		#Determin if the token has expired or will within a day
		if (
			as.numeric(unname(token["expiresIn"]))>
			(as.numeric(Sys.time())+(60*60*24))
		){
			return(token)
		} else {
			return(NULL)
		}
	} else {
		return(NULL)
	}  
  }
  potentialToken <- .getApiKeyIfValid(apiKeyRdsPath)
  if(is.null(potentialToken)){
    #internal helper function to manage the secrets file
    .readSecretsFile <- function(
		apiClientIdSecretPath="",
		defaultFilename=basename(apiClientIdSecretPath)
    ){
	apiClient <- NULL
	if(!file.exists(apiClientIdSecretPath)){
		Filters <- matrix(
			c(
				"RDS file", ".RDS",
				"RDS file", ".rds", 
				"R code", ".R",
				"R code", ".r",
				"R code", ".s",
				"All files", "*"
			),
	            6, 2, byrow = TRUE
		)
		apiClientIdSecretPath <- tcltk::tk_choose.files(
			default=defaultFilename,
			caption=paste0("Select ",selectKeyMethodTwo),
			multi = FALSE, filters=Filters
		)
	}
	pathExt <- tolower(tools::file_ext(apiClientIdSecretPath))
	if(pathExt == "r") {
		source(apiClientIdSecretPath)	
	} else if(pathExt == "rds"){
		apiClient <- readRDS(apiClientIdSecretPath)
	}
	return(apiClient)
	rm("apiClient")
    }
	# Documentation found at:
	# https://docs.microsoft.com/en-us/linkedin/shared/authentication/client-credentials-flow?context=linkedin/context
	# ---
	# set some default values
	if(curl::has_internet()){
		fCancel <- FALSE
		strCancelMsg <- ""
	} else {
		fCancel <- TRUE
		strCancelMsg <- "no internet connection;"
	}	
	selectKeyMethodOne <- " By entering values in a prompt "
	selectKeyMethodTwo <- paste0(" By selecting the secrets file (.R or .RDS) ")
	if((!fCancel) && (!file.exists(apiClientIdSecretPath))){
		selectedKeyMethod <- NULL
		selectedKeyMethod <- tcltk::tk_select.list(
			c(
				selectKeyMethodOne,
				selectKeyMethodTwo,
				"","",""
			), 
			title = paste0(
				"\n\tSelect one of the following methods to \t\n",
				"\tsupply the application ID and Secret \t\n"
			)
		)
		if(selectedKeyMethod == selectKeyMethodOne){
			strApiClientID <- getPass::getPass(
				"Enter the API Client ID", 
				noblank = TRUE, 
				forcemask = TRUE
			)			
			strApiClientSecret <- getPass::getPass(
				"Enter the API Client Secret", 
				noblank = TRUE, 
				forcemask = TRUE
			)
		} else if(selectedKeyMethod == selectKeyMethodTwo){
			apiClient <- .readSecretsFile(apiClientIdSecretPath)
			strApiClientID <- unname(apiClient["ID"])
			strApiClientSecret <- unname(apiClient["Secret"])
		} else {
			fCancel <- TRUE
			strCancelMsg <- paste0(strCancelMsg,"user aborted secret selection method;")
		}
	} else if(!fCancel){
			apiClient <- .readSecretsFile(apiClientIdSecretPath)
			strApiClientID <- unname(apiClient["ID"])
			strApiClientSecret <- unname(apiClient["Secret"])
	}	
	if (
		(!fCancel) && 
		exists("strApiClientID") && 
		exists("strApiClientSecret")
	){
		# ---	
		# Negotiate the API Key using this post method
		#	POST /oauth/v2/accessToken HTTP/1.1
		# Host: www.linkedin.com
		# Content-Type: application/x-www-form-urlencoded
		# grant_type=client_credentials&client_id={your_client_id}&client_secret={your_client_secret}
		# ---	
		# A bash curl example 
		#`curl \
		#`-X POST https://www.linkedin.com/oauth/v2/accessToken  \
		#`-d 'grant_type=client_credentials&client_id=78br9a25210ukc&client_secret=OzZvS...' \
		#`-H 'Content-Type: application/x-www-form-urlencoded'
		# apiResponse should be JSON in the form of:
		# {
		#     "access_token": "AQV8...",
		#     "expires_in": "1800"
		# }
		# ---
		# using httr::POST, alternativly we could use ?RCurl::postForm
		# may or may not need to use set_config
		# [TODO] Currently can't get https over proxy working
		# handle proxy if needed
		https_method <- handleProxy()

		# [TODO] use a working https_method to get API Key, and API Queries

		# build form data
		oauth_params <-  as.list(c(
			grant_type="client_credentials",
			client_id=strApiClientID,
			client_secret=strApiClientSecret
		))
		# add headers
		oauth_header <- "application/x-www-form-urlencoded"
		names(oauth_header)<- "Content-Type"
		httr::add_headers(oauth_header)
		oAuthResponse <- httr::POST(
			url="https://www.linkedin.com/oauth/v2/accessToken",
			body=oauth_params,
			encode="form"
		)
		if (httr::http_type(oAuthResponse) == "application/json"){
			token <- c(
				accessToken=
					httr::content(oAuthResponse)$access_token,
				expiresIn=
					Sys.time() + as.numeric(httr::content(oAuthResponse)$expires_in)
			)
			if(dir.exists(dirname(apiKeyRdsPath))){
				if(file.exists(apiKeyRdsPath)){file.remove(apiKeyRdsPath)}
				saveRDS(token,apiKeyRdsPath)
			}
			return(unname(token["accessToken"]))
		} else {
			strCancelMsg <- paste0(strCancelMsg,"unexpected response from Linkedin:http_type not json")
		}
	} else {
		fCancel <- TRUE
		if(strCancelMsg == "") {
			strCancelMsg <- "missing Api Client ID and Secrets;"
		}
	}
	if (fCancel){
		tcltk::tkmessageBox(
			message=paste0("\nError: ",strCancelMsg ,"\n "),
			title="Get Linkein API Access Token"
		)
		return(NULL)
	}
  } else { 
	# potentialToken is not expired so let's use it
	# if we get an error on first use that the token is invalid then
	# we should delete the apiToken file and call this function again
	return(unname(potentialToken["accessToken"]))
  }
}

handleProxy <- function(){
	currentProxyInfo <- curl::ie_proxy_info()
	httpTestUrl <- "http://httpbin.org/get"
	httpsTestUrl <- "https://httpbin.org/get"
	if(!is.null(currentProxyInfo$Proxy)){
		useProxy <- TRUE

		#[TODO] if needed use setx to set windows user environment variables?
		Sys.setenv(http_proxy = httpProxy)
		Sys.setenv(https_proxy = httpsProxy)
		Sys.setenv(ALL_PROXY = httpProxyAll )
		Sys.setenv(NO_PROXY = currentProxyInfo$ProxyBypass)
		Sys.setenv(CURL_SSL_BACKEND="openssl")

		# a function to work behind a VPN...
		# See: https://cran.r-project.org/web/packages/curl/vignettes/windows.html
		ieProxies <- strsplit(currentProxyInfo$Proxy,";")
      		# find the list element that contains https or http 
		#[ASSUMPTION] currently only works if each http and https only has one value
		httpsElementNumber <- unlist(lapply(ieProxies, function(ch) grep("https=", ch)))
		httpElementNumber <- unlist(lapply(ieProxies, function(ch) grep("http=", ch)))
		httpProxy <- unlist(ieProxies)[httpElementNumber]
		httpsProxy <- unlist(ieProxies)[httpsElementNumber]
		httpProxyAll <- unlist(ieProxies)
		httpProxy <- stringr::str_replace(httpProxy,"=","://")
		httpsProxy <- stringr::str_replace(httpsProxy,"=","://")
		httpProxyAll <- stringr::str_replace(httpProxyAll,"=","://")			
		httpProxyAll <- paste(httpProxyAll ,collapse=";")
		
		#some options that might be helpful
		#[TODO] neet to handle errors durring requests
		#httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))	
		#httr::set_config(config(ssl_options = 1L))
            #httr::set_config(use_proxy(url=httpProxy,port=8080, username=Sys.getenv("username")))

		# set default values
		httpCurlSuccess <- FALSE
		httpsCurlSuccess <- FALSE
		httpHttrSuccess <- FALSE
		httpsHttrSuccess <- FALSE
		httpBaseUrlSuccess <- FALSE
		httpsBaseUrlSuccess <- FALSE
		
		
		#libcurl documentation: https://ec.haxx.se/libcurl/libcurl-proxies
		h <- curl::new_handle(proxy = httpProxy, verbose = TRUE)

		# check named vector value with: proxyAccess["httpsHttrSuccess"]

		# Can't curl a https site, as it returns this error:
		#Error in curl_fetch_memory("https://httpbin.org/get", handle = h) : 
		#  Unsupported proxy '...', libcurl is built without the HTTPS-proxy support.
		#h <- new_handle(proxy = httpsProxy, verbose = TRUE)
		#req <- curl_fetch_memory("https://httpbin.org/get", handle = h)
		# More Tests
		# httr::GET(httpTestUrl) works but not for https ?

		
	} else {
		useProxy <- FALSE
		h <- curl::new_handle(verbose = TRUE)
	}
	# Thes are tests that internet access over http and https are successfull with different methods
	curlHttpTest = tryCatch({
		req <- curl::curl_fetch_memory(httpTestUrl, handle = h)
		httpCurlSuccess <- req$status_code == 200
	}, warning = function(warr) {
	    	message(paste0("Warning: http curl_fetch_memory test;",warr))
	}, error = function(err) {
	    	message(paste0("Error: http curl_fetch_memory test;",err))
	}, finally = {
	    	#message("Attempted http curl_fetch_memory")
	})
	
	curlHttpsTest = tryCatch({
		req_s <- curl::curl_fetch_memory(httpTestUrl, handle = h)
		httpsCurlSuccess <- req_s$status_code == 200
	}, warning = function(warr) {
	    	message(paste0("Warning: https curl_fetch_memory test;",warr))
	}, error = function(err) {
	    	message(paste0("Error: https curl_fetch_memory test;",err))
	}, finally = {
	    	#message("Attempted https curl_fetch_memory")
	})

	httrHttpTest = tryCatch({
		request <- httr::GET(httpTestUrl)
		httpHttrSuccess <- request$status_code == 200
	}, warning = function(warr) {
	    	message(paste0("Warning: http httr::GET test;",warr))
	}, error = function(err) {
	    	message(paste0("Error: http httr::GET test;",err))
	}, finally = {
	    	#message("Attempted http httr::GET")
	})

	httrHttpsTest = tryCatch({
		request_s <- httr::GET(httpsTestUrl)
		httpsHttrSuccess <- request_s$status_code == 200
	}, warning = function(warr) {
	    	message(paste0("Warning: https httr::GET test;",warr))
	}, error = function(err) {
	    	message(paste0("Error: https httr::GET test;",err))
	}, finally = {
	    	#message("Attempted https httr::GET")
	})

	baseUrlHttpTest = tryCatch({
		httpBaseUrlSuccess <- readLines(base::url(httpTestUrl))[2] == "  \"args\": {}, "
	}, warning = function(warr) {
	    	message(paste0("Warning: http base::url test;",warr))
	}, error = function(err) {
	    	message(paste0("Error: http base::url test;",err))
	}, finally = {
	    	#message("Attempted http base::url")
	})

	baseUrlHttpsTest = tryCatch({
		httpsBaseUrlSuccess <- readLines(base::url(httpsTestUrl))[2] == "  \"args\": {}, "
	}, warning = function(warr) {
	    	message(paste0("Warning: https base::url test;",warr))
	}, error = function(err) {
	    	message(paste0("Error: https base::url test;",err))
	}, finally = {
	    	#message("Attempted https base::url")
	})

	#Return a named vector of successes/failures
	proxyAccess <- c(
		useProxy=useProxy,
		httpCurlSuccess=httpCurlSuccess,
		httpsCurlSuccess=httpsCurlSuccess,
		httpHttrSuccess=httpHttrSuccess, 
		httpsHttrSuccess=httpsHttrSuccess,
		httpBaseUrlSuccess=httpBaseUrlSuccess, 
		httpsBaseUrlSuccess=httpsBaseUrlSuccess 
	)
	return(proxyAccess)
}
