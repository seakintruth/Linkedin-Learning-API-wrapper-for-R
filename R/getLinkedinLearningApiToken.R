pacman::p_load(httr,jsonlite,getPass,tcltk,curl)

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
    .configureForProxy <- function(){
	currentProxyInfo <- curl::ie_proxy_info()
	httpTestUrl <- "http://httpbin.org/get"
	httpsTestUrl <- "https://httpbin.org/get"
	if(!is.null(currentProxyInfo$Proxy)){
		# a function to work behind a VPN...
		# See: https://cran.r-project.org/web/packages/curl/vignettes/windows.html
		currentProxyInfo <- curl::ie_proxy_info()
		ieProxies <- strsplit(currentProxyInfo$Proxy,";")
      		# find the list element that contains https or http 
		#[ASSUMPTION] currently only works if each http and https only has one value
		httpsElementNumber <- unlist(lapply(ieProxies, function(ch) grep("https=", ch)))
		httpElementNumber <- unlist(lapply(ieProxies, function(ch) grep("http=", ch)))
		httpProxy <- unlist(ieProxies)[httpElementNumber]
		httpsProxy <- unlist(ieProxies)[httpsElementNumber]
		httpProxy <- str_replace(httpProxy,"=","://")
		httpsProxy <- str_replace(httpsProxy,"=","://")
		#[TODO] if needed use setx to set windows user environment variables?
		Sys.setenv(http_proxy = httpProxy)
		Sys.setenv(https_proxy = httpsProxy)
		#Sys.setenv(ALL_PROXY = httpsProxy)
		Sys.setenv(NO_PROXY = currentProxyInfo$ProxyBypass)
		# this is our test that curl is working
		#[TODO] neet to handle errors durring requests
		h <- new_handle(proxy = httpProxy, verbose = TRUE)
		req <- curl_fetch_memory(httpTestUrl, handle = h)
		# Can't curl a https site, as it returns this error:
		#Error in curl_fetch_memory("https://httpbin.org/get", handle = h) : 
		#  Unsupported proxy 'https://NMCIProxyB1Secure:8443', libcurl is built without the HTTPS-proxy support.
		#h <- new_handle(proxy = httpsProxy, verbose = TRUE)
		#req <- curl_fetch_memory("https://httpbin.org/get", handle = h)
			# More Tests
		httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))	
		httr::set_config(config(ssl_options = 1L))
            httr::set_config(use_proxy(url=httpProxy,port=8080, username=Sys.getenv("username")))
		request <- httr::GET(httpTestUrl)
	} else {
		Sys.setenv(ALL_PROXY = "")
		Sys.setenv(http_proxy = "")
		Sys.setenv(https_proxy = "")
		Sys.setenv(NO_PROXY = "")
		# Test that curl works
		request <- httr::GET(httpTestUrl)
	}
	return(request$status_code == 200) # && req$status_code == 200 && 
  }
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
		# handle proxy if needed
		.configureForProxy()

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
