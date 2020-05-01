# Linkedin-Learning-API-wrapper-for-R
Interactive API helper functions for Linkedin Learning 

# Functions
## [get.linkedin.access.token()|./R/getLinkedinLearningApiToken.R]
 Linked in learning API keys are valid for several months at a time, 
 so if we have one we don't need get a new one

 If a file path is not passed we prompt for the secrets or file selection
 The secrets file is a R file that wil be sourced that contains a named character vector like:
```
apiClient <- c(
	ID = "some...",
	Secret = "value..."
)
```
 or an .RDS file containing just that vector created with
 saveRDS(apiClient,apiClientIdSecretPath)  

 If you use a secrets file or include the apiKeyRdsPath a API Key file will be added to the parent folder.
