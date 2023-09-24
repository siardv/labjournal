require(rvest)
require(xml2)
require(tidyverse)

# function to get collaborators and names from GS profiles
fcollabs <- function(gsid, lookforcollabs) {
  
  htmlpage1 <- read_html(paste0("https://scholar.google.nl/citations?user=", gsid, "&hl=en")) # so we paste the google scholar id
  profilename <- htmlpage1 %>% html_nodes(xpath = "//*/div[@id='gsc_prf_in']") %>% html_text() # we extract the profile name of that google scholar page
  profilecollabs1 <- as.data.frame(0) # empty df necessary for later
  profilecollabs2 <- as.data.frame(0) # empty df necessary for later
  
  if (lookforcollabs == 1) { # so if you want to look for collabs, set function to 1
    
    htmlpage2 <- read_html(paste0("https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user=", gsid)) # so we paste the google scholar id
    profilecollabs1 <-  htmlpage2 %>% html_nodes(css="h3") %>% html_text() # get names
    profilecollabs1 <-  as.data.frame(profilecollabs1)
    
    profilecollabs2 <- htmlpage2 %>% html_nodes("a") %>% html_attr("href") # get the link
    profilecollabs2 <- profilecollabs2[seq_along(profilecollabs2) %% 2 > 0]
    profilecollabs2 <- substring(profilecollabs2, 23)
    
  }
  if (nrow(profilecollabs1)>1) { # if there ARE collabs
    
    profilecollabs1 <- as.data.frame(profilecollabs1) # we want to...
    profilecollabs2 <-  as.data.frame(profilecollabs2)
    profilecollabs1[,c("coauth_id")] <- profilecollabs2[,1]
    
    profilecollabs1[,c("gs_id")] <- gsid #... add gs_ids of focal GS profile
    profilecollabs1[,c("name")] <- profilename #...and the the profile name of GS profile attached
    
    names(profilecollabs1)[1] <- "coauth"
    
  } else {
    profilecollabs1 <- as.data.frame(cbind(gsid, profilename)) # if NOT looking for collabs...
    names(profilecollabs1) <- c("gs_id", "name") #...we only attach gs_id and profilename
    
  }
  return(profilecollabs1)
  
}


