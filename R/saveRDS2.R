#'saveRDS2
#'
#'Wrapper for saveRDS, add function to checking if file already exist in local directory
#'
#'@param object any R object accepted by saveRDS function
#'@param filname the name and/or path of the file to be saved
#'
#'@export
saveRDS2 <- function(object, filename = "uname_file_RDS2.RDS"){
  
  if(!file.exists(filename)){
    
    saveRDS(object, filename)
    
  }else{
    paste(filename, "already exists")
  }    
  
}
