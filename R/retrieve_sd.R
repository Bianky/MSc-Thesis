retrieve_sd <- function(gd_folder, local_folder) {
  # function to retrieve satellite data from google drive
  # gd_folder is the google drive folder from which data is retrieved
  # local_folder is the folder at local pc where the data will be saved
  
  folder <- drive_get(gd_folder)
  
  files_in_folder <- drive_ls(folder)
  
  data_folder <- local_folder
  
  # loop through all files and download them
  for (i in seq_len(nrow(files_in_folder))) {
    drive_download(
      files_in_folder[i, ],
      path = file.path(data_folder, files_in_folder$name[i]),
      overwrite = TRUE
    )
  }
  
}