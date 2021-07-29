#Script to Move Pictures and Videos to new directories, based on month and year taken. 
#Currently works for my Honor phone and dates in Format YYYYMMDD
move_pictures <- function(path_from, path_to = "D:/Bilder"){
library(magrittr)

path_old <- path_from #Path from where to copy
path_new <- path_to #Main Dir to copy to

dir <- fs::dir_ls(path_old, recurse = TRUE) %>% #List all files in all subdirectories
  dplyr::as_tibble() %>% 
  dplyr::mutate(date = dplyr::case_when( #Some date regexes
                  stringr::str_detect(value,"((?<=(_|-))(.{8})(?=(_|-)))") == TRUE ~ lubridate::as_date(stringr::str_extract(value,"((?<=(_|-))(.{8})(?=(_|-)))")),
                  stringr::str_detect(value,"(\\d{2}\\.\\d{2}\\.\\d{2})") == TRUE ~ lubridate::dmy(stringr::str_extract(value,"(\\d{2}\\.\\d{2}\\.\\d{2})"))
                ),
                year = lubridate::year(date),
                month = lubridate::month(date),
                type = dplyr::case_when( #Classify type of data
                  stringr::str_detect(value, ".jpg|.jpeg|JPG|JPEG|CR2|cr2|bmp|png|tif") == TRUE ~ "Pictures",
                  stringr::str_detect(value, "(?<=/)(VID|SL_MO_VID|mp4|MP4)") == TRUE~ "Videos",
                  TRUE ~ "Other"
                ),
                filename = stringr::str_extract(value, "([^\\/]+)$"), # Extract filename
                path = stringr::str_c(path_new, "/",year, "/",month, "/", filename)) %>% #Create new filepath
  dplyr::filter(type != "Other") 

not_moveable <- dir %>% #Handle files without meaningful filename (e.g. without date)
  dplyr::filter(is.na(path))
if(nrow(not_moveable > 0)) {
  cat(crayon::yellow(
    stringr::str_c(
      "The following files were not parsed:\n\n",
      stringr::str_c(not_moveable$value, collapse = "\n"),
      "\n\nTrying to get date information from exif data..."
    )
  ))
  
  exif <- exifr::read_exif(not_moveable$value) %>% #Read exif data
    dplyr::mutate(
      date = min( #Take the youngest date
        lubridate::as_date(FileModifyDate), 
        lubridate::as_date(FileCreateDate)
      ),
      year = lubridate::year(date),
      month = lubridate::month(date),
      type = ifelse(FileType == "JPEG", "Picture", "other"),
      path = stringr::str_c(path_new, "/", year, "/", month, "/", FileName)
    ) %>%
    dplyr::select("value" = SourceFile, #Same form as dir
                  date,
                  year,
                  month,
                  type,
                  "filename" = FileName,
                  path)
  
  if (nrow(exif) > 0) {
    
    dir <- dir %>% # Bind rows if any pictures with exif data
      dplyr::filter(!is.na(path)) %>% 
      dplyr::bind_rows(.,exif) %>% 
      tibble::rowid_to_column()
    
    cat(
      crayon::green("Extraction was successful for:\n\n") %+% crayon::bgGreen(stringr::str_c(exif$filename, collapse = "\n"))
    )
  } else {
    cat(crayon::bgRed("Unfortunately couldn't extract anything from exif data..."))
  }
} else {
  
dir <- dir %>% 
  tibble::rowid_to_column()  #Rowids needed for applying

}

move_file <- function(id){
  
  newpath <- dir %>% #Path to copy to
    dplyr::filter(rowid == id) %>% 
    dplyr::pull(path) 
  
  
  oldpath <- dir %>% # Path to copy from
    dplyr::filter(rowid == id) %>% 
    dplyr::pull(value)
  
  if(!fs::dir_exists(stringr::str_extract(newpath, "^(.+)\\/"))){ #Create subdirectory if doesn't exist
    fs::dir_create(stringr::str_extract(newpath, "^(.+)\\/"))
  }
  
  #Check if file already exists
  
  if(fs::file_exists(newpath) == TRUE){
    cat(crayon::magenta(stringr::str_c("The file ", newpath, " already exists in this directory. \nSkipping\n")))
    return()
  }
  
  fs::file_move(oldpath, newpath) #Move files. Maybe function option to move or copy?
  
  cat(stringr::str_c("Succesfully moved ", crayon::blue(oldpath), " to ", crayon::green(newpath), "\n"))
    
}

#Run to copy
purrr::map_df(dir$rowid, move_file) # Apply
}
