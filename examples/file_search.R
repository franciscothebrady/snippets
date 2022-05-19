## search through your scripts for specific string 
# setwd('/projects/franciscobrady/datapalooza/file_search/')

file_search <- function(path = '.', 
                        file_ext = '.R', 
                        string = 'francisco',
                        not_string = NULL) {
  require(tibble)
  require(dplyr)
  if(exists('file_ext')){
    file_ext <- paste0(file_ext,'$')
  } else file_ext <- '.*'
  # big list of all files (recursive) 
  files <- tibble(file = list.files(path = path,
                                    pattern = file_ext, 
                                    full.names = TRUE, 
                                    recursive = T))
  # map readlines through each file 
  file_text <- purrr::map(
    .x = files$file,
    .f = readLines
    )
  names(file_text) <- files$file
  # unlist and unnest 
  files_df <- file_text %>%
    unlist() %>%
    enframe %>%
    dplyr::rename(file = name, 
                  code = value)
  # now I had a df with filenames and each row is a line of code 
  files_df <- files_df %>%
    # grep for lines with string in them 
    filter(grepl(string, code))
  # extra info 
  files_df <- files_df %>% 
    mutate(dir = basename(dirname(file))) %>%
    select(dir, file, code)
  # not filter (if exists)
  if(!is.null(not_string)){
    files_df <- files_df %>%
      dplyr::filter(!grepl(not_string, files))
    }
  
  return(files_df)
}

file_search(path = '.', file_ext = '.py', string = 'pd')

file_search(path = '.', file_ext = '.R', string = 'francisco') %>% View

file_search(path = '.', 
            file_ext = '.R', 
            string = 'lorenzo', not_string = 'francisco')
file_search(path = '.', 
            file_ext = '.R', 
            string = 'francisco', 
            not_string = 'lorenzo')


# x <- 'franc.R'
# sub('[.R]*', '', x)
# gsub(paste0('[',file_ext,']'), '', x)
# gsub(paste0('\\.',file_ext,'*'), '', x)
# 
# sub('bb.*', '', x)
# 
# sub('.*bb', '', x)
# 
# sub('\\..*', '', x)
# 
# sub('.*\\.', '', x)
