# Download Excel source files
download_source <- function() {
  
  require(tidyverse)
  
  # Read dim_source
  dim_source <- read_csv("Model/Source/StarSchema/dim_source.csv") 
  
  if (!class(dim_source$last_update) == "Date") {
    dim_sourceP <- dim_source %>% 
      mutate(last_update = parse_date(last_update, format = "%m/%d/%Y")) %>% 
      drop_na(source_url, source_ext, source_destfile)
  } else {
    dim_sourceP <- dim_source%>% 
      drop_na(source_url, source_ext, source_destfile)
  }
  
  if (nrow(dim_sourceP) > 0) {
    
    # Download latest files
    success <- rep(TRUE, nrow(dim_sourceP))
    map_dbl(seq_along(success), function(i) {
      tryCatch(
        {
          destination <- str_c(dim_sourceP$source_destfile[i], ".", dim_sourceP$source_ext[i])
          if (dim_sourceP$source_entity[i] == "BanRep") {
            download_api(dim_sourceP$source_url[i], destination)  
          } else {
            download.file(
              url = dim_sourceP$source_url[i],
              destfile = destination,
              mode = "wb"
            )
          }
          cat(str_c("OK: '", dim_sourceP$source_name[i], "' successfully downloaded.\n"))
        }, error = function(c) {
          success[i] <<- 0
          cat(str_c("FAIL: '", dim_sourceP$source_name[i], "' could not be downloaded.\n"))
        }
      )
      return(0)
    })
    
    # Update latest month
    dim_source %>%
      mutate(
        last_update = unlist(Map(function(k, x, y) {
          i <- which(dim_sourceP$source_key == y)
          n <- success[i]
          if (length(n) > 0) {
            if (success[i]) {
              return(Sys.Date())
            } else {
              return(x)
            }
          } else {
            return(x)
          }
        },
        seq_along(last_update), 
        last_update, 
        source_key))
      ) %>%
      mutate(last_update = as.Date(as.numeric(as.character(last_update)), origin = "1970-01-01")) %>% 
      write_csv("Model/Source/StarSchema/dim_source.csv")
      
    print(success)
  } 
  
  return(0)
  
}

download_api <- function(url, dest) {
  
  require(httr)
  
  content_type = "text/html; charset=utf-8"
  while (content_type == "text/html; charset=utf-8") {
    request <- GET(url,
                   add_headers(`Accept` = 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3',
                               `Accept-Encoding` = 'gzip, deflate',
                               `Accept-Language` = 'es-ES,es;q=0.9',
                               `Connection` = 'keep-alive',
                               `Host` = 'obieebr.banrep.gov.co',
                               `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.90 Safari/537.36'),
                   write_disk(dest, overwrite = T),
                   verbose()
    )
    content_type = request$all_headers[[1]]$headers$`content-type`
  }
  
}
