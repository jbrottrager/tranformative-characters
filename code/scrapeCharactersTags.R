################### Scraping Character Tags on Fanfiktion.de ################### 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(rvest)
library(stringr)
library(stringi)


readUrl <- function(url) {
  out <- tryCatch( {
    base <- read_html(url)
  },
  error = function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    # return value in case of error
    out <- 1
  }
  )
  return(out)
}

ff_ids <- readLines("data\\ff_chunk_ids.txt")
ff_ids_filtered <- gsub("(.+?)[-_].+$", "\\1", ff_ids)

meta_ids <- data.frame(ff_ids, ff_ids_filtered)

base_url <- "https://www.fanfiktion.de/s/"


meta <- as.data.frame(matrix(NA, nrow = length(ff_ids), ncol = 3))
names(meta) <- c("id", "characters", "tags")

failed <- c()
for (i in 1:length(ff_ids_filtered)){
  page <- readUrl(paste0(base_url, ff_ids_filtered[i]))
  if (is.numeric(page)) {
    message("\n\nIndexing is paused.")
    Sys.sleep(print(sample(1:5)[1]))
    failed <- append(failed, ff_ids_filtered[i])
    next()
  }
  meta[i,]$id <- ff_ids_filtered[i]
  characters <- gsub("[\n\t]+", "" ,
                     as.character(html_nodes(page, xpath = '//span[@class="badge badge-character"]/text()')))
  tags <- gsub("[\n \t]+", "" ,
               as.character(html_nodes(page, xpath = '//div[@class="small-font center block"]/text()')))
  meta[i,]$characters <- list(characters)
  meta[i,]$tags <- list(tags)
  message(i, " of ", length(ff_ids_filtered), " is done!") 
  if (i %% 17 == 0) {
    Sys.sleep(print(sample(2:7)[1]))
  }
}

meta <- na.omit(meta)

meta_slash <- as.data.frame(matrix(NA, nrow = nrow(meta), ncol = 3))
names(meta_slash) <- c("id", "slash", "characters")

for (i in 1:nrow(meta)) {
  tags <- unlist(meta[i,]$tags)
  category <- tags[2]
  cat_slash <- gsub(".+/(.+)", "\\1", category)
  meta_slash[i,]$id <- meta[i,]$id
  meta_slash[i,]$slash <- cat_slash
  meta_slash[i,]$characters <- paste(unlist(meta[i,]$characters), collapse = ", ")
}

meta_slash <- merge(meta_slash, meta_ids, by.x = "id", by.y = "ff_ids_filtered", 
                    all.x = TRUE)

write.csv(meta_slash, "meta_slash.csv")

meta_slash <- read.csv("meta_slash.csv")

characters_ls <- c()
texts_ls <- c()
for (i in 1:nrow(meta_slash)) {
  if (meta_slash[i,]$characters == "") {
    characters_ls <- append(characters_ls, NA)
    texts_ls <- append(texts_ls, meta_slash[i,]$ff_ids)#
    next
  }
  characters <- strsplit(meta_slash[i,]$characters, split = ", ")
  for (j in 1:length(characters[[1]])) {
    characters_ls <- append(characters_ls, characters[[1]][j])
    texts_ls <- append(texts_ls, meta_slash[i,]$ff_ids)
  }
}

df <- data.frame(characters_ls, texts_ls)
names(df) <- c("character", "text")

write.csv(df, "characters_texts.csv")

write.csv(meta_ids, "meta_ids.csv")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#