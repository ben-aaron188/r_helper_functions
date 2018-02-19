###############################################################################
### Creates a dataframe with raw texts from directory
### Usable in spacy_ner_r pipeline
###############################################################################
require(stringr)
require(tm)

### these might be required ###
# library(data.table)
# dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_77.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
# require(rJava)
# library(qdap)
# library(openNLP)


txt_df_from_dir = function(dirpath, include_processed = FALSE){
  currentwd = getwd()
  setwd(dirpath)
  file_list = list.files(pattern = '*.txt')
  data = do.call("rbind"
                 , lapply(file_list
                          , FUN=function(files) {
                            print(files)
                            print(nchar(readChar(files
                                                 , file.info(files)$size)))
                            paste(files
                                  , readChar(files
                                            , file.info(files)$size)
                                  , sep="@@@")
                          }
                 )
  )

  id = 1:nrow(data)
  data = cbind(data, id)

  data = as.data.frame(data)
  names(data) = c('text', 'id')

  data$Filename = as.factor(str_extract(data$text, '^[^@@@]*'))
  data$text = sub('.*\\@@@(.*)','\\1', data$text)

  data = data[,-2]

  if(include_processed == T){
    data$text_proc = sapply(data$text, function(x){
      tm_vec_col = Corpus(VectorSource(x))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_contraction))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_number))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_abbreviation))
      tm_vec_col = tm_map(tm_vec_col, removePunctuation)
      tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))
      tm_vec_col = tm_map(tm_vec_col, stripWhitespace)
      tm_vec_col = tm_map(tm_vec_col, removeWords, tm::stopwords("en"))
      tm_vec_col = tm_map(tm_vec_col, stripWhitespace)
      tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))
      tm_vec_col = tm_map(tm_vec_col, stemDocument, language = 'en')
      as.character(as.matrix(tm_vec_col$content))
    })
  }

  data$id = 1:nrow(data)
  setwd(currentwd)

  return(data)
}

#CHANGELOG:
#6 DEC 2017: ADDED processing pipeline for additional text column
#END CHANGELOG

#usage example:
#new_data = txt_df_from_dir(dirpath = './my_text_folder', include_processed = T)

#View(new_data)

#load as:
# source('./txt_df_from_dir.R')