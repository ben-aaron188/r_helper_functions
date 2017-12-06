###############################################################################
### Extracts POS via Spacy from text in dataframe #############################
### allows for counts of total POS and unique occurrences #####################
###############################################################################

require(spacyr)

get_pos_count = function(df_identifier, df_textcol, unique_extr = F, pos_type, verbose = T){

  sapply(seq(df_identifier), function(i){
    parsedtxt = spacy_parse(as.character(df_textcol[i]), dependency = FALSE, lemma = FALSE, pos = TRUE)
    pos_tags = parsedtxt
    pos_tags_unique = parsedtxt[!(duplicated((parsedtxt$token))),]
    pos_type_mod = toupper(pos_type)
    pos_tokens = pos_tags[pos_tags$pos == pos_type_mod, 'token']
    pos_tokens_unique = pos_tags_unique[pos_tags_unique$pos == pos_type_mod, 'token']

    if(verbose == T){
      if(unique_extr == T){
        if(pos_type_mod == 'ALL'){
          extracted_count = length(unique(pos_tags$pos))
        } else {
          extracted_count = sum(pos_tags_unique$pos == pos_type_mod)
          if(extracted_count > 0){
            print(paste("found:", pos_type_mod, "-->", pos_tokens_unique, "<---", 'in', df_identifier[i], sep=" "))
          } else {
            print(paste("no:", pos_type_mod, 'in', df_identifier[i], sep=" "))
          }

        }
      } else if(unique_extr == F){
        if(pos_type_mod == 'ALL'){
          extracted_count = length(pos_tags$pos)
        } else {
          extracted_count = sum(pos_tags$pos == pos_type_mod)
          if(extracted_count > 0){
            print(paste("found:", pos_type_mod, "-->", pos_tokens, "<---", 'in', df_identifier[i], sep=" "))
          } else {
            print(paste("no:", pos_type_mod, 'in', df_identifier[i], sep=" "))
          }
        }
      }
    } else if(verbose == F){
      if(unique_extr == T){
        if(pos_type_mod == 'ALL'){
          extracted_count = length(unique(pos_tags$pos))
        } else {
          extracted_count = sum(pos_tags_unique$pos == pos_type_mod)
          # if(extracted_count > 0){
          #   print(paste("found:", pos_type_mod, "-->", pos_tokens_unique, "<---", 'in', df_identifier[i], sep=" "))
          # } else {
          #   print(paste("no:", pos_type_mod, 'in', df_identifier[i], sep=" "))
          # }

        }
      } else if(unique_extr == F){
        if(pos_type_mod == 'ALL'){
          extracted_count = length(pos_tags$pos)
        } else {
          extracted_count = sum(pos_tags$pos == pos_type_mod)
          # if(extracted_count > 0){
          #   print(paste("found:", pos_type_mod, "-->", pos_tokens, "<---", 'in', df_identifier[i], sep=" "))
          # } else {
          #   print(paste("no:", pos_type_mod, 'in', df_identifier[i], sep=" "))
          # }
        }
      }
    }

    if(verbose == T){
      print("============================")
    }

    return(extracted_count)
  })

}

###CHANGELOG:
# 6 DEC 2017: ADDED 'VERBOSE' AS ARGUMENT FOR NEAT CONSOLE PRINTING
###END CHANGELOG

#usage example
# spacy_initialize(python_executable = '/Library/Frameworks/Python.framework/Versions/3.5/bin/python3')
# get_pos_count(df_identifier = data$Filename
#               , df_textcol = data$text
#               , unique_extr = F
#               , pos_type = 'ADJ'
#               , verbose = T)
# 'pos_type' argument must be any of the universal pos tags: http://universaldependencies.org/u/pos/all.html
# for unique occurrence of POS set 'unique_extr = T'
# spacy_finalize()

#load as:
# source('./get_pos_count.R')