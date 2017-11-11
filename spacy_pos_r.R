###############################################################################
### Extracts POS via Spacy from text in dataframe #############################
### allows for counts of total POS and unique occurrences #####################
###############################################################################

require(spacyr)

get_pos_count = function(df_identifier, df_textcol, unique_extr = F, pos_type){

  sapply(seq(df_identifier), function(i){
    parsedtxt = spacy_parse(as.character(df_textcol[i]), dependency = FALSE, lemma = FALSE, pos = TRUE)
    pos_tags = parsedtxt
    pos_tags_unique = parsedtxt[!(duplicated((parsedtxt$token))),]
    pos_type_mod = toupper(pos_type)

    print(paste(pos_type_mod, 'at', df_identifier[i], sep=" "))

    if(unique_extr == T){
      if(pos_type_mod == 'ALL'){
        extracted_count = length(unique(pos_tags$pos))
      } else {
        extracted_count = sum(pos_tags_unique$pos == pos_type_mod)
      }
    } else if(unique_extr == F){
      if(pos_type_mod == 'ALL'){
        extracted_count = length(pos_tags$pos)
      } else {
        extracted_count = sum(pos_tags$pos == pos_type_mod)
      }
    }
    return(extracted_count)
  })

}

#usage example
# spacy_initialize(python_executable = '/Library/Frameworks/Python.framework/Versions/3.5/bin/python3')
# get_pos_count(test_data$unid, test_data$statement1_content, F, 'ADJ')
# 'pos_type' argument must be any of the universal pos tags: http://universaldependencies.org/u/pos/all.html
# for unique occurrence of POS set 'unique_extr = T'
# spacy_finalize()

#load as:
# source('./get_pos_count.R')