###############################################################################
### Matches a character string against a list of 300k or 10k English words
### Wordlists taken from: 
#### - https://github.com/dwyl/english-words
#### - https://github.com/first20hours/google-10000-english/blob/master/google-10000-english.txt
### inspiration from: https://stackoverflow.com/a/43049698 and https://stackoverflow.com/a/8082262
### Vectorized for DF use
### author: B Kleinberg https://github.com/ben-aaron188
###############################################################################

if (!require(stringi)){
  install.packages('stringi')
} 
require(stringi)

if (!require(data.table)){
  install.packages('data.table')
} 
require(data.table)


#load list of 370k words
##reset WD
currentwd = getwd()
setwd('/Users/bennettkleinberg/GitHub/r_helper_functions/english_word_match')
english_words_300k = fread('words_alpha.txt', header=F)
english_words_10k = fread('google_10k_list.txt', header=F)
names(english_words_300k) = 'words'
names(english_words_10k) = 'words'
english_words_300k$control_vec = 1
english_words_10k$control_vec = 1
setwd(currentwd)

match_english = function(input_col, which_dict, output_kind = 'match', output_type = 'prop'){
  
  if(which_dict == '10k'){
    english_words = english_words_10k
  } else if (which_dict == '300k'){
    english_words = english_words_300k
  }
    
  sapply(seq(input_col), function(i){
      print(paste(i, '/', length(input_col), sep=""))
      mod_string = paste(input_col[i], collapse = ' ')
      mod_string = str_replace_all(mod_string, "[.,;:!?]", "")
      mod_string = tolower(mod_string)
      mod_string = unlist(str_split(mod_string, " "))
      mod_string = mod_string[nchar(mod_string) > 0]
      
      #transform to data table
      text.raw = data.table(text = mod_string
                            , index = 1:length(mod_string))
      
      text.english_match = merge(text.raw
                                 , english_words
                                 , by.x = 'text'
                                 , by.y = 'words'
                                 , all.x = TRUE)
      text.english_match = text.english_match[order(index),]
      text.english_match$ascii = stri_enc_isascii(mod_string)
      text.english_match$control_vec[is.na(text.english_match$control_vec)] = 0
      unmatched_words = text.english_match$text[text.english_match$control_vec == 0]
      unmatched_words = paste(unmatched_words, collapse = ' ')
      
      if(output_kind == 'match'){
        if(output_type == 'prop'){
          unname(prop.table(table(text.english_match$control_vec == 0))[1])
        } else if(output_type == 'count'){
          unname(table(text.english_match$control_vec == 0)[1])
        }
      } else if(output_kind == 'ascii'){
        if(output_type == 'prop'){
          unname(prop.table(table(text.english_match$ascii == 0))[1])
        } else if(output_type == 'count'){
          unname(table(text.english_match$ascii == 0)[1])
        }
      } else if(output_kind == 'unmatched_words'){
        unmatched_words
      }
    })
}

#for loop alternative
#processing speed is fastest for the sapply method
match_english_loop = function(input_col, output_kind = 'match', output_type = 'prop'){
  for(i in 1:length(input_col)){
    print(paste(i, '/', length(input_col), sep=""))
    mod_string = paste(input_col[i], collapse = ' ')
    mod_string = str_replace_all(mod_string, "[.,;:!?]", "")
    mod_string = tolower(mod_string)
    mod_string = unlist(str_split(mod_string, " "))
    mod_string = mod_string[nchar(mod_string) > 0]
    
    #transform to data table
    text.raw = data.table(text = mod_string
                          , index = 1:length(mod_string))
    
    text.english_match = merge(text.raw
                               , english_words
                               , by.x = 'text'
                               , by.y = 'words'
                               , all.x = TRUE)
    text.english_match = text.english_match[order(index),]
    text.english_match$ascii = stri_enc_isascii(mod_string)
    text.english_match$control_vec[is.na(text.english_match$control_vec)] = 0
    unmatched_words = text.english_match$text[text.english_match$control_vec == 0]
    unmatched_words = paste(unmatched_words, collapse = ' ')
    
    if(output_kind == 'match'){
      if(output_type == 'prop'){
        unname(prop.table(table(text.english_match$control_vec == 0))[1])
      } else if(output_type == 'count'){
        unname(table(text.english_match$control_vec == 0)[1])
      }
    } else if(output_kind == 'ascii'){
      if(output_type == 'prop'){
        unname(prop.table(table(text.english_match$ascii == 0))[1])
      } else if(output_type == 'count'){
        unname(table(text.english_match$ascii == 0)[1])
      }
    } else if(output_kind == 'unmatched_words'){
      unmatched_words
    }
  }
}

#CHANGELOG
#27 Nov: init
#28 Nov: included tracker+ added for loop impl.
#10 Feb: added unmatched word option
#19 Feb: added 10k word list and which_dict param


#usage example:
# match_english(dt.data$text[1:2], which_dict == '10k') #default params
# match_english(dt.data$text[1:2], which_dict == '10k', outpuy_kind ="unmatched_words") #unmatched words
# match_english(input_col = dt.data$text[1:2]) #default params
# 
# match_english(input_col = dt.data$text[1:2]
#               , which_dict == '10k'
#               , output_kind = 'ascii'
#               , output_type = 'prop') #custom settings


#load as:
# source('match_english.R')

### END

#TODO
#- different languages
#- speed optimization