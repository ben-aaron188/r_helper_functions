###############################################################################
### Matches a character string against a list of XX English words
### Wordlist taken from: https://github.com/dwyl/english-words
### inspiration from: https://stackoverflow.com/a/43049698 and https://stackoverflow.com/a/8082262
### Vectorized for DF use
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
english_words = fread('words_alpha.txt', header=F)
names(english_words) = 'words'
english_words$control_vec = 1
setwd(currentwd)

match_english = function(input_col, output_kind = 'match', output_type = 'prop'){
    sapply(seq(input_col), function(i){
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
      }
    })
}

#CHANGELOG
#27 Nov: init


#usage example:
# match_english(dt.data$text[1:2]) #default params
# match_english(input_col = dt.data$text[1:2]) #default params
# 
# match_english(input_col = dt.data$text[1:2]
#               , output_kind = 'ascii'
#               , output_type = 'prop') #custom settings


#load as:
# source('match_english.R')

### END

#TODO
#- different languages
#- speed optimization