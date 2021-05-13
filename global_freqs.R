# word ease
# freq rank in table
# choose unique / non-unique
# scale to


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
# https://norvig.com/ngrams/count_1w.txt
rank_10k = fread('google_10k_list.txt', header=F)
rank_10k[, rank := 1:.N]
names(rank_10k)[1] = 'words'
rank_10k$control_vec = 1

setwd(currentwd)

global_freqs = function(input_col, func = 'mean', only_unique = FALSE){
  
  sapply(seq(input_col), function(i){
    print(paste(i, '/', length(input_col), sep=""))
    mod_string = paste(input_col[i], collapse = ' ')
    mod_string = str_replace_all(mod_string, "[.,;:!?]", "")
    mod_string = str_replace_all(mod_string, "[\r\n\t]", " ")
    mod_string = tolower(mod_string)
    mod_string = unlist(str_split(mod_string, " "))
    mod_string = mod_string[nchar(mod_string) > 0]
    
    #transform to data table
    text.raw = data.table(text = mod_string
                          , index = 1:length(mod_string))
    
    if(only_unique == TRUE){
      text.raw = text.raw[!duplicated(text.raw$text), ]
    } 
    
    text.english_match = merge(text.raw
                               , rank_10k
                               , by.x = 'text'
                               , by.y = 'words'
                               , all.x = TRUE)
    text.english_match = text.english_match[order(index),]
    text.english_match$control_vec[is.na(text.english_match$control_vec)] = 0
    
    #return
    if(func == 'mean'){
      mean(text.english_match$rank, na.rm=T)
    } else if(func == 'median'){
      median(text.english_match$rank, na.rm=T)
    } 
    
    
  })
  
}


#usage example:
# global_freqs(dt.data$text[1:2]) #default params
# # global_freqs(dt.data$text[1:2], only_unique = T) #controlling whether to count unique values or not
