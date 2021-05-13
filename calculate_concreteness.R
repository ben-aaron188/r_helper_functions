#####################################################################
### PREPROCESS CONCRETENESS SCORES IN LANGUAGE
### - CALCULATES CONCRETENESS SCORE PER TEXT-CASE
### - RETURNS WEIGHTED TDM, DTM, TFIDFs
### FOR CONCRETENESS RATINGS SEE: Brysbaert, Warriner, Kuperman (2014) BRM
####################################################################

#needs wd with helper functions
#easiest is to use the repo on: https://github.com/ben-aaron188/r_helper_functions

#deps
source('/Users/bennettkleinberg/GitHub/r_helper_functions/txt_df_from_dir.R')
source('/Users/bennettkleinberg/GitHub/r_helper_functions/toNumeric.R')
require(data.table)
require(tokenizers)
#read concreteness data Brysbaert
#DO NOT RUN
# files = list.files(pattern = '*.txt')
# concr = fread(files[1]
#               , header=T)
# concr = as.data.frame(concr)
# names(concr) = tolower(names(concr))
# concr_corpus_file = Corpus(VectorSource(concr$word))
# concr_stemmed = tm_map(concr_corpus_file, stemDocument, language = 'en')
# concr_stemmed_df = as.data.frame(as.matrix(concr_stemmed$content))
# names(concr_stemmed_df) = 'word'
# concr = cbind(concr, concr_stemmed_df$word)
# names(concr)[10] = 'word_stemmed'
#
#
# save(concr
#      , file = 'brysbaert_concr.RData')
#END DO NOT RUN
load('/Users/bennettkleinberg/GitHub/r_helper_functions/concreteness/brysbaert_concr.RData')

get_concreteness = function(input_txt_col, stemming_global, type){

  t1 = Sys.time()
  print(paste('--- STARTING CONCRETENESS CALC. at: ', t1))
  print("-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-")

  f1 = sapply(input_txt_col, function(x){

    word_vec_for_df = unlist(tokenizers::tokenize_words(x))
    word_vec_for_df_length = length(word_vec_for_df)
    
    if(word_vec_for_df_length != 0){
      
      df = data.frame('id' = 1:word_vec_for_df_length
                      , 'word' = word_vec_for_df)
      if(stemming_global == T){
        concr_df = merge(df, concr
                         , by='word')
      } else if(stemming_global == F){
        concr_df = merge(df, concr
                         , by.x='word'
                         , by.y='word_stemmed')
      }
    
      concr_sum = sum(concr_df$conc.m)
      concr_avg = mean(concr_df$conc.m)
        
    } else {
      
      concr_sum = 0
      concr_avg = 0
      
    }
    
    if(type == 'sum'){
      return(concr_sum)
    } else if(type == 'mean'){
      return(concr_avg)
    }
  })


  t2 = Sys.time()
  elapsed = t2-t1
  print(paste('--- FINISHED ---'))
  print(t2-t1)

  return(f1)
}

#usage example:
# data$conc = get_concreteness(data$text[1:20], F, 'mean')

