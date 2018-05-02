#####################################################################
### EXTRACTS SENTIMENT AND CONCRETENESS DIMENSION ON TOKEN LEVEL
### - BUILDS ON SYUZHET PACKAGE https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
### - CONCRETENESS DICT FROM: Brysbaert, Warriner, Kuperman (2014) BRM
### - ALLOWS FOR STEMMED AND NON-STEMMED EXTRACTION
### - RETURNS DATAFRAME WITH RESCALED VALUES FOR RUNNING NARRATIVE TIME
### - ACCEPTS DATAFRAME COLUMNS WITH TEXT
####################################################################

# NOTE on dependencies: this code requires local dependencies from the repo mentioned below. Set the wd to the repo to avoid problems.

#needs wd with helper functions
#easiest is to use the repo on: https://github.com/ben-aaron188/r_helper_functions

#DO NOT RUN
# syuzhet.dict = get_sentiment_dictionary()
# syuzhet.dict_corpus_file = Corpus(VectorSource(syuzhet.dict$word))
# syuzhet.dict_stemmed = tm_map(syuzhet.dict_corpus_file, stemDocument, language = 'en')
# syuzhet.dict_stemmed_df = as.data.frame(as.matrix(syuzhet.dict_stemmed$content))
# names(syuzhet.dict_stemmed_df) = 'word'
# syuzhet.dict = cbind(syuzhet.dict, syuzhet.dict_stemmed_df$word)
# names(syuzhet.dict)[3] = 'word_stemmed'
# syuzhet.dict = syuzhet.dict[, c(3,2)]
# names(syuzhet.dict)[1] = 'word'
# syuzhet.dict = aggregate(value ~ word
#                          , data=syuzhet.dict
#                          , FUN=mean)
# save(syuzhet.dict
#      , file = './syuzhet_dep/syuzhet_dict_stemmed.RData')
#
# concr = fread('./concreteness/Concreteness_ratings_Brysbaert_et_al_BRM.txt'
#               , header=T)
# concr = as.data.frame(concr)
# names(concr) = tolower(names(concr))
# concr_corpus_file = Corpus(VectorSource(concr$word))
# concr_stemmed = tm_map(concr_corpus_file, stemDocument, language = 'en')
# concr_stemmed_df = as.data.frame(as.matrix(concr_stemmed$content))
# names(concr_stemmed_df) = 'word'
# concr = cbind(concr, concr_stemmed_df$word)
# names(concr)[10] = 'word_stemmed'
# concr.dict_stemmed = concr[, c(10, 3)]
# names(concr.dict_stemmed) = c('word', 'value')
# concr.dict_stemmed = aggregate(value ~ word
#                          , data=concr.dict_stemmed
#                          , FUN=mean)
# concr.dict_nonstemmed = concr[, c(1, 3)]
# names(concr.dict_nonstemmed) = c('word', 'value')
# concr.dict_nonstemmed = aggregate(value ~ word
#                                , data=concr.dict_nonstemmed
#                                , FUN=mean)
#
# save(concr.dict_stemmed
#      , concr.dict_nonstemmed
#      , file = './syuzhet_dep/brysbaert_dict_two_versions.RData')
#END DO NOT RUN


#SUPERSEDED: USE THE ...min FUNCTION BELOW
# get_narrative_dim = function(txt_input_col
#                      , txt_id_col
#                      , dimension
#                      , transform_values
#                      , low_pass_filter_size
#                      , stemming = T
#                      , bins = 100
#                      , transposing = F){
#   require(syuzhet)
#   require(tm)
#   require(qdap)
#
#   currentwd = getwd()
#   t1 = Sys.time()
#   if(dimension == 'concreteness'){
#     load('./syuzhet_dep/brysbaert_dict_two_versions.RData')
#     if(stemming == T){
#       concr_lexicon = concr.dict_stemmed
#     } else if(stemming == F){
#       concr_lexicon = concr.dict_nonstemmed
#     }
#   } else if(dimension == 'sentiment'){
#     load('./syuzhet_dep/syuzhet_dict_stemmed.RData')
#     if(stemming == T){
#       sent_lexicon = syuzhet.dict
#     } else if(stemming == F){
#       sent_lexicon = get_sentiment_dictionary()
#     }
#   }
#
#   if(stemming == T){
#     txt_col = sapply(txt_input_col, function(x){
#       tm_vec_col = Corpus(VectorSource(x))
#       tm_vec_col = tm::tm_map(tm_vec_col, content_transformer(replace_contraction))
#       tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_number))
#       tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_abbreviation))
#       tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))
#       tm_vec_col = tm_map(tm_vec_col, removeWords, tm::stopwords("en"))
#       tm_vec_col = tm_map(tm_vec_col, stemDocument, language = 'en')
#       as.character(as.matrix(tm_vec_col$content))
#     })
#   } else if(stemming == F){
#     txt_col = sapply(txt_input_col, function(x){
#       tm_vec_col = Corpus(VectorSource(x))
#       tm_vec_col = tm::tm_map(tm_vec_col, content_transformer(replace_contraction))
#       tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_number))
#       tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_abbreviation))
#       tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))
#       #tm_vec_col = tm_map(tm_vec_col, removeWords, tm::stopwords("en"))
#       #tm_vec_col = tm_map(tm_vec_col, stemDocument, language = 'en')
#       as.character(as.matrix(tm_vec_col$content))
#     })
#     #txt_col = txt_input_col
#   }
#
#   empty_matrix = matrix(data = 0
#                         , nrow = bins
#                         , ncol = length(txt_col)
#                         )
#   for(i in 1:length(txt_col)){
#     print(paste('---> performing sentiment extraction on text: ', txt_id_col[i], sep=""))
#     text.tokens = syuzhet::get_tokens(txt_col[i], pattern = "\\W")
#     if(dimension == 'sentiment'){
#       text.scored = get_sentiment(text.tokens, method = 'custom', lexicon = sent_lexicon)
#     } else if(dimension == 'concreteness'){
#       text.scored = get_sentiment(text.tokens, method = 'custom', lexicon = concr_lexicon)
#     }
#     text.scored_binned = get_dct_transform(text.scored
#                                          , x_reverse_len=bins
#                                          , low_pass_size = low_pass_filter_size
#                                          , scale_range = transform_values
#                                          )
#     sentiment_rolled_rescaled = rescale_x_2(text.scored_binned)
#     empty_matrix[, i] = sentiment_rolled_rescaled$z
#     empty_matrix[, i] = text.scored_binned
#   }
#   if(transposing == T){
#     final_df = as.data.frame(t(empty_matrix))
#     row.names(final_df) = txt_id_col
#   } else if(transposing == F){
#     final_df = as.data.frame(empty_matrix)
#     colnames(final_df) = txt_id_col
#   }
#   t2 = Sys.time()
#   print(t2-t1)
#   setwd(currentwd)
#   return(final_df)
# }

#minified example: sentence based, takes into account amplifiers and negators
get_narrative_dim_min = function(txt_input_col
                             , txt_id_col
                             , low_pass_filter_size
                             , clean = F
                             , transform_values = F
                             , min_sentences = 10
                             ){
  currentwd = getwd()
  t1 = Sys.time()
  require(tm)
  require(qdap)
  require(sentimentr)
  
  # Necessary replacing of abbreviations because they interfere with sentence splitting
  for(i in 1:length(txt_input_col)){
    print(paste('---> replacing abbreviations on text: ', txt_id_col[i], sep=""))
    txt_input_col[i] = replace_abbreviation(txt_input_col[i])
  }
  
  # Cleaning: replace contractions & numbers and make lower case
  if (clean == T) {
    txt_col = sapply(txt_input_col, function(x){
      tm_vec_col = Corpus(VectorSource(x))
      tm_vec_col = tm::tm_map(tm_vec_col, content_transformer(replace_contraction))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_number))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))
      as.character(as.matrix(tm_vec_col$content))
    })
    txt_col = txt_input_col
  }
  
  # Sentence-based sentiment extraction 
    print(paste('SENTENCE-BASED SENTIMENT EXTRACTION USING: ', method))
    txt_col = txt_input_col
    empty_matrix = matrix(data = 0
                          , nrow = 100
                          , ncol = length(txt_col)
    )
    for(i in 1:length(txt_col)){
      print(paste('---> performing sentiment extraction on text: ', txt_id_col[i], sep=""))
      text.sentences = sentimentr::get_sentences(txt_col[i])
      if(length(text.sentences) >= min_sentences){
          text.scored = sentiment(text.sentences)$sentiment
          text.scored_binned = get_dct_transform(text.scored
                                               , x_reverse_len=100
                                               , low_pass_size = low_pass_filter_size
                                               , scale_range = transform_values
                                               , scale_vals = normalize_values)
        empty_matrix[, i] = text.scored_binned
      } else {
        empty_matrix[, i] = rep(NA, 100)
      }
    }
    final_df = as.data.frame(empty_matrix)
    colnames(final_df) = txt_id_col
    t2 = Sys.time()
    print(t2-t1)
    setwd(currentwd)
    return(final_df)
  }

#CHANGELOG:
#- 2 MAY 2018: made sentiment extr. sentence-based,
#              adjusted scaling terminology,
#              added text cleaning (some optional)
#- 7 MAR 2018: added faster alternatives with 'meanr' and 'sentimentr'
#- 8 MAR 2018: added error catch with NAs for too short input data
#- 24 MAR 2018: added arg for low pass filter size

# data = data.frame('text' = character(2)
#                   , 'text_id' = character(2))
# data$text = 'This is a super, great positive sentence. I just love doing this. Now this will be very negative sentence. With disgusting words and ugly phrases. 
#                We stay negative for a while. We go even more terrible and awful. Things are getting better now.
#                It's starting to feel really good. I'm ecstatic at this point. This is amazing. I am incredibly positive now.' 
# data$text_id = 'text1'
#
# my_sentiment_analysis = get_narrative_dim_min_snt(txt_input_col = txt_input_col = data$text
#                       , txt_id_col = data$text_id
#                       , low_pass_filter_size = 5
#                       , clean = F
#                       , transform_values = T
#                       , normalize_values = F
#                       )
#
# plot(1:100
#      , my_sentiment_analysis$text1
#      , type ="h"
#      , col = "red")
#
#
# my_concreteness_analysis = get_narrative_dim(txt_input_col = data$text
#                                   , txt_id_col = data$text_id
#                                   , dimension = 'concreteness'
#                                   , stemming = F
#                                   , transposing = F)
#
# plot(1:100
#      , my_concreteness_analysis$text1
#      , type ="h"
#      , col = "red")

#load as:
#source('./get_narrative_dim.R)