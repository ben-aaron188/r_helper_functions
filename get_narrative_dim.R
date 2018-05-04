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
get_narrative_structure_sentences = function(txt_input_col
                                         , txt_id_col
                                         , low_pass_filter_size
                                         , min_length = 10
                                         , bins = 100
                                         , replace_abbr = T
                                         , clean = F
                                         , transform_values = T
                                         , normalize_values = F
){
  currentwd = getwd()
  t1 = Sys.time()

  if(replace_abbr == T | clean == T){
    require(tm)
    require(qdap)
  }

  require(sentimentr)
  require(syuzhet)

  if (clean == T & replace_abbr == T) {
    print('---> PREPROCESSING: replacing abbreviations and standard cleaning')
    # Replacing of abbreviations because they interfere with sentence splitting (recommended)
    # Cleaning: replace contractions & numbers and make lower case
    txt_col = sapply(txt_input_col, function(x){
      tm_vec_col = Corpus(VectorSource(x))
      tm_vec_col = tm::tm_map(tm_vec_col, content_transformer(replace_contraction))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_number))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_abbreviation))
      as.character(as.matrix(tm_vec_col$content))
    })
  } else if (clean == T & replace_abbr == F) {
    print('---> PREPROCESSING: standard cleaning')
    txt_col = sapply(txt_input_col, function(x){
      tm_vec_col = Corpus(VectorSource(x))
      tm_vec_col = tm::tm_map(tm_vec_col, content_transformer(replace_contraction))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_number))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))

      as.character(as.matrix(tm_vec_col$content))
    })
  } else if (clean == F & replace_abbr == T) {
    print('---> PREPROCESSING: replacing abbreviations')
    txt_col = sapply(txt_input_col, function(x){
      tm_vec_col = Corpus(VectorSource(x))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_abbreviation))
      as.character(as.matrix(tm_vec_col$content))
    })
  } else if (clean == F & replace_abbr == F) {
    print('---> PREPROCESSING: none')
    txt_col = txt_input_col
  }

  # Sentence-based sentiment extraction
  empty_matrix = matrix(data = 0
                        , nrow = bins
                        , ncol = length(txt_col)
  )

  for(i in 1:length(txt_col)) {
    print(paste('---> performing sentiment extraction on text: ', txt_id_col[i], sep=""))
    print('SENTENCE-BASED SENTIMENT EXTRACTION USING SENTIMENTR')
    sentiment_base = sentimentr::get_sentences(txt_col[i])
    if(length(unlist(sentiment_base)) >= min_length){
      text.scored = sentimentr::sentiment(sentiment_base)$sentiment
      text.scored_binned = get_dct_transform(text.scored
                                             , x_reverse_len=bins
                                             , low_pass_size = low_pass_filter_size
                                             , scale_range = transform_values
                                             , scale_vals = normalize_values)
      empty_matrix[, i] = text.scored_binned
    } else {
      empty_matrix[, i] = rep(NA, bins)
    }
  }
    final_df = as.data.frame(empty_matrix)
    colnames(final_df) = txt_id_col
    t2 = Sys.time()
    print(t2-t1)
    setwd(currentwd)
    return(final_df)
}

#token-based equivalent: does not account for valence shifters
get_narrative_structure_tokens = function(txt_input_col
                             , txt_id_col
                             , low_pass_filter_size
                             , min_length = 10
                             , bins = 100
                             , replace_abbr = T
                             , clean = F
                             , transform_values = T
                             , normalize_values = F
                             ){
  currentwd = getwd()
  t1 = Sys.time()

  if(replace_abbr == T | clean == T){
    require(tm)
    require(qdap)
  }

  require(sentimentr)
  require(syuzhet)


  if (clean == T & replace_abbr == T) {
    print('---> PREPROCESSING: replacing abbreviations and standard cleaning')
    # Replacing of abbreviations because they interfere with sentence splitting (recommended)
    # Cleaning: replace contractions & numbers and make lower case
    txt_col = sapply(txt_input_col, function(x){
      tm_vec_col = Corpus(VectorSource(x))
      tm_vec_col = tm::tm_map(tm_vec_col, content_transformer(replace_contraction))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_number))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_abbreviation))
      as.character(as.matrix(tm_vec_col$content))
    })
  } else if (clean == T & replace_abbr == F) {
    print('---> PREPROCESSING: standard cleaning')
    txt_col = sapply(txt_input_col, function(x){
      tm_vec_col = Corpus(VectorSource(x))
      tm_vec_col = tm::tm_map(tm_vec_col, content_transformer(replace_contraction))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_number))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(tolower))

      as.character(as.matrix(tm_vec_col$content))
    })
  } else if (clean == F & replace_abbr == T) {
    print('---> PREPROCESSING: replacing abbreviations')
    txt_col = sapply(txt_input_col, function(x){
      tm_vec_col = Corpus(VectorSource(x))
      tm_vec_col = tm_map(tm_vec_col, content_transformer(replace_abbreviation))
      as.character(as.matrix(tm_vec_col$content))
    })
  } else if (clean == F & replace_abbr == F) {
    print('---> PREPROCESSING: none')
    txt_col = txt_input_col
  }

  # Sentence-based sentiment extraction
    empty_matrix = matrix(data = 0
                          , nrow = bins
                          , ncol = length(txt_col)
    )
    for(i in 1:length(txt_col)) {
      print(paste('---> performing sentiment extraction on text: ', txt_id_col[i], sep=""))
      print('TOKEN-BASED SENTIMENT EXTRACTION USING SENTIMENTR ')
      sentiment_base = syuzhet::get_tokens(txt_col[i], pattern = "\\W")
      sentiment_base = sentimentr::get_sentences(sentiment_base)
      if(length(sentiment_base) >= min_length){
          text.scored = sentimentr::sentiment(sentiment_base)$sentiment
          text.scored_binned = get_dct_transform(text.scored
                                               , x_reverse_len=bins
                                               , low_pass_size = low_pass_filter_size
                                               , scale_range = transform_values
                                               , scale_vals = normalize_values)
        empty_matrix[, i] = text.scored_binned
      } else {
        empty_matrix[, i] = rep(NA, bins)
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
#- 7 MAR 2018: added faster alternatives with 'meanr' and 'sentimentr'
#- 8 MAR 2018: added error catch with NAs for too short input data
#- 24 MAR 2018: added arg for low pass filter size
#- 28 APR 2018: added sentence argument if data are sentence segmented (only for sentimenrr!)
#- 2 MAY 2018: made sentiment extr. sentence-based,
#              adjusted scaling terminology,
#              added text cleaning (some optional)
#- 4 MAY 2018: split of token- and sentence-based version
#              integrated pull request

data = data.frame('text' = character(2)
                  , 'text_id' = character(2))
data$text = c("This is a super, great positive sentence. I just love doing this. Now this will be very negative sentence. With disgusting words and ugly phrases. We stay negative for a while. We go even more terrible and awful. Things are getting better now. It's starting to feel really good. I'm ecstatic at this point. This is amazing. I am incredibly positive now."
              , "I haven't been sad in a long time. I am extremely happy today. It's a good day. But suddenly I'm only a little bit happy. What a great festival this is. I have never ever been so happy. I cannot say how much I do not love this.")
data$text_id = 1:2

get_narrative_structure_tokens(txt_input_col = data$text
                      , txt_id_col = data$text_id
                      , low_pass_filter_size = 5
                      , clean = T
                      , replace_abbr = T
                      , min_length = 10
                      , bins = 100
                      , transform_values = T
                      , normalize_values = F
                      )

get_narrative_structure_sentences(txt_input_col = data$text
                               , txt_id_col = data$text_id
                               , low_pass_filter_size = 5
                               , clean = T
                               , replace_abbr = T
                               , min_length = 2
                               , bins = 100
                               , transform_values = T
                               , normalize_values = F
                               )

#
# my_concreteness_analysis = get_narrative_dim(txt_input_col = data$text
#                                   , txt_id_col = data$text_id
#                                   , dimension = 'concreteness'
#                                   , stemming = F
#                                   , transposing = F)

# plot(1:100
#      , my_concreteness_analysis$text1
#      , type ="h"
#      , col = "red")

#load as:
#source('./get_narrative_dim.R)