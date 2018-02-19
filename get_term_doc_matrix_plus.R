#####################################################################
### PREPROCESS CONCRETENESS SCORES IN LANGUAGE
### - RETURNS WEIGHTED TDM, DTM, TFIDFs
####################################################################

#needs wd with helper functions
#easiest is to use the repo on: https://github.com/ben-aaron188/r_helper_functions

#deps
setwd('/Users/bennettkleinberg/GitHub/r_helper_functions')
source('./txt_df_from_dir.R')
source('./toNumeric.R')
require(data.table)

get_term_doc_matrix_plus = function(input_txt_col, stemming_global, tdm_type, sparsity, add_weights){

  t1 = Sys.time()
  print(paste('--- STARTING TERM-DOC MATRIX PROCESSING at: ', t1))
  print("-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-")


  input_txt_col = data$text
  tdm_type = 'tfidf'
  sparsity = .95
  tfidf_stemming = F
  add_weights = T

  data_for_matrix = data.frame('id' = 1:length(input_txt_col)
                               , 'text' = input_txt_col)

  text_corpus1 = Corpus(VectorSource(data_for_matrix$text))

  tdm_controls = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)
                      #, stemming = tfidf_stemming
                      #stemming is handled via the input_txt_col argument
  )

  if(tdm_type == 'tfidf'){
    tdm = TermDocumentMatrix(text_corpus1
                             , control = tdm_controls)
  } else if(tdm_type == 'freq'){
    tdm = TermDocumentMatrix(text_corpus1)
  }

  tdm = removeSparseTerms(tdm, sparse = sparsity)
  tdm = as.matrix(round(tdm, 4))
  tdm = as.data.frame(tdm)
  tdm$word = row.names(tdm)

  if(stemming_global == T){
    #remove duplicates from stemmed word per max(conc.m) and min(conc.sd)
    nrow_before = nrow(concr)
    prop_dupl_1 = round(prop.table(table(duplicated(concr$word_stemmed)))[2], 4)*100

    if(prop_dupl_1 > 0){
      concr = concr[which(abs(concr$conc.m) == ave(concr$conc.m, concr$word_stemmed,
                                                   FUN=function(x) max(abs(x)))), ]
      prop_dupl_2 = round(prop.table(table(duplicated(concr$word_stemmed)))[2], 4)*100
      if(prop_dupl_2 > 0){
        concr = concr[which(abs(concr$conc.sd) == ave(concr$conc.sd, concr$word_stemmed,
                                                      FUN=function(x) min(abs(x)))), ]
      }
    }
    nrow_after_prop = round(1-(nrow(concr)/nrow_before), 4)

    concr_match = concr[concr$word_stemmed %in% row.names(tdm), ]
    concr_match = concr_match[,c(1,3,4,10)]
    tdm = merge(tdm, concr_match
                , by.x='word'
                , by.y='word_stemmed'
                , all.x = T)
    print(paste('-- removed ', nrow_after_prop, '% of stemmed words.'))
  } else if(stemming_global == F){
    concr_match = concr[concr$word %in% row.names(tdm), ]
    concr_match = concr_match[,c(1,3,4,10)]
    tdm = merge(tdm, concr_match
                , by.x='word'
                , by.y='word'
                , all.x = T)
  }
  tdm$conc.m[is.na(tdm$conc.m)] = 1
  row.names(tdm) = tdm$word

  if(add_weights == T){
    tdm_new = tdm
    tdm_new[, 2:(ncol(tdm_new)-3)] = apply(tdm_new[, 2:(ncol(tdm_new)-3)]
                                           , 2
                                           , function(x){
                                             x*tdm_new$conc.m
                                           })
  } else if(add_weights == F){
    tdm_new = tdm
  }

  tdm_new_trans = as.data.frame(t(tdm_new)[-1,])
  tdm_new_trans = tdm_new_trans[-c((nrow(tdm_new_trans)-3):nrow(tdm_new_trans)),]
  tdm_new_trans$id = as.numeric(row.names(tdm_new_trans))

  tdm_new_trans[,1:(ncol(tdm_new_trans))] = apply(tdm_new_trans[,1:(ncol(tdm_new_trans))]
                                                  , 2
                                                  , toNumeric)

  t2 = Sys.time()
  elapsed = t2-t1
  print(paste('--- FINISHED WITH TERM-DOC PROCESSING at: ', t2))
  print(paste('Arguments set to:', 'input_txt_col:', deparse(substitute(input_txt_col)), '||| tdm_type:', tdm_type, '||| sparsity: ', sparsity, '||| stemming_global:', stemming_global, '||| add_weight:', add_weights, sep=" "))
  print(t2-t1)

  return(tdm_new_trans)
}


# ngram_plus_1 = get_term_doc_matrix_plus(input_txt_col = data$text
#                                         , tdm_type = 'tfidf'
#                                         , sparsity = .95
#                                         , tfidf_stemming = F
#                                         , add_weights = T)
#
# data_merged = merge(data, ngram_plus_1, by='id')
