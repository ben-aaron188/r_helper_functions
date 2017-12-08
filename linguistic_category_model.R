#####################################################################
### EXTRACT CATEGORIES FOR THE LINGUISTICS CATEGORY MODEL
### CALCULATES LCM SCORE FROM DF INPUT (see below)
### SEE: Seih, Beier & Pennebaker (2017) JLSP
####################################################################

#needs wd with helper functions
#easiest is to use the repo on: https://github.com/ben-aaron188/r_helper_functions

require(tokenizers)
source('./spacy_pos_r.R')
source('./txt_df_from_dir.R')
load('./lcm/hgi.RData')
#read Harvard's General Inquirer dictionary
###DO NOT RUN OR MODIFY!!!
# hgi_dic = read.csv('./lcm/inquireraugmented.csv',
#                    header=T)
# hgi = hgi_dic[-1, c(1, 117:119)]
#
# hgi$type = apply(hgi[,2:4], 1, function(x){
#   x[nchar(as.character(x)) > 0]
# })
# hgi$type = ifelse(hgi$type == 'character(0)', NA, hgi$type)
# hgi$Entry = tolower(hgi$Entry)
#
# #remove #s
# pattern_regex = '\\#.*$'
# hgi$Entry = sub(pattern_regex,'\\1', hgi$Entry)
#
# hgi = hgi[!is.na(hgi$type), ]
# hgi = droplevels(hgi)
#
# names(hgi) = tolower(names(hgi))
#
# #remove duplicates
# hgi = hgi[!duplicated(hgi$entry),]
#
# hgi_corpus_file = Corpus(VectorSource(hgi$entry))
# entry_stemmed = tm_map(hgi_corpus_file, stemDocument, language = 'en')
# entry_stemmed_df = as.data.frame(as.matrix(entry_stemmed$content))
# names(entry_stemmed_df) = 'word'
#
# hgi = cbind(hgi, entry_stemmed_df$word)
# names(hgi)[6] = 'entry_stemmed'
#
# save(hgi
#      , file = 'hgi.RData')
###

tokenize_words_vec_length = function(input_txt_col){
  length_vec = sapply(input_txt_col, function(x){
    length(as.vector(unlist(tokenize_words(x) )))
  })
  return(length_vec)
}

get_iav_dav_sv = function(input_txt_col, type, on_processed_text = FALSE){
  type_mod = toupper(type)
  words = input_txt_col

  freq_in_vec = sapply(input_txt_col, function(x){
    word_vec = as.vector(unlist(tokenize_words(x) ))
    if(on_processed_text == T){
      table(word_vec %in% hgi$entry_stemmed[hgi$type == type_mod])[2]
    } else {
      table(word_vec %in% hgi$entry[hgi$type == type_mod])[2]
    }
  })
  return(freq_in_vec)
}


calculate_lcm = function(df_name, verbose = F){
  t1 = Sys.time()
  print(paste('--- STARTING LCM CALCULATION FOR DF: ', deparse(substitute(df_name)), ' at: ', t1))
  print("-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-")

  data_for_lcm = df_name

  data_for_lcm$nwords = tokenize_words_vec_length(data_for_lcm$text)
  data_for_lcm$nwords_proc = tokenize_words_vec_length(data_for_lcm$text_proc)

  #POS tagging
  data_for_lcm$adj = get_pos_count(df_identifier = data_for_lcm$Filename
                             , df_textcol = data_for_lcm$text
                             , unique_extr = F
                             , pos_type = 'ADJ'
                             , verbose = verbose)
  data_for_lcm$adj_rel = round((data_for_lcm$adj/data_for_lcm$nwords)*100, 4)

  data_for_lcm$noun = get_pos_count(df_identifier = data_for_lcm$Filename
                              , df_textcol = data_for_lcm$text
                              , unique_extr = F
                              , pos_type = 'NOUN'
                              , verbose = verbose)
  data_for_lcm$noun_rel = round((data_for_lcm$noun/data_for_lcm$nwords)*100, 4)

  #HGI freqs + props
  data_for_lcm$iav = get_iav_dav_sv(input_txt_col = data_for_lcm$text
                              , type = 'iav')
  data_for_lcm$iav_rel = round((data_for_lcm$iav/data_for_lcm$nwords)*100, 4)
  data_for_lcm$iav_proc = get_iav_dav_sv(input_txt_col = data_for_lcm$text_proc
                                   , type = 'iav'
                                   , on_processed_text = T)
  data_for_lcm$iav_proc_rel = round((data_for_lcm$iav_proc/data_for_lcm$nwords_proc)*100, 4)

  data_for_lcm$dav = get_iav_dav_sv(input_txt_col = data_for_lcm$text
                              , type = 'dav')
  data_for_lcm$dav_rel = round((data_for_lcm$dav/data_for_lcm$nwords)*100, 4)
  data_for_lcm$dav_proc = get_iav_dav_sv(input_txt_col = data_for_lcm$text_proc
                                   , type = 'dav'
                                   , on_processed_text = T)
  data_for_lcm$dav_proc_rel = round((data_for_lcm$dav_proc/data_for_lcm$nwords_proc)*100, 4)

  data_for_lcm$sv = get_iav_dav_sv(input_txt_col = data_for_lcm$text
                             , type = 'sv')
  data_for_lcm$sv_rel = round((data_for_lcm$sv/data_for_lcm$nwords)*100, 4)
  data_for_lcm$sv_proc = get_iav_dav_sv(input_txt_col = data_for_lcm$text_proc
                                  , type = 'sv'
                                  , on_processed_text = T)
  data_for_lcm$sv_proc_rel = round((data_for_lcm$sv_proc/data_for_lcm$nwords_proc)*100, 4)

  data_for_lcm[is.na(data_for_lcm)] = 0

  #LCM formula: LCM = ((DAV*1)+(IAV*2)+(SV*3)+(ADJ*4)+(NOUNS*5))/(DAV+IAV+SV+ADJ+NOUNS)
  data_for_lcm$lcm = ((data_for_lcm$dav_rel*1)+(data_for_lcm$iav_rel*2)+(data_for_lcm$sv_rel*3)+(data_for_lcm$adj_rel*4)+(data_for_lcm$noun_rel*5))/(data_for_lcm$dav_rel+data_for_lcm$iav_rel+data_for_lcm$sv_rel+data_for_lcm$adj_rel+data_for_lcm$noun_rel)

  data_for_lcm$lcm_proc = ((data_for_lcm$dav_proc_rel*1)+(data_for_lcm$iav_proc_rel*2)+(data_for_lcm$sv_proc_rel*3)+(data_for_lcm$adj_rel*4)+(data_for_lcm$noun_rel*5))/(data_for_lcm$dav_proc_rel+data_for_lcm$iav_proc_rel+data_for_lcm$sv_proc_rel+data_for_lcm$adj_rel+data_for_lcm$noun_rel)

  t2 = Sys.time()
  elapsed = t2-t1
  print(paste('--- FINISHED WITH LCM CALCULATION FOR DF: ', deparse(substitute(df_name)), ' at: ', t2))
  print(t2-t1)

  return(data_for_lcm)

}


#usage example:
# df_with_lcm = calculate_lcm(old_df)
# use the new df 'df_with_lcm'
# !!! NEEDS TO BE DONE WITH A DF COMPATIBLE WITH THE txt_df_from_dir FORMAT !!!

#load as:
# source('./linguistic_category_model.R')