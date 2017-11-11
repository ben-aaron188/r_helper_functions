###############################################################################
### Calculates individual reabability indices from string column (vectorized) #
### Built on the 'readability' package
###############################################################################


require(readability)

get_single_readability = function(input_col, index_kind, multi = T){
  if(multi == T){
    sapply(seq(input_col), function(i){
      readability(input_col[i], NULL)[[index_kind]]
    })
  } else if(multi == F){
    readability(input_col, NULL)[[index_kind]]
  }
}

#usage example:
# data$fleschkincaid = get_single_readability(data$text, 'Flesch_Kincaid')
# valid options for argument 'index_kind':
# - 'Flesch_Kincaid'
# - 'Gunning_Fog_Index'
# - 'Coleman_Liau'
# - 'SMOG'
# - 'Automated_Readability_Index'
# - 'Average_Grade_Level'

#load as:
# source('')