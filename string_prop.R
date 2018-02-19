###############################################################################
### Returns a new string of a given proportion of the length of the input string
### - vectorized
###############################################################################

string_prop = function(txt_col, prop){
  new_string = substr(as.character(txt_col)
                   , 0
                   , nchar(as.character(txt_col))*prop)
  return(new_string)
}


#usage example:
#col_of_fifty_percent_length = string_prop(txt_col = data$text, prop = 0.5)

#load as:
# source('./string_prop.R')