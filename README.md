# r_helper_functions

These functions might be useful to others who work on comp. linguistics problems with R.

Current functions (9 Mar. 2018)

## Computational linguistics functions
- process texts in folder to r dataframe [txt_df_from_dir.R](https://github.com/ben-aaron188/r_helper_functions/blob/master/txt_df_from_dir.R)
  - includes recursive file retrieval
- extract parts-of-speech frequencies and named entities in R with the pyhton-to-R bridge for SpaCy [https://github.com/kbenoit/spacyr](spacyr)
  - [spacy_ner_r.R](https://github.com/ben-aaron188/r_helper_functions/blob/master/spacy_ner_r.R)
  - [spacy_pos_r.R](https://github.com/ben-aaron188/r_helper_functions/blob/master/spacy_pos_r.R)
- get vectorized readability indices (using [Tyler Rinker](https://github.com/trinker)'s [readability package](https://cran.r-project.org/web/packages/readability/index.html))
  - [get_single_readability.R](https://github.com/ben-aaron188/r_helper_functions/blob/master/get_single_readability.R)
- calculate the Linguistic Category Model (LCM) as proposed by [Seih et al. (2017)](http://journals.sagepub.com/doi/abs/10.1177/0261927X16657855)
  - [linguistic_category_model.R](https://github.com/ben-aaron188/r_helper_functions/blob/master/linguistic_category_model.R)
- calculate a linguistic concreteness score per text
  - this function uses the 40k+ human annotation by [Brysbaert et al. (2014)](https://www.ncbi.nlm.nih.gov/pubmed/24142837)
  - [calculate_concreteness.R](https://github.com/ben-aaron188/r_helper_functions/blob/master/calculate_concreteness.R)
- extract the narrative structure of texts
  - this is based on the [syuzhet package](https://github.com/mjockers/syuzhet) and the [sentimentr](https://github.com/trinker/sentimentr) packages
  - includes a minimal version for faster processing of massive text datasets
  - allows for multidimensional narrative structure modelling (currently sentiment and concreteness)
  - Note: the function below is built to deal with non-punctuated data and performs token-based sentiment extraction
    - if you have data with sentence boundaries, use the `get_narrative_dim_min(...)` function
  - [get_narrative_dim.R](https://github.com/ben-aaron188/r_helper_functions/blob/master/get_narrative_dim.R)

## Effect size calculations
- [Cohen's f](https://github.com/ben-aaron188/r_helper_functions/blob/master/cohensf.R)
- [Cohen's d (within-subjects)](https://github.com/ben-aaron188/r_helper_functions/blob/master/dz_within_CI.R)
- [Cohen's d (between-subjects)](https://github.com/ben-aaron188/r_helper_functions/blob/master/ds_between_CI.R)


## some misc function(s) that need refinement
- coerced multiclass classification in supervised machine learning with variable thresholds: [cmc.R](https://github.com/ben-aaron188/r_helper_functions/blob/master/cmc.R)

Let me know if there are any bugs.
