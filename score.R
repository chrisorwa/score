

'
Script      : Score
Created     : May, 2016
Author(s)   : iHub Research
Version     : v1.0
License     : Proprietary

Description : compute the sentiment of sentence(s)
'

#load pre-defined scripts
source('sentiment.R')

score <-function(sentence)
  # ===========================================================================
#     Perform numeric sentiment score on a sentence
#
# Args:
#   text: a dataframe with single entry text 
#
# Returns:
#   Float: 
# ===========================================================================
{
  c = ComputeSentimentScore(sentence)
  
  d <- strsplit(removeWords(x = as.character(c[1]),words = stopwords(kind = 'SMART')),split = ' ')[[1]]
  d <- d[!d%in%""]
  
  if(c[2] == 0 & c[3] == 0){
    neutral_score = 0.5
    return(neutral_score)
  }
  
  if(c[2] == c[3])
  {
    cc = as.numeric(c[2]) + as.numeric(c[3])
    scored = cc/length(d)
    return(scored)
  }
  if(c[2] > c[3]){
    negative_score = as.numeric(c[2])/length(d)
    return(negative_score)
  }
  
  else{
    if(c[3] > c[2]){
      positive_score = as.numeric(c[3])/length(d)
      if(positive_score < 0.5){
        positive_score = 1 - positive_score
        return(positive_score)
      } else{
        return(positive_score)
      }
    }
  }
}
