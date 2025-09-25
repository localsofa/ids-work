library(RKorAPClient) 
library(tidyverse)

# Test1
test1 <- KorAPConnection(verbose = TRUE) %>%
  auth() %>%
   collocationAnalysis("Packung",
     vc = c("textClass=sport", "textClass!=sport"),
     leftContextSize = 1, rightContextSize = 1, topCollocatesLimit = 20
   ) %>%
   dplyr::filter(logDice >= 5) 
 
# view(test1)


# Test2
test2 <- KorAPConnection(verbose = TRUE) %>%
  auth() %>%
   collocationAnalysis("focus(in [tt/p=NN] {[tt/l=setzen]})",
     leftContextSize = 1, rightContextSize = 0, exactFrequencies = FALSE, topCollocatesLimit = 20
   )
 



# Hawara
testHw <- KorAPConnection(verbose = TRUE) %>%
  auth() %>%
  collocationAnalysis("Hawara",
                      leftContextSize = 1, rightContextSize = 1, topCollocatesLimit = 20
  ) 

testHb <- KorAPConnection(verbose = TRUE) %>%
  auth() %>%
  collocationAnalysis("Habara",
                      leftContextSize = 1, rightContextSize = 1, topCollocatesLimit = 20
  ) 

