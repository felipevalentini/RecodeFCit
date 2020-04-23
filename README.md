# RecodeFCit

Demo package for recoding items triplets from forced choice to binary variables.

Avaliable functions:

checkTypo = check for typos in the dataset.

checkTie = check for tie (it means the same answer for the least and the most, or two items with the same rank - it does happen in the paper and pencil format).

recodeErrors = recode typos or ties into missing. We recommend you carefully check out the dataset before running this function.

recodeData = recode the data from the formats MOLE (most and least) and RANK to binary comparisons. Dataset must be in binary comparisons for running T-IRT model in MPlus or Lavaan.

Installing in R:

library(devtools)

remotes::install_github("felipevalentini/RecodeFCit")
