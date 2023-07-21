
if(!dir.exists("./grambank-analysed/R_grambank/output")){
    dir.create("./grambank-analysed/R_grambank/output")
}

# creating a full Grambank file: first, within the submodule itself, and next
# in the data folder within the repository 
setwd("./grambank-analysed/R_grambank")

source("./make_wide.R")

GB_wide <- read.csv("./output/GB_wide/GB_wide_strict.tsv", header = TRUE, sep = '\t', stringsAsFactors=FALSE) 

#setwd("./../../")

GB_wide <- GB_wide %>%
  write_tsv(file="./../../data/GB_wide_strict.tsv")


#extracting glottolog: first, within the submodule itself, and next in the data folder within the repository 

source("./make_glottolog-cldf_table.R")

cldf_wide_df <- cldf_wide_df %>%
  write_tsv(file="./../../data/glottolog-cldf_wide_df.tsv") #taking the final dataframe cldf_wide_df from the previous script and saving it in our datafolder

setwd("./../../")
