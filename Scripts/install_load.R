dependencies <- c("adegenet","hierfstat","poppr","corehunter","ape","pegas",
                  "pheatmap","edgeR","readr","reshape2","tidyverse","polysat")

# devtools 

if( !is.element("devtools",rownames(installed.packages() ) ) ){
  install.packages("devtools")
  install.packages("BiocManager")
}
library(devtools)

# Install missing packages

missingPackages <- function(pkg){
  if( !is.element(pkg,rownames(installed.packages() ) ) ){
    message(pkg, "-----> Package is not installed ")
    BiocManager::install(pkg)
  }
}

for(i in dependencies){
  missingPackages(i)
  library(i, character.only = TRUE)
}