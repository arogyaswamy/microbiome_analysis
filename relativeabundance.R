relative.abundance = function(otu.table){
  # Given an feature table as a data.frame, this function returns a data.frame
  # containing relative abundances of those features (like OTUs) In the feature table, columns
  # should represent features and rows should represent samples.
  reads.per.sample = rowSums(otu.table)
  for(i in 1:length(names(otu.table))){
    for(j in 1:length(reads.per.sample)){
      otu.table[j,i] = otu.table[j,i] / reads.per.sample[j]
    }
  }
  return(otu.table)
}
