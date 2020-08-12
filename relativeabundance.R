relative.abundance = function(otu.table){
  # Given an OTU table as a data.frame, this function returns a data.frame
  # containing relative abundances of those OTUs. In the OTU table, columns
  # should represent OTUs and rows should represent samples.
  reads.per.sample = rowSums(otu.table)
  for(i in 1:length(names(otu.table))){
    for(j in 1:length(reads.per.sample)){
      otu.table[j,i] = otu.table[j,i] / reads.per.sample[j]
    }
  }
  return(otu.table)
}