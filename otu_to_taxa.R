otu.to.taxonomy = function(otu.table, tax.table, tax.rank = "Phylum"){
  # Produces a data.frame of the observations at a taxonomic rank, given a feature
  # table and a feature-to-taxonomy map
  sample.list = sort(row.names(otu.table))
  taxa = unique(as.character(tax.table[,tax.rank]))
  observations = data.frame(SampleID = sample.list)
  
  for(i in 1:length(taxa)){
    otus.for.curr.tax = row.names(tax.table)[which(tax.table[,tax.rank] == taxa[i])]
    my.subset = as.data.frame(otu.table[,which(names(otu.table) %in% otus.for.curr.tax)])
    #curr.sums = rowSums(my.subset)
    curr.sums = data.frame(SampleID = observations$SampleID, Taxon = rowSums(my.subset))
    names(curr.sums)[2] = taxa[i]
    observations = merge(observations, curr.sums)
  }
  row.names(observations) = observations$SampleID
  observations$SampleID = NULL
  return(observations)
}