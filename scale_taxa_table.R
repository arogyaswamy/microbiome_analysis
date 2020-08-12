scale_taxa_table = function(taxatable){
  totalcolumn = numeric(length(taxatable$SampleName))
  for(i in 1:length(totalcolumn)){
    totalcolumn[i] = sum(taxatable[i,-1])
  }
  taxatable$Total = totalcolumn
  scaledtable = cbind(SampleName = taxatable$SampleName, (taxatable[,2:(length(names(taxatable)) - 1)] / taxatable$Total))
  rownames(scaledtable) = scaledtable$SampleName
  names(scaledtable)[grep("_Unknown_", names(scaledtable))] = "Other"
  scaledtable$SampleName = NULL
  return(scaledtable)
}