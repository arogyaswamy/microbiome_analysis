clean_core_otu_file = function(target.file = "core_microbiomes/core_otus_100.txt"){
  # Read in QIIME generated core OTU profile
  core_otus = read.delim2(target.file, header=FALSE, comment.char="#")
  
  # Clean up taxa IDs
  core_otus$V2 = gsub("[][']", "", core_otus$V2)
  core_otus$V2 = gsub("([ ^]|^)u", "\\1", core_otus$V2)
  core_otus$V2 = gsub(",", ";", core_otus$V2)
  
  # Split IDs into taxonomic levels
  core.tax = split_assignments(core_otus$V2)
  
  # Clean up taxonomy labels
  for(i in 1:dim(core.tax)[2]){
    core.tax[,i] = as.character(core.tax[,i])
    for(j in 1:dim(core.tax)[1]){
      core.tax[j,i] = gsub("^.__", "", core.tax[j,i])
    }
    core.tax[,i] = factor(core.tax[,i])
  }
  
  # Reunite OTU IDs with taxa labels
  core.tax = cbind(OTU_ID = core_otus$V1, core.tax)
  return(core.tax)

}

convert_otu_table_to_rel_abundance = function(otu.table){
  divisors = rowSums(otu.table)
  for(i in 1:length(divisors)){
    otu.table[i,] = otu.table[i,] / divisors[i]
  }
  return(otu.table)
}

analyze_core_otu_table = function(taxa.table, taxa.rank = "Phylum", mapping){
  hits = taxa.table[,grep(taxa.rank, names(taxa.table))]
  pie = data.frame(Taxa = levels(hits), 
                   Value = NA)
  for(i in 1:length(pie$Taxa)){
    pie$Value[i] = length(which(hits == pie$Taxa[i]))
  }
  return(pie)
}