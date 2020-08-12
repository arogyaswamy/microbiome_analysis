# Script to read taxa-by-sample files from Microbiome Analyst output
ingest_taxa = function(file_name){
  # Read file
  taxtable = read.csv(file_name)
  # Convert sample names to upper case if needed
  names(taxtable) = toupper(names(taxtable))
  # Add a level to the taxonomy column factor and then specify the last row as
  # "_Unknown_" because R won't allow NA values in row or column names
  levels(taxtable$X) = c(levels(taxtable$X), "_Unknown_")
  taxtable$X[dim(taxtable)[1]] = "_Unknown_"
  row.names(taxtable) = taxtable$X
  # Remove taxonomy column since it is redundant with row.names
  taxtable$X = NULL
  # Transpose table
  taxtable = as.data.frame(t(taxtable))
  # Promote the sample names to their own column so that it can be merged with
  # other tables (e.g., alphadiv, mapping files)
  taxtable = cbind(SampleName = row.names(taxtable), taxtable)
  
  return(taxtable)
  }

phyla = ingest_taxa("phyla_by_sample.csv")
classes = ingest_taxa("classes_by_sample.csv")
orders = ingest_taxa("orders_by_sample.csv")