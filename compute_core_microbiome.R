  # Compute the core microbiome of a given OTU table, or a similar feature
  # table, at a given threshold, optionally filtered by some criteria based on a
  # metadata or mapping file
compute_core_microbiome = function(community.table, #Community data matrix/feature table
                                   threshold = 1, #Proportion of samples in which a taxon must be observed
                                   fields = NULL, #Column names from mapping file. Can be a vector.
                                   valid.states = NULL, #A vector containing valid states for the fields. If fields is a vector with more than one element, must be a list, where each item is a vector of valid states for the corresponding field.
                                   mapping.file = NULL,
                                   sample.name.col = "SampleName") { #The column from the mapping file containing sample names. The sample names should be the row names for the community data matrix
  # Validate threshold. If it's in (0,1], assume it's a fraction; if it's in
  # (1,100], assume it's a percentage; if it's numeric but > 100, assume it's an
  # actual number of samples. If for some reason it's not even a numeric,
  # chastise the user. If it's 0 or less, insult the user.
  if(class(threshold) != "numeric"){
    stop("The threshold needs to be a number, preferrably a proportion/fraction, but it could even be a percentage > 1%. Get with the program, my friend.")
  }else if(threshold > 100){
    warning("Threshold value is greater than 100, so it is treated as a minimum number of samples! Results may not reflect your assumptions!!")
    threshold = threshold / nrow(community.table)
  }else if(threshold > 1){
    warning("Threshold value is between 1 and 100, so it's treated as a percentage. Preferred usage is a proportion.")
    threshold = threshold / 100
  }else if(threshold == 0){
    stop("What? Why would you want to know which features found in your samples are found in none of your samples? :(")
  }else if(threshold < 0){
    stop("Whaaat? A negative number?? What does that even mean??")
  }else{
    # Congrats, you can follow instructions. Or you're just messing with the
    # system and used a complex number or something. Excellent use of time.
  }

  # Check that fields has the same length as the list of valid.states, if any is given
  if (!any(is.null(fields),
           is.null(valid.states),
           is.null(mapping.file))) {
    if (length(fields) != length(valid.states)) {
      warning("Arguments 'fields' and 'valid.states' must have the same length.")
      return(NULL)
    }
      # Subset OTU table according to criteria if given
    for (i in 1:length(fields)) {
      community.table = community.table[which(row.names(community.table) %in% getElement(mapping.file, sample.name.col)[which(getElement(mapping.file, fields[i]) %in% valid.states[i])]),]
    }
  }
  
  # This is to generate a named numeric vector of the right length. All of the
  # values will be changed in a later step
  times.observed = colSums(community.table)
  for(i in 1:length(times.observed)) {
    times.observed[i] = as.numeric(length(which(
      getElement(community.table, names(times.observed)[i]) == 0
    )))
    times.observed[i] = nrow(community.table) - times.observed[i]
  }
  
  # Convert threshold from proportion to minimum observation count
  threshold = ceiling(threshold * nrow(community.table))
  # Subset the observation count and convert it to a data.frame
  times.observed = times.observed[which(times.observed >= threshold)]
  core = data.frame(Taxon = names(times.observed), 
                    PresentInXSamples = times.observed)
  return(core)
}