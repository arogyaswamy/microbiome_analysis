clean.dist = function(dm, ref){ omit = which(!names(as.data.frame(as.matrix(dm))) %in% ref$SampleName); fixed = as.dist(as.matrix(dm)[-omit,-omit]); return(fixed) }
