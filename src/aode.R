aode <- function(formula, data) {
  
  # HELPER FUNCTIONS
  
  # incrementator
  inc <- function(x) {
    eval.parent(substitute(x <- x + 1))
  }
  
  # m-estimation
  mest <- function(v, m, p0=1/m) { 
    if(m == 0) {
      ifelse(v == 0, 0, v/sum(v))
    } else {
      (v + m * p0)/(sum(v) + m)
    }
  }

  #MAIN
  
  #class
  cl <- all.vars(formula[[2]])

  #attributes
  attrs <- attributes(data[, names(data) != cl])$names

  #initialize attribute-value counts
  valrank <- sapply(attrs, 
                function(at)
                  array(0, dim=c(nlevels(data[[at]])), dimnames=list(levels(data[[at]])))
              )
  
  #initialize (attribute-value, class) function
  atclfun <- function() {
              sapply(attrs, 
                function(at) {
                  array(0, dim=c(nlevels(data[[at]]), nlevels(data[[cl]])), 
                    dimnames=list(levels(data[[at]]), levels(data[[cl]])))
                }
              )
            }
  
  #initialize (attribute-value, class) counts   
  atclcorel <- atclfun()
  
  #initialize (attribute-value, attribute-value, class) counts
  #atatclcorel[[attribute]][[value]][[attribute]][value, class]
  atatclcorel <- sapply(attrs, 
                function(at) {
                  array(list(atclfun()), dim=c(nlevels(data[[at]])), dimnames=list(levels(data[[at]])))
                }
              )
            
  #fill arrays
  for (irow in (1:nrow(data))) {
    for (at in attrs) {
      
      val <- data[[at]][irow]
      cla <- data[[cl]][irow]
      
      inc(valrank[[at]][val])
      inc(atclcorel[[at]][val, cla])
      
      for (atn in attrs) {
      
        valn <- data[[atn]][irow]
        
        inc(atatclcorel[[at]][[val]][[atn]][valn, cla])
      }
    }
  }  
  
  #probabilities
  for (at in attrs) {
    nvalat <- nlevels(data[[at]])
    
    for(val in 1:nvalat) {
      for (atn in attrs) {
        
        atatclcorel[[at]][[val]][[atn]] = 
          apply(atatclcorel[[at]][[val]][[atn]], 2, 
            function(x) {
              mest(x, nlevels(data[[atn]]))
            }
          )
      }
    }
  }
    
  #models attributes
  `class<-`(list(
        attributes  = attrs, 
        class       = cl, 
        atcounter    = valrank, 
        pxy   = sapply(atclcorel, function(x) mest(x, prod(dim(x))))  , 
        pxxy  = atatclcorel), 
      "aode")
}


predict.aode <- function(model, m, example) {
  #all attributes
  allattrs  <- names(example) %in% names(model$pxxy)
  #valuable attributes
  mattrs    <- sapply(model$attr, function(x) { model$atcounter[[x]][example[[x]]] > m})
  
  #all indexes
  allindexes  <- 1:length(model$attr)
  #valuable indexes
  mindexes    <- allindexes[mattrs]
  
  resclass <- apply(
    mapply(
      function(i) {
        pxxc <- model$pxxy[[i]][[example[[i]]]]
        model$pxy[[i]][example[[i]],] * 
          apply(
            mapply(
              function(a, v)
                pxxc[[a]][v,],
              allindexes, 
              example[allattrs]
            ), 
            1, 
            prod
          )
        }, 
      mindexes
    ), 
    1, 
    sum
  )
  
  resclass/sum(resclass)
}

prediction.aode <- function(model, m, data) {
  t(sapply(1:nrow(data), function(i) predict.aode(model, m, data[i,])))
}
