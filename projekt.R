mydata <- read.table(text="
    at1   at2   res
1   a1     b1   t
2   a1     b1   t
3   a1     b2   t
4   a1     b3   f
5   a2     b2   t
6   a2     b2   f
7   a2     b2   f
8   a2     b3   f")

weather <- read.table(text="
    outlook   temperature humidity  wind    play
1   sunny     hot         high      normal  no
2   sunny     hot         high      high    no
3   overcast  hot         high      normal  yes
4   rainy     mild        high      normal  yes
5   rainy     cold        normal    normal  yes
6   rainy     cold        normal    high    no
7   overcast  cold        normal    high    yes
8   sunny     mild        high      normal  no
9   sunny     cold        normal    normal  yes
10  rainy     mild        normal    normal  yes
11  sunny     mild        normal    high    yes
12  overcast  mild        high      high    yes
13  overcast  hot         normal    normal  yes
14  rainy     mild        high      high    no")

weatherc <- read.table(text="
    outlook   temperature humidity  wind    play
1   sunny     27          80        normal  no
2   sunny     28          65        high    no
3   overcast  29          90        normal  yes
4   rainy     21          75        normal  yes
5   rainy     17          40        normal  yes
6   rainy     15          25        high    no
7   overcast  19          50        high    yes
8   sunny     22          95        normal  no
9   sunny     18          45        normal  yes
10  rainy     23          30        normal  yes
11  sunny     24          55        high    yes
12  overcast  25          70        high    yes
13  overcast  30          35        normal  yes
14  rainy     26          85        high    no")

weatherr <- read.table(text="
    outlook   temperature humidity  wind    playability
1   sunny     27          80        normal  0.48
2   sunny     28          65        high    0.46
3   overcast  29          90        normal  0.68
4   rainy     21          75        normal  0.52
5   rainy     17          40        normal  0.54
6   rainy     15          25        high    0.47
7   overcast  19          50        high    0.74
8   sunny     22          95        normal  0.49
9   sunny     18          45        normal  0.64
10  rainy     23          30        normal  0.55
11  sunny     24          55        high    0.57
12  overcast  25          70        high    0.68
13  overcast  30          35        normal  0.79
14  rainy     26          85        high    0.33")

aode <- function(formula, data) {
  
  # helper function
  inc <- function(x) {
    eval.parent(substitute(x <- x + 1))
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
  atatclcorel <- sapply( attrs, 
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
    for(val in (1:nlevels(data[[at]]))) {
      for (atn in attrs) {
        
        atatclcorel[[at]][[val]][[atn]] = 
          apply(atatclcorel[[at]][[val]][[atn]], 2, 
            function(x) {
              ifelse(x == 0, 0, x/sum(x)) 
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
        pxy   = sapply(atclcorel, function(x) {x/sum(x)}), 
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
        model$pxy[[i]][example[[i]]] * 
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

aodew <- aode(play~., weather)
prediction.aode(aodew, 1, weather)
