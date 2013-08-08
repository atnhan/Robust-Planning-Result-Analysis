require("data.table")

approaches <- c("pure_ff", "default")
domains <- c("satellite", "freecell")

# Get list of incomplete domains for a STRIPS domain
# Example: zenotravel --> "zenotravel.pddl.0p_0a_1d.0" etc
get_incomplete_domains <- function(domain) {
  num_ppres <- 6
  num_padds <- 6
  num_pdels <- 6
  
  prefix <- paste(domain,"pddl",sep=".")
  result <- rep(prefix, num_ppres * num_padds * num_pdels - 1)
  index <- 1
  for (p in 0:(num_ppres-1))
    for (a in 0:(num_padds-1))
      for (d in 0:(num_pdels-1)) {
        if (p + a + d > 0) {
          s <- paste(as.character(p),"p", sep="");
          s <- paste(s, paste(as.character(a),"a", sep=""), sep="_");
          s <- paste(s, paste(as.character(d),"d", sep=""), sep="_");
          result[index] <- paste(result[index], s, sep=".")
          result[index] <- paste(result[index], "0", sep=".")
          index <- index + 1
        }
      }
  return (result)
}

get_problems <- function() {
  num_probs <- 10
  prefix <- "pfile"
  result <- rep(prefix, num_probs)
  for (i in 1:num_probs) {
    result[i] <- paste(result[i],as.character(i),sep="")
  }
  return (result)
}

freecell_lower_pure_ff <- data.table(read.csv("../csv/PISA/pure_ff/freecell@lower@pure_ff.csv", comment.char='#'))
freecell_default <- data.table(read.csv("../csv/DeFault//freecell@default.csv", comment.char='#'))

# Maximum robust plans for each instances
robust_plans_freecell_lower_pure_ff <- freecell_lower_pure_ff[, list(max_robustness = max(plan_robustness)), by=list(domain, problem)]
robust_plans_freecell_default <- freecell_default[, list(max_robustness = max(plan_robustness)), by=list(domain, problem)]

# Maximum robust plans returned by two approaches
robust_plans_freecell_lower_pure_ff_AND_default <- merge(robust_plans_freecell_lower_pure_ff, robust_plans_freecell_default, 
                                                         by=c("domain","problem"))

robustness_by_lower_pure_ff <- robust_plans_freecell_lower_pure_ff_AND_default[robust_plans_freecell_lower_pure_ff_AND_default$max_robustness.x]

plans_comparison <- function() {
  
}


