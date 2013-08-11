require("data.table")
require("ggplot2")
require("reshape2")

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

# Input:
# + domain_list: list of domain names. Ex: domain_list <- c("zenotravel", "depots", "rover")
# + file_for_first_approach: csv files produced by the first approach for each domain above.
# Example: file_for_first_approach <- c("../csv/PISA/inc_robust/zenotravel@lower@inc_robust.csv",
#                                       "../csv/PISA/inc_robust/depots@lower@inc_robust.csv",
#                                       "../csv/PISA/inc_robust/rover@lower@inc_robust.csv")
# + file_for_second_approach: csv files produced by the second approach for each domain above.
# Example: file_for_second_approach <- c("../csv/PISA/pure_ff/zenotravel@lower@pure_ff.csv",
#                                        "../csv/PISA/pure_ff/depots@lower@pure_ff.csv",
#                                        "../csv/PISA/pure_ff/rover@lower@pure_ff.csv")
# Note: the two file list must have the same length
# + ouput_file: csv output file
robust_plans_comparison <- function(domain_list,
                                    file_for_first_approach,
                                    file_for_second_approach,
                                    output_file) {
  N <- length(domain_list)
  
  summary_table <- data.table(domain=rep("", N), better=rep(0, N),
                              equal=rep(0, N), worse=rep(0, N))
  row_count <- 0
  
  for (i in 1:length(file_for_first_approach)) {
    first_table <- data.table(read.csv(file_for_first_approach[i], comment.char='#'))
    second_table <- data.table(read.csv(file_for_second_approach[i], comment.char='#'))
    
    robust_plans_first_table <- first_table[, list(max_robustness = max(plan_robustness)), by=list(domain, problem)]
    robust_plans_second_table <- second_table[, list(max_robustness = max(plan_robustness)), by=list(domain, problem)]
    
    d <- merge(robust_plans_first_table, robust_plans_second_table, all.x = TRUE, by=c("domain","problem"))
    d[is.na(d)] <- 0
    better_count = nrow(d[d$max_robustness.x > d$max_robustness.y])
    #print(better_count)
    
    d <- merge(robust_plans_first_table, robust_plans_second_table, by=c("domain","problem"))
    equal_count = nrow(d[d$max_robustness.x == d$max_robustness.y])
    #print(equal_count)
    
    d <- merge(robust_plans_first_table, robust_plans_second_table, all.y = TRUE, by=c("domain","problem"))
    d[is.na(d)] <- 0
    worse_count = nrow(d[d$max_robustness.x < d$max_robustness.y])
    #print(worse_count)
    
    row_count <- row_count + 1
    summary_table[row_count,] <- list(domain_list[i],better_count, equal_count, worse_count)
  }
  
  write.csv(summary_table, output_file)
}

domain_list <- c("zenotravel", "depots", "rover", "driverlog", "freecell", "satellite")

file_for_lower_inc_robust <- c("../csv/PISA/inc_robust/zenotravel@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/depots@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/rover@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/driverlog@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/freecell@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/satellite@lower@inc_robust.csv")

file_for_lower_pure_ff <- c("../csv/PISA/pure_ff/zenotravel@lower@pure_ff.csv",
                            "../csv/PISA/pure_ff/depots@lower@pure_ff.csv",
                            "../csv/PISA/pure_ff/rover@lower@pure_ff.csv",
                            "../csv/PISA/pure_ff/driverlog@lower@pure_ff.csv",
                            "../csv/PISA/pure_ff/freecell@lower@pure_ff.csv",
                            "../csv/PISA/pure_ff/satellite@lower@pure_ff.csv")

file_for_upper_inc_robust <- c("../csv/PISA/inc_robust/zenotravel@upper@inc_robust.csv",
                               "../csv/PISA/inc_robust/depots@upper@inc_robust.csv",
                               "../csv/PISA/inc_robust/rover@upper@inc_robust.csv",
                               "../csv/PISA/inc_robust/driverlog@upper@inc_robust.csv",
                               "../csv/PISA/inc_robust/freecell@upper@inc_robust.csv",
                               "../csv/PISA/inc_robust/satellite@upper@inc_robust.csv")

file_for_upper_pure_ff <- c("../csv/PISA/pure_ff/zenotravel@upper@pure_ff.csv",
                            "../csv/PISA/pure_ff/depots@upper@pure_ff.csv",
                            "../csv/PISA/pure_ff/rover@upper@pure_ff.csv",
                            "../csv/PISA/pure_ff/driverlog@upper@pure_ff.csv",
                            "../csv/PISA/pure_ff/freecell@upper@pure_ff.csv",
                            "../csv/PISA/pure_ff/satellite@upper@pure_ff.csv")

file_for_default <- c("../csv/DeFault/zenotravel@default.csv",
                      "../csv/DeFault/depots@default.csv",
                      "../csv/DeFault/rover@default.csv",
                      "../csv/DeFault/driverlog@default.csv",
                      "../csv/DeFault/freecell@default.csv",
                      "../csv/DeFault/satellite@default.csv")

robust_plans_comparison(domain_list[1:4], file_for_lower_inc_robust[1:4], file_for_lower_pure_ff[1:4], "../summary_tables/lower_inc_robust_VS_pure_ff.csv")

robust_plans_comparison(domain_list[1:4], file_for_upper_inc_robust[1:4], file_for_upper_pure_ff[1:4], "../summary_tables/upper_inc_robust_VS_pure_ff.csv")

robust_plans_comparison(domain_list[5:6], file_for_default[5:6], file_for_upper_pure_ff[5:6], "../summary_tables/default_vs_upper_pure_ff.csv")

robust_plans_comparison(domain_list[5:6], file_for_default[5:6], file_for_lower_pure_ff[5:6], "../summary_tables/default_vs_lower_pure_ff.csv")





