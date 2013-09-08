require("data.table")
require("ggplot2")
require("reshape2")

approaches <- c("pure_ff", "default")
domains <- c("satellite", "freecell")

domain_list <- c("zenotravel", "depots", "driverlog", "rover", "freecell", "satellite")

file_for_lower_inc_robust <- c("../csv/PISA/inc_robust/zenotravel@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/depots@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/driverlog@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/rover@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/freecell@lower@inc_robust.csv",
                               "../csv/PISA/inc_robust/satellite@lower@inc_robust.csv")

file_for_lower_annotations_free_ff <- c("../csv/PISA/annotations_free_ff_rp/zenotravel@lower@annotations_free_ff_rp.csv",
                                        "../csv/PISA/annotations_free_ff_rp/depots@lower@annotations_free_ff_rp.csv",
                                        "../csv/PISA/annotations_free_ff_rp/driverlog@lower@annotations_free_ff_rp.csv",
                                        "../csv/PISA/annotations_free_ff_rp/rover@lower@annotations_free_ff_rp.csv",
                                        "../csv/PISA/annotations_free_ff_rp/freecell@lower@annotations_free_ff_rp.csv",
                                        "../csv/PISA/annotations_free_ff_rp/satellite@lower@annotations_free_ff_rp.csv")

file_for_default <- c("../csv/DeFault/zenotravel@default.csv",
                      "../csv/DeFault/depots@default.csv",
                      "../csv/DeFault/rover@default.csv",
                      "../csv/DeFault/driverlog@default.csv",
                      "../csv/DeFault/freecell@default.csv",
                      "../csv/DeFault/satellite@default.csv")


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
num_max_robustness_instances_comparison <- function(domain_list,
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
    
#     d <- merge(robust_plans_first_table, robust_plans_second_table, all.x = TRUE, by=c("domain","problem"))
#     d[is.na(d)] <- 0
#     better_count = nrow(d[d$max_robustness.x > d$max_robustness.y])
#     #print(better_count)
#     
#     d <- merge(robust_plans_first_table, robust_plans_second_table, by=c("domain","problem"))
#     equal_count = nrow(d[d$max_robustness.x == d$max_robustness.y])
#     #print(equal_count)
#     
#     d <- merge(robust_plans_first_table, robust_plans_second_table, all.y = TRUE, by=c("domain","problem"))
#     d[is.na(d)] <- 0
#     worse_count = nrow(d[d$max_robustness.x < d$max_robustness.y])
#     #print(worse_count)
    
    # COUNTING BASED ONLY ON INSTANCES SOLVED BY BOTH THE TWO APPROACHES. USE WHEN WE DONT HAVE ALL INSTANCES.
    d <- merge(robust_plans_first_table, robust_plans_second_table, by=c("domain","problem"))
    better_count = nrow(d[d$max_robustness.x > d$max_robustness.y])
    #print(better_count)

    equal_count = nrow(d[d$max_robustness.x == d$max_robustness.y])
    #print(equal_count)
    
    worse_count = nrow(d[d$max_robustness.x < d$max_robustness.y])
    #print(worse_count)
    
    
    
    row_count <- row_count + 1
    summary_table[row_count,] <- list(domain_list[i],better_count, equal_count, worse_count)
  }
  
  write.csv(summary_table, output_file)
}

# # Compare the maximal robustness returned by two approaches
# max_robustness_comparison <- function(domain_list,
#                                     file_for_first_approach,
#                                     file_for_second_approach) {
#   # Number of domains
#   N <- length(domain_list)
#   
#   for (i in 1:N) {
#     # Read result files produced by each approach for this domain
#     first_table <- data.table(read.csv(file_for_first_approach[i], comment.char='#'))
#     second_table <- data.table(read.csv(file_for_second_approach[i], comment.char='#'))
#     
#     # Extract for each instance (incomplete domain + problem) the maximal robustness
#     max_robustness_first_table <- first_table[, list(max_robustness = max(plan_robustness)), by=list(domain, problem)]
#     max_robustness_second_table <- second_table[, list(max_robustness = max(plan_robustness)), by=list(domain, problem)]
#     
#     # Merge the two tables, only consider instances both approach produce plans
#     d <- merge(max_robustness_first_table, max_robustness_second_table, by=c("domain","problem"))
#     
#     
#     output <- paste(domain_list[i],"_MAX_ROBUST.csv",sep="")
#     write.csv(d, output)
#   }
# }


# Compare the number of plans returned by two approaches
num_plans_comparison <- function(domain_list,
                                      file_for_first_approach,
                                      file_for_second_approach, output_file) {
  # Number of domains
  N <- length(domain_list)
  
  summary_table <- data.table(domain=rep("", N), better=rep(0, N),
                              equal=rep(0, N), worse=rep(0, N))
  row_count <- 0
  
  for (i in 1:N) {
    # Read result files produced by each approach for this domain
    first_table <- data.table(read.csv(file_for_first_approach[i], comment.char='#'))
    second_table <- data.table(read.csv(file_for_second_approach[i], comment.char='#'))
    
    # Extract for each instance (incomplete domain + problem) the maximal robustness
    num_plans_first_table <- first_table[, list(num_plans = max(plan_id)), by=list(domain, problem)]
    num_plans_second_table <- second_table[, list(num_plans = max(plan_id)), by=list(domain, problem)]
    
    # Merge the two tables, only consider instances for which both approaches produce plans
    d <- merge(num_plans_first_table, num_plans_second_table, all.x = TRUE, by=c("domain","problem"))
    d[is.na(d)] <- 0
    better_count = nrow(d[d$num_plans.x > d$num_plans.y])    
    d <- merge(num_plans_first_table, num_plans_second_table, by=c("domain","problem"))
    equal_count = nrow(d[d$num_plans.x == d$num_plans.y])
    d <- merge(num_plans_first_table, num_plans_second_table, all.y = TRUE, by=c("domain","problem"))
    d[is.na(d)] <- 0
    worse_count = nrow(d[d$num_plans.x < d$num_plans.y])
    
    
#     d <- merge(num_plans_first_table, num_plans_second_table, by=c("domain","problem"))
#     better_count = nrow(d[d$num_plans.x > d$num_plans.y])    
#     equal_count = nrow(d[d$num_plans.x == d$num_plans.y])
#     worse_count = nrow(d[d$num_plans.x < d$num_plans.y])
    
    row_count <- row_count + 1
    summary_table[row_count,] <- list(domain_list[i],better_count, equal_count, worse_count)
  }
  
  write.csv(summary_table, output_file)
  
}

mean_plan_length_comparison <- function(domain_list,
                                 file_for_first_approach,
                                 file_for_second_approach, output_file) {
  # Number of domains
  N <- length(domain_list)
  
  summary_table <- data.table(domain=rep("", N), shorter=rep(0, N),
                              equal=rep(0, N), longer=rep(0, N))
  row_count <- 0
  
  for (i in 1:N) {
    # Read result files produced by each approach for this domain
    first_table <- data.table(read.csv(file_for_first_approach[i], comment.char='#'))
    second_table <- data.table(read.csv(file_for_second_approach[i], comment.char='#'))
    
    # Extract for each instance (incomplete domain + problem) the maximal robustness
    mean_plan_length_first_table <- first_table[, list(mean_plan_length = mean(plan_length)), by=list(domain, problem)]
    mean_plan_length_second_table <- second_table[, list(mean_plan_length = mean(plan_length)), by=list(domain, problem)]
    
    # Merge the two tables, only consider instances for which both approaches produce plans
    d <- merge(mean_plan_length_first_table, mean_plan_length_second_table, by=c("domain","problem"))
    shorter_count = nrow(d[d$mean_plan_length.x < d$mean_plan_length.y])    
    equal_count = nrow(d[d$mean_plan_length.x == d$mean_plan_length.y])
    longer_count = nrow(d[d$mean_plan_length.x > d$mean_plan_length.y])
    
    row_count <- row_count + 1
    summary_table[row_count,] <- list(domain_list[i],shorter_count, equal_count, longer_count)
    
    #out <- paste("mean_plan_length@", domain_list[i],sep="")
    #write.csv(d, out)
  }
  
  write.csv(summary_table, output_file)
  
}


length_of_equal_robustness_comparison <- function(domain_list,
                                        file_for_first_approach,
                                        file_for_second_approach, output_file) {
  # Number of domains
  N <- length(domain_list)
  
  summary_table <- data.table(domain=rep("", N), shorter=rep(0, N),
                              equal=rep(0, N), longer=rep(0, N))
  row_count <- 0
  
  for (i in 1:N) {
    # Read result files produced by each approach for this domain
    first_table <- data.table(read.csv(file_for_first_approach[i], comment.char='#'))
    second_table <- data.table(read.csv(file_for_second_approach[i], comment.char='#'))
    
    # Extract for each instance (incomplete domain + problem) the maximal robustness
    mean_plan_length_first_table <- first_table[, list(mean_plan_length = mean(plan_length)), by=list(domain, problem)]
    mean_plan_length_second_table <- second_table[, list(mean_plan_length = mean(plan_length)), by=list(domain, problem)]
    
    # Merge the two tables, only consider instances for which both approaches produce plans
    d <- merge(mean_plan_length_first_table, mean_plan_length_second_table, by=c("domain","problem"))
    shorter_count = nrow(d[d$mean_plan_length.x < d$mean_plan_length.y])    
    equal_count = nrow(d[d$mean_plan_length.x == d$mean_plan_length.y])
    longer_count = nrow(d[d$mean_plan_length.x > d$mean_plan_length.y])
    
    row_count <- row_count + 1
    summary_table[row_count,] <- list(domain_list[i],shorter_count, equal_count, longer_count)
    
    #out <- paste("mean_plan_length@", domain_list[i],sep="")
    #write.csv(d, out)
  }
  
  write.csv(summary_table, output_file)
  
}

time_for_first_plan_comparison <- function(domain_list,
                                           file_for_first_approach,
                                           file_for_second_approach, output_file) {
  # Number of domains
  N <- length(domain_list)
  
  summary_table <- data.table(domain=rep("", N), faster=rep(0, N),
                              equal=rep(0, N), slower=rep(0, N))
  row_count <- 0
  
  for (i in 1:N) {
    # Read result files produced by each approach for this domain
    first_table <- data.table(read.csv(file_for_first_approach[i], comment.char='#'))
    second_table <- data.table(read.csv(file_for_second_approach[i], comment.char='#'))
    
    # Extract for each instance (incomplete domain + problem) the maximal robustness
    time_first_plan_first_table <- first_table[, list(time_first_plan = min(total_time)), by=list(domain, problem)]
    time_first_plan_second_table <- second_table[, list(time_first_plan = min(total_time)), by=list(domain, problem)]
    
    # Merge the two tables, only consider instances for which both approaches produce plans
    d <- merge(time_first_plan_first_table, time_first_plan_second_table, by=c("domain","problem"))
    faster_count = nrow(d[d$time_first_plan.x < d$time_first_plan.y])    
    equal_count = nrow(d[d$time_first_plan.x == d$time_first_plan.y])
    slower_count = nrow(d[d$time_first_plan.x > d$time_first_plan.y])
    
    row_count <- row_count + 1
    summary_table[row_count,] <- list(domain_list[i],faster_count, equal_count, slower_count)
    
    #out <- paste("mean_plan_length@", domain_list[i],sep="")
    #write.csv(d, out)
  }
  
  write.csv(summary_table, output_file)
  
}

time_for_last_plan_comparison <- function(domain_list,
                                           file_for_first_approach,
                                           file_for_second_approach, output_file) {
  # Number of domains
  N <- length(domain_list)
  
  summary_table <- data.table(domain=rep("", N), faster=rep(0, N),
                              equal=rep(0, N), slower=rep(0, N))
  row_count <- 0
  
  for (i in 1:N) {
    # Read result files produced by each approach for this domain
    first_table <- data.table(read.csv(file_for_first_approach[i], comment.char='#'))
    second_table <- data.table(read.csv(file_for_second_approach[i], comment.char='#'))
    
    # Extract for each instance (incomplete domain + problem) the maximal robustness
    time_last_plan_first_table <- first_table[, list(time_last_plan = max(total_time)), by=list(domain, problem)]
    time_last_plan_second_table <- second_table[, list(time_last_plan = max(total_time)), by=list(domain, problem)]
    
    # Merge the two tables, only consider instances for which both approaches produce plans
    d <- merge(time_last_plan_first_table, time_last_plan_second_table, by=c("domain","problem"))
    faster_count = nrow(d[d$time_last_plan.x < d$time_last_plan.y])    
    equal_count = nrow(d[d$time_last_plan.x == d$time_last_plan.y])
    slower_count = nrow(d[d$time_last_plan.x > d$time_last_plan.y])
    
    row_count <- row_count + 1
    summary_table[row_count,] <- list(domain_list[i],faster_count, equal_count, slower_count)
    
    #out <- paste("mean_plan_length@", domain_list[i],sep="")
    #write.csv(d, out)
  }
  
  write.csv(summary_table, output_file)
  
}

mean_time_comparison <- function(domain_list,
                                          file_for_first_approach,
                                          file_for_second_approach, output_file) {
  # Number of domains
  N <- length(domain_list)
  
  summary_table <- data.table(domain=rep("", N), faster=rep(0, N),
                              equal=rep(0, N), slower=rep(0, N))
  row_count <- 0
  
  for (i in 1:N) {
    # Read result files produced by each approach for this domain
    first_table <- data.table(read.csv(file_for_first_approach[i], comment.char='#'))
    second_table <- data.table(read.csv(file_for_second_approach[i], comment.char='#'))
    
    # Extract for each instance (incomplete domain + problem) the maximal robustness
    mean_time_first_table <- first_table[, list(mean_time = mean(total_time)), by=list(domain, problem)]
    mean_time_second_table <- second_table[, list(mean_time = mean(total_time)), by=list(domain, problem)]
    
    # Merge the two tables, only consider instances for which both approaches produce plans
    d <- merge(mean_time_first_table, mean_time_second_table, by=c("domain","problem"))
    faster_count = nrow(d[d$mean_time.x < d$mean_time.y])    
    equal_count = nrow(d[d$mean_time.x == d$mean_time.y])
    slower_count = nrow(d[d$mean_time.x > d$mean_time.y])
    
    row_count <- row_count + 1
    summary_table[row_count,] <- list(domain_list[i],faster_count, equal_count, slower_count)
    
    #out <- paste("mean_plan_length@", domain_list[i],sep="")
    #write.csv(d, out)
  }
  
  write.csv(summary_table, output_file)
  
}

#num_max_robustness_instances_comparison(domain_list[1:4], file_for_lower_annotations_free_ff[1:4], file_for_lower_inc_robust[1:4], "../summary_tables/num_max_robustness_instances@lower_annotations_free_ff_VS_inc_robust.csv")

#num_max_robustness_instances_comparison(domain_list[5:6], file_for_default[5:6], file_for_lower_inc_robust[5:6], "../summary_tables/num_max_robustness_instances@default_VS_lower_inc_robust.csv")

num_max_robustness_instances_comparison(domain_list[5:6], file_for_default[5:6], file_for_lower_annotations_free_ff[5:6], "../summary_tables/num_max_robustness_instances@default_VS_lower_annotations_free_ff.csv")

#num_plans_comparison(domain_list[5:6], file_for_default[5:6], file_for_lower_inc_robust[5:6], "../summary_tables/num_plans@default_VS_inc_robust.csv")

#num_plans_comparison(domain_list[1:4], file_for_lower_annotations_free_ff[1:4], file_for_lower_inc_robust[1:4],"../summary_tables/num_plans@lower_annotations_free_ff_VS_inc_robust.csv")

#mean_plan_length_comparison(domain_list[5:6], file_for_default[5:6], file_for_lower_inc_robust[5:6], "../summary_tables/mean_plan_length@default_VS_lower_inc_robust.csv")

#mean_plan_length_comparison(domain_list[1:4], file_for_lower_annotations_free_ff[1:4], file_for_lower_inc_robust[1:4], "../summary_tables/mean_plan_length@lower_annotations_free_ff_VSr_inc_robust.csv")

#time_for_first_plan_comparison(domain_list[5:6], file_for_default[5:6], file_for_lower_inc_robust[5:6], "../summary_tables/time_first_plan@default_VS_lower_inc_robust.csv")

#time_for_last_plan_comparison(domain_list[5:6], file_for_default[5:6], file_for_lower_inc_robust[5:6], "../summary_tables/time_last_plan@default_VS_lower_inc_robust.csv")

#time_for_first_plan_comparison(domain_list[1:4], file_for_lower_annotations_free_ff[1:4], file_for_lower_inc_robust[1:4], "../summary_tables/time_first_plan@lower_annotations_free_ff_VS_inc_robust.csv")

#time_for_last_plan_comparison(domain_list[1:4], file_for_lower_annotations_free_ff[1:4], file_for_lower_inc_robust[1:4], "../summary_tables/time_last_plan@lower_annotations_free_ff_VS_inc_robust.csv")

mean_time_comparison(domain_list[5:6], file_for_default[5:6], file_for_lower_inc_robust[5:6], "../summary_tables/mean_time@default_VS_lower_inc_robust.csv")

mean_time_comparison(domain_list[1:4], file_for_lower_annotations_free_ff[1:4], file_for_lower_inc_robust[1:4], "../summary_tables/mean_time@lower_annotations_free_ff_VS_inc_robust.csv")
