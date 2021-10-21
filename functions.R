#Basic simulation functions
library(grid)
library(dplyr)
library(data.table)
plot_strat_overtime = function( compartment,df_health_workers, df_normal,df_baseline,this_time) {
  p <- ggplot(df, aes(x = time, y = percent)) +
    theme_minimal(base_size = 12) + 
    annotate("rect", xmin=0, xmax=this_time, ymin=0, ymax=Inf, alpha=0.5, fill= "#dddddd") +
    
    geom_line(aes(color = strat), size = 0.01) +
    geom_line(data = df_baseline, aes(x = time, y = percent), col = "#4F4F4F", size = 0.5, 
              linetype = "dashed")+ 
    xlab("Time (days)") +
    scale_x_continuous(expand = c(0,0), limit = c(0, 350)) +#, breaks = c(0, 100, 200, 300)) +
    scale_fill_manual(name = "Allocation Strategy",
                      values = c ("health_workers"="#FC9272", "normal"="#74C476")) +
    theme(legend.position = "none")  
  if (compartment == "I") {
    ymax <- 50000
    p <- p + ylab("\nInfected (n)")
  } else if (compartment == "R") {
    ymax <- 60
    p <- p + ylab("Cumulative\nincidence (%)")
  } else if (compartment == "D") {
    ymax <- 400
    p <- p + ylab("Cumulative\nmortality (n)") 
  }
  p <- p + scale_y_continuous(expand = c(0,0), limit = c(0, ymax)) + 
    coord_fixed(350*5/(6*ymax))+
    geom_area(data=df_normal, aes(x=time, y=percent, fill = "normal"), alpha=0.8)+
    geom_area(data=df_health_workers, aes(x=time, y=percent, fill = "health_workers"), alpha=0.9)
}
plot_over_vax_avail = function(outcome, var = FALSE){
  library(ggplot2)
  theme_set(theme_minimal(base_size = 12))
  x_normal_switch     <- when_strat_switch(list_normal, 1:9)
  x_health_workers_switch       <- when_strat_switch(list_health_workers, 10)
  # get dataframe for specific outcome
  if (var){
    df <- get_reduction_df_var(outcome)
    
    x_normal_switch     <- when_strat_switch(list_normal, 1:9)
    x_health_workers_switch       <- when_strat_switch(list_health_workers, 10)
  } else {df <- get_reduction_df(outcome)}
  
  points_x <- c(x_health_workers_switch)
  points_y <- c(df[df$strat == "health_workers" & df$vax_avail == x_health_workers_switch & df$variable == "constant", ]$reduction)
  if (x_normal_switch > 0){
    points_x <- c(points_x, x_normal_switch)
    points_y <- c(points_y, df[df$strat == "normal" & df$vax_avail == x_normal_switch & df$variable == "constant", ]$reduction)
  }
  if (var){
    points_x <- c(points_x, x_health_workers_switch_var)
    points_y <- c(points_y, df[df$strat == "health_workers" & df$vax_avail == x_health_workers_switch_var & df$variable == "var", ]$reduction)
    if (x_normal_switch_var > 0){
      points_x <- c(points_x, x_normal_switch_var)
      points_y <- c(points_y, df[df$strat == "normal" & df$vax_avail == x_normal_switch_var & df$variable == "var", ]$reduction)
    }
  }
  df_var <- df[df$variable == "var", ]
  df_const <- df[df$variable == "constant", ]
  p <- ggplot() +
    geom_line(aes(x = df_const$vax_avail, y = df_const$reduction, col = df_const$strat),
              size = 0.5, alpha = 0.9) +
    geom_line(aes(x = df_var$vax_avail, y = df_var$reduction, col = df_var$strat), 
              linetype="dashed",size = 1, alpha = 0.9) +
    xlab("Total vaccine supply (% of population)") +
    scale_fill_manual(name = "Allocation Strategy",
                      values = c ("health_workers"="#FC9272", "normal"="#74C476")) +
    scale_y_continuous(expand = c(0,0), limit = c(0, 75), breaks = c(0, 25, 50, 75)) +
    scale_x_continuous(expand = c(0,0), limit = c(0, 50)) +#, breaks = c(0,25,50)) +
    coord_fixed(50*4/(5*75)) +
    theme(legend.position = "none") +
    guides(colour = guide_legend(override.aes = list(size=3)))
  p <- p + geom_point(aes(x = points_x, y = points_y), size = 1) 
  if (outcome == "cases"){ p <- p + ylab("Reduction in\ninfections (%)")}
  else if (outcome == "deaths"){ p <- p + ylab("Reduction in\ndeaths (%)")}
  return(p)
}
when_strat_switch = function(list_df, groups){
  other_groups <- 1:11
  other_groups <- other_groups[!other_groups %in% groups]
  x_switch <- -1
  for (i in 1:length(list_df)){
    temp <- list_df[[i]]
    if (any(temp[dim(temp)[1], other_groups + 10] > 0)){
      x_switch <- i-1 
      break
    }
  }
  return(x_switch)
}
get_v_e = function(p, y0, hinge_age){
  x <- c(0, 10, 20, 30, 40, 50, 60, 70, 80,60,60)
  y <- y0 + ((p-y0)/(80-hinge_age))*(x - hinge_age)
  index <- match(c(hinge_age), x)
  y[1:index] <- y0
  
  y
}
scale_u_for_R0 = function(u, C, wanted_R0){
  scalehigh <- 10
  scalelow <- 100
  R0_high <- compute_R0(u/scalehigh, C)
  R0_low <- compute_R0(u/scalelow, C)
  if (R0_high < wanted_R0 || R0_low > wanted_R0){
    print("Error: wanted_R0 is not in the original range of R0 values")
    return(-1)
  }
  running = TRUE
  while(running){
    scale_next = (scalehigh + scalelow)/2
    temp <- compute_R0(u/scale_next, C)
    if (temp > wanted_R0){
      scalehigh <- scale_next
    } else {
      scalelow <- scale_next
    }
    if (abs(temp - wanted_R0) < 0.0001){
      return(scale_next)
    }
  }
}
compute_R0 = function(u, C){
  gamma <- 1/5 
  Du <- diag(u, 11)
  Dy <- diag(1/gamma, 11)
  NGM <- Du %*% C %*% Dy
  R0  <- abs(eigen(NGM)$values[1])
}
calculate_derivatives = function(t, x, parameters){
  num_compartment <- 6
  num_groups <- (length(x)-1)/num_compartment
  S    <- as.matrix(x[1:num_groups])
  Sv   <- as.matrix(x[(1*num_groups+1):(2*num_groups)])
  E    <- as.matrix(x[(2*num_groups+1):(3*num_groups)])
  I    <- as.matrix(x[(3*num_groups+1):(4*num_groups)])
  R    <- as.matrix(x[(4*num_groups+1):(5*num_groups)])
  D    <- as.matrix(x[(5*num_groups+1):(6*num_groups)])
  vax_supply <- x[6*num_groups+1]
  S[S < .Machine$double.eps] <- 0
  Sv[Sv < .Machine$double.eps] <- 0
  E[E < .Machine$double.eps] <- 0
  I[I < .Machine$double.eps] <- 0
  R[R < .Machine$double.eps] <- 0
  D[D < .Machine$double.eps] <- 0
  u <- parameters$u
  C <- parameters$C
  d_E <- parameters$d_E
  d_I <- parameters$d_I
  v_e <- parameters$v_e
  v_e_type <- parameters$v_e_type
  
  lambda <- C%*%(I/(N_i-D))*u
  
  if (v_e_type == "aorn") {
    # all-or-nothing
    dSv <- rep(0, num_groups)
  }
  dS <- -(S*lambda)
  dE  <- S*lambda - d_E*E
  dI  <- E*d_E - I*d_I
  dR  <- I*d_I*(1-IFR)
  dD  <- I*d_I*IFR
  dvax_supply <- 0
  out <- c(dS,dSv,dE,dI,dR,dD,dvax_supply)
  list(out)
}
move_vaccinated_event <- function(t, x, parms){
  v_e <- parms$v_e
  v_e_type <- parms$v_e_type
  num_perday <- parms$num_perday
  vax_proportion <- parms$vax_proportion
  groups <- parms$groups
  pop_total <- parms$pop_total
  num_compartment <- 6
  num_groups <- (length(x)-1)/num_compartment
  S    <- as.matrix(x[1:num_groups])
  Sv   <- as.matrix(x[(1*num_groups+1):(2*num_groups)])
  E    <- as.matrix(x[(2*num_groups+1):(3*num_groups)])
  I    <- as.matrix(x[(3*num_groups+1):(4*num_groups)])
  R    <- as.matrix(x[(4*num_groups+1):(5*num_groups)])
  D    <- as.matrix(x[(5*num_groups+1):(6*num_groups)])
  vax_supply <- x[6*num_groups+1]
  if (vax_supply >= num_perday*pop_total){
    nvax <- num_perday*pop_total
    vax_supply <- vax_supply - num_perday*pop_total
  } else {
    nvax <- vax_supply
    vax_supply <- 0
  }
  vax_distribution <- nvax*vax_proportion
  vax_eligible <- S
  if (any(vax_distribution > vax_eligible)){ 
    # make sure everyone in the specificed age groups are vaccinated
    if (!all(vax_distribution[groups] > vax_eligible[groups])){
      temp <- vax_distribution
      temp[vax_distribution > vax_eligible] <- vax_eligible[vax_distribution > vax_eligible]
      leftover_vax <- sum(vax_distribution - temp)
      full_groups <- 1:11
      full_groups <- full_groups[vax_distribution > vax_eligible]
      leftover_groups <- groups[!groups %in% full_groups]
      people_to_vax <- sum(vax_eligible[leftover_groups])
      vax_proportion <- rep(0, num_groups)
      vax_proportion[leftover_groups] <- vax_eligible[leftover_groups]/people_to_vax
      vax_leftover_dist <- leftover_vax*vax_proportion 
      vax_distribution <- temp + vax_leftover_dist 
    }
    if (any(vax_distribution > vax_eligible)){ 
      temp <- vax_distribution
      temp[vax_distribution > vax_eligible] <- vax_eligible[vax_distribution > vax_eligible]
      leftover_vax <- sum(vax_distribution - temp)
      leftover_groups <- 1:11
      leftover_groups <- leftover_groups[!leftover_groups %in% groups]
      people_to_vax <- sum(vax_eligible[leftover_groups])
      vax_proportion <- rep(0, num_groups)
      vax_proportion[leftover_groups] <- vax_eligible[leftover_groups]/people_to_vax 
      vax_leftover_dist <- leftover_vax*vax_proportion
      vax_distribution <- temp + vax_leftover_dist
    }
    if (any(vax_distribution > vax_eligible)){
      vax_distribution[vax_distribution > vax_eligible] = vax_eligible
    }
  }
  alpha <- vax_distribution/vax_eligible
  alpha[alpha == Inf] <- 0 
  alpha[is.nan(alpha)] <- 0 
  if(any(alpha > 1)){print("WARNING: alpha > 1 in move_vaccinated")
    alpha[alpha>1] <- 0} 
  if (v_e_type == "aorn") {
    dS  <- -S*alpha 
    dSv <- (S*alpha*v_e)
  }
  S    <- S + dS
  Sv   <- Sv + dSv
  out <- c(S,Sv,E,I,R,D,vax_supply)
}
get_reduction_df = function(outcome){
  total <- rep(NA, 202)
  if (outcome == "cases"){
    total <- compile_total_cases(total)
    baseline <- compute_total_cases_new(list_normal$`0`)
  } else if (outcome == "deaths"){
    total <- compile_total_deaths(total)
    baseline <- compute_total_deaths_new(list_normal$`0`)
  } 
  num_strategies <- 2
  vax_avail <- c(rep(seq(0, 100, by = 1), num_strategies))
  num_per_list <- 101
  strat <- c(rep("health_workers", num_per_list), rep("normal", num_per_list))
  variable <-  c(rep("constant", num_per_list))
  baseline <- c(rep(baseline, num_per_list))
  reduction <- (1-(total/baseline))*100
  df <- data.frame(vax_avail, strat, reduction, variable)
}
get_rowindex <- function(df, rName) {
  which(match(colnames(df), rName) == 1)
}
compile_total_cases = function(total){
  count <- 1
  for (i in list_health_workers){
    total[count] <- compute_total_cases_new(i)
    count <- count + 1
  }
  for (i in list_normal){
    total[count] <- compute_total_cases_new(i)
    count <- count + 1
  }
  total
}
compile_total_deaths = function(total){
  count <- 1
  for (i in list_health_workers){
    total[count] <- compute_total_deaths_new(i)
    count <- count + 1
  }
  for (i in list_normal){
    total[count] <- compute_total_deaths_new(i)
    count <- count + 1
  }
  total
}
compute_total_cases_new = function(df){
  infections <- rep(0,num_groups) # number of age groups
  
  R_index <- 46 # col number for R 
  D_index <- 57
  for (i in 1:num_groups) {
    # infections = total # recovered - initial recovered (seropositive)
    infections[i] <- df[dim(df)[1], R_index] - df[1, R_index] + df[dim(df)[1], D_index]
    R_index  <- R_index + 1
    D_index  <- D_index + 1
  }
  tot_infections <- sum(infections)/pop_total * 100
}
compute_total_deaths_new = function(df){
  deaths <- rep(0,num_groups)
  D_index <- 57
  
  for (i in 1:num_groups) {
    deaths[i] <- df[dim(df)[1], D_index]
    D_index <- D_index + 1
  }
  tot_deaths <- sum(deaths)/pop_total * 100
}
gather_compartments_overtime <- function(df, compartment, strat){
  final_time <- as.numeric(dim(df)[1])-1
  if (compartment == "I"){
    start <- "I1"
    end <- "I10"
  } else if (compartment == "R"){
    start <- "R1"
    end <- "D10"
  } else if (compartment == "D"){
    start <- "D1"
    end <- "D10"
  }
  total_df <- data.frame(time = 0:final_time,
                         percent = ((apply(df[, get_rowindex(df, start):get_rowindex(df, end)], 
                                           1, sum))/pop_total)*100,
                         strat = rep(strat, final_time +1))
}
