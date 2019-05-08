# Constants

industry.names <- c(
  "Energy/Natural Resources/Utilities",
  "Hospitality/Leisure/Sports",
  "Insurance",
  "Other",
  "Real Estate / Construction",
  "Government",
  "Retail/Wholesale",
  "Legal Services",
  "Manufacturing",
  "Nonprofit",
  "Communications",
  "Healthcare",
  "Education",
  "Technology",
  "Consulting",
  "Financial Services"
)
industry.acronyms <- c("Nat", "Hos", "Ins", "Otr", "Rec", "Gov", "Ret", "Leg", "Man", "Non", "Com", "Hea", "Edu", "Tec", "Con", "Fin")


set.seed(05072019)

recruiting.cycle <- 6 # weeks in a cycle
num.epochs <- 75 # years

num.industries <- 16
companies.per.industry <- 7 # 80 total companies
num.students <- 500

duration.mean <- 3 # mean weeks a company recruits
quota.mean <- 9  # mean students a company wishes to hire
minimum.company.alpha <- 0.2
minimum.industry.alpha <- 1

num.com <- num.industries*companies.per.industry
alpha.start <- 3
beta.start <- 3

# Set initials
industry.chars <- data.frame(IND = industry.acronyms, 
                                       alpha = rep(alpha.start, num.industries), 
                                       beta = rep(beta.start, num.industries))
company.chars <- data.frame(name = paste(rep("c-", num.com),seq.int(num.com), sep=""), 
                                       IND = rep(industry.acronyms, rep(companies.per.industry)),
                                       alpha = rep(alpha.start, num.com),
                                       beta = rep(beta.start, num.com),
                                       entry.week = sample(recruiting.cycle, replace = TRUE, size = num.com),
                                       duration = rpois(num.com, duration.mean - 1) + rep(1, num.com),
                                       quota = rpois(num.com, quota.mean - 1) + rep(1, num.com))


create.sim.data <- function() {
  ind.pref.mat <- t(replicate(num.students, rbeta(n = num.industries, shape1 = industry.chars$alpha, shape2 = industry.chars$beta)))
  rownames(ind.pref.mat) <- paste(rep("s-", num.students),seq.int(num.students))
  colnames(ind.pref.mat) <- industry.acronyms
  
  ind.attra.mat <- t(replicate(num.students, rbeta(n = num.industries, shape1 = alpha.start, shape2 = beta.start))) # maybe should be a normal with min/max of 0/1?
  ind.attra.mat <- (ind.attra.mat + ind.pref.mat) / 2 # mean
  rownames(ind.pref.mat) <- paste(rep("s-", num.students),seq.int(num.students))
  colnames(ind.pref.mat) <- industry.acronyms
  
  
                                              
  student.appeal.to.company <- rnorm(num.students*num.com, 
                                     # vector of num.com copies of each attractiveness
                                     mean = rep(c(t(ind.attra.mat)), rep(companies.per.industry)), 
                                     sd = 0.05)
  student.appeal.to.company <- matrix(student.appeal.to.company, ncol = num.com)
  rownames(student.appeal.to.company) <- paste(rep("s-", num.students),seq.int(num.students))
  colnames(student.appeal.to.company) <- company.chars$name
  
  company.appeal.to.student <- c(t(replicate(num.students, rbeta(n = num.com, shape1 = company.chars$alpha, shape2 = company.chars$beta))))
  company.appeal.to.student <- (company.appeal.to.student + rep(c(t(ind.pref.mat)), rep(companies.per.industry))) / 2
  company.appeal.to.student <- matrix(company.appeal.to.student, ncol = num.com)
  rownames(company.appeal.to.student) <- paste(rep("s-", num.students),seq.int(num.students))
  colnames(company.appeal.to.student) <- company.chars$name
  
  application.status <- matrix(rep(0, num.students*num.com), ncol = num.com)
  rownames(application.status) <- paste(rep("s-", num.students),seq.int(num.students))
  colnames(application.status) <- company.chars$name
  
  recruiting.df <- data.frame(Status = rep(0, num.com),
                              Name = company.chars$name,
                              Remaining.Time = company.chars$duration,
                              Remaining.Quota = company.chars$quota)
  
  return(list(student.appeal.to.company = student.appeal.to.company,
              company.appeal.to.student = company.appeal.to.student,
              application.status = application.status,
              recruiting.df = recruiting.df))
}

simulation.results = list()
simulation.results$company.performance <- matrix(rep(NA, num.com*num.epochs), ncol = num.epochs)
simulation.results$company.performance.mean <- rep(NA, num.epochs)
simulation.results$company.performance.normalized <- matrix(rep(NA, num.com*num.epochs), ncol = num.epochs)
simulation.results$company.performance.spread <- rep(NA, num.epochs)
simulation.results$company.performance.ranks <- matrix(rep(NA, num.com*num.epochs), ncol = num.epochs)
simulation.results$company.performance.sd <- rep(NA, num.epochs)
simulation.results$unemployment <- rep(NA, num.epochs)
simulation.results$mean.student.utility <- rep(NA, num.epochs)
simulation.results$industry.performance <- matrix(rep(NA, num.industries*num.epochs), ncol = num.epochs)
simulation.results$mean.employed.student.utility <- rep(NA, num.epochs)

for (epoch in 1:num.epochs) {
  
  sim.data <- create.sim.data()
  student.appeal.to.company = sim.data$student.appeal.to.company
  company.appeal.to.student = sim.data$company.appeal.to.student
  application.status = sim.data$application.status
  recruiting.df = sim.data$recruiting.df
  
  
  for (week in 1:recruiting.cycle) {
    # some companies appear to recruit this week (ore return)
    # based on duration and start
    for (company in 1:num.com) {
      if (company.chars[company,]$entry.week == week) {
        recruiting.df$Status[company] <- 1 #1 corresponds to entered
        recruiting.df$Remaining.Time[company] <- min(company.chars$duration[company], recruiting.cycle-week+1)
        recruiting.df$Remaining.Quota[company] <- company.chars$quota[company]
      }
    }

    # students apply to companies based on industry preference
    for (student in 1:num.students) {
      if(!(4 %in% application.status[student,])) {
        this.student.preference <- company.appeal.to.student[student,]
        this.student.preference <- sort(this.student.preference, decreasing = TRUE)
        iterator <- 1
        while(((recruiting.df$Status[which(names(this.student.preference)[iterator] == recruiting.df$Name)] != 1) || (application.status[student,names(this.student.preference)[iterator]] != 0)) && (iterator < num.com)) {
          iterator <- iterator + 1      
        }
        if (iterator < num.com) {
          application.status[student,names(this.student.preference)[iterator]] <- 1
        }
      }
    } 
    
    #company accepts or rejects applicants
    for (company in 1:num.com) {
      if (recruiting.df$Status[company] == 1) {
        #Look at how many students the company will accept
        num_to_accept <- min(ceiling(recruiting.df$Remaining.Quota[company] / recruiting.df$Remaining.time[company] * 1.5), recruiting.df$Remaining.Quota[company])
        this.company.preference <- student.appeal.to.company[,company]
        this.company.preference <- sort(this.company.preference,decreasing = TRUE)
        offersGiven <- 0
        #Give offers to that many
        for (student in 1:num.students) {
            if (application.status[names(this.company.preference)[student],company] == 1) {
              if (offersGiven < num_to_accept) {
                #Don't reject (give offer to )the top num_to_accept students 
                offersGiven <- offersGiven + 1
              }
              else {
                #Reject the rest of the applicants
                application.status[names(this.company.preference)[student],company] <- 2
              }
            }
        }
        
      }
    }
    
    #students accept or reject offer
    for (student in 1:num.students) {
      if (1 %in% application.status[student,]) {
        this.student.application <- application.status[student,]
        applied.company <- names(this.student.application)[this.student.application == 1]
        this.student.preference <- company.appeal.to.student[student,]
        this.student.preference <- sort(this.student.preference, decreasing = TRUE)
        iterator <- 1
        while(names(this.student.preference)[iterator] != applied.company[1]) {
          iterator <- iterator + 1      
        }
        if(iterator/num.com <= exp(week/2 - 3)) {
          application.status[value=student,applied.company] <- 4
          
          recruiting.df$Remaining.Quota[which(recruiting.df$Name == applied.company)] <- recruiting.df$Remaining.Quota[which(recruiting.df$Name == applied.company)] - 1 
        }
        else {
          application.status[value=student,applied.company] <- 3
        }
      }
    }
      
    #companies update their quotas and remaining times, leaving if either is fulfilled
    for (company in 1:num.com) {
      if (recruiting.df$Status[company] == 1) {
        recruiting.df$Remaining.Time[company] <- recruiting.df$Remaining.Time[company] - 1
##      recruiting.df$Remaining.Quota[company] <- recruiting.df$Remaining.Quota[company] - () #Subtract accepted students
        if ((recruiting.df$Remaining.Time[company] == 0) || (recruiting.df$Remaining.Quota[company] == 0)) {
          recruiting.df$Status[company] <- 2
        }
      }
    }
  }
  
  # Gather statistics
  
  unemployed <- 0
  employed <- 0
  for (student in 1:num.students) {
    if (4 %in% application.status[student,]) {
      employed <- employed + 1
    }
    else {
      unemployed <- unemployed + 1
    }
  }
  
  total_spots_available <- 0
  total_spots_taken <- 0
  total_spots_remaining <- 0
  for (company in 1:num.com) {
    total_spots_available <- total_spots_available + company.chars$quota[company]
    total_spots_taken <- total_spots_taken + company.chars$quota[company] - recruiting.df$Remaining.Quota[company]
    total_spots_remaining <- total_spots_remaining + recruiting.df$Remaining.Quota[company]
  }
  
  simulation.results$unemployment[epoch] <- unemployed / num.students
  
  company.chars[recruiting.df$Remaining.Quota > 0,]
  recruiting.df[recruiting.df$Remaining.Quota > 0,]
  
  # Company Update Rule
  
  # Less offers per spot is better
  offers.per.spot <- rep(NA, num.com)
  for (i in 1:num.com) {
    offers.per.spot[i] <- (sum(application.status[,i] == 3) + sum(application.status[,i] == 4))
  }
  offers.per.spot <- offers.per.spot / (company.chars$quota - recruiting.df$Remaining.Quota)
  
  # Higher percent filled is better
  percent.filled <- (company.chars$quota - recruiting.df$Remaining.Quota) / company.chars$quota
  
  # application.status.df <- data.frame(application.status, row.names = rownames(student.appeal.to.company))
  
  # Higher Avg. Quality of Students is Better
  quality.of.students <- rep(NA, num.com)
  for (i in 1:num.com) {
    temp <- application.status[,i] == 4
    hired <- names(temp[temp != FALSE])
    quality.of.students[i] <- sum(student.appeal.to.company[names(temp[temp != FALSE]),i]) / length(hired)
  }
  
 # We create a score for each company:
 # (Avg. Quality)*Percent Filled / Offers per spot
  company.scores <- quality.of.students * percent.filled / offers.per.spot
  names(company.scores) <- company.chars$name
  company.scores[is.nan(company.scores)] <- 0
  company.adj.scores <- company.scores - mean(company.scores) # parameter
  
  simulation.results$company.performance[,epoch] <- company.scores
  simulation.results$company.performance.mean[epoch] <- mean(company.scores)
  simulation.results$company.performance.normalized[,epoch] <- simulation.results$company.performance[,epoch] / simulation.results$company.performance.mean[epoch]
  simulation.results$company.performance.spread[epoch] <- max(company.scores) - min(company.scores)
  simulation.results$company.performance.sd[epoch] <- sd(company.scores)
  
  # Now we adjust the alphas of each company
  company.chars$alpha <- company.chars$alpha + company.adj.scores
  company.chars$alpha[company.chars$alpha < minimum.company.alpha] <- minimum.company.alpha # Keep bad performing companies alive
  
  sorted.company.scores <- sort(company.scores, decreasing = TRUE)
  for (i in 1:num.com) {
    name <- company.chars$name[i]
    simulation.results$company.performance.ranks[i, epoch] <- which(names(sorted.company.scores) == name)
  }
  
  # We aggregate company scores in each industry to adjust the industry score
  # and adjust industry alphas by them
  for (i in 1:num.industries) {
    companies.in.industry <- company.chars$name[company.chars$IND == industry.acronyms[i]]
    simulation.results$industry.performance[i,epoch] <- mean(company.scores[companies.in.industry])
  }
  adj.indust.perf <- simulation.results$industry.performance[,epoch] - mean(simulation.results$industry.performance[,epoch])
  industry.chars$alpha <- industry.chars$alpha + adj.indust.perf
  industry.chars$alpha[industry.chars$alpha < minimum.industry.alpha] <- minimum.industry.alpha
  
  # We judge student utility
  # Higher Avg. Quality of Company is Better
  total.student.utility <- 0
  for (i in 1:num.students) {
    if (4 %in% application.status[i,]) {
    temp <- application.status[i,] == 4
    company <- names(temp[temp != FALSE])
      companies.sorted <- sort(company.appeal.to.student[i,], decreasing = TRUE)
      multiplier <- 1 - (which(names(companies.sorted) == company)/num.com)
      total.student.utility <-  total.student.utility + (company.appeal.to.student[i,company]*multiplier) 
    }
  }
  
  simulation.results$mean.student.utility[epoch] <- total.student.utility / num.students
  simulation.results$mean.employed.student.utility[epoch] <- total.student.utility / employed

}

