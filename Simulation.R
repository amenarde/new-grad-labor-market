# Constants

num.industries <- 16
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

#set.seed(04282019)
recruiting.cycle <- 6 # weeks
num.epochs <- 10
companies.per.industry <- 5
num.com <- num.industries*companies.per.industry
alpha.start <- 3
beta.start <- 3
num.students <- 500

# Set initials
industry.chars <- data.frame(IND = industry.acronyms, 
                                       alpha = rep(alpha.start, num.industries), 
                                       beta = rep(beta.start, num.industries))
company.chars <- data.frame(name = paste(rep("c-", num.com),seq.int(num.com), sep=""), 
                                       IND = rep(industry.acronyms, rep(companies.per.industry)),
                                       alpha = rep(alpha.start, num.com),
                                       beta = rep(beta.start, num.com),
                                       entry.week = sample(recruiting.cycle, replace = TRUE, size = num.com),
                                       duration = rpois(num.com, 2) + rep(1, num.com),
                                       quota = rpois(num.com, 6) + rep(1, num.com))


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
  
  company.chars[recruiting.df$Remaining.Quota > 0,]
  recruiting.df[recruiting.df$Remaining.Quota > 0,]
  
  # here gather statistics regarding performance of recuriting cycle, report / use to update
  # here update rule should be implemented -- update companies/industries based on performance
}





