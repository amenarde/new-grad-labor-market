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

set.seed(042820190000)
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
                                       quota = rpois(num.com, 4) + rep(1, num.com))


create.students <- function() {
  ind.pref.mat <- t(replicate(num.students, rbeta(n = num.industries, shape1 = industry.chars$alpha, shape2 = industry.chars$beta)))
  rownames(ind.pref.mat) <- paste(rep("s-", num.students),seq.int(num.students))
  colnames(ind.pref.mat) <- industry.acronyms
  
  ind.attra.mat <- t(replicate(num.students, rbeta(n = num.industries, shape1 = alpha.start, shape2 = beta.start))) # maybe should be a normal with min/max of 0/1?
  ind.attra.mat <- (ind.attra.mat + ind.pref.mat) / 2 # mean
  rownames(ind.pref.mat) <- paste(rep("s-", num.students),seq.int(num.students))
  colnames(ind.pref.mat) <- industry.acronyms
  
  student.characteristics <- data.frame(name = paste(rep("s-", num.students),seq.int(num.students), sep=""),
                                        pref = ind.pref.mat,
                                        attra = ind.attra.mat)
  return(student.characteristics)
}

for (epoch in num.epochs) {
  student.chars <- create.students()
  
  for (week in recruiting.cycle) {
    # some companies appear to recruit this week (ore return)
    # based on duration and start
    for (company in company.chars) {
      if (company.chars$entry.week == week) {
        recruiting.df$Status[company] <- 1 #1 corresponds to entered
        recruiting.df$Remaining.Time[company] <- min(company.chars$duration[company], recruiting.cycle-week+1)
        recruiting.df$Remaining.Quota[company] <- company.chars$quota[company]
      }
    }

    # students choose which companies to apply to
    # based on industry preference
    for (student in num.students) {
      if(!(4 %in% application.status[student,])) {
        this.student.preference <- company.appeal.to.student[student,]
        apply(this.student.preference, 1, sort)
        iterator <- 1
        while(recruiting.df$Status[names(this.student.preference)[iterator]] != 1 && application.status[student,names(this.student.preference)[iterator]] != 0 && iterator < num.com) {
          iterator <- iterator + 1      
        }
        if (iterator < num.com) {
          application.status[student,names(this.student.preference)[iterator]] <- 1
        }
      }
    } 
    
    # companies look at their applied students and choose who to accept
      # based on the students appeal to industry and company quota
    
    # accepted students choose to accept/reject offer
      # based on industry preference and remaining weeks left in epoch
    for (student in num.students) {
      if (1 %in% application.status[student,]) {
        this.student.application <- application.status[student,]
        ##Continuing code here
      }
    }
      
    #companies update their quotas and remaining times, leaving if either is fulfilled
    for (company in company.chars) {
      if (recruiting.df$Status[company] == 1) {
        recruiting.df$Remaining.Time[company] <- recruiting.df$Remaining.Time[company] - 1
##      recruiting.df$Remaining.Quota[company] <- recruiting.df$Remaining.Quota[company] - () #Subtract accepted students
        if ((recruiting.df$Remaining.Time[company] == 0) || (recruiting.df$Remaining.Quota[company] == 0)) {
          recruiting.df$Status[company] <- 2
        }
      }
    }
      
  }
  
  
  # here gather statistics regarding performance of recuriting cycle, report / use to update
  # here update rule should be implemented -- update companies/industries based on performance
}





