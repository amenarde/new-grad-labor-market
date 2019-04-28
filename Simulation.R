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
  
  application.status <- matrix(rep(0, num.students*num.com), ncol = num.com)
  
  recruiting.df <- data.frame(Status = rep(0, num.com),
                              Remaining.Time = company.chars$duration,
                              Remaining.Quota = company.chars$quota)
  
  return(list(student.appeal.to.company = student.appeal.to.company,
              company.appeal.to.student = company.appeal.to.student,
              application.status = application.status,
              recruiting.df = recruiting.df))
}

for (epoch in num.epochs) {
  
  for (week in recruiting.cycle) {
    # some companies appear to recruit this week (ore return)
      # based on duration and start
    
    # students choose which companies to apply to
      # based on company
    
    # companies look at their applied students and choose who to accept
      # based on the students appeal to industry and company (remaining) quota
    
    # accepted students choose to accept/reject offer
      # based on industry preference and remaining weeks left in epoch
      # is company
  }
  
  
  # here gather statistics regarding performance of recuriting cycle, report / use to update
  # here update rule should be implemented -- update companies/industries based on performance
}





