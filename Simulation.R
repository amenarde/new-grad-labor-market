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

industry.acronyms <- c("Nat", "Hos", "Ins", "Otr", "REC", "Gov", "Ret", "Leg", "Man", "Non", "Com", "Hea", "Edu", "Tec", "Con", "Fin")

betas.industries <- matrix(rep(c(3,3), length(industry.names)), ncol=2)