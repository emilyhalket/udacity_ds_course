# -------- playing with UC berk 1973 admit numbers for udacity course lesson 1------- #
# June 24 2015

admit <- read.csv('ucberk_73_admit.csv', header = TRUE)

levels(admit$Dept) #6 departments A-F

#just look at number of males/females accepted/rejected

#overview
ov <- aggregate(admit$Freq, by = list(admit$Admit, admit$Gender),sum)
colnames(ov) <- c('status', 'gender', 'num')

tots<- tapply(ov$num, ov$gender, sum) #get totals by gender, Female [1] Male [2]


perF <- ov$num[ov$status=='Admitted' & ov$gender == 'Female']/tots[1]
perM <- ov$num[ov$status=='Admitted' & ov$gender == 'Male']/tots[2]

#confirm that overall acceptance rate is 44.5% for males and 30.4% for females

#look by department
#make a function to pass into aggregate to get percentage admitted?

prct <- function(x){
  result = x[1]/sum(x)
  return(100*result)
  
}
accep <- aggregate(admit$Freq, by = list(admit$Dept, admit$Gender), prct)
colnames(accep) <- c('dept', 'gender', 'acceptance_rate')

#plot acceptance rate by department/gender to see how similar
library(lattice)
a <- xyplot(accep$acceptance_rate ~ accep$dept, groups = accep$gender, col = c('red', 'blue'),
       xlab = "department", ylab = "Acceptance Rate", main = "1973 UC Berk")

#look at apps by gender by departments

app <- aggregate(admit$Freq, by = list(admit$Dept, admit$Gender), sum)
colnames(app) <- c('dept', 'gender', 'total_apps')

b <- barchart(app$total_apps ~ app$dept, groups = app$gender, col = c('pink', "blue"), 
              xlab = "department", ylab = "total apps")

print(c(a,b))

#So dept C has the most female applicants (more than males) and a lower acceptance rate for females
#dept A&B have higher acceprance rates for females, but very few female applicants (so not represented
#in overall rate)