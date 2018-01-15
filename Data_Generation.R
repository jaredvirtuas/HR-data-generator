

#An Overall function including everything, indicate n as the number of observations you want.
HRdataGenerator = function(n = integer()){

#Generation of Normal Distribution with range
NormalDistribution = function(n, mean, sd, a = -Inf, b = Inf){
  as.integer(qnorm(runif(n, pnorm(a, mean, sd), pnorm(b, mean, sd)), mean, sd))
}

#Age
Age = NormalDistribution(n, 45, 7, 20, 67)

#Department
Department = c(rep(1, 3), rep(2, 0.2*n), rep(3, 0.2*n), rep(4, 0.15*n), rep(5, 0.15*n), rep(6, 0.15*n), rep(7, 0.15*n-3))

#Gender
Gender = c()
for(i in 1:n){
  if(Department[i] %in% c(2,3)){Gender[i]=rbinom(1, 1, 0.78)}
  else if(Department[i] %in% c(4,5,7)){Gender[i]=rbinom(1, 1, 0.36)}
  else{Gender[i]=rbinom(1, 1, 0.52)}
}

#Position
Position = c(rep(4, 1), rep(1, length(which(Department==1))-1))
for(i in 2:length(unique(Department))){
  Position = c(Position, c(rep(3, 1), rep(2, as.integer(0.2*(length(which(Department==i))-1))), rep(1, length(which(Department==i))-1-as.integer(0.2*(length(which(Department==i))-1)))))
}

#Education
Education = c()
for(i in 1:n){
  if(Age[i]>50){Education = c(Education, c(sample(c(1:4), 1, prob=c(0.60, 0.2, 0.15, 0.05), rep=T)))}
  else if(Age[i]>40 && Age[i]<=50){Education = c(Education, c(sample(c(1:4), 1, prob=c(0.45, 0.3, 0.2, 0.05), rep=T)))}
  else if(Age[i]>28 && Age[i]<=40){Education = c(Education, c(sample(c(1:4), 1, prob=c(0.3, 0.4, 0.2, 0.1), rep=T)))}
  else if(Age[i]>25 && Age[i]<=28){Education = c(Education, c(sample(c(1:4), 1, prob=c(0.35, 0.45, 0.2, 0.0), rep=T)))}
  else if(Age[i]>22 && Age[i]<=25){Education = c(Education, c(sample(c(1:4), 1, prob=c(0.45, 0.55, 0.0, 0.0), rep=T)))}
  else {Education = c(Education, c(sample(c(1:4), 1, prob=c(1.0, 0.0, 0.0, 0.0), rep=T)))}
  }

#MaritalStatus
MaritalStatus = c()
for(i in 1:n){
  if(Age[i]>40){MaritalStatus = c(MaritalStatus, c(sample(c(1:2), 1, prob=c(0.3, 0.7), rep=T)))}
  else if(Age[i]>30 && Age[i]<=40){MaritalStatus = c(MaritalStatus, c(sample(c(1:2), 1, prob=c(0.4, 0.6), rep=T)))}
  else if(Age[i]>20 && Age[i]<=30){MaritalStatus = c(MaritalStatus, c(sample(c(1:2), 1, prob=c(0.6, 0.4), rep=T)))}
  else{MaritalStatus = c(MaritalStatus, c(sample(c(1:2), 1, prob=c(0.9, 0.1), rep=T)))}
}


#Overtime 
Overtime = c()
group.HighEmployee = c()
group.HighManager = c()
group.HighEM = c()
group.LowEmployee = c()
group.LowManager = c()
group.LowEM = c()
group.CEO = c()

for (i in 1:n) {
  if (Department[i] %in% c(2,3,4,6) && Position[i]==1) {group.HighEmployee = c(group.HighEmployee, i)}
  else if (Department[i] %in% c(2,3,4,6) && Position[i]==2) {group.HighManager = c(group.HighManager, i)}
  else if (Department[i] %in% c(2,3,4,6) && Position[i]==3) {group.HighEM = c(group.HighEM, i)}
  else if (Department[i] %in% c(1,5,7) && Position[i]==1) {group.LowEmployee = c(group.LowEmployee, i)}
  else if (Department[i] %in% c(1,5,7) && Position[i]==2) {group.LowManager = c(group.LowManager, i)}
  else if (Department[i] %in% c(1,5,7) && Position[i]==3) {group.LowEM = c(group.LowEM, i)}
  else {group.CEO = c(group.CEO, i)}
}

OTG1 = NormalDistribution(length(group.HighEmployee), 10, 1, 5, 15)
OTG2 = NormalDistribution(length(group.HighManager), 20, 1, 15, 25)
OTG3 = NormalDistribution(length(group.HighEM), 27, 1, 25, 30)
OTG4 = rep(0, length(group.LowEmployee))
OTG5 = NormalDistribution(length(group.LowManager), 2, 0.5, 1, 3)
OTG6 = NormalDistribution(length(group.LowEM), 4, 0.5, 3, 5)
OTG7 = as.integer(runif(1, min=31, max =35))

for(i in 1:length(OTG1)) {Overtime[group.HighEmployee[i]] = OTG1[i]}
for(i in 1:length(OTG2)) {Overtime[group.HighManager[i]] = OTG2[i]}
for(i in 1:length(OTG3)) {Overtime[group.HighEM[i]] = OTG3[i]}
for(i in 1:length(OTG4)) {Overtime[group.LowEmployee[i]] = OTG4[i]}
for(i in 1:length(OTG5)) {Overtime[group.LowManager[i]] = OTG5[i]}
for(i in 1:length(OTG6)) {Overtime[group.LowEM[i]] = OTG6[i]}
for(i in 1:length(OTG7)) {Overtime[group.CEO[i]] = OTG7[i]}


#Avg. working hours
WorkingHours = c()
LowHours = NormalDistribution(length(group.LowEmployee), 150, 5, 140, 160)
LowHoursCount = 1
for (i in 1:n){
  if (Overtime[i]==0) {
    WorkingHours[i] = LowHours[LowHoursCount]
    LowHoursCount = LowHoursCount + 1
  }
  else {WorkingHours[i] = 160 + Overtime[i]}
}


#Job Tenure function to calculate the B-boundry for JobTenure
JobTenure = c()
bLimit<- function(Age, Education){
       if(Education==2) {x=3}
  else if(Education==3) {x=5}
  else if(Education==4) {x=8}
  else {x=0}
  b.Boundry = (Age-20-x)
  if(b.Boundry<0){return (b.Boundry=0)}
  else {return (b.Boundry)}
}

for(i in 1:n){ 
     JobTenure[i] = as.integer(sample(0:bLimit(Age[i], Education[i]), 1)) 
    }


#Salary
Salary = c()
for(i in 1:n){
  if(Position[i]==1 && JobTenure[i]<=7){Salary[i] = 1000 + 10*(WorkingHours[i] - Overtime[i]) + 15*Overtime[i]}
  else if(Position[i]==1 && JobTenure[i]>7) {Salary[i] = 1200 + 10*(WorkingHours[i] - Overtime[i]) + 15*Overtime[i]}
  else if(Position[i]==2 && JobTenure[i]<=7){Salary[i] = 2000 + 20*(WorkingHours[i] - Overtime[i]) + 30*Overtime[i]}
  else if(Position[i]==2 && JobTenure[i]>7) {Salary[i] = 2200 + 20*(WorkingHours[i] - Overtime[i]) + 30*Overtime[i]}
  else if(Position[i]==3 && JobTenure[i]<=7){Salary[i] = 3000 + 30*(WorkingHours[i] - Overtime[i]) + 45*Overtime[i]}
  else if(Position[i]==3 && JobTenure[i]>7){Salary[i] = 3200 + 30*(WorkingHours[i] - Overtime[i]) + 45*Overtime[i]}
  else{Salary = 20000}
  }

  
#Satisfaction Level
SatisfactionLevel= c()
for(i in 1:n){
  #"MaritalStatus=="Married" TRUE
  if(MaritalStatus[i]== 2){ 
    if(Overtime[i]>=5){SatisfactionLevel[i] = as.integer(sample(0:19, 1))} #"Married" TRUE & "Overtime>=5" TRUE
    else {SatisfactionLevel[i] = as.integer(sample(50:69, 1))} #"Married" TRUE & "Overtime>=5" FALSE
  } #"MaritalStatus=="Married" TRUE branch ends
  
  #"MaritalStatus=="Married" FALSE
  else {
    if(Overtime[i]>=8){ 
    #"Overtime>=8" TRUE branch
      if(Age[i]<=40){SatisfactionLevel[i] = as.integer(sample(70:84, 1))} #"Overtime>=8" TRUE & "Age<=40" TRUE
      else {SatisfactionLevel[i] = as.integer(sample(20:29, 1))}#"Overtime>=8" TRUE & "Age<=40"FALSE
    }
    else {
      #"Overtime>=8" FALSE branch
      if(Salary[i]<=2800){SatisfactionLevel[i]= as.integer(sample(30:49,1))} #"Overtime>=8" FALSE & "Salary<=2800" TRUE
      else{SatisfactionLevel[i] = as.integer(sample(85:100,1))} #"Overtime>=8" FALSE & "Salary<=2800" FALSE
    }
  } #"MaritalStatus=="Married" FALSE branch ends
  
}


#EQ & IQ Score, correlated to SatisifactionLevel
# Create a matrix contains two variables, these will be modified to be EQ, IQ
x23 <- scale(matrix( rbeta( 2*n, 60, 27.1 ), ncol=2 ))
# Combine with SatisfactionLevel
x123 <- cbind(scale(SatisfactionLevel), x23)
# find the current correlation matrix
c1 <- var(x123)
# cholesky decomposition to get independence corelation
chol1 <- solve(chol(c1))
newx <-  x123 %*% chol1 
# create new correlation structure(Please change the number here if you want)
newc <- matrix( 
  c(1  , 0.7, -0.6, 
    0.7, 1  , 0  ,
    -0.6, 0  , 1  ), ncol=3 )
# check that it is positive definite
eigen(newc)
chol2 = chol(newc)
finalx = newx %*% chol2 * sd(SatisfactionLevel) + mean(SatisfactionLevel)
#Rescaled to meet the real case of EQ and IQ
#Average EQ is 90-100, highest is 160
#95% of the poplulation have IQ range from 70 ~ 130
EQ = round(scales:::rescale(finalx[,2], to = c(70, 150)), digits = 0)
IQ = round(scales:::rescale(finalx[,3], to = c(70, 135)), digits = 0)




################################  Recording ################################

#Gender Recording
for(i in 1:length(Gender)){
  if(Gender[i] == 1){
    Gender[i] = "Male"
  }else{
    Gender[i] = "Female"
  }
}
  
#Department Recoding
for(i in 1:length(Department)){
  if(Department[i] == 1){Department[i] = "Executive Office"} 
  else if(Department[i] == 2){Department[i] = "R&D"}
  else if(Department[i] == 3){Department[i] = "IT"}
  else if(Department[i] == 4){Department[i] = "Marketing"}
  else if(Department[i] == 5){Department[i] = "HR"}
  else if(Department[i] == 6){Department[i] = "Sales"}
  else{Department[i] = "Accounting"}
}
  
#Position Recoding
for(i in 1:length(Position)){
  if(Position[i] == 1){Position[i] = "Employee"} 
  else if(Position[i] == 2){Position[i] = "Manager"}
  else if(Position[i] == 3){Position[i] = "Executive Manager"}
  else {Position[i] = "Chef Executive Manager"}
}
  
#Education Recoding
for(i in 1:length(Education)){
  if(Education[i] == 1){Education[i] = "high school"} 
  else if(Education[i] == 2){Education[i] = "bachelor"}
  else if(Education[i] == 3){Education[i] = "master"}
  else {Education[i] = "doctor"}
}

#Marital Status Recording
for(i in 1:length(MaritalStatus)){
  if(MaritalStatus[i] == 1){MaritalStatus[i] = "Single"} 
  else {MaritalStatus[i] = "Married"}
}

#Combine every column, and store it as Global Dataframe.
HRdata <<- as.data.frame(cbind(Age, Gender, Department, Position, Education, MaritalStatus, Overtime, WorkingHours, JobTenure, Salary, SatisfactionLevel, EQ, IQ ))
View(HRdata)
}

#If you want to output the data as csv, use this, replace YourPath with your path
write.csv(HRdata, file = "YourPath/HRdata.csv")


