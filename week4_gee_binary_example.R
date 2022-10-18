library(data.table)
library(geepack)
library(doBy)
obesity <- fread("muscatine.dat")
colnames(obesity) <- c("id","gender","cohort","current_age","occasion","status")
obesity$gender <- as.factor(obesity$gender)
obesity$id <- as.factor(obesity$id)

# Summary Obesity proportions 
summary1 <- obesity[,j=list(prop_obese = mean(status,na.rm=TRUE)*100), by = c("gender","cohort","occasion")]
dcast(summary1, gender + cohort ~ occasion, value = "prop_obese")

# GEE Model 1: 
# - with cohort effects
# - quadratic trend with age 
# - interaction with gender 

gee1 <- geeglm(status ~ gender * (cohort + I(cohort^2) + I(current_age - cohort) + I(current_age^2 - cohort^2)), id = id, data = obesity, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee1)
L <- matrix(0,ncol=10,nrow=4) # ncol = number of coeffcients in the model, nrow = number of tests 
L[1,c(3,5)]  <- c(1,-1)
L[2,c(4,6)]  <- c(1,-1)
L[3,c(7,9)]  <- c(1,-1)
L[4,c(8,10)] <- c(1,-1)
L
esticon(gee1,L=L,joint.test = TRUE)

# GEE Model 2: 
# - without cohort effects
# - quadratic trend with age 
# - interaction with gender 
gee2 <- geeglm(status ~ gender * (current_age + I(current_age^2)), id = id, data = obesity, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee2)
L <- matrix(0,ncol=6,nrow=2) # ncol = number of coeffcients in the model, nrow = number of tests 
L[1,c(5)]  <- c(1)
L[2,c(6)]  <- c(1)
L
esticon(gee2,L=L,joint.test = TRUE)

# GEE Model 3: 
# - without cohort effects
# - quadratic trend with age 
# - no interaction with gender 
gee3 <- geeglm(status ~ gender + (current_age + I(current_age^2)), id = id, data = obesity, family = binomial(link = "logit"), corstr = "unstructured")
summary(gee3)
# GEE Model 4: 
# - Same as Model 3 but with AR 1 correlation 
gee4 <- geeglm(status ~ gender + (current_age + I(current_age^2)), id = id, data = obesity, family = binomial(link = "logit"), corstr = "ar1")
summary(gee4)
