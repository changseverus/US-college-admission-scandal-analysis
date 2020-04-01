library(haven)    # load the package that contain functions to read SAS files into R environment
library(plm)      # package that can process panel data in R
library(margins)  # package that can compute the marginal effect of model

file<-read_sas("college_admission.sas7bdat") # read SAS file into R
file$COLLEGE<-file$PSECHOICE!=1
file$COLLEGE<-as.numeric(file$COLLEGE)  # set up COLLEGE variable, whether one decide to enroll or not

probit<-
  glm(COLLEGE~GRADES+FAMINC+FAMSIZ+PARCOLL+FEMALE+BLACK,family=binomial(link="probit"),data=fil
      e)
summary(probit) # probit model

logit<-
  glm(COLLEGE~GRADES+FAMINC+FAMSIZ+PARCOLL+FEMALE+BLACK,family=binomial(link="logit"),data=file)
summary(logit) # logit model

sum(file$COLLEGE)/length(file$COLLEGE) # the rate that attend college
mean(file$FAMINC) # sample average of family income variable

x<-data.frame("GRADES"=10,"FAMINC"=51.3935,"FAMSIZ"=4,"PARCOLL"=1,"FEMALE"=1,"BLACK"=1)
predict.glm(probit,newdata=x) # predict the result of a new observation with probit model

marginal_effects(probit,x) # see the marginal effect of the variables in this observation



data<-read_sas("college2.sas7bdat") # read the other SAS file to R
pdata<-plm.data(data,index=c("hh","t")) # set up panel data

ols<-lm(l~x,data=pdata) # ordinary least square model

pool<-plm(l~x,data=pdata,model="pooling")
fixed<-plm(l~x,data=pdata,model="within")
summary(fixed) # fixed effect model

random<-plm(l~x,data=pdata,model="random")
summary(random) # random effect model

pFtest(fixed,pool) # test the hypothesis that individual intercepts all equal
