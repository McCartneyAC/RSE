setwd("C:\\Users\\Andrew\\Downloads")
main<-read.csv("StatLab1.csv")
#############################
#    Libraries              #
############################
library(lattice)
library(psych)
library(car)
library(pwr)
library(pastecs)
library(multcomp)
############################
head(main)
attach(main)
attractive<-as.factor(attractive)
time<-as.factor(time)
############################
twoway<-anova(lm(labs~time+attractive+time:attractive, data=main))
str(twoway)
twoway
###using SPSS results given#
1-pf(10.329, 3,36)
1-pf(27.06,2,36)
1-pf(3.29,6,26)
############################
interaction.plot(attractive,time,labs)
### Simple effects

## for Time
morning <- subset(main, time=="1"); dim(morning)
afternoon <- subset(main, time=="2"); dim(afternoon)

# simple effect of condition over morninger
simple.morning <- anova(lm(labs~attractive, morning)); str(simple.morning)
F.morning <- (simple.morning$"Mean Sq"[1])/(twoway$"Mean Sq"[4]); F.morning
p.morning <- 1-pf(F.morning, simple.morning$"Df"[1], twoway$"Df"[4]); p.morning

# simple effect of condition over afternoon
simple.afternoon <- anova(lm(labs~attractive, afternoon)); str(simple.afternoon)
F.afternoon <- (simple.afternoon$"Mean Sq"[1])/(twoway$"Mean Sq"[4]); F.afternoon
p.afternoon <- 1-pf(F.afternoon, simple.afternoon$"Df"[1], twoway$"Df"[4]); p.afternoon

## for Attractive
unattractive <- subset(main, attractive=="1")
slightly <- subset(main, attractive=="2")
moderately<- subset(main, attractive=="3")
very <- subset(main, attractive=="4")


# simple effect of attractive over unattractive
simple.un <- anova(lm(labs~time, unattractive)); str(simple.un)
F.un <- (simple.un$"Mean Sq"[1])/(twoway$"Mean Sq"[4]); F.un
p.un <- 1-pf(F.un, simple.un$"Df"[1], twoway$"Df"[4]); p.un

# simple effect of attractive over slightly
simple.slightly <- anova(lm(labs~time, slightly)); str(simple.slightly)
F.slightly <- (simple.slightly$"Mean Sq"[1])/(twoway$"Mean Sq"[4]); F.slightly
p.slightly <- 1-pf(F.slightly, simple.slightly$"Df"[1], twoway$"Df"[4]); p.slightly

# simple effect of attractive over moderately
simple.mod <- anova(lm(labs~time, moderately)); str(simple.mod)
F.mod <- (simple.mod$"Mean Sq"[1])/(twoway$"Mean Sq"[4]); F.mod
p.mod <- 1-pf(F.mod, simple.mod$"Df"[1], twoway$"Df"[4]); p.mod

# simple effect of attractive over very
simple.very <- anova(lm(labs~time, very)); str(simple.very)
F.very <- (simple.very$"Mean Sq"[1])/(twoway$"Mean Sq"[4]); F.very
p.very <- 1-pf(F.very, simple.very$"Df"[1], twoway$"Df"[4]); p.very

################################
slightly
unattractive
moderately
very
sqrt(42.78/436.45)
################################
omega_factorial <- function(n,a,b,SSa,SSb,SSab,SSr)
{
  MSa <- SSa/(a-1)
  MSb <- SSb/(b-1)
  MSab <- SSab/((a-1)*(b-1))
  MSr <- SSr/(a*b*(n-1))
  varA <- ((a-1)*(MSa-MSr))/(n*a*b)
  varB <- ((b-1)*(MSb-MSr))/(n*a*b)
  varAB <- ((a-1)*(b-1)*(MSab-MSr))/(n*a*b)
  varTotal <- varA + varB + varAB + MSr
  print(paste("Omega-Squared A: ", varA/varTotal))
  print(paste("Omega-Squared B: ", varB/varTotal))
  print(paste("Omega-Squared AB: ", varAB/varTotal))
}

#where varA = attractive & varB = time

omega_factorial(8, 2, 4, 6.13, 136.9, 78.4, 436.45)
