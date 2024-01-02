# Script for:
# A camera trap based assessment of climate-driven phenotypic plasticity of seasonal moulting in an endangered carnivore.
# Lucie Laporte-Devylder, Kristine R. Ulvund, Lars Rod-Eriksen, Ola Olsson, Oystein Flagstad, Arild Landa, Nina E. Eide, Craig R. Jackson.

#This R script is built to be used with the associated data files on dryad for the analysis of environmentally-driven plasticity of moulting phenology

#Packages
install.packages("lmerTest")
install.packages("lme4")
install.packages("car")
install.packages("effects")
library(lme4)
library(lmerTest)
library(car)
library(effects)


#INPUT DATA AND WORKFLOW FOR THE LMM ON PLASTICITY OF MOULTING PHENOLOGY

Fur.moult <- read.csv2("seasonal_moulting_phenology.csv")
View(Fur.moult)

#Linearize data: set moult_score on logit scale 
Fur.moult$moult_p <- Fur.moult$moult_score/100 #score converted to proportions
Fur.moult$moult_p[Fur.moult$moult_p==0] <- .01 #replace 0 by small++ value
Fur.moult$moult_q <- qlogis(Fur.moult$moult_p) #logit scale to linearize data

#Standardize variables : center mean around 0 and scale with respect to the sd
Fur.moult$csdate <- scale(Fur.moult$date, center = TRUE, scale = TRUE) # center scale the "date" variable
Fur.moult$cssnow_continuous <- scale(Fur.moult$snow_continuous, center = TRUE, scale = TRUE) # center scale the "snow_continuous" variable
Fur.moult$cssnow_depth <- scale(Fur.moult$snow_depth, center = TRUE, scale = TRUE) # center scale the "snow_depth" variable
Fur.moult$cstemperature <- scale(Fur.moult$temperature, center = TRUE, scale = TRUE) # center scale the "temperature" variable
Fur.moult$csrodent <- scale(Fur.moult$rodent, center = TRUE, scale = TRUE) # center scale the "rodent" variable

#set variable YEAR as factor
fyear <- factor(Fur.moult$year)
#set variable RODENT as a factor
frodent <- factor(Fur.moult$rodent)
#check factorization
str(Fur.moult)

#Run LMM analysis (one model per climatic variable)

#m1snow: snow_continuous
summary(m1snow <- lmer(moult_q~csdate*cssnow_continuous+frodent+morph+(1+csdate|indiv_ID)+(1|site)+(1|year),Fur.moult))

#m1temp: temperature
summary(m1temp <- lmer(moult_q~csdate*cstemperature+frodent+morph+(1+csdate|indiv_ID)+(1|site)+(1|year),Fur.moult))

#m1depth: snow_depth
summary(m1depth <- lmer(moult_q~csdate*cssnow_depth+frodent+morph+(1+csdate|indiv_ID)+(1|site)+(1|year),Fur.moult))

#model validation
#within model: Anova
Anova(m1snow)
Anova(m1temp)
Anova(m1depth)
#Nakagawa R2
install.packages("performance")
library(performance)
r2_nakagawa(m1snow,by_group = FALSE)
r2_nakagawa(m1temp,by_group = FALSE)
r2_nakagawa(m1depth,by_group = FALSE)

#Keep models with significant climatic factor (snow_continuous & temperature)
#remove RODENT from models (non sig)

#m2snow: snow_continuous
summary(m2snow <- lmer(moult_q~csdate*cssnow_continuous+morph+(1+csdate|indiv_ID)+(1|site)+(1|year),Fur.moult))

#m2temp: mean_temp
summary(m2temp <- lmer(moult_q~csdate*cstemperature+morph+(1+csdate|indiv_ID)+(1|site)+(1|year),Fur.moult))

#model validation
#within model: Anova
Anova(m2snow)
Anova(m2temp)
#Nakagawa R2
library(performance)
r2_nakagawa(m2snow,by_group = FALSE)
r2_nakagawa(m2temp,by_group = FALSE)



#############
## FIGURES ##
############# 


############# FIGURE: Progression curves per year


#Moult progression based on YEAR
#m0 <- lmer(moult_q~fyear*csdate+(1+csdate|indiv_ID)+(1|site)+(1|year),Fur.moult)
#m1 <- lmer(moult_q~fyear*csdate+(1+csdate|indiv_ID)+(1|site),Fur.moult)
m2 <- lmer(moult_q~fyear*date+(1+csdate|indiv_ID)+(1|site),Fur.moult)

#response curve per YEAR ----

eff <- effect('fyear:date',m2,xlevels=20)

par(mfrow=c(4,2))

fact <- which(sapply(eff$variables, function(x) x$is.factor))
covar <- which(sapply(eff$variables, function(x) x$is.factor)==F)
factdat <- eff$data[, eff$variables[[fact]]$name]
x <- sapply(eff$variables, function(x) x$levels)[[covar]]

dstr <- c('1 May','1 June','1 July','1 August')
doy <- c(121,152,182,213)

i <- 0
for (L in levels(factdat)){
  i <- i+1
  
  ydata <- eff$data[factdat==L,1]
  xdata <- eff$data[factdat==L, eff$variables[[2]]$name]
  ydata <- plogis(ydata)
  
  ee <- matrix(plogis(summary(eff)$effect[i,]))
  el <- matrix(plogis(summary(eff)$lower[i,]))
  eu <- matrix(plogis(summary(eff)$upper[i,]))
  
  if (L==2011 | L==2015){
    main=paste(L,'*')
  } else {
    main=L
  }
  plot(ee~x, type='n', ylim=c(0,1), xlim=c(120,240), bty='l', xlab=' ', ylab='Prop. winter fur', 
       main=main, cex.lab=1,cex.axis=1,xaxt='n')
  points(jitter(ydata)~jitter(xdata),pch=19,cex=1,col=rgb(.25,.25,.9,.25))
  polygon(c(x, rev(x)), c(eu, rev(el)), col=rgb(.8,.8,.8,.5), density=-1, border=NA)
  lines(ee~x, lwd=2)
  
  axis(1,at=doy,labels=dstr)
}


############# FIGURE: Moult phenology under increasing values of days with snow

ggplot(transform(Fur.moult,
                 fct = cut(snow_continuous, seq(125,205,20))),
       aes(date, moult_score)) +
  geom_point() +
  facet_wrap(~fct)
#or as a coplot (moult progression as number of days with snow increases)
given.snow <- co.intervals(Fur.moult$snow_continuous, number = 4, overlap = .1)
coplot(moult_q ~ date | snow_continuous, data = Fur.moult, given.v = given.snow, rows = 1)


#Moult progression as temperature increases
ggplot(transform(Fur.moult,
                 fct = cut(temperature, seq(3,9,2))),
       aes(date, moult_score)) +
  geom_point() +
  facet_wrap(~fct)


############# FIGURE: Average initiation and completion dates per year and coloured according to length of snow season

install.packages("plyr")
library(dplyr)

#subset the dataset to narrow down the moult scores to initiation and completion
subset_95_0 <- subset(Fur.moult, Fur.moult$moult_score%in%c(0,95),)
str(subset_95_0)

#summary function to plot means and error bars, retrieved from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                        conf.interval=.95, .drop=TRUE) {
    library(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                     c(N    = length2(xx[[col]], na.rm=na.rm),
                       mean = mean   (xx[[col]], na.rm=na.rm),
                       sd   = sd     (xx[[col]], na.rm=na.rm)
                     )
                   },
                   measurevar
    )
    
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
  }

#Plot moult initiation & completion per year, according to length of snow season
#create column for mean snow days per year
moult_snow <- ddply(subset_95_0,.(year),transform,mean_snow_continuous = mean(snow_continuous))
#create se (for error bars)
moult_95_0 <- summarySE(moult_snow, measurevar="date", groupvars=c("year","moult_score", "mean_snow_continuous"))
moult_95_0
#Build graph
fyear <- factor(moult_95_0$year) #set YEAR as factor
ggplot(moult_95_0, aes(x=fyear, y=date, colour=mean_snow_continuous)) + geom_errorbar(aes(ymin=date-se, ymax=date+se), width=.1) + geom_point(shape=19, size=9) + xlab("year") + coord_flip() + scale_y_continuous(breaks=seq(120,220,10)) + scale_color_gradient2(midpoint=164, low = "red", mid = "lightblue", high = "#56B4E9")

#Plot moult initiation & completion per year, according to temperature values
#create se (for error bars)
moult_95_0_temp <- summarySE(moult_snow, measurevar="date", groupvars=c("year","moult_score", "temperature"))
moult_95_0_temp
#Build graph
fyear <- factor(moult_95_0_temp$year) #set YEAR as factor
ggplot(moult_95_0_temp, aes(x=fyear, y=date, colour=temperature)) + geom_errorbar(aes(ymin=date-se, ymax=date+se), width=.1) + geom_point(shape=19, size=9) + xlab("year") + coord_flip() + scale_y_continuous(breaks=seq(120,220,10)) + scale_color_gradient2(midpoint=5.4, low = "#56B4E9", mid = "lightblue", high = "red")


