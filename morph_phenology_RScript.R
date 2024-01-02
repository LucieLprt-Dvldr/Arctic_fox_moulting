# Script for:
# A camera trap based assessment of climate-driven phenotypic plasticity of seasonal moulting in an endangered carnivore.
# Lucie Laporte-Devylder, Kristine R. Ulvund, Lars Rod-Eriksen, Ola Olsson, Oystein Flagstad, Arild Landa, Nina E. Eide, Craig R. Jackson.

#This R script is built to be used with the associated data files on dryad for the analysis of moulting phenology of white and blue arctic foxes

#INPUT DATA AND WORKFLOW FOR ANALYSIS OF MOULTING PHENOLOGY FOR BLUE AND WHITE MORPHS

Morph.moult <- read.csv2("morph_phenology.csv")
View(Morph.moult)

#convert morph as a factor
Morph.moult$morph <- factor(Morph.moult$morph, levels = c("B", "W"))
#convert year as a factor 
Morph.moult$fyear <- factor(Morph.moult$year)
#convert rodent as a factor
Morph.moult$rodent <- factor(Morph.moult$rodent)
str(Morph.moult)
summary(Morph.moult)


#plot moulting timing for white and blue foxes separately
#initiation (95%)
ggplot(Morph.moult, aes(x=fyear, y=start_95, fill=morph)) +geom_boxplot() + scale_fill_manual(values=c("#D2B48C", "#FCFCFC")) +scale_y_continuous(name="start date") #+ coord_flip()
#median (50%)
ggplot(Morph.moult, aes(x=fyear, y=median_50, fill=morph)) +geom_boxplot() + scale_fill_manual(values=c("#D2B48C", "#FCFCFC")) +scale_y_continuous(name="median date") #+ coord_flip()
#completion (0%)
ggplot(Morph.moult, aes(x=fyear, y=end_0, fill=morph)) +geom_boxplot() + scale_fill_manual(values=c("#D2B48C", "#FCFCFC")) +scale_y_continuous(name="end date") #+ coord_flip()

par(mfrow = c(1,3), mar = c(5, 4, 1, 2))


#### T-tests on median dates of moulting

##Warmest years: 2011 & 2018
#subset the dataset to keep moult scores in 2011 and 2018 only
MORPH_11_18 <- subset(Morph.moult, Morph.moult$year %in%c(2011,2018),)
#T-test
t.test(MORPH_11_18$median_50~MORPH_11_18$morph)
boxplot(median_50~morph,data=MORPH_11_18)

##coldest years: 2012 & 2015
#subset the dataset to keep moult scores in 2012 and 2015 only
MORPH_12_15 <- subset(Morph.moult, Morph.moult$year %in%c(2012,2015),)
#T-test
t.test(MORPH_12_15$median_50~MORPH_12_15$morph)
boxplot(median_50~morph,data=MORPH_12_15)

##intermediate years: 2013, 2014, 2016, 2017
#subset the dataset to keep moult scores in 2012 and 2015 only
MORPH_interm <- subset(Morph.moult, Morph.moult$year %in%c(2013,2014,2016,2017),)
#T-test
t.test(MORPH_interm$median_50~MORPH_interm$morph)
boxplot(median_50~morph,data=MORPH_interm)


############# FIGURE: Differences on the median date of moulting for White and Blue arctic foxes
#plot the series
#2011-2018
ggplot(MORPH_11_18, aes(x=morph, y=median_50, fill=morph)) +geom_boxplot() + expand_limits(y=c(120,210)) + scale_fill_manual(values=c("#D2B48C", "#FCFCFC")) +scale_y_continuous(name="median date") +scale_x_discrete(name="morph", labels=c("Blue","White")) +ggtitle("Warm years")
#2012-2015
ggplot(MORPH_12_15, aes(x=morph, y=median_50, fill=morph)) +geom_boxplot() + expand_limits(y=c(120,210)) + scale_fill_manual(values=c("#D2B48C", "#FCFCFC")) +scale_y_continuous(name="median date") +scale_x_discrete(name="morph", labels=c("Blue","White")) +ggtitle("Cold years")
#other years
ggplot(MORPH_interm, aes(x=morph, y=median_50, fill=morph)) +geom_boxplot() + expand_limits(y=c(120,210)) + scale_fill_manual(values=c("#D2B48C", "#FCFCFC")) +scale_y_continuous(name="median date") +scale_x_discrete(name="morph", labels=c("Blue","White")) +ggtitle("Intermediate years")




#### ANOVA & Tukey test to determine the two years with largest significant difference

# In term of snow duration (best fitted model) -> 2011-2015
# For model selection on moult phenology, see "seasonal_moulting_phenology_Rscript.R"
Snow.aov2 <- aov(snow_continuous ~ fyear, data = Morph.moult)
summary(Snow.aov2)
TukeyHSD(Snow.aov2)

# In term of median date of moulting -> 2011-2015
Moult.aov2 <- aov(median_50 ~ fyear, data = Morph.moult)
summary(Moult.aov2)
TukeyHSD(Moult.aov2)


