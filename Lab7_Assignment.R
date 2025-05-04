# Lab 7 Assignment: Difference in Means and ANOVA
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [70 points] Open the R file "Lab7_Assignment.R" and answer the questions bellow
# 2. [30 points] Run a T-test and an ANOVA test in your data.


#---- Part 1. Open the R file "Lab7_Assignment.R" and answer the questions bellow

# 1.1 load the same household data used in the Lab7_Script.R file, create the object `HTS`
library(data.table)
library(foreign)

hts <- data.table(read.spss("datasets/HTS.household.10regions.sav",to.data.frame = T))

# 2. Recreate the same steps used in the lab to run a T-test, but instead, consider the following:
# 2.1 Run a T-Test to show if the household income means is statistically different between households living in single family residences or not (use the whole sample). Produce two pdfs, one with an histogram pdf plot, and another with the simulated hypothesis testing plot showing where the T-statistic falls. Provide a short interpretation of your results

# step 1 - Independence:

hts[,.N,by=htype]
hts<-hts[htype%in%c("single family detached", "other","multi-family","single family attached")]

hts[, htype := fifelse(
  htype %in% c("single family detached", "single family attached"),
  "single family residences",
  fifelse(
  htype %in% c("multi-family", "other"),
  "multi-family residences",
    as.character(htype)
  )
)]

hts[, htype := factor(htype, levels = c("single family residences", "multi-family residences"))]

# step 1 - Normality: Both categories appear to have normal distribution

library(ggplot2)

p1 <- ggplot(data=hts,aes(x=hhincome))+
  geom_histogram()+
  facet_grid(htype~.)

ggsave(filename = "HistogramHHIncome.pdf",plot = p1)

# step 1 - Variance: The variance of the categories seem equal (boxplots are not too crazy)

ggplot(data=hts,aes(x=hhincome, y=htype))+
  geom_boxplot()

# desc stats
hts[,.(mean=mean(hhincome,na.rm=T),sd=sd(hhincome,na.rm=T),Obs=.N),by=.(htype)]


#Step 2: dependent variable types
class(hts$hhincome) ; class(hts$htype)

# Step 3: H0: t-statistic = 0 (Mean of Single Family - Mean of Other = 0)

# Step 4: Conduct t-test
two_tailed_t_test<-hts[,t.test(formula = hhincome ~ htype)] # two-tailed
two_tailed_t_test

one_tailed_t_test<-hts[,t.test(formula = hhincome ~ htype,alernative = 'greater')] # one-tailed
one_tailed_t_test

# Step 5. Interpret results
#Both T-tests are statistically significant and both having a p-value of < 2.2e-16.This states that the null hypothesis (H0) can be rejected and state that the difference in household income between single family residences and multi-family residences is not due to chance.

pdf("t_distribution_plot.pdf", width = 8, height = 6)

curve(dt(x, df = 2225.1), from = -35, to = 35)
abline(h=0,col='blue')
points(x=two_tailed_t_test$statistic,y=0,col='red')

upper975 <- qt(p = .975, df = 2225.1)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df = 2225.1)
abline(v = lower025,y=0,col='red')

dev.off()
# 2.2 Filter the sample to select only the region of San Antonio. Prepare an T-Test to show if the household vehicle miles traveled (in natural logs - lnvmt) is statistically different between households living in neighborhoods with a job-population index (variable `jobpop`) over and under the city median (of the `jobpop` variable of course)

htsSA <- data.table(read.spss("datasets/HTS.household.10regions.sav",to.data.frame = T))
htsSA <- htsSA[region%in%"San Antonio, TX"]

median_jobpop <- median(htsSA$jobpop, na.rm = TRUE)
htsSA$jobpop_group <- ifelse(htsSA$jobpop > median_jobpop, "Above Median", "Below Median")


# 2.2 using the same data set (San Antonio sample), run an ANOVA test to see if there are significant differences between income categories and vehicle miles traveled by household. Follow the same steps used in the ANOVA exercise done in class. Produce three pdfs: one checking the normality assumption of the dependent variable, a second one checking the presence of outliers, and a third one showing the Tukey (post hoc) T-tests plot.


# Step 1: Independence:

# Step 2: Normality
Normality_Assumption <- ggplot(data = htsSA[, .(Mean_lnvmt = mean(lnvmt, na.rm = TRUE)), by = income_cat], aes(x = income_cat, y = Mean_lnvmt)) +
  geom_point()

ggsave(filename = "NormalityAssumption.pdf",plot = Normality_Assumption)

# Step 1: Equal Variance Assumption: 
ggplot(data=htsSA, aes(x=income_cat, y= lnvmt))+
  geom_boxplot()

#deleting outliers 
htsSA_bp<-boxplot(htsSA$lnvmt~htsSA$income_cat)
ggsave(filename = "OutlierPressence.pdf",plot = htsSA_bp)

outliers <- htsSA_bp$out

htsSA[lnvmt%in%outliers,]
htsSA2<-htsSA[!lnvmt%in%outliers,]

boxplot(htsSA$lnvmt~htsSA$income_cat)
boxplot(htsSA2$lnvmt~htsSA2$income_cat)


# Step 1: dependent variable:

hist(htsSA2$lnvmt)

# Step 1: variance homogeneity
bartlett.test(lnvmt ~ income_cat, data=htsSA2)

# Step 4: one-way anova

fit<-aov(htsSA2$lnvmt~htsSA2$income_cat)
summary(fit)

#post-hoc test
TukeyHSD(fit)

TukeyPlot <- plot(TukeyHSD(fit))

ggsave(filename = "TukeyTestPlot.pdf",plot = TukeyPlot)


# 2. [30 points] Run a T-test and an ANOVA test in your data.

#H0 There is no significant association between walking to work and proximity to work in Bexar County.

library(tidycensus)
library(tidyverse)

#Proximity to Work
commute_time_labels <- c(
  B08303_001 = "Total_Commuters",
  B08303_002 = "Less_than_5_min",
  B08303_003 = "5_to_9_min",
  B08303_004 = "10_to_14_min"
)

SA_commute <- get_acs(
  geography = "tract",
  state = "TX",
  county = "Bexar",
  variables = names(commute_time_labels),
  year = 2022,
  survey = "acs5")

SA_commute <- SA_commute %>%
  select(GEOID, variable, estimate) %>%
  spread(variable, estimate) %>%
  rename_with(~ commute_time_labels[.x], .cols = names(commute_time_labels))

SA_commute <- SA_commute %>%
  mutate(Total_ShortTripCommuters = rowSums(across(c("Less_than_5_min", "5_to_9_min","10_to_14_min")), na.rm = TRUE),
         Pct_ShortTripCommuters = (Total_ShortTripCommuters / Total_Commuters) * 100)

#Percentage of residents walking to work

SA_Walk <- get_acs(
  geography = "tract",
  variables = c("B08301_001", "B08301_019"),
  state = "TX",
  county = "Bexar",
  year = 2022,
  survey = "acs5",
  wide = "true"
)

SA_Walk <- SA_Walk %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) %>%
  rename(
    total_commuters = B08301_001,
    walking_commuters = B08301_019
  )

SA_Walk <- SA_Walk %>%
  mutate(
    pct_walking_commuters = (walking_commuters / total_commuters) * 100
  )

SA_combined <- SA_commute %>%
  inner_join(SA_Walk, by = "GEOID")

SA_combined <- SA_combined %>%
  mutate(group = ifelse(Pct_ShortTripCommuters > 20, "Higher Short Trips", "Lower Short Trips"))

#Step 1 Normality Test

ggplot(data = SA_combined, aes(x = walking_commuters)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_grid(group ~ .)


#Step 1 Variance:
ggplot(data=SA_combined,aes(x=walking_commuters, y=group))+
  geom_boxplot()

#Conduct T Test
WALK_two_tailed_t_test <- t.test(formula = pct_walking_commuters ~ group, data = SA_combined) # two-tailed
WALK_two_tailed_t_test

WALK_one_tailed_t_test <- t.test(formula = pct_walking_commuters ~ group, alternative = 'greater', data = SA_combined) # one-tailed
WALK_one_tailed_t_test

curve(dt(x, df = 241.48), from = -10, to = 10)
abline(h=0,col='blue')
points(x=WALK_two_tailed_t_test$statistic,y=0,col='red')

upper975 <- qt(p = .975, df = 241.48)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df = 241.48)
abline(v = lower025,y=0,col='red')

#T-tests state the variables are statistically significant having p-values of 2.317e-07 and 1.159e-07 .This means that the null hypothesis (H0) can be rejected and the correlation of percentage of people walking to work and their commute timess are not due to chance.

# ANOVA

SA_combined_data <- as.data.table(SA_combined)

ggplot(data = SA_combined_data[, .(Mean_pct_walking_commuters = mean(pct_walking_commuters, na.rm = TRUE)), by = group], aes(x = group, y = Mean_pct_walking_commuters)) +
  geom_point()

# Step 1: Equal Variance Assumption: 
ggplot(data=SA_combined_data, aes(x=group, y= pct_walking_commuters))+
  geom_boxplot()

#deleting outliers 
SA_combined_data_bp<-boxplot(SA_combined_data$pct_walking_commuters~SA_combined_data$group)

outliers <- SA_combined_data$out

SA_combined_data[pct_walking_commuters%in%outliers, ]
SA_combined_data2<-SA_combined_data[!pct_walking_commuters%in%outliers,]

boxplot(SA_combined_data$pct_walking_commuters~SA_combined_data$group)
boxplot(SA_combined_data2$pct_walking_commuters~SA_combined_data2$group)

# Step 1: dependent variable:

hist(SA_combined_data2$pct_walking_commuters)

# Step 1: variance homogeneity
bartlett.test(pct_walking_commuters ~ group, data=SA_combined_data2)

# Step 4: one-way anova

fit_SA<-aov(SA_combined_data2$pct_walking_commuters~SA_combined_data2$group)
summary(fit_SA)

#post-hoc test
TukeyHSD(fit_SA)

TukeyPlot_SA <- plot(TukeyHSD(fit_SA))


