setwd("/Users/cindychiu/IDS702/final proj")
salary = read.csv('Levels_Fyi_Salary_Data.csv')
library(dplyr)
library(ggplot2)
library(mice)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(lme4)
library(sjPlot)
library(influence.ME)
library(lattice)
library(HLMdiag)
library(car)

###################### Data Cleaning  ######################
# make all the company name to lower case to aggregate without duplicate
salary$company_t = tolower(salary$company)

# the total yearly compensation distribution (has a very long tail)
# some data points with more than 2500000 are pulling the mean very high
hist(salary$totalyearlycompensation, breaks =  100)
boxplot(salary$totalyearlycompensation)
hist(salary[salary$totalyearlycompensation <= 1000000, 'totalyearlycompensation' ], breaks = 100)
abline(v= mean(salary$totalyearlycompensation), col='red')
abline(v= median(salary$totalyearlycompensation), col='blue')

hist(salary$totalyearlycompensation, breaks = 100)

## most people didn't fill out the complete data for base salary
## we will go for the total compensation 
hist(salary[salary$basesalary <= 280000 , 'basesalary' ], breaks = 50)
hist(salary$basesalary , breaks = 100)
boxplot(salary$basesalary)

## filter company with more than 100 data points in the dataset 
company_count = salary %>% group_by(company_t) %>% count()
more_than_hund_comp = company_count  %>% filter(n >= 100) %>% select(company_t)

plot(log(df$yearsofexperience), df$totalyearlycompensation)
plot(log(df$yearsatcompany), df$totalyearlycompensation)



df = salary %>% filter(company_t %in% more_than_hund_comp$company_t)
df = df %>% select(totalyearlycompensation, company_t, title, location,Race,Education, gender, yearsofexperience,yearsatcompany )
hist(df$totalyearlycompensation, breaks = 100)
boxplot(df$totalyearlycompensation)
md.pattern(df)

# splitting up the location string to city, state and country 
df_comp = separate(data = df, col = location, into = c("city", "state","country"), sep = ", ")
df_comp$country = ifelse(is.na(df_comp$country), 'United States', df_comp$country)


# removing potential bad data (from more 1000000, which is more than 995 quantile)
quantile(df_comp$totalyearlycompensation, c(.95, .975, .995))
nrow(df_comp[df_comp$totalyearlycompensation > 1000000,]) 
df_comp = df_comp %>% filter(totalyearlycompensation <= 1000000)

# modify some bad data (like country not being processed correctly )
# df_comp %>% filter(state =='Israel')
df_comp[df_comp$state == 'Israel', 'country'] = 'Israel'
df_comp[df_comp$country == 'Czech Republic', 'country'] = 'Czech Rep.'
df_comp[df_comp$country == 'Burma', 'country'] = 'Myanmar'
df_comp[df_comp$country == 'Marshall Islands', 'country'] = 'Marshall Is.'
df_comp[df_comp$country == 'Netherlands Antilles', 'country'] = 'Netherlands'
df_comp[df_comp$country == 'Hong Kong (SAR)', 'country'] = 'Hong Kong'



############################### EDA #####################################
df_comp$logtotalcomp = log(df_comp$totalyearlycompensation)
## salary v.s years of experience 
ggplot(df_comp,
       aes(x=yearsofexperience, y=logtotalcomp)) +
  geom_point() +
  geom_smooth(method="lm",col="red3") + 
  labs(title="Experience v.s Salary",
       x="Years of experience ",y="salary") + 
  theme_classic() + theme(legend.position="none") 

## salary v.s years of experience 
ggplot(df_comp,
       aes(x=yearsatcompany, y=logtotalcomp)) +
  geom_point() +
  geom_smooth(method="lm",col="red3") + 
  labs(title="Years at company v.s Salary",
       x="years at company",y="salary") + 
  theme_classic() + theme(legend.position="none") 


## company vs. salary 
ggplot(df_comp,
       aes(x=company_t, y=totalyearlycompensation, fill=company_t)) +
  geom_boxplot() +
  labs(title="Company v.s Total compensation",
       x="Company",y="Total compensation") + 
  theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


## state vs. salary 
ggplot(df_comp[df_comp$country=='United States', ],
       aes(x=state, y=totalyearlycompensation, fill=state)) +
  geom_boxplot() +
  labs(title="States in US v.s Total compensation",
       x="state",y="Total compensation") + 
  theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# NY CA and WA seems to have much higher median 


ggplot(df_comp,
       aes(x=country, y=totalyearlycompensation, fill=country)) +
  geom_boxplot() +
  labs(title="Country v.s Total compensation",
       x="country",y="Total compensation") + 
  theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# US and Switzerland have higher median 


ggplot(df_comp,
       aes(x=title, y=totalyearlycompensation, fill=title)) +
  geom_boxplot() +
  labs(title="title v.s Total compensation",
       x="title",y="Total compensation") + 
  theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
# Software Engineer Manageer has highest pay 

################# interaction between none hierchical terms #######################
# title v.s years of experience 
ggplot(df_comp,
       aes(x=yearsofexperience,  y=totalyearlycompensation)) +
  geom_point() +
  geom_smooth(method="lm",col="red3")+
  labs(title="year of experience v.s salary by title",
       x="year of experience",y="compensation") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ title)

# title v.s years at company

ggplot(df_comp,
       aes(x=yearsatcompany , y=totalyearlycompensation)) +
  geom_point() +
  geom_smooth(method="lm",col="red3")+
  labs(title="year at company v.s salary by title",
       x="years at company",y="compensation") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ title)




# years at company v.s years of experience
ggplot(df_comp, aes(x=yearsofexperience ,  y=yearsatcompany)) +
  geom_point() + geom_smooth(method="lm",col="red3")+
  labs(title="years at company v.s years of experience",
       x="years of experience",y="years at company") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ bulk_purchase)


################## hierichical ####################
ggplot(df_comp[df_comp$company_t %in% c('amazon','google','microsoft', 'facebook','apple','oracle','salesforce','intel','ibm'), ],
       aes(x=title, y=totalyearlycompensation, fill=title)) +
  geom_boxplot() +
  labs(title="title v.s Total compensation",
       x="title",y="Total compensation") + 
  theme_classic() + 
  theme(legend.position="none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~company_t)
# plotting the top 9 companies 



# plotting the top 9 companies 

ggplot(df_comp[df_comp$company_t %in% c('amazon','google','microsoft', 'facebook','apple','oracle','salesforce','intel','ibm'), ],
       aes(x=yearsofexperience,  y=logtotalcomp)) +
  geom_point() +
  geom_smooth(method="lm",col="red3")+
  labs(title="year of experience v.s salary by company",
       x="year of experience",y="compensation") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ company_t)

ggplot(df_comp[df_comp$company_t %in% c('amazon','google','microsoft', 'facebook','apple','oracle','salesforce','intel','ibm'), ],
       aes(x=yearsatcompany,  y=logtotalcomp)) +
  geom_point() +
  geom_smooth(method="lm",col="red3")+
  labs(title="year of experience v.s salary by title",
       x="year of experience",y="compensation") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ company_t)


country_agg = df_comp %>% group_by(country) %>% count()
country_more_than_10 = filter(country_agg, n>10)
sample_country <- sample(unique(country_more_than_10$country),16,replace=F)


#### sample country 
ggplot(df_comp[is.element(df_comp$country,sample_country),],
       aes(x=title, y=totalyearlycompensation, fill=title)) +
  geom_boxplot() +
  labs(title="title vs compensation by country",
       x="title",y="compensation") + 
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ country)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### sample country 
ggplot(df_comp[is.element(df_comp$country,c('United States','India','Canada', 'United Kingdom')),],
       aes(x=title, y=totalyearlycompensation, fill=title)) +
  geom_boxplot() +
  labs(title="title vs compensation by country",
       x="title",y="compensation") + 
  theme_classic() + theme(legend.position="none") + facet_wrap( ~ country)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(df_comp[is.element(df_comp$country,sample_country),], 
       aes(x=yearsofexperience,  y=logtotalcomp)) +
  geom_point() +
  geom_smooth(method="lm",col="red3")+
  labs(title="year of experience v.s salary by country",
       x="year of experience",y="compensation") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ country)

ggplot(df_comp[is.element(df_comp$country,sample_country),], 
       aes(x=yearsatcompany,  y=logtotalcomp)) +
  geom_point() +
  geom_smooth(method="lm",col="red3")+
  labs(title="years at company v.s salary by country",
       x="years at company",y="compensation") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ country)
# only a few countries have enough data points 


company_agg = df_comp %>% group_by(company_t) %>% count()
company_more_than_10 = filter(company_agg, n>10)
sample_company <- sample(unique(company_more_than_10$company_t),16,replace=F)
test = df_comp %>% filter(company_t %in% sample_company) %>% filter(country %in% sample_country)

### company v.s title 
ggplot(test,
       aes(x=country, y=totalyearlycompensation, fill=title)) +
  geom_boxplot() +
  labs(title="title vs compensation by country",
       x="titel",y="compensation") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ company_t)



## Trying to see how many countries and freq we have 
## To determine whether we should split to 'US'/'Non-US' or continent etc 

world <- ne_countries(scale = "medium", returnclass = "sf")
world2 = left_join(world, country_agg, by= c('name'='country') )
ggplot(data = world2) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient2()

############################# Model Selection ##############################
# filter out the data points that have less than 10 data points 
country_filtered = df_comp[is.element(df_comp$country,country_more_than_10$country),]
country_filtered$logtotalcomp = log(country_filtered$totalyearlycompensation)


full_sample <- country_filtered %>% sample_n(4546)


nullModel1 = lm(data = country_filtered , logtotalcomp~ 1)
fullModel1 = lm(data = country_filtered, logtotalcomp ~ yearsatcompany* yearsofexperience *title  )


n = nrow(country_filtered)
bic1 = step(nullModel1, scope = formula(fullModel1),direction="both",trace=0, k=log(n))
aic1 = step(nullModel1, scope = formula(fullModel1),direction="both",trace=0)
aic2 = lm(data = country_filtered, logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
            yearsofexperience:yearsatcompany + title:yearsatcompany)
aic1$call
bic1$call
anova(bic1, aic1)

bic4 = step(nullModel1, scope = formula(fullModel1),direction="forward",trace=0, k=log(n))
aic4 = step(nullModel1, scope = formula(fullModel1),direction="forward",trace=0)
aic4$call
bic4$call

bic5 = step(nullModel1, scope = formula(fullModel1),direction="backward",trace=0, k=log(n))
aic5 = step(nullModel1, scope = formula(fullModel1),direction="backward",trace=0)
aic5$call
bic5$call


###########################################################################

### using the log for y 
### the model with interaction, which is the AIC model, is more significant 

vif(aic2)
plot(aic3, which=2)

# the vif score for aic 1 and aic 2 is really high
# using aic model 3 is reducing the 
aic3 = lm(data = country_filtered, logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
            title:yearsatcompany)

aic2$call
bic2$call
### not using the log for y 
### the model with interaction, which is the AIC model, is more significant 

Modelt1 <- lmer(logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                  yearsofexperience:yearsatcompany + title:yearsatcompany + (1|country), data= country_filtered,
                control = lmerControl(calc.derivs=FALSE)) 
Modelt2 <- lmer(logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                  yearsofexperience:yearsatcompany + title:yearsatcompany + (1|company_t), data= country_filtered,
                control = lmerControl(calc.derivs=FALSE)) 
Modelt3 <- lmer(logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                  yearsofexperience:yearsatcompany + title:yearsatcompany + (1|company_t) + (title|country)
                , data= country_filtered,
                control = lmerControl(calc.derivs=FALSE)) 
Modelt4 <- lmer(logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                  title:yearsatcompany + (1|company_t) + (title|country)
                , data= country_filtered,
                control = lmerControl(calc.derivs=FALSE)) 
Modelt5 <- lmer(logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                  title:yearsatcompany + (1|company_t) + (1|country)
                , data= country_filtered,
                control = lmerControl(calc.derivs=FALSE)) 
Modelt6 <- lmer(logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                  title:yearsatcompany + (1|company_t) 
                , data= country_filtered,
                control = lmerControl(calc.derivs=FALSE)) 

Modelt7 <- lmer(logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                  title:yearsatcompany + (1|country) 
                , data= country_filtered,
                control = lmerControl(calc.derivs=FALSE)) 
## the one with all the interaction 
AIC(Modelt1) # 40528.26
AIC(Modelt2) # 48667.87
AIC(Modelt3) # 17109.34 # not using 
###############################
## the acutal one that we need to use 
AIC(Modelt4) # 18354.03
AIC(Modelt5) # 18754.17
AIC(Modelt6) # 49209.1
AIC(Modelt7) #  41253.5



############################# Model Assessment ##############################
# fitted values v.s residuals
# don't see a particular trend here so we're good 
plot(Modelf2, which=2)
plot(Modelt3, which=2)
plot(Modelt4, which=2)

vif(Modelt4)

### Normality 
# the left tail is not fitting it well
# aligns with what we've seen
# but we can't do much to improve this 

qqnorm(residuals(Modelt4), xlim=c(-4,4), ylim=c(-4,4))
qqline(residuals(Modelt4), col='red')


### Linearity
ggplot(country_filtered, aes(x = yearsofexperience, y = residuals(Modelt4))) + geom_point(alpha=.07)+
  # geom_hline(yintercept = 0, col='red3')+
  geom_smooth(method="lm",col="red3") +
  theme_classic() + labs(title = 'Residuals v.s Experience', x= 'Experience', y='residuals')

ggplot(country_filtered, aes(x = yearsatcompany, y = residuals(Modelt4))) + geom_point(alpha=.07)+
  # geom_hline(yintercept = 0, col='red3')+
  geom_smooth(method="lm",col="red3") +
  theme_classic() + labs(title = 'Residuals v.s Years at company', x= 'Years at company', y='residuals')


############################# Interpretation ##############################
library(ggthemes)
ggplot(data = country_filtered, aes(x = yearsofexperience, y = predict(Modelt4),color=title)) +
  geom_smooth(method = "lm", fullrange = FALSE, size = 0.5) +
  geom_jitter(aes(x = yearsofexperience, y =logtotalcomp, group = title, color=title),
              alpha = 0.05) +
  labs(x = "years of experience", y = "log total compensation") +
  ggtitle("Random Effects Model") +
  scale_colour_discrete('Title') +
  theme_tufte()+
  theme(text = element_text(size=12),legend.position = c(0.9, 0.4))


plt = dotplot(ranef(Modelt4, condVar=TRUE))$country
plt
# woah netflix is so different from others 
dotplot(ranef(Modelt4, condVar=TRUE))$company_t
## extreme country
(ranef(Modelt4)$country)["United States",]
(ranef(Modelt4)$country)["Mexico",]
## extreme company
(ranef(Modelt4)$company_t)["general motors",]
(ranef(Modelt4)$company_t)["netflix",]

tab_model(Modelt4 )

################################################################################################ 
########################################### Part II ############################################
################################################################################################ 


############################# Data Cleaning ##############################

# trying to look at only the complete data set 
# dropping all the NA for race, gender and education 
md.pattern(complete_set)
# we have around 14k complete data 
complete_set <- drop_na(df_comp, Race)
complete_set <- drop_na(complete_set, gender)
complete_set <- drop_na(complete_set, Education)

# still have to remove the country only having less than 10 data point 
country_agg_t = complete_set %>% group_by(country) %>% count()
country_more_than_10_t = filter(country_agg_t, n>10)
complete_filtered = complete_set[is.element(complete_set$country,country_more_than_10_t$country),]
complete_filtered$logtotalcomp = log(complete_filtered$totalyearlycompensation)

complete_filtered$gender = as.factor(complete_filtered$gender)
complete_filtered$Race = as.factor(complete_filtered$Race)
complete_filtered$Education = as.factor(complete_filtered$Education)


############################# Data Cleaning ##############################

nullModel_comp= lm(data = complete_filtered , logtotalcomp~ 1)
nullModel_comp2 = lm(data = complete_filtered, logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                       title:yearsatcompany)
fullModel_comp = lm(data = complete_filtered, logtotalcomp ~  title * yearsofexperience* yearsatcompany* Race * Education * gender )
summary(fullModel_comp)


complete_filtered %>% group_by(gender) %>% count() #3 levels 
complete_filtered %>% group_by(Race) %>% count() # 5 levels 
complete_filtered %>% group_by(Education) %>% count() # 5 levels 



n_comp = nrow(complete_filtered)
bic_comp = step(nullModel_comp, scope = formula(fullModel_comp),direction="both",trace=0, k=log(n_comp))
aic_comp = step(nullModel_comp, scope = formula(fullModel_comp),direction="both",trace=0)
aic_comp$call
bic_comp$call

bic_comp2 = step(nullModel_comp2, scope = formula(fullModel_comp),direction="both",trace=0, k=log(n_comp))
aic_comp2 = step(nullModel_comp2, scope = formula(fullModel_comp),direction="both",trace=0)
aic_comp2$call
bic_comp2$call
# aic wins (it's significant )
# more interactions 
anova(bic_comp, test)

### compare the non multilevel part
test = lm(logtotalcomp ~ yearsofexperience + Education + title + 
            Race + yearsatcompany + gender + Education:Race + 
            yearsofexperience:title + title:yearsatcompany+
            Education:gender + Race:gender + 
            yearsatcompany:gender, data = complete_filtered)

orig = lm( logtotalcomp ~ yearsofexperience + title + yearsatcompany + 
             yearsofexperience:title + title:yearsatcompany, data = complete_filtered)


### full model using the complete data set 
Model_comp <- lmer(logtotalcomp ~ yearsofexperience + Education + title + 
                     Race + yearsatcompany + gender + Education:Race + 
                     yearsofexperience:title + title:yearsatcompany+
                     Education:gender + Race:gender + 
                     yearsatcompany:gender
                   + ( 1 | company_t) + (title|country), data = complete_filtered,
                   control = lmerControl(calc.derivs=FALSE)) 


Model_comp_orig <- lmer(logtotalcomp ~ yearsofexperience + title + yearsatcompany + yearsofexperience:title + 
                          title:yearsatcompany + (1|company_t) + (title|country),  data = complete_filtered,
                        control = lmerControl(calc.derivs=FALSE)) 


AIC(Model_comp) # 6107.195
AIC(Model_comp_orig) # 6589.9

summary(Model_comp_orig) # the residual variance 0.29
summary(Model_comp) # the residual variance 0.28


tab_model(Model_comp_orig)
tab_model(Model_comp)
