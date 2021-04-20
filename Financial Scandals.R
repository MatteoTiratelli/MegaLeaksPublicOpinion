##########################################

##########  REPLICATION FILE    ##########

##########################################

# Libor Rate Setting: July 2012
# Offshore Leaks: April 2013
# Luxembourg Leaks: 5 November 2014
# Swiss Leaks: February 2015
# Panama Papers: 3 April 2016
# Paradise Papers: 5 November 2017

# Get German survey data and add, cut Libor

# LexisNexis: https://github.com/JBGruber/LexisNexisTools, https://dlab.berkeley.edu/blog/human-rights-coverage-over-time-tutorial-automated-text-analysis
# Hansard API, Congressional Record API

# ALLBUS (Germany) - Libor is only match
# 2012: April 2012 to September 2012
# 2014: March 2014 to September 2014
# 2016: April 2016 to September 2016
# 2018: April 2018 to September 2018

# ZA6888: Politbarometer 2016 (weekly collection from jan to dec) - same data 
# collection process annually so could match it to all presumably - 
#   has a left-right self-placement variable; tax evasion question (Steuerhinterziehung)

# GLES - collected in 2013, 2017 - no match

# German Socio-Economic Panel - biannual Jan-Aug 2017, Jan-Aug 2018 etc
# NB: "For all samples, the last months of fieldwork are dedicated to contacting 
# difficult-to-reach households and respondents."

## Title: Do financial corruption scandals affect public opinion? Evidence from natural experiments relating to Libor, LuxLeaks and the Paradise Papers

## This replication file requires the following packages: tidyverse, haven, gtrendsR, rvest and gghighlight

## Data (registration require for download): 
# British Social Attitudes 2012 - http://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200006
# General Social Survey 2012 - https://www.icpsr.umich.edu/web/NADAC/studies/35478
# European Social Survey 2014 - https://www.europeansocialsurvey.org/data/download.html?r=7
# ESS 2014 Integrated contact form data file - https://www.europeansocialsurvey.org/download.html?file=ESS7CFe02_1&y=2014
# European Values Survey 2017 - https://dbk.gesis.org/dbksearch/SDesc2.asp?ll=10&notabs=1&af=&nf=&search=&search2=&db=E&no=7500

## Table of contents
# Section 1: Libor-BSA
# Section 2: Libor-GSS
# Section 3: LuxLeaks-ESS
# Section 4: Paradise-EVS
# Section 5: Google Trends
# Section 6: Tables and figures

## Each section begins with the data cleaning, and then proceeds to the regression analysis
# apart from the final section which builds the figures from the earlier regression analyses


library(tidyverse)
library(haven)
library(rvest)
library(parameters)
library(sandwich)

setwd("/Users/matteo/Downloads/FinancialScandals/Data")

Output <- tibble(variable = character(), event = character(), country = character(), 
                 coef = numeric(), min = numeric(), max = numeric(), pvalue = numeric(), n = numeric())


##########################################

##########      Libor-BSA       ##########

##########################################

## BSA 2012 - data cleaning
# Libor Rate Setting: Barclays fined 27 June
# fieldwork Jun - Sept 2012

#SA_12 <- as_tibble(read.delim("bsa12.tab", stringsAsFactors = FALSE, sep = "\t"))
# Emailed asking where I can find the interview date records



##########################################

##########      Libor-GSS       ##########

##########################################

## GSS 2012 - data cleaning
# Libor Rate Setting: Barclays fined 27 June
# fieldwork 20/03/2012 - 05/09/2012

GSS_12 <- as_tibble(read.delim("35478-0001-Data.tsv", stringsAsFactors = FALSE, sep = "\t"))

GSS_12$Treatment <- ifelse(GSS_12$DATEINTV > 627 & GSS_12$DATEINTV < 713, 1, 
                           ifelse(GSS_12$DATEINTV > 712, NA, 0))
table(GSS_12$Treatment) # Treated = 808, Control = 4012, two weeks leaves treated at 249


## GSS 2012 Analysis

# CONFINAN: Banks and financial institutions, CONBUS: Major companies
# Question: As far as the people running this institution are concerned, 
# would you say you have a great deal of confidence, only some confidence, 
# or hardly any confidence at all in them? 1 =	A great deal, 2 =	Only some, 3 = Hardly any
# HELPNOT: Should government do more or less? 1 = more, 5 = less
# TRUST: 1 = Can trust, 2 = Cannot trust, 3 = Depends
# POLVIEWS: 1 = Extremely Liberal, 7 = Extremely Conservative
# SEX: 1 = M, 2 = F
# NEWS: How often read a newspaper? 1 = Everyday, 5 = Never

GSS_12$CONFINAN <- ifelse(GSS_12$CONFINAN %in% c(0,8,9), NA, GSS_12$CONFINAN) # Only asked to subsample of GSS that year
GSS_12$zCONFINAN <- scale(GSS_12$CONFINAN)
GSS_12$CONBUS <- ifelse(GSS_12$CONBUS %in% c(0,8,9), NA, GSS_12$CONBUS) # Only asked to subsample of GSS that year
GSS_12$zCONBUS <- scale(GSS_12$CONBUS)
GSS_12$HELPNOT <- ifelse(GSS_12$HELPNOT %in% c(0,8,9), NA, GSS_12$HELPNOT) # Only asked to subsample of GSS that year
GSS_12$zHELPNOT <- scale(GSS_12$HELPNOT)
GSS_12$TRUST <- ifelse(GSS_12$TRUST %in% c(0,8,9), NA, GSS_12$TRUST)
GSS_12$TRUST <- ifelse(GSS_12$TRUST == 2, 2.5, GSS_12$TRUST) # Swap cannot and depends
GSS_12$TRUST <- ifelse(GSS_12$TRUST == 3, 2, GSS_12$TRUST)
GSS_12$TRUST <- ifelse(GSS_12$TRUST == 2.5, 3, GSS_12$TRUST)
GSS_12$zTRUST <- scale(GSS_12$TRUST)
GSS_12$POLVIEWS <- ifelse(GSS_12$POLVIEWS %in% c(0,8,9), NA, GSS_12$POLVIEWS) # Only asked to subsample of GSS that year
GSS_12$zPOLVIEWS <- scale(GSS_12$POLVIEWS)
GSS_12$INCOME06 <- ifelse(GSS_12$INCOME06 %in% c(26, 98), NA, GSS_12$INCOME06)
GSS_12$zINCOME06 <- scale(GSS_12$INCOME06)
GSS_12$EDUC <- ifelse(GSS_12$EDUC %in% c(98,99), NA, GSS_12$EDUC)
GSS_12$zEDUC <- scale(GSS_12$EDUC)
GSS_12$AGE <- ifelse(GSS_12$AGE %in% c(98,99), NA, GSS_12$AGE)
GSS_12$zAGE <- scale(GSS_12$AGE)
GSS_12$NEWS <- ifelse(GSS_12$NEWS %in% c(0,8,9), NA, GSS_12$NEWS) # Only asked to subsample of GSS that year
GSS_12$zNEWS <- scale(GSS_12$NEWS)

DV <- c('zCONBUS','zTRUST','zPOLVIEWS')
variables <- c('Confidence in major corporations','Social trust',
               'Left-Right')

for (i in 1:3){
  model <- glm(paste(DV[i], '~ factor(Treatment) + zAGE + factor(SEX) + zINCOME06 + zEDUC'),
               family = gaussian, data = GSS_12, weights = WTCOMBNR)
  temp <- confint(model, parm = 'factor(Treatment)1', level = 0.95)
  results <- tibble(variable = as.character('2010'), event = as.character('2010'), 
                    country = as.character('2010'), 
                    coef = as.numeric(2010), min = as.numeric(2010), max = as.numeric(2010))
  results$variable[1] <- variables[i]
  results$event[1] <- 'Libor Scandal (27/06/2012)'
  results$country[1] <- 'US'
  results$coef[1] <- model$coefficients[2]
  results$min[1] <- temp[1]
  results$max[1] <- temp[2]
  results$pvalue <- summary(model)$coefficients[,4][2]
  results$n <- sum(GSS_12$Treatment == 1, na.rm=TRUE)
  bind_rows(Output, results) -> Output
}

remove(DV, variables, i, temp, results, GSS_12, model)


##########################################

##########    LuxLeaks-ESS      ##########

##########################################

## ESS 2014 - Luxembourg Leaks: 5 November 2014
# Countries with fieldwork overlap: Austria, Belgium, Denmark, Estonia, Finland, France, 
# Germany, Ireland, Netherlands, Norway, Slovenia, Sweden, Switzerland, UK

ESS_contact <- read_dta("ess7CFe02_1.dta")
ESS_contact <- ESS_contact[, grepl( "date|monv|idno|cntry|yearv", names(ESS_contact))]
ESS_contact[ESS_contact == 66 ] <- NA
ESS_contact[ESS_contact == 99 ] <- NA
ESS_contact[ESS_contact == 6666 ] <- NA
ESS_contact[ESS_contact == 9999 ] <- NA
ESS_contact$proddate <- NULL
GB <- ESS_contact[ESS_contact$cntry=='GB',]
ESS_contact <- ESS_contact[!ESS_contact$cntry=='GB',]

ESS_contact[,1:162] %>%
  pivot_longer(!c(idno, cntry), names_to = "variable", values_to = "values") %>%
  dplyr::group_by(idno, cntry) %>% # real slow...
  fill(values, .direction = "down") %>%
  dplyr::ungroup() %>%
  .[.$variable %in% c('date80','monv80'),] %>%
  mutate(values = sprintf("%02d",.$values)) %>%
  mutate(row = row_number()) %>%
  pivot_wider(id_cols = c(row, idno, cntry),
              names_from = variable, 
              values_from = values) %>%
  select(-row) %>%
  fill(date80, .direction = "down") %>%
  fill(monv80, .direction = "up") %>%
  distinct(idno, cntry, .keep_all = TRUE) %>%
  mutate(intdate = paste(monv80, date80, sep="")) %>%
  .[,c(1,2,5)] -> LookUp
remove(ESS_contact)

GB %>%
  pivot_longer(!c(idno, cntry), names_to = "variable", values_to = "values") %>%
  dplyr::group_by(idno) %>%
  fill(values, .direction = "down") %>%
  dplyr::ungroup() %>%
  .[.$variable %in% c('date80','monv80','yearv20'),] %>%
  mutate(values = sprintf("%02d",.$values)) %>%
  mutate(row = row_number()) %>%
  pivot_wider(id_cols = c(row, idno, cntry),
              names_from = variable, 
              values_from = values) %>%
  select(-row) %>%
  dplyr::group_by(idno) %>%
  fill(monv80, .direction = "downup") %>%
  dplyr::ungroup() %>%
  fill(date80, .direction = "down") %>%
  fill(yearv20, .direction = "up") %>%
  distinct(idno, cntry, .keep_all = TRUE) %>%
  mutate(intdate = paste(yearv20, monv80, date80, sep="")) %>%
  .[,c(1,2,6)] %>%
  filter(as.numeric(.$intdate)<20151001) %>% # removes second late period of fieldwork
  bind_rows(LookUp,.) -> LookUp
remove(GB)

# read_html('https://www.europeansocialsurvey.org/data/deviations_7.html') %>% 
#   html_nodes("table.zebra") -> Fieldwork
# as_tibble(html_table(Fieldwork[[1]])) -> Fieldwork

LookUp %>%
  mutate(year = 2015) %>%
  mutate(year = ifelse(as.numeric(intdate)>0505 & cntry == 'AT', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0201 & cntry == 'BE', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0209 & cntry == 'CZ', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0217 & cntry == 'DK', 2014, year)) %>%
  mutate(year = ifelse(cntry == 'EE', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0209 & cntry == 'FI', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0303 & cntry == 'FR', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0205 & cntry == 'DE', 2014, year)) %>% # HU all in 2015
  mutate(year = ifelse(as.numeric(intdate)>0131 & cntry == 'IE', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0115 & cntry == 'NL', 2014, year)) %>% # IL, LT all in 2015
  mutate(year = ifelse(as.numeric(intdate)>0108 & cntry == 'NO', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0217 & cntry == 'DK', 2014, year)) %>% # PL, PT all 2015
  mutate(year = ifelse(as.numeric(intdate)>0201 & cntry == 'SI', 2014, year)) %>%
  mutate(year = ifelse(as.numeric(intdate)>0130 & cntry == 'SE', 2014, year)) %>% # ES all in 2015
  mutate(year = ifelse(as.numeric(intdate)>0220 & cntry == 'CH', 2014, year)) -> LookUp
LookUp$intdate <- paste0(LookUp$year, LookUp$intdate)
LookUp$year <- NULL

ESS_14 <- read_dta("ESS7e02_2.dta")
ESS_14 <- merge(ESS_14, LookUp, by = c('idno', 'cntry'), all.x = TRUE)
ESS_14 <- ESS_14[ESS_14$cntry %in% c('AT','BE','DK','FI','FR','DE','IE','NL',
                                     'NO','SE','SI','CH'),] # Only countries where fieldwork overlaps
remove(LookUp, Fieldwork)

ESS_14$Treatment <- ifelse(ESS_14$intdate > 20141105 & ESS_14$intdate < 20141120, 1, 
                           ifelse(ESS_14$intdate > 20141119, NA, 0))
table(ESS_14$Treatment) #   not treated: 12618, treated: 11832 

## ESS 2014 analysis

ESS_14$ppltrst <- ifelse(ESS_14$ppltrst %in% c(77, 88, 99), NA, ESS_14$ppltrst)
ESS_14$ppltrst <- ((ESS_14$ppltrst+1)*-1)+11 # inverted so that 10 is can't trust
ESS_14$lrscale <- ifelse(ESS_14$lrscale %in% c('NA(b)', 'NA(c)', 'NA(d)'), NA, ESS_14$lrscale) # 10 is right
ESS_14$gndr <- ifelse(ESS_14$gndr == 'NA(d)', NA, ESS_14$gndr) # 2 is female
ESS_14$agea <- ifelse(ESS_14$agea == 'NA(d)', NA, ESS_14$agea)
ESS_14$hinctnta <- ifelse(ESS_14$hinctnta %in% c('NA(b)', 'NA(c)', 'NA(d)'), NA, ESS_14$hinctnta) # 
ESS_14$edulvlb <- ifelse(ESS_14$edulvlb %in% c(5555, 'NA(b)', 'NA(c)', 'NA(d)'), NA, ESS_14$edulvlb) # NB am treating these ordered levels as numbers but they jump aroud a fair bit...

countries <- c('AT','BE','DK','FI','FR','DE','IE','NL',
               'NO','SE','SI','CH')

DV <- c('Zppltrst','Zlrscale') # No variable for trust major corporations
variables <- c('Social trust','Left-Right')

for (i in 1:2){
  for (j in 1:12){
    subset <- ESS_14[ESS_14$cntry == countries[j],]
    subset$Zppltrst <- scale(subset$ppltrst)
    subset$Zlrscale <- scale(subset$lrscale)
    subset$Zagea <- scale(subset$agea)
    subset$Zhinctnta <- scale(subset$hinctnta)
    subset$Zedulvlb <- scale(subset$edulvlb)
    model <- glm(paste(DV[i], '~ factor(Treatment) + Zedulvlb + factor(gndr) + Zagea + Zhinctnta'), 
                 family = gaussian, data = subset, weights = pspwght)
    temp <- confint(model, parm = 'factor(Treatment)1', level = 0.95)
    results <- tibble(variable = as.character('2010'), event = as.character('2010'), 
                      country = as.character('2010'), 
                      coef = as.numeric(2010), min = as.numeric(2010), max = as.numeric(2010))
    results$variable[1] <- variables[i]
    results$event[1] <- 'Luxemburg Leaks (05/11/2014)'
    results$country[1] <- countries[j]
    results$coef[1] <- model$coefficients[2]
    results$min[1] <- temp[1]
    results$max[1] <- temp[2]
    results$pvalue <- summary(model)$coefficients[,4][2]
    results$n <- sum(subset$Treatment == 1, na.rm=TRUE)
    bind_rows(Output, results) -> Output
  }
}

remove(ESS_14, model, results, subset, countries, DV, i, j, temp, variables)



##########################################

##########    Paradise-EVS      ##########

##########################################

## EVS 2017 - Paradise Papers: 5 November 2017
# Countries with fieldwork overlap: Croatia, Czechia, Denmark, Germany, 
# Iceland, Netherlands, Slovakia, Slovenia, Sweden, Switzerland

EVS_17 <- read_dta("ZA7500_v4-0-0.dta")
EVS_17$intdate <- ifelse(EVS_17$v277 %in% c(-4,-2), NA, EVS_17$v277)
EVS_17 <- EVS_17[EVS_17$c_abrv %in% c('HR','CZ','DK','DE','IS','NL','SK','SI','SE','CH'),]
EVS_17$Treatment <- ifelse(EVS_17$intdate > 20171105 & EVS_17$intdate < 20171120, 1, 
                           ifelse(EVS_17$intdate > 20171119, NA, 0))
table(EVS_17$Treatment)

EVS_17$v261 <- ifelse(EVS_17$v261 < 0, NA, EVS_17$v261) # income
EVS_17$v243_edulvlb <- ifelse(EVS_17$v243_edulvlb < 0, NA, EVS_17$v243_edulvlb) # education level
EVS_17$v243_edulvlb <- ifelse(EVS_17$v243_edulvlb == 6666, NA, EVS_17$v243_edulvlb)
EVS_17$v226 <- ifelse(EVS_17$v261 < 0, NA, EVS_17$v226) # year of birth
EVS_17$v225 <- ifelse(EVS_17$v225 < 0, NA, EVS_17$v225) # gender 2 is female

EVS_17$v128 <- ifelse(EVS_17$v128 < 0, NA, EVS_17$v128) # v128 trust in major companies, 4 none at all
EVS_17$v150 <- ifelse(EVS_17$v150 < 0, NA, EVS_17$v150) # v150 ok to cheat on tax, 10 always
EVS_17$v31 <- ifelse(EVS_17$v31 < 0, NA, EVS_17$v31) # v31 general social trust, 2 can't trust
EVS_17$v102 <- ifelse(EVS_17$v102 < 0, NA, EVS_17$v102) # Left-Right self-placement, right is 10

countries <- c('HR','CZ','DK','DE','IS','NL','SK','SI','SE','CH')

DV <- c('Zv128','Zv31','Zv102')
variables <- c('Confidence in major corporations','Social trust',
               'Left-Right')
for (i in 1:3){
  for (j in 1:10){
    subset <- EVS_17[EVS_17$c_abrv == countries[j],]
    subset$Zv102 <- scale(subset$v102)
    subset$Zv261 <- scale(subset$v261)
    subset$Zv243_edulvlb <- scale(subset$v243_edulvlb)
    subset$Zv226 <- scale(2017-subset$v226)
    subset$Zv128 <- scale(subset$v128)
    subset$Zv31 <- scale(subset$v31)
    subset$Zv150 <- scale(subset$v150)
    model <- glm(paste(DV[i], '~ factor(Treatment) + Zv243_edulvlb + factor(v225) + Zv261 + Zv226'), 
                 family = gaussian, data = subset, weights = gweight)
    temp <- confint(model, parm = 'factor(Treatment)1', level = 0.95)
    results <- tibble(variable = as.character('2010'), event = as.character('2010'), 
                      country = as.character('2010'), 
                      coef = as.numeric(2010), min = as.numeric(2010), max = as.numeric(2010))
    results$variable[1] <- variables[i]
    results$event[1] <- 'Paradise Papers (05/11/2017)'
    results$country[1] <- countries[j]
    results$coef[1] <- model$coefficients[2]
    results$min[1] <- temp[1]
    results$max[1] <- temp[2]
    results$pvalue <- summary(model)$coefficients[,4][2]
    results$n <- sum(subset$Treatment == 1, na.rm=TRUE)
    bind_rows(Output, results) -> Output
  }
}

remove(countries, DV, i, j, temp, variables, subset, results, model, EVS_17)



##########################################

##########  Figures and tables  ##########

##########################################

## Coeficient plots

Output$labels <- paste0(Output$country, ' (# treated = ',Output$n,')')

Output[Output$variable == "Confidence in major corporations",] -> temp
ggplot(temp, aes(y = as.factor(labels), x = coef)) +
  geom_point(alpha = 0.4, size = 3) + geom_errorbarh(aes(xmin = min, xmax = max), size = 0.2, alpha = 0.4, height = 0) +
  geom_point(data=temp[temp$pvalue<=0.05,], size = 3) + geom_errorbarh(data=temp[temp$pvalue<=0.05,], aes(xmin = min, xmax = max), size = 0.25, height = 0) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey') +
  theme_bw() +
  ylab('Country') + xlab('Standardised coefficients') + ggtitle("Major companies") +
  facet_grid(rows = vars(event), scales = 'free_y', space = 'free_y') #-> Confidence_plot

Output[Output$variable == "Social trust",] -> temp
ggplot(temp, aes(y = as.factor(labels), x = coef)) +
  geom_point(alpha = 0.4, size = 3) + geom_errorbarh(aes(xmin = min, xmax = max), size = 0.2, alpha = 0.4, height = 0) +
  geom_point(data=temp[temp$pvalue<=0.05,], size = 3) + geom_errorbarh(data=temp[temp$pvalue<=0.05,], aes(xmin = min, xmax = max), size = 0.25, height = 0) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey') +
  theme_bw() +
  ylab('Country') + xlab('Standardised coefficients') + ggtitle('Trust') +
  facet_grid(rows = vars(event), scales = 'free_y', space = 'free_y') #-> Trust_plot

Output[Output$variable == "Left-Right",] -> temp
ggplot(temp, aes(y = as.factor(labels), x = coef)) +
  geom_point(alpha = 0.4, size = 3) + geom_errorbarh(aes(xmin = min, xmax = max), size = 0.2, alpha = 0.4, height = 0) +
  geom_point(data=temp[temp$pvalue<=0.05,], size = 3) + geom_errorbarh(data=temp[temp$pvalue<=0.05,], aes(xmin = min, xmax = max), size = 0.25, height = 0) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey') +
  theme_bw() +
  ylab('Country') + xlab('Standardised coefficients') + ggtitle("Left-Right") +
  facet_grid(rows = vars(event), scales = 'free_y', space = 'free_y') #-> LR_plot

# to remove facet_grid labels: theme(strip.background = element_blank(), strip.text = element_blank())

##########################################

##########    Google Trends     ##########

##########################################

## Data from TaxTrends.py


## Google trends

files <- c("Tax Evasion.csv", "Tax Haven.csv", "Kim Kardashian.csv")
labels <- c("Tax Evasion", "Tax Haven", "Placebo")
OECD <- c("AU","AT","BE","CA","CL","CO","CZ","DK","EE","FI","FR","DE","GR","HU","IS","IE",
          "IL","IT","JP","KI","LV","LT","LU","MX","NL","NZ","NO","PL","PT","SX","SI","ES",
          "SE","CH","TR","AE","TZ")
dates <- c(#'2012-06-01',  # Libor 
           '2013-04-01',  # Offshore Leaks
           '2014-11-01',  # Lux Leaks
           '2015-02-01',  # Swiss Leaks
           '2016-04-01',  # Panama Papers
           '2017-11-01',  # Paradise Papers
           '2020-08-15')  # FinCEN Papers

Output <- tibble(Parameter = character(), Coefficient = numeric(), 
                 CI_low = numeric(), CI_high = numeric(), p = numeric(), search = character())

for (i in 1:3){
  GoogleTrend <- read.csv(files[i])
  GoogleTrend$date <- as.POSIXct(GoogleTrend$date)
  GoogleTrend$Treatment <- ifelse(as.character(GoogleTrend$date) %in% dates,1,0)
  names(GoogleTrend)[1] <- "geo"
  names(GoogleTrend)[3] <- "hits"
  GoogleTrend <- GoogleTrend[GoogleTrend$geo %in% OECD,]
  GoogleTrend %>%
    group_by(geo) %>%
    mutate(trend = row_number(),
           lag = dplyr::lag(Treatment, n = 1, default = NA),
           lead = dplyr::lead(Treatment, n = 1, default = NA)) -> GoogleTrend
  model <- lm(hits ~ factor(lag) + factor(Treatment) + 
                          factor(lead) + factor(geo)*trend + factor(geo), 
                        data = GoogleTrend)
  mp <- model_parameters(model, robust = TRUE, vcov_estimation = "CL", vcov_type = "HC1", vcov_args = list(cluster = GoogleTrend$geo))
  results <- as_tibble(mp[2:4,])
  results <- results[,c(1,2,4,5,8)]
  results$search <- labels[i]
  bind_rows(Output, results) -> Output
}

Output$Parameter %>%
  as.factor() %>%
  fct_recode('t-1' = 'factor(lead)1', 't' = 'factor(Treatment)1', 't+1' = 'factor(lag)1') -> Output$Parameter
Output$Parameter <- factor(Output$Parameter, levels = c("t+1", "t", "t-1"))

ggplot(Output, aes(y = as.factor(Parameter), x = Coefficient)) +
  geom_point(size = 3) + geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), size = 0.2, height = 0) +
  geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey') +
  theme_bw() +
  ylab('Month of Data Leak') + xlab("Coefficients with 95% cluster-robust confidence intervals") + 
  labs(title = "Figure 4: The impact of data leaks on Google search activity", caption = "Notes: Estimated effect on monthly Google search activity for each topic listed. Models estimated with country level fixed effects, \ncountry-specific linear trends and cluster robust standard errors.") +
  facet_grid(rows = vars(search), scales = 'free_y', space = 'free_y') +
  theme(plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0)) -> GooglePlot


#### Handsard

Hansard <- read_html("https://hansard.parliament.uk/search/Contributions?endDate=2020-11-01&searchTerm=tax+haven&startDate=2012-01-01")

Hansard %>% 
  html_node("main#main-content") %>%
  html_text("div.search-results") %>%
  html_nodes("div.tertiary-info") %>%
  html_text()
 ### NOT WORKING

TRY PYTHON???
  
  import requests
from bs4 import BeautifulSoup
import csv
import unicodecsv as csv

url = 'https://hansard.parliament.uk/search/Contributions?endDate=2020-11-01&searchTerm=tax+haven&startDate=2012-01-01'
response = requests.get(url)
html = response.content
soup = BeautifulSoup(html, "html.parser")
list = soup.find("div", class_='card-list')
dates = list.findAll("div", class_="tertiary-info)
                     
  


