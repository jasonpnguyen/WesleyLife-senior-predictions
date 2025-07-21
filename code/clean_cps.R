#clean_cps is where we will read in the CPS CSV and clean the data and
#create the new cleaned version that we will use for the remainder of the project

library(tidyverse) #includes ggplot2 and dplyer
library(ggthemes) #optional but classy ;)
library(logistf) #firth's penalized
library(glmnet) # for fitting lasso, ridge regressions (GLMs)

cps <- read.csv("data/cps_00006.csv")
#make sure file path is correct



#each row of cps is an INDIVIDUAL within a family
cps <- cps %>%
  mutate(SEX = SEX - 1 , # Create dummy variables
         CHILD = ifelse(AGE < 18, 1, 0), #Make sure under 18 is counted as child
         ELDERLY = ifelse(AGE > 64, 1, 0), #Elderly = anyone over 64
         BLACK = ifelse(RACE==200, 1, 0),
         HISPANIC = ifelse(HISPAN>0, 1, 0),
         EDUC = as.integer(EDUC %in% c(91,92,111,123,124,125)),
         EMP = as.integer(EMPSTAT %in% c(1,10,12)),
         MARRIED = as.integer(MARST %in% c(1,2)),
         DIFF = ifelse(DIFFANY==2, 1, 0),
         COUNTY = as.factor(COUNTY))


#currently, one row of cps = one individual
#however, we want to make prediction on the family level


#aggregate to the family level - this is where we choose FAMILY-LEVEL traits
#that we want to calculate. For example, household size is equal to the
#number of rows for that family.
cps_data <- cps %>%
  group_by(CPSID = as.factor(CPSID)) %>%
  summarise(COUNTY = first(COUNTY),
            #family level weight
            weight = first(HWTFINL),
            
            #household size
            hhsize = n(),
            
            #Y variables - i.e., measures of hunger
            #see CPS website for details
            #FSSTATUS, etc. is the same for each member -just take first value for each family
            FSTOTXPNC_perpers = FSTOTXPNC/hhsize, # In per person terms
            FSSTATUS = first(FSSTATUS),
            FSSTATUSMD = first(FSSTATUSMD),
            FSFOODS = first(FSFOODS),
            FSWROUTY = first(FSWROUTY),
            FSBAL = first(FSBAL),
            FSRAWSCRA = first(FSRAWSCRA),
            FSTOTXPNC = first(FSTOTXPNC),
            FSSTATUS = first(FSSTATUS),
            #count of family members in various categories
            female = sum(SEX),
            hispanic = sum(HISPANIC),
            black= sum(BLACK),
            kids= sum(CHILD),
            elderly= sum(ELDERLY),
            education= sum(EDUC),
            married= sum(MARRIED),
            #Here we asked Chat GPT to make a numeric variable out of income data
            #which was origionally categorical
            FAMINC_numeric= first(
              case_when(
                FAMINC == 100 ~ 2500,    # Median of "Under $5,000" is approximately $2,500
                FAMINC == 110 ~ 500,     # Median of "Under $1,000" is approximately $500
                FAMINC == 111 ~ 250,     # Median of "Under $500" is $250
                FAMINC == 112 ~ 750,     # Median of "$500 - 999" is $750
                FAMINC == 120 ~ 1500,    # Median of "$1,000 - 1,999" is $1,500
                FAMINC == 121 ~ 1250,    # Median of "$1,000 - 1,499" is $1,250
                FAMINC == 122 ~ 1750,    # Median of "$1,500 - 1,999" is $1,750
                FAMINC == 130 ~ 2500,    # Median of "$2,000 - 2,999" is $2,500
                FAMINC == 131 ~ 2250,    # Median of "$2,000 - 2,499" is $2,250
                FAMINC == 132 ~ 2750,    # Median of "$2,500 - 2,999" is $2,750
                FAMINC == 140 ~ 3500,    # Median of "$3,000 - 3,999" is $3,500
                FAMINC == 141 ~ 3250,    # Median of "$3,000 - 3,499" is $3,250
                FAMINC == 142 ~ 3750,    # Median of "$3,500 - 3,999" is $3,750
                FAMINC == 150 ~ 4500,    # Median of "$4,000 - 4,999" is $4,500
                FAMINC == 200 ~ 6500,    # Median of "$5,000 - 7,999" is $6,500
                FAMINC == 210 ~ 6250,    # Median of "$5,000 - 7,499" is $6,250
                FAMINC == 220 ~ 5500,    # Median of "$5,000 - 5,999" is $5,500
                FAMINC == 230 ~ 7000,    # Median of "$6,000 - 7,999" is $7,000
                FAMINC == 231 ~ 6750,    # Median of "$6,000 - 7,499" is $6,750
                FAMINC == 232 ~ 6500,    # Median of "$6,000 - 6,999" is $6,500
                FAMINC == 233 ~ 7250,    # Median of "$7,000 - 7,499" is $7,250
                FAMINC == 234 ~ 7500,    # Median of "$7,000 - 7,999" is $7,500
                FAMINC == 300 ~ 8750,    # Median of "$7,500 - 9,999" is $8,750
                FAMINC == 310 ~ 7750,    # Median of "$7,500 - 7,999" is $7,750
                FAMINC == 320 ~ 8250,    # Median of "$8,000 - 8,499" is $8,250
                FAMINC == 330 ~ 8750,    # Median of "$8,500 - 8,999" is $8,750
                FAMINC == 340 ~ 8500,    # Median of "$8,000 - 8,999" is $8,500
                FAMINC == 350 ~ 9500,    # Median of "$9,000 - 9,999" is $9,500
                FAMINC == 400 ~ 12500,   # Median of "$10,000 - 14,999" is $12,500
                FAMINC == 410 ~ 10500,   # Median of "$10,000 - 10,999" is $10,500
                FAMINC == 420 ~ 11500,   # Median of "$11,000 - 11,999" is $11,500
                FAMINC == 430 ~ 11250,   # Median of "$10,000 - 12,499" is $11,250
                FAMINC == 440 ~ 11000,   # Median of "$10,000 - 11,999" is $11,000
                FAMINC == 450 ~ 12500,   # Median of "$12,000 - 12,999" is $12,500
                FAMINC == 460 ~ 13500,   # Median of "$12,000 - 14,999" is $13,500
                FAMINC == 470 ~ 13750,   # Median of "$12,500 - 14,999" is $13,750
                FAMINC == 480 ~ 13500,   # Median of "$13,000 - 13,999" is $13,500
                FAMINC == 490 ~ 14500,   # Median of "$14,000 - 14,999" is $14,500
                FAMINC == 500 ~ 17500,   # Median of "$15,000 - 19,999" is $17,500
                FAMINC == 510 ~ 15500,   # Median of "$15,000 - 15,999" is $15,500
                FAMINC == 520 ~ 16500,   # Median of "$16,000 - 16,999" is $16,500
                FAMINC == 530 ~ 17500,   # Median of "$17,000 - 17,999" is $17,500
                FAMINC == 540 ~ 16250,   # Median of "$15,000 - 17,499" is $16,250
                FAMINC == 550 ~ 18750,   # Median of "$17,500 - 19,999" is $18,750
                FAMINC == 560 ~ 19000,   # Median of "$18,000 - 19,999" is $19,000
                FAMINC == 600 ~ 22500,   # Median of "$20,000 - 24,999" is $22,500
                FAMINC == 700 ~ 37500,   # Median of "$25,000 - 49,999" is $37,500
                FAMINC == 710 ~ 27500,   # Median of "$25,000 - 29,999" is $27,500
                FAMINC == 720 ~ 32500,   # Median of "$30,000 - 34,999" is $32,500
                FAMINC == 730 ~ 37500,   # Median of "$35,000 - 39,999" is $37,500
                FAMINC == 740 ~ 45000,   # Median of "$40,000 - 49,999" is $45,000
                FAMINC == 800 ~ 75000,   # Median of "$50,000 and over" is $75,000
                FAMINC == 810 ~ 62500,   # Median of "$50,000 - 74,999" is $62,500
                FAMINC == 820 ~ 55000,   # Median of "$50,000 - 59,999" is $55,000
                FAMINC == 830 ~ 67500,   # Median of "$60,000 - 74,999" is $67,500
                FAMINC == 840 ~ 87500,   # Median of "$75,000 and over" is $87,500
                FAMINC == 841 ~ 87500,   # Median of "$75,000 - 99,999" is $87,500
                FAMINC == 842 ~ 125000,  # Median of "$100,000 - 149,999" is $125,000
                FAMINC == 843 ~ 150000,  # Median of "$150,000 and over" is $150,000
                FAMINC == 995 ~ NA_real_, # Missing
                FAMINC == 996 ~ NA_real_, # Refused
                FAMINC == 997 ~ NA_real_, # Don't know
                FAMINC == 999 ~ NA_real_  # Blank
              )
            )
  ) %>% ungroup()

#each row of cps_data is a FAMILY
#note... we just calculated the number of people in each family that belong
#to the above groups. perhaps that isn't the best way? Would proportions be good
#in addition or instead of sums?!

#summary(cps_data) # see extremes for food security variables
#https://cps.ipums.org/cps-action/variables/search


#Next we want to make sure we get rid of any "dirty" data by making it 
#NA and also we will start this code by making some interaction variables
cps_data <- cps_data %>%
  mutate(
    # Female proportion: number of females divided by household size
    Female_Proportion = ifelse(hhsize > 0, female / hhsize, NA_real_),
    
    # Elderly proportion: number of elderly divided by household size
    Elderly_Proportion = ifelse(hhsize > 0, elderly / hhsize, NA_real_),
    
    # Family income per household member
    Income_Per_Capita = ifelse(hhsize > 0, FAMINC_numeric / hhsize, NA_real_),
    
    #Now we start making sure the only variables left in this data are 1s and 0s since the Y's should be predicted as bianary
    FSSTATUS = ifelse(FSSTATUS %in% c(98,99), NA, FSSTATUS),
    FSSTATUSMD = ifelse(FSSTATUSMD %in% c(98,99), NA, FSSTATUSMD),
    FSFOODS = ifelse(FSFOODS %in% c(98,99), NA, FSFOODS),
    FSWROUTY = ifelse(FSWROUTY %in% c(96,97,98,99), NA, FSWROUTY),
    FSBAL = ifelse(FSBAL %in% c(96,97,98,99), NA, FSBAL),
    FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98,99), NA, FSRAWSCRA),
    FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC),
    # Recoding variables to binary (0 or 1) to simplify analysis:
    # If the value of the variable is greater than 1, set it to 1; otherwise, set it to 0.
    FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
    FSSTATUSMD = ifelse(FSSTATUSMD > 1, 1, 0),
    FSFOODS = ifelse(FSFOODS > 1, 1, 0),
    FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),
    FSBAL = ifelse(FSBAL > 1, 1, 0),
    FSRAWSCRA=ifelse(FSRAWSCRA > 1, 1, 0))

summary(cps_data)
#View your updated Data to make sure it looks fully cleaned