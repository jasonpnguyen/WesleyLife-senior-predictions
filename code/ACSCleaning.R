#install.packages("haven") to read in sas file
library(haven)

acs <- read_sas("data/spm_pu_2022.sas7bdat")

#to calculate weights (go ahead and just copy/paste this):
acs <- acs%>%
  filter(st == "19")%>%
  group_by(serialno = as.factor(serialno)) %>%
  arrange(desc(Sex), desc(Age)) %>%
  mutate(weight = first(wt)) %>% select(-wt) %>% ungroup()

# create same variables as in CPS
acs <- acs %>%
  mutate(SEX = Sex - 1 , # since female = 2
         CHILD = ifelse(Age < 18, 1, 0), #SAME as cps definition
         ELDERLY = ifelse(Age > 64, 1, 0), #SAME as cps definition
         BLACK = ifelse(Race==2, 1, 0), #SAME as cps definition (see data dictionary)
         HISPANIC = ifelse(Hispanic>0, 1, 0), #SAME as cps definition (see data dictionary)
         EDUC = as.integer(Education %in% c(3,4)),
         MARRIED = as.integer(Mar %in% c(1)),
         PUMA = as.factor(PUMA))

#aggregate up to family level
acs_data <- acs %>%
  group_by(serialno = as.factor(serialno)) %>%
  summarise(PUMA = first(PUMA),
            hhsize = length(serialno),
            #counts of people with various features - just like for CPS
            female = sum(SEX),
            hispanic = sum(HISPANIC),
            black= sum(BLACK),
            FAMINC_numeric = sum(AGI),
            kids= sum(CHILD),
            elderly= sum(ELDERLY),
            education= sum(EDUC),
            married= sum(MARRIED),
            weight = weight[1],
            Female_Proportion = (female / hhsize),
            
            # Elderly proportion: number of elderly divided by household size
            Elderly_Proportion = (elderly / hhsize),
            
            # Family income per household member
            Income_Per_Capita = (FAMINC_numeric / hhsize),
            
            
  )
#each row of acs_data is a FAMILY