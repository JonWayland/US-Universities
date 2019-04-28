
####################
### College Data ###
####################

# Source: https://collegescorecard.ed.gov/data/

# https://catalog.data.gov/dataset/college-scorecard


library("tidyverse")

mrCohort <- read.csv("https://raw.githubusercontent.com/JonWayland/US-Universities/master/Most-Recent-Cohorts-Scorecard-Elements.csv")

data_dictionary <- read.csv("https://raw.githubusercontent.com/JonWayland/US-Universities/master/Data_Dictionary.csv") 

# Data dictionary function
dataDic <- function(colName){
  # This function takes a column name as input and returns the defintion  
  subset(data_dictionary, VARIABLE.NAME == colName) %>%
    mutate(Desc = as.character(NAME.OF.DATA.ELEMENT)) %>% 
    .$Desc %>% 
    writeLines
}


dataDic("UNITID")


summary(as.numeric(as.character(mrCohort$PCIP51)))


#####################
### Data Cleaning ###
#####################

# Fixing the first column name
newNames<- append(c("UNITID"), names(mrCohort[,2:ncol(mrCohort)]))
colnames(mrCohort) <- newNames


str(mrCohort) # Truncates column list

str(mrCohort, list.len=ncol(mrCohort))


dataDic("OPEID6")
unique(mrCohort$PPTUG_EF)

# Fixing those that are not currently factor variables
mrCohort$UNITID <- as.factor(mrCohort$UNITID)
mrCohort$OPEID <- as.factor(mrCohort$OPEID)
mrCohort$OPEID6 <- as.factor(mrCohort$OPEID6)

mrCohort$HCM2 <- as.factor(mrCohort$HCM2)
mrCohort$PREDDEG <- as.factor(mrCohort$PREDDEG)
mrCohort$HIGHDEG <- as.factor(mrCohort$HIGHDEG)
mrCohort$CONTROL <- as.factor(mrCohort$CONTROL)
mrCohort$CURROPER <- as.factor(mrCohort$CURROPER)

# Fixing those that are factor but should be numeric
# Anything beginning with ACT, SAT, PCI, UGDS, NPT, PCT, RET, C150 seems to be continuous

# Example for converting a factor to numeric
x<-as.factor(0.0227)
as.numeric(x)
as.numeric(as.character(x))


# Converting the patterns recognized to numeric
bgn <- function(prefix){
  startsWith(names(mrCohort), prefix)
}
prefixes <- c("ACT", "SAT", "PCI", "UGDS", "NPT", "PCT", "RET", "C150", "RPY", "GRAD", "UG25", "MD_EARN", "GT_", "PPTUG")

# Creating indices
criteriaInd <- rowSums(sapply(prefixes, bgn))*(1:ncol(mrCohort))

# Getting the column names
cols<-names(mrCohort[,criteriaInd])

# Conversion must be taken in steps -> character first then numeric
mrCohort[cols] <- sapply(mrCohort[cols],as.character)

mrCohort[cols] <- sapply(mrCohort[cols],as.numeric)

# Note: Some values are expressed as "PrivacySuppressed" or "(Other)": Treating these as missing values for this purpose. 
## There will be a warning message "NAs introduced by coercion" .. This will be okay.


############################
### Exploratory Analysis ###
############################

# Getting only the numeric variables
numericCols <- unlist(lapply(mrCohort, is.numeric))  
mrCohort_numeric <- mrCohort[ , numericCols]


# Correlation matrix (using pairwise complete observations)
corMatrix <- round(cor(mrCohort_numeric, use = "pairwise.complete.obs"),2)

# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
library(reshape2)
melted_cormat <- melt(corMatrix)

melted_cormat %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("")+ylab("")+
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")

library(scales)


dataDic("PREDDEG")

dataDic("RELAFFIL")

dataDic("UGDS_BLACK")

dataDic("MD_EARN_WNE_P10")

mrCohort %>% 
  ggplot(aes(UGDS_ASIAN, SAT_AVG))+
  geom_point()

mrCohort %>% 
  #mutate(PREDDEG = case_when(PREDDEG == 0 ~ 'Not Classified',
  #                           PREDDEG == 1 ~ 'Certificate',
  #                           PREDDEG == 2 ~ 'Associates',
  #                           PREDDEG == 3 ~ 'Bachelors',
  #                           PREDDEG == 4 ~ 'Graduate',
  #                           TRUE ~ 'ERROR')) %>%
  ggplot(aes(MD_EARN_WNE_P10, SAT_AVG)) +
  geom_jitter(aes(size = UGDS_BLACK, fill = PREDDEG), alpha = 0.65, shape = 21, col = "gray")+
  scale_size_continuous(name = "Percentage of Students\nWho are Black",range=c(1, 10), labels = percent)+
  coord_cartesian(xlim = c(20000,125000))+
  scale_x_continuous(name = "Median earnings of students working and not enrolled 10 years after entry", labels = dollar)+
  guides(fill = guide_legend(title = "Predominant Degree Granting", override.aes = list(size = 5)),
         values = c("Not Classified", "Certificate", "Associates", "Bachelors", "Graduate"))+
  #scale_fill_manual(values = c("Not Classified", "Certificate", "Associates", "Bachelors", "Graduate")) +
  ylab("Average SAT Score") +
  theme_bw()


summary(mrCohort$RELAFFIL)

mrCohort %>%
  filter(PREDDEG == 3, MD_EARN_WNE_P10 < 25000) %>%
  select(INSTNM)

datNew <- mrCohort_numeric %>% select(-c(C150_4_POOLED_SUPP,C150_L4_POOLED_SUPP,RPY_3YR_RT_SUPP))



### PCA ###

dataDic("GRAD_DEBT_MDN_SUPP")

pc <- prcomp(mrCohort_numeric)


nrow(na.omit(mrCohort_numeric))

mrCohort_numeric

# Seeing how many missing values by column there are

missing <- data.frame(Column_Name = as.character(),
                      Total_Missing = as.integer())

for(i in 1:ncol(mrCohort_numeric)){
  missing <- rbind(missing, data.frame(
    Column_Name = names(mrCohort_numeric)[i],
    Total_Missing = sum(is.na(mrCohort_numeric[,i]))
  ))
}

missing %>%
  arrange(desc(Total_Missing))


dataDic("PCTFLOAN")
dataDic("PCTPELL")
dataDic("PPTUG_EF")
dataDic("UG25ABV")

pcColList <- missing %>% 
  filter(Total_Missing < 500) %>%
  .$Column_Name

nrow(na.omit(mrCohort[,pcColList]))








