###INTRO (install these packages if not already installed)
library(akima)
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(plotly)
library(tidyverse)
library(usmap)

#Filters original dataset into different versions for later use
CNRD <- College_Naming_Rights_Database_Database_13_
CNRDminusLA <- filter(CNRD, CNRD$School != "University of Southern California")
CNRDminusLAUT <- filter(CNRDminusLA, CNRDminusLA$`Total Deal Price` < 100000000)
CNRDDealFilter <- filter(CNRDminusLAUT, CNRDminusLAUT$`Deal Length (Years)` < 100)

###TABLES
#creates pivot table by sport
Sports <- CNRD %>%
  group_by(Sport) %>%
  summarize(obs_count = n())
Sports$percent <- (Sports$obs_count / sum(Sports$obs_count))

#creates pivot table by private v public company
PvPCompany <- CNRD %>%
  group_by(`Private vs Public Company`) %>%
  summarize(obs_count = n())
PvPCompany$percent <- (PvPCompany$obs_count / sum(PvPCompany$obs_count))

#creates pivot table by private v public school
PvPSchool <- CNRD %>%
  group_by(`Private vs Public School`) %>%
  summarize(obs_count = n())
PvPSchool$percent <- (PvPSchool$obs_count / sum(PvPSchool$obs_count))

#creates pivot table by alumni and private v public company
CompanyAlumni <- CNRD %>%
  group_by(`Private vs Public Company`, `Alumni as Decision Maker`) %>%
  summarize(obs_count = n())
CompanyAlumni$percent <- (CompanyAlumni$obs_count / sum(CompanyAlumni$obs_count))

#creates pivot table by pvp school and alumni
CompanySchool <- CNRD %>%
  group_by(`Private vs Public School`, `Alumni as Decision Maker`) %>%
  summarize(obs_count = n())
CompanySchool$percent <- (CompanySchool$obs_count / sum(CompanySchool$obs_count))

#creates pivot table by alumni
Alumni <- CNRD %>%
  group_by(`Alumni as Decision Maker`) %>%
  summarize(obs_count = n())
Alumni$percent <- (Alumni$obs_count / sum(Alumni$obs_count))

#creates pivot table by HQ
HQ <- CNRD %>%
  group_by(CNRD$`Is Company HQ In Same State As The School?`) %>%
  summarize(obs_count = n())
HQ$percent <- (HQ$obs_count / sum(HQ$obs_count))

#creates pivot table by HQ and alumni
HQAlumni <- CNRD %>%
  group_by(CNRD$`Is Company HQ In Same State As The School?`, `Alumni as Decision Maker`) %>%
  summarize(obs_count = n())
HQAlumni$percent <- (HQAlumni$obs_count / sum(HQAlumni$obs_count))

#creates pivot table with year signed and previous relationship
YearPrevRel <- CNRD %>%
  group_by(CNRD$`Year the Last Contract Was Signed`, CNRD$`Alumni as Decision Maker`, CNRD$`Is Company HQ In Same State As The School?`) %>%
  summarize(obs_count = n())
YearPrevRel$percent <- (YearPrevRel$obs_count / sum(YearPrevRel$obs_count))

###GRAPHS
#Plots city population vs venue capacity and then calculates the correlation coefficient between the two variables
ggplot(data = CNRDminusLA, mapping = aes(x = CNRDminusLA$`City Population`, y = CNRDminusLA$`Venue Capacity`))+
  geom_point()+
  labs(title = "City Population vs Venue Capacity", x = "City Population", y = "Venue Capacity")
cor(CNRDminusLA$`City Population`, CNRDminusLA$`Venue Capacity`, use = "complete.obs")

#Plots city population vs avg attendance and then calculates the correlation coefficient between the two variables
ggplot(data = CNRDminusLA, mapping = aes(x = CNRDminusLA$`City Population`, y = CNRDminusLA$`Avg. Attendance at Venue`))+
  geom_point()+
  labs(title = "City Population vs Avg Attendance", x = "City Population", y = "Avg Attendance")
cor(CNRDminusLA$`City Population`, CNRDminusLA$`Avg. Attendance at Venue`, use = "complete.obs")

#Plots venue capacity vs avg attendance and then calculates the correlation coefficient between the two variables
ggplot(data = CNRDminusLA, mapping = aes(x = CNRDminusLA$`Venue Capacity`, y = CNRDminusLA$`Avg. Attendance at Venue`))+
  geom_point()+
  labs(title = "Venue Capacity vs Avg Attendance", x = "Venue Capacity", y = "Avg Attendance")
cor(CNRDminusLA$`Venue Capacity`, CNRDminusLA$`Avg. Attendance at Venue`, use = "complete.obs")

#Plots contract year vs deal length
ggplot(data = CNRDDealFilter, mapping = aes(x = CNRDDealFilter$`Year the Last Contract Was Signed`, y = CNRDDealFilter$`Deal Length (Years)`))+
  geom_point()+
  geom_smooth()+
  labs(title = "Year of Last Contract vs Deal Length", x = "Last Contract", y = "Deal Length")
cor(CNRDDealFilter$`Year the Last Contract Was Signed`, CNRDDealFilter$`Deal Length (Years)`, use = "complete.obs")

#Plots contract year vs college average annual value
ggplot(data = CNRDDealFilter, mapping = aes(x = CNRDDealFilter$`Year the Last Contract Was Signed`, y = CNRDDealFilter$`College AAV`))+
  geom_point()+
  geom_smooth()+
  labs(title = "Year of Last Contract vs College AAV", x = "Last Contract", y = "College AAV")
cor(CNRDDealFilter$`Year the Last Contract Was Signed`, CNRDDealFilter$`College AAV`, use = "complete.obs")

#Plots deal length vs total deal value (with a trendline) and then calculates the correlation coefficient between the two variables
ggplot(data = CNRDDealFilter, mapping = aes(x = CNRDDealFilter$`Deal Length (Years)`, y = CNRDDealFilter$`Total Deal Price`))+
  geom_point()+
  geom_smooth()+
  labs(title = "Deal Length vs Total Deal Value", x = "Deal Length", y = "Total Deal Value")
cor(CNRDDealFilter$`Deal Length (Years)`, CNRDDealFilter$`Total Deal Price`, use = "complete.obs")

#Plots deal length vs total deal value (with a trendline and name of school instead of a point) and then calculates the correlation coefficient between the two variables
ggplot(data = CNRDDealFilter, mapping = aes(x = CNRDDealFilter$`Deal Length (Years)`, y = CNRDDealFilter$`Total Deal Price`))+
  geom_text(label = CNRDDealFilter$School)+
  geom_smooth()+
  labs(title = "Deal Length vs Total Deal Value", x = "Deal Length", y = "Total Deal Value")

#Filters out University of Washington because it is an obvious outlier
CNRDDealFilterminUW <- filter(CNRDDealFilter, CNRDDealFilter$School != "University of Washington")

#Plots average attendance vs total deal value without University of Washington and then calculates the new correlation coefficient between the two variables
ggplot(data = CNRDDealFilterminUW, mapping = aes(x = CNRDDealFilterminUW$`Avg. Attendance at Venue`, y = CNRDDealFilterminUW$`College AAV`))+
  geom_point()+
  geom_smooth()+
  labs(title = "Avg. Attendance vs Total Deal Value", x = "Avg. Attendance", y = "Total Deal Value")
cor(CNRDDealFilterminUW$`Avg. Attendance at Venue`, CNRDDealFilterminUW$`Total Deal Price`, use = "complete.obs")

#Plots average attendance vs total deal value without University of Washington and then calculates the new correlation coefficient between the two variables
ggplot(data = CNRDDealFilterminUW, mapping = aes(x = CNRDDealFilterminUW$`Avg. Attendance at Venue`, y = CNRDDealFilterminUW$`College AAV`))+
  geom_text(label = CNRDDealFilterminUW$School)+
  geom_smooth()+
  labs(title = "Avg. Attendance vs Total Deal Value", x = "Avg. Attendance", y = "Total Deal Value")
cor(CNRDDealFilterminUW$`Avg. Attendance at Venue`, CNRDDealFilterminUW$`Total Deal Price`, use = "complete.obs")

###PROFESSIONAL COMPARISONS###
#Calculates the average college cents on the dollar ratio before comparing to professional sports deals and stores it in avgCtoPPercentb4Division
CNRDProComp <- CNRDDealFilter
avgCNR <- (sum(CNRDDealFilter$`College AAV`)/nrow(CNRDDealFilter))
avgPNR <- (sum(CNRDProComp$`Professional AAV`)/nrow(CNRDProComp))
avgCtoPPercentb4Division <- (avgCNR / avgPNR)

#Calculates the average college cents on the dollar ratio
#after comparing to professional sports
CNRDProComp$`College Cents on the Pro Dollar` <- (CNRDProComp$`College AAV` / CNRDProComp$`Professional AAV`)
avgCtoPPercentafterDivision <- (sum(CNRDProComp$`College Cents on the Pro Dollar`) / nrow(CNRDProComp))

#Creates a new column in CNRDProComp for z-scores of the ratios
CNRDProComp$`Ratio Z-Score` <- ((CNRDProComp$`College Cents on the Pro Dollar` - avgCtoPPercentafterDivision) / sd(CNRDProComp$`College Cents on the Pro Dollar`))
CNRDProCompOutliers <- filter(CNRDProComp, CNRDProComp$`Ratio Z-Score` >= 1.5 | CNRDProComp$`Ratio Z-Score` <= -1.5)

sd(CNRDProComp$`College Cents on the Pro Dollar`)

#Creates heat map of US by different categories
CNRDDealFilter$fips <- CNRDDealFilter$`FIPS Code`
plot_usmap(data = CNRDDealFilter, values = "College AAV")
plot_usmap(data = CNRDDealFilter, values = "Year the Last Contract Was Signed")
plot_usmap(data = CNRDDealFilter, values = "Deal Length (Years)")
plot_usmap(data = CNRDDealFilter, values = "City Population")
plot_usmap(data = CNRDDealFilter, values = "Venue Capacity")
plot_usmap(data = CNRDDealFilter, values = "Avg. Attendance at Venue")
plot_usmap(data = CNRDDealFilter, values = "Number of Students at the Schools")
plot_usmap(data = CNRDDealFilter, values = "Professional AAV")

cor(CNRDDealFilter$`Avg. Attendance at Venue`, CNRDDealFilter$`Number of Students at the Schools`, use = "complete.obs")

CNRDyear <- filter(CNRDDealFilter, CNRDDealFilter$`Year the Last Contract Was Signed` >= 2004)

