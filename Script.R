install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("skimr")
library(dplyr)
library(tidyverse)
library (ggplot2)
library(scales)
library(RColorBrewer)
library(lubridate)
library(skimr)

#STEP 1 : Total Number of Patients from Each Center
#The first analysis involved calculating how many patients from each labor center visited the hospital.
#This helped in understanding which centers were sending the most workers for treatment.

Clinics_Center <- count(Hospital%>%
                          group_by(Center))
head(Clinics_Center)
#Results as follows:
# A tibble: 6 × 2
# Groups:   Center [6]
#    Center        n
#     <chr>     <int>
#  1 Center 1    207
#  2 Center 10    68
#  3 Center 11    96

##STEP 2 : Patient Distribution Across Hospital Departments (Clinics)
#Next, I explored how many patients from each center visited the different hospital clinics.
#This gave a clear picture of which medical issues were most common across the centers.

Intersects <- Hospital%>%
  group_by(Center,Clinic) %>% 
  summarise(Cases=n())%>%
  arrange(-Cases)
head(Intersects)
#Results as follows:
# A tibble: 6 × 3
# Groups:   Center [4]
#    Center    Clinic      Cases
#    <chr>     <chr>       <int>
#  1 Center 7  Surgery       144
#  2 Center 7  Dermatology   130
#  3 Center 9  Surgery       113

##STEP 3 : Matching Data from Both Analyses
#We combined the data from Step 1 and Step 2
#to get a complete picture of the patient distribution across centers and clinics.

total <- data.frame(full_join(Intersects,
                    Clinics_Center, by = c("Center")))
head(total)
#Results as follows:
#     Center      Clinic Cases Total_Center
#1  Center 7     Surgery   144 568
#2  Center 7 Dermatology   130 568
#3  Center 9     Surgery   113 388


##STEP 4 : Ratio Analysis for Clinic Visits
#To make the analysis more accurate, I calculated the percentage of clinic visitors relative to the total patients from each center.
#This accounted for the unequal distribution of workers across the centers, providing a better understanding of which centers had more serious health issues.
#For example, if two centers had 100 patients in the surgery department but: 
#one had a total of 700 workers and the other only 250, the second center would have a higher rate of issues.

Result <- total %>%
          mutate(Perc. = (total$Cases/total$n)*100)%>%
          as.data.frame()
colnames(Result) <- 
  c("Center","Clinic","Patients_in_Clinic","Total_Patients_of_Center","Percentage (%)")
head(Result)
#Results as follows:
#    Center      Clinic Patients_in_Clinic Total_Patients_of_Center Percentage (%)
#1  Center 7     Surgery                144                      568       25.35211
#2  Center 7 Dermatology                130                      568       22.88732
#3  Center 9     Surgery                113                      388       29.12371


##Visualization of Clinic Visits by Center
#I then visualized the percentage of patients visiting each clinic across all centers.
#This visualization highlighted which clinics were most frequently visited and from which centers.

Clinic_Visits <- ggplot(data = Result, aes(x = "", y = `Percentage (%)`, fill = Clinic)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  facet_wrap(~ Center) +
  geom_col(color= "black") +
  labs(fill = "Clinic") +
  scale_fill_brewer(palette = "Set3")+
  labs(title = "   Clinics Visits per Center",
       subtitle = "A Ratio of the volume of Centers' Patients in each Clinic")+
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 14))

#STEP 5 : Clinic Visit Patterns Over 12 Months
#Then I analyzed the monthly trends of visits to each clinic to understand seasonal patterns or recurring issue

Monthly_Visits<-Hospital%>%
                     group_by(Clinic,month(Timestamp))%>%
                     summarise(Visits=n())
colnames(Monthly_Visits) <- 
  c("Clinic","Month","Visits")
Monthly_Visits$Month <- factor(Monthly_Visits$Month, levels = 1:12, 
                               labels = c("January", "February", "March", "April", 
                                          "May", "June", "July", "August", 
                                          "September", "October", "November", "December"))
head(Monthly_Visits)

#Visualizing

Visiting_Pattern <- ggplot(data = Monthly_Visits) +
  geom_col(mapping = aes(x = Month, y = Visits)) +
  facet_wrap(~Clinic) +
  labs(title = "Clinics Monthly Visits",
       subtitle = "Volume of Visits per Clinic each Month") +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1))

## >> Insight: Nearly half of the clinics showed increased visits during the summer months.
#This suggested a need for further investigation into how environmental factors, such as heat and poor living conditions, might be impacting workers' health.

#STEP 6 : Hospitalization Rates by Clinic
#I then analyzed the chances of patients being hospitalized for more than one night in each clinic.
#This giving insight into which clinics had more serious cases that required extended medical attention.

Hospitalization <- Hospital%>%
  filter(Hospitalization != "Not Hospitalized",
         Clinic != "Reception")%>%
  group_by(Clinic)%>%
  summarise(Hospitalized=n())

#Visualizing
Hospitalization_Plot <- ggplot(data=Hospitalization)+
  geom_col(mapping = aes(x = Clinic, y = Hospitalized))+
  labs(title = "   Hospitalization Rate",
       subtitle = "Volume of Patients Kept in each Clinic for +1 Day")+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14),
   axis.text.x = element_text(angle=25))
