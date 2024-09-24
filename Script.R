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

#Total Number of Patients from each Center
Clinics_Center <- count(Hospital%>%
                          group_by(Center))
head(Clinics_Center)

#Patients in each Clinic from each Center
Intersects <- Hospital%>%
  group_by(Center,Clinic) %>% 
  summarise(Cases=n())%>%
  arrange(-Cases)
head(Intersects)

#Combinig both tables
total <- data.frame(full_join(Intersects,
                    Clinics_Center, by = c("Center")))
head(total)

#Making it useful
Result <- total %>%
          mutate(Perc. = (total$Cases/total$n)*100)%>%
          as.data.frame()
colnames(Result) <- 
  c("Center","Clinic","Patients_in_Clinic","Total_Patients_of_Center","Percentage (%)")
Result <- arrange(Result,-Result$`Percentage (%)`)


View(Result)

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
ggsave("Clinic_Visits.jpeg", plot = Clinic_Visits, width = 8, height = 7, dpi = 300)


Monthly_Visits<-Hospital%>%
                     group_by(Clinic,month(Timestamp))%>%
                     summarise(Visits=n())
colnames(Monthly_Visits) <- 
  c("Clinic","Month","Visits")

Visiting_Pattern <- ggplot(data = Monthly_Visits) +
  geom_col(mapping = aes(x = Month, y = Visits)) +
  facet_wrap(~Clinic) +
  labs(title = "   Clinics Monthly Visits",
       subtitle = "Volume of Visits per Clinic each Month") +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 14))
ggsave("Visiting_Pattern.jpeg", plot = Visiting_Pattern, width = 10, height = 8, dpi = 300)


Hospitalization <- Hospital%>%
  filter(Hospitalization != "Not Hospitalized",
         Clinic != "Reception")%>%
  group_by(Clinic)%>%
  summarise(Hospitalized=n())

Hospitalization_Plot <- ggplot(data=Hospitalization)+
  geom_col(mapping = aes(x = Clinic, y = Hospitalized))+
  labs(title = "   Hospitalization Rate",
       subtitle = "Volume of Patients Kept in each Clinic for +1 Day")+
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14),
   axis.text.x = element_text(angle=25))
ggsave("Hospitalization_Plot.jpeg", plot = Hospitalization_Plot, width = 10, height = 8, dpi = 300)
