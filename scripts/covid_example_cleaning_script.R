library(tidyverse)
library(readxl)
library(here)
library(runner) # rollmean function for integer windows
library(lubridate) # work with date data
library(ggmap)
library(osmdata)
library(plotly)


covid_data <- read_excel(here("data", "covid_example_data.xlsx"))

# Data checking here


### 

hospital_covid_data <- covid_data %>% 
  filter(hosp_admidt_FALSE <= max(pos_sampledt_FALSE, na.rm = TRUE)) # remove obvious incorrect dates



# Create a rolling window of case numbers.

rolling_avg_case_numbers <- covid_data %>% 
  filter(confirmed_case == "Yes") %>% 
  group_by(pos_sampledt_FALSE) %>% 
  summarise(cases = n()) %>% 
  drop_na(pos_sampledt_FALSE) %>% 
  mutate(infection = runner::sum_run(cases, k = 7, idx = as.Date(pos_sampledt_FALSE)))

rolling_avg_hosp_numbers <- hospital_covid_data %>% 
  filter(confirmed_case == "Yes",
         hospitalized == "Yes") %>% 
  group_by(hosp_admidt_FALSE) %>% 
  summarise(hospital_cases = n()) %>% 
  drop_na(hosp_admidt_FALSE) %>% 
  mutate(hospital = runner::sum_run(hospital_cases, k = 7, idx = as.Date(hosp_admidt_FALSE)))

seven_day_rolling_data <- full_join(rolling_avg_case_numbers, rolling_avg_hosp_numbers, 
          by = c("pos_sampledt_FALSE" = "hosp_admidt_FALSE")) %>% 
  select(pos_sampledt_FALSE, infection, hospital) %>% 
  pivot_longer(cols = !pos_sampledt_FALSE, names_to = "cases_vs_hospitalisation", values_to = "rolling_avg")

# ribbon for case numbers
seven_day_rolling_data %>% 
  ggplot(aes(x = pos_sampledt_FALSE,
             y = rolling_avg,
         fill = cases_vs_hospitalisation,
         alpha = cases_vs_hospitalisation))+
  geom_area(position = "identity")+
  scale_alpha_manual(values = c(1, 0.2))


descending_case_data <- seven_day_rolling_data %>% 
  arrange(desc(pos_sampledt_FALSE)) %>% 
  mutate(rolling_avg = replace_na(rolling_avg, 00))


######### Symptoms tracker


symptom_tracker <- function(data){

total_patients <- nrow(distinct(data,PID))

symptoms <- data %>% 
  select(-sym_startdt_FALSE, -sym_resolved, -sym_resolveddt_FALSE) %>% 
  pivot_longer((starts_with("sym_")), names_to="symptoms", values_to = "presence_absence") %>% 
  filter(presence_absence == "Yes") %>% 
  group_by(symptoms) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/total_patients)

return(symptoms)
}

### ordered barchart
symptom_plot <- function(data){
  
data %>% 
ggplot(aes(x = reorder(symptoms, +freq), y = freq)) +
  geom_col()+
  geom_text(aes(y = (freq + 0.02), 
            x = symptoms,
            label = scales::percent(freq, accuracy = 2))) +
  scale_y_continuous(breaks = seq (0,1,0.1))+
  coord_flip()

}

max <- max(covid_data$pos_sampledt_FALSE, na.rm=TRUE)
max_30 <- max - as.difftime(30, unit = "days")

min <- min(covid_data$pos_sampledt_FALSE, na.rm=TRUE)
min_90 <- max + as.difftime(90, unit = "days")

covid_data %>% 
  filter(between(pos_sampledt_FALSE, max_30, max)) %>% 
  symptom_tracker() %>% 
  symptom_plot()

covid_data %>% 
  filter(between(pos_sampledt_FALSE, min, min_90)) %>% 
  symptom_tracker() %>% 
  symptom_plot()



### Diverging barplot

late <- covid_data %>% 
  filter(between(pos_sampledt_FALSE, max_30, max)) %>% 
  symptom_tracker()

early <- covid_data %>% 
  filter(between(pos_sampledt_FALSE, min, min_90)) %>% 
  symptom_tracker()

inner_join( early,late,  by = "symptoms") %>% 
  mutate(freq = freq.y - freq.x) %>% 
  symptom_plot()+
  ggtitle("Changing symptoms")



#### MAPS

max_7 <- max - as.difftime(7, unit = "days")

position_data <- covid_data %>% 
  filter(longitude_JITT < -75) %>% 
  filter(latitude_JITT > 30) %>% 
  filter(pos_sampledt_FALSE > max_7, na.rm=TRUE)





atlanta_map <- get_map("Atlanta", maptype = "roadmap")

# center = c(lon = -2.9437, lat = 53.45), zoom = 10, maptype = 'terrain', color = 'color'))

map <- ggmap(atlanta_map)+geom_point(data = position_data,
                              aes(x = longitude_JITT, 
                                  y = latitude_JITT,
                                  text = pos_sampledt_FALSE),
                              color = "red", size = 2)

ggplotly(map, tooltip="text")
