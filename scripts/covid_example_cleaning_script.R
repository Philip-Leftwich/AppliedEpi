library(tidyverse)
library(readxl)
library(here)
library(runner) # rollmean function for integer windows
library(lubridate) # work with date data
library(ggmap)
library(osmdata)
library(plotly)
library(showtext)
library(patchwork)
library(gt)
library(gtsummary)
library(apyramid)
library(reactablefmtr)


### Set google fonts for all plots ###
font_add_google("Roboto Condensed", "Roboto Condensed")

### custom theme for all plots
theme_custom <- function (base_size = 14, base_family = "Roboto Condensed") {
  half_line <- base_size/2
  theme(
    line = element_line(color = "black", size = .5,
                        linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", color = "black",
                        size = .5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        color = "black", size = base_size,
                        lineheight = .9, hjust = .5, vjust = .5,
                        angle = 0, debug = FALSE),
    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = element_text(size = base_size * 1.1, color = "gray30"),
    axis.ticks = element_line(color = "gray30", size = .7),
    axis.ticks.length = unit(half_line / 1.5, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.title.x = element_text(vjust = 1, size = base_size * 1.3,
                                face = "bold"),
    axis.title.x.top = element_text(
                                    vjust = 0),
    axis.title.y = element_text(angle = 90, vjust = 1,
                                
                                size = base_size * 1.3, face = "bold"),
    axis.title.y.right = element_text(angle = -90, vjust = 0
                                     ),
    legend.background = element_rect(color = NA),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = element_rect(fill = "gray95", color = "white"),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    legend.box.background = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(base_size, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(fill = "white", color = "gray30"),
    strip.text = element_text(color = "black", size = base_size),
    strip.text.y = element_text(angle = -90),
    strip.text.y.left = element_text(angle = 90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    plot.background = element_rect(color = NA),
    plot.title = element_text(size = base_size * 1.8, hjust = .5,
                              vjust = 1, face = "bold"),
    plot.title.position = "panel",
    plot.subtitle = element_text(size = base_size * 1.3,
                                 hjust = .5, vjust = 1),
    plot.caption = element_text(size = rel(0.9), hjust = 1, vjust = 1),
    plot.caption.position = "panel",
    plot.tag = element_text(size = rel(1.2), hjust = .5, vjust = .5),
    plot.tag.position = "topleft",
    complete = TRUE
  )
}

# set geom text size for all plots
GeomText$default_aes$size <- 5

### data ###
covid_data <- read_excel(here("data", "covid_example_data.xlsx"))


### Tidying ###

# Set an upper and lower date limit to capture last 6 months of data
max <- max(covid_data$pos_sampledt_FALSE, na.rm=TRUE)
max_6month <- max - as.difftime(26, unit = "weeks")


# filter data
covid_data <- covid_data %>% 
  filter(pos_sampledt_FALSE > max_6month, na.rm=TRUE)

# remove obvious incorrect dates for hospital data - future!
hospital_covid_data <- covid_data %>% 
  filter(hosp_admidt_FALSE <= max(pos_sampledt_FALSE, na.rm = TRUE))

# Check hospital records that don't log a positive test.
hospital_covid_data %>% 
  select(pos_sampledt_FALSE) %>% 
  is.na() %>% 
  sum()


# select only hospital dates if +/- 30 days of a covid test
hospital_covid_data <- hospital_covid_data %>% 
  mutate(test_window = pos_sampledt_FALSE - hosp_admidt_FALSE ) %>% 
  filter(between(test_window, -30, 30))

################## Create a rolling window of cases and hospital numbers ######

rolling_avg_case_numbers <- covid_data %>% 
  filter(confirmed_case == "Yes") %>% 
  group_by(pos_sampledt_FALSE) %>% 
  summarise(cases = n()) %>% 
  drop_na(pos_sampledt_FALSE) %>% 
  mutate(new_cases = cases) %>% 
  mutate(cases = runner::sum_run(cases, k = 7, idx = as.Date(pos_sampledt_FALSE)))

rolling_avg_hosp_numbers <- hospital_covid_data %>% 
  filter(confirmed_case == "Yes",
         hospitalized == "Yes") %>% 
  group_by(hosp_admidt_FALSE) %>% 
  summarise(hospital_cases = n()) %>% 
  drop_na(hosp_admidt_FALSE) %>% 
  mutate(hospitalisations = runner::sum_run(hospital_cases, k = 7, idx = as.Date(hosp_admidt_FALSE)))

seven_day_rolling_data <- full_join(rolling_avg_case_numbers, rolling_avg_hosp_numbers, 
          by = c("pos_sampledt_FALSE" = "hosp_admidt_FALSE")) %>% 
  select(pos_sampledt_FALSE, cases, hospitalisations, new_cases, hospital_cases) %>% 
  pivot_longer(cols = !pos_sampledt_FALSE, names_to = "cases_vs_hospitalisation", values_to = "rolling_avg")

#### Ribbon plot for seven day cumulative case numbers

Ribbon_plot <- seven_day_rolling_data %>% 
  filter(cases_vs_hospitalisation %in% c("cases", "hospitalisations")) %>% 
  group_by(cases_vs_hospitalisation) %>% 
  fill(rolling_avg, .direction = "up") %>% # prevent gaps in hospital data
  ggplot(aes(x = as_date(pos_sampledt_FALSE),
             y = rolling_avg,
             fill = cases_vs_hospitalisation,
             alpha = cases_vs_hospitalisation))+
  geom_area(position = "identity", colour = "grey50")+
  scale_alpha_manual(values = c(0.4, 1),
                     labels = c("Cases", 
                                "Hospitalisations"))+
  scale_fill_manual(values=c("#275182", "#FE3255"),
                    labels = c("Cases", 
                               "Hospitalisations"))+
  scale_colour_manual(values=c("#275182", "#FE3255"),
                    labels = c("Cases", 
                               "Hospitalisations"))+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  labs(x = "",
       y = "")+
  geom_vline(aes(colour = cases_vs_hospitalisation, xintercept = as_date(max(pos_sampledt_FALSE)- as.difftime(7, unit = "days"))),
               linetype = "dashed")+
  theme_custom()+
  theme(legend.position="bottom",
        legend.title = element_blank(), strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(hjust = 0.9))+
  facet_wrap(~ cases_vs_hospitalisation, scales = "free_y", ncol = 1)

### Tabulated data ###

# to calculate most recent stats
descending_case_data <- seven_day_rolling_data %>% 
  arrange(desc(pos_sampledt_FALSE)) %>% 
  mutate(rolling_avg = replace_na(rolling_avg, 0))

### Rolling case data table

dashboard_table <- descending_case_data %>% 
  slice(1:28) %>% 
  pivot_wider(names_from = cases_vs_hospitalisation, values_from = rolling_avg) %>%
  gt(id = "one") %>% # required for css
  tab_spanner(
    label = "Cumulative (last 7 days) ",
    columns = c(cases, hospitalisations)) %>% 
  tab_spanner(
    label = "Daily new cases",
    columns = c(new_cases, hospital_cases)) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 1)) %>% 
  cols_label(pos_sampledt_FALSE = "Date",
             cases = "Cases",
             hospitalisations = "Hospitalisations",
             new_cases = "New cases",
             hospital_cases = "New Hospitalisations") %>% 
  opt_css(
    css = "
    
    #one .gt_row {
      padding: 20px 30px;
    }
    #one tr:hover {
    background-color: lightgrey;
    }
    #one .gt_col_heading {
      text-align: center !important;
    }
    "
  )

### Symptoms tracker ###

### Example of turning a script into reusable functions. 

symptom_tracker <- function(data){

total_patients <- nrow(distinct(data,PID))

symptom_sum <- data %>% 
  select(-sym_startdt_FALSE, -sym_resolved, -sym_resolveddt_FALSE) %>% 
  pivot_longer((starts_with("sym_")), names_to="symptoms", values_to = "presence_absence") %>% 
  filter(presence_absence == "Yes") %>% 
  mutate(symptoms = as.factor(symptoms)) %>% 
  mutate(symptoms = recode(symptoms, sym_cough = "Cough",
                           sym_fever = "Fever",
                           sym_headache = "Headache",
                           sym_losstastesmell = "Loss of taste or smell",
                           sym_myalgia = "Myalgia",
                           sym_sorethroat = "Sore throat",
                           sym_subjfever = "Fever")) %>% 
  mutate(symptoms = factor(symptoms, levels = unique(symptoms))) %>% 
  group_by(symptoms) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/total_patients) %>% 
  mutate(
    perc = paste0(sprintf("%4.1f", n / total_patients * 100), "%"),
    ## customize label for the first category
    perc = if_else(row_number() == 2, paste(perc, "of all COVID cases"), perc)
  )

  
symptom_sum %>% 
ggplot(aes(x = reorder(symptoms, +freq), y = freq*100)) +
  geom_col(fill = "goldenrod1",
             colour = "white",
           alpha = 1)+
  geom_text(aes(label = perc),
            nudge_y = -0.5,
            hjust = 1,
            fontface="bold",
            colour = "black")+
  scale_y_continuous(breaks = seq (0,100, 10),
                     expand = c(0,0))+
  coord_flip()+
  labs(x = "",
       y = "")+
    theme_custom()+
    theme(axis.ticks.y = element_blank())


}


# Run data through the above function
symptoms_plot <- covid_data %>% 
  symptom_tracker() 






#### MAPS

max_7 <- max - as.difftime(7, unit = "days")


position_data <- covid_data %>% 
  filter(longitude_JITT < -75) %>% 
  filter(latitude_JITT > 30) %>% 
  filter(pos_sampledt_FALSE > max_7, na.rm=TRUE)


### note this will not run unless a separate google key is provided
key <- read_table(here("google_api.txt"), col_names = FALSE)
register_google(key)

atlanta_map <- get_map("Atlanta", maptype = "roadmap")


map <- ggmap(atlanta_map)+geom_point(data = position_data,
                              aes(x = longitude_JITT, 
                                  y = latitude_JITT,
                                  text = paste('Date', pos_sampledt_FALSE)),
                              color = "red", size = 2)+
  labs(x = "Longitude",
       y = "Latitude")






##### Demography tables




### Race and ethnicity table

summary.table <- covid_data %>% 
  mutate(case_race = na_if(case_race, "UNKNOWN")) %>% 
  mutate(case_race = str_to_title(case_race)) %>% 
  mutate(case_eth = na_if(case_eth, "NOT SPECIFIED")) %>% 
  mutate(case_eth = str_to_title(case_eth)) %>%
  select(case_race, case_eth) %>% 
  rename("Race" = case_race,
         "Ethnicity" = case_eth) %>% 
  mutate(`Race` = forcats::fct_relevel(`Race`, "Other", after = 6)) %>% 
  group_by(`Ethnicity`, `Race`) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(per = count/sum(count)) %>% 
  drop_na(`Ethnicity`) %>% 
  pivot_wider(names_from = `Ethnicity`, values_from = c(count, per)) %>% 
  mutate(Race = replace_na(Race, "Unknown")) %>% 
  rename(`Hispanic/Latino (N)` = `count_Hispanic/Latino`,
         `Non-Hispanic/Latino (N)` = `count_Non-Hispanic/Latino`,
         `Hispanic/Latino (%)` = `per_Hispanic/Latino`,
         `Non-Hispanic/Latino (%)` = `per_Non-Hispanic/Latino`)


race_ethnicity_table <- reactable(
  data = summary.table,
  pagination = FALSE,
  columns = list(
    `Hispanic/Latino (%)`= colDef(
      cell = data_bars(summary.table, 
                       align_bars = "right", 
                       text_position = "inside-end", 
                       number_fmt = scales::percent,
                       force_outside = c(0,0.2),
                       max_value = 1)
    ),
    `Non-Hispanic/Latino (%)` = colDef(
      cell = data_bars(summary.table, 
                       align_bars = "left",
                       text_position = "inside-end",
                       number_fmt = scales::percent,
                       force_outside = c(0,0.2),
                       max_value = 1)
      )))


##### Demographic pyramid

### Check ages
covid_data %>% 
  summarise(min = min(case_age, na.rm = T),
            max = max(case_age, na.rm = T))

qplot(covid_data$case_age)

### Pyramid plot

demographic_pyramid <- covid_data %>% 
  filter(between(case_age, 2, 106)) %>% 
  mutate(age_range = cut(case_age, breaks = c(2,12,16,25,35,50,70,106),
                         labels = c("2-11", "12-16", "17-24", "25-34", "35-49", "50-69", "70+"))) %>% 
  filter(case_gender == c("Male", "Female")) %>% 
  rename(`Age Range` = age_range,
         `Gender` = case_gender) %>% 
  age_pyramid(data = .,
              age_group = "Age Range",
              split_by = "Gender",
              proportional = TRUE)+
  theme_custom()

