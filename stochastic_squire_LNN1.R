#Author: Prof. Thomas Achia & Lucy Njoki
#Date:29/7/2020
#--------------STOCHASTIC SEIR MODEL USING R-squire library-----------------------------
#
# Adopting the UK MRC R-squire model that was developed for the following purpose:
# 1.  A stochastic age-structured SEIR model incorporating explicit passage through healthcare 
#     settings and explicit progression through disease severity stages.
# 2.  The ability to calibrate the model to different epidemic start-dates based on 
#     available death data.
# 3.  Simulate the impacts of different control interventions (including general social 
#     distancing, specific shielding of elderly populations, and more stringent suppression 
#     strategies).
#------------------------------------------------------------------------------------------

#-------------------------------------------------------------------

# Odin
# https://cran.r-project.org/web/packages/odin/vignettes/discrete.html
# http://epirecip.es/epicookbook/chapters/sir-stochastic-discretestate-discretetime/r_odin

# The COVID-19 pandemic in Kenya

# https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Kenya
# First COVID 19 case in Kenya March 13, 2020
# School closure in Kenya on March 15, 2020
# Social gatherings banned 25 March

# June 6, 2020 dusk-to-dawn curfews extended
# Kenya had partial lockdown start in Nairobi and Mombasa on April 
#-------------------------------------------------------------------
rm(list = ls(all = TRUE))

suppressS({
  library(squire)
  library(patchwork)
  library(tidyverse) 
  library(ggthemes)
  library(extrafont)
  library(fcuk)
  library(lubridate)
})


# Directory

#outputs<-"//fpv-ke1.intl.cdc.gov/private/yrb2/My Documents/Thomas CDC/COVID-19 response/modeling/stochastic_approach/outputs"



# Get the population

npop = 47563609 # From the 2019 Census

f= c(0.13,0.13,0.13,0.11,0.09,0.08,0.08,0.06,0.05,
     0.04,0.03,0.02,0.02,0.01,0.01,0.01) # Age proportions Based on the 2019 Census
population =  npop*f      # number in each age class

pop <- get_population("Kenya")
population <- pop$n

population<-round(47563609*population/sum(population))
# Rescale population to the population of Kenya

# Get the mixing matrix

contact_matrix <- get_mixing_matrix("Kenya")
# Different from the Prem et al (POLYMOD) paper 

# Parameters: https://mrc-ide.github.io/squire/articles/parameters.html

# https://mrc-ide.github.io/squire/

# Unmitigate scenario
# model using baseline parameters and no control interventions
r <- run_explicit_SEEIR_model(population = population,
                              contact_matrix_set = contact_matrix,
                              country = "Kenya",dt = 1)
# Plot
plot(r)

unmitigated<-temo(r) %>%
  mutate(scenario_type = "unmitigated")
head(unmitigated)

write.csv(unmitigated,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\unmitigated.csv")


# Intervention scenarios

# Set a 20% reduction in the contact matrix after 2 days :

r1 <- run_explicit_SEEIR_model(population = population, 
                               tt_contact_matrix = c(0, 2),
                               contact_matrix_set = list(contact_matrix,
                                                         contact_matrix*0.8),
                               country = "Kenya",dt = 1)

scenario1<-temo(r1)%>%
  mutate(scenario_type = "scenario1")
head(scenario1)
write.csv(scenario1,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario1")

plot(r1)

# Set a 50% reduction in the contact matrix after 2 days :

r2 <- run_explicit_SEEIR_model(population = population, 
                               tt_contact_matrix = c(0, 2),
                               contact_matrix_set = list(contact_matrix,
                                                         contact_matrix*0.5),
                               country = "Kenya",dt = 1)

scenario2<-temo(r2) %>%
  mutate(scenario_type = "scenario2")
head(scenario2)

write.csv(scenario2,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario2")
plot(r2)


# Set a 30% reduction in the contact matrix after 2 days :
r2a <- run_explicit_SEEIR_model(population = population, 
                               tt_contact_matrix = c(0, 2),
                               contact_matrix_set = list(contact_matrix,
                                                         contact_matrix*0.7),
                               R0 = 2.5,country = "Kenya",dt = 1)

scenario2a<-temo(r2a) %>%
  mutate(scenario_type = "scenario2a")
head(scenario2a)
#write.csv(scenario2,"//fpv-ke1.intl.cdc.gov/private/yrb2/My Documents/Thomas CDC/COVID-19 response/modeling/stochastic_approach/outputs/scenario2a.csv")

plot(r2)

# Set a 60% reduction in the contact matrix after 2 days :

r3 <- run_explicit_SEEIR_model(population = population, 
                               tt_contact_matrix = c(0, 2),
                               contact_matrix_set = list(contact_matrix,
                                                         contact_matrix*0.4),
                               country = "Kenya",dt = 1)

scenario3<-temo(r3) %>%
  mutate(scenario_type = "scenario3")

head(scenario3)
write.csv(scenario3,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario3")

plot(r3)

# Set a 80% reduction in the contact matrix after 2 days :

r4 <- run_explicit_SEEIR_model(population = population, 
                               tt_contact_matrix = c(0, 2),
                               contact_matrix_set = list(contact_matrix,
                                                         contact_matrix*0.2),
                               country = "Kenya",dt = 1)

scenario4<-temo(r4) %>%
  mutate(scenario_type = "scenario4")
head(scenario4)

write.csv(scenario4,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario4")

plot(r4)

#merging the scenarios datasets
scenarios_data <- data.frame(Reduce(function(x, y) merge(x, y, all=TRUE), 
                                    list(unmitigated,scenario1, scenario2, scenario3, scenario4)))

head(scenarios_data)
class(scenarios_data)

scenarios_data %>%
  glimpse() %>%
  names()

#turn off the scientific notation
options(scipen = 999)

scenarios_data$scenario_type <- as.factor(scenarios_data$scenario_type) #convert to factors

#Daily deaths

theme_set(theme_tufte()) #setting my theme

plots_data <- scenarios_data %>%
  group_by(scenario_type)

p1 <-ggplot(plots_data, aes(x = time, y = deaths, colour = scenario_type)) +
  geom_line()+
  labs(y = "Daily Deaths",  x = "Time",
       title = "Daily Deaths in the Different Interventions ") +
  theme(plot.title = element_text(family = "Times New Roman", size = 15, 
                                  hjust = 0.5, face = "bold"),
        legend.position = "right",
        legend.title = element_text(family = "Times New Roman", size = rel(1.2)),
        legend.text = element_text(family = "Times New Roman", size = rel(1.0)),
        axis.title = element_text(family = "Times New Roman", size = rel(1.2), 
                                  hjust = 0.5, vjust = 10),
        axis.text = element_text(family = "Times New Roman", size = rel(1.0)),
        axis.line.y = element_line(colour = "black", size = 0.5),
        axis.line.x = element_line(colour = "black", size = 0.5)) +
  scale_color_discrete(name = "Scenarios", labels = c("Scenario1", "Scenario2","Scenario2a" ,
                                                      "Scenario3",
                                                      "Scenario4", "Unmitigated"))
ggplotly(p1)
#Infections

p2 <-ggplot(plots_data, aes(x = time, y = infections, colour = scenario_type)) +
  geom_line()+
  labs(y = "Infections",  x = "Time",
       title = "Infections in the Different Interventions ") +
  theme(plot.title = element_text(family = "Times New Roman", size = 15, 
                                  hjust = 0.5, face = "bold"),
        legend.position = "right",
        legend.title = element_text(family = "Times New Roman", size = rel(1.2)),
        legend.text = element_text(family = "Times New Roman", size = rel(1.0)),
        axis.title = element_text(family = "Times New Roman", size = rel(1.2), 
                                  hjust = 0.5, vjust = 10),
        axis.text = element_text(family = "Times New Roman", size = rel(1.0)),
        axis.line.y = element_line(colour = "black", size = 0.5),
        axis.line.x = element_line(colour = "black", size = 0.5)) +
  scale_color_discrete(name = "Scenarios", labels = c("Scenario1", "Scenario2","Scenario2a",
                                                      "Scenario3",
                                                      "Scenario4", "Unmitigated"))
p1/p2



# Calibrate the model
#*******************************************************************************
#Data importing and cleaning

#download the data: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

cases <- read_csv(here::here("time_series_covid19_confirmed_global.txt")) 

cases1 <- cases %>% gather("date", "cases", 5:192) #merge datescases

deaths <- read_csv(here::here("time_series_covid19_deaths_global.txt"))

deaths1 <- deaths %>% gather("date", "deaths", 5:192) #merge the dates 

recover <- read_csv(here::here("time_series_covid19_recovered_global.txt")) 

recover1 <- recover %>% gather("date", "recover", 5:192) #merge the dates

#merge the deaths, cases and recover datasets
df <- left_join(cases1, deaths1, by = c("Province/State", "Country/Region",
                                       "Lat", "Long","date"))

mydata <- left_join(df, recover1, by = c("Province/State", "Country/Region",
                                        "Lat", "Long","date"))

#rename variables Country/Region & Province/State

names(mydata)[names(mydata) == "Country/Region"] <- "Country"
names(mydata)[names(mydata) == "Province/State"] <- "Province"

#filter for Kenya only & Remove the province
covidKE <- mydata %>% filter(Country == "Kenya") %>%
  select(-Province)

head(covidKE)

write.csv(covidKE,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\Covid19KE.csv")

#*******************************************************************************
#import the data
covke <- read.csv("D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\Covid19KE.csv")
View(covke)
class(covke)
covke %>%
  glimpse()

covke$date <- mdy(covke$date) #convert date from character

# set up for parallelisation
future::plan(future::multiprocess())

# Fit model
out <- calibrate(data = covke,  R0_min = 1.5,  R0_max = 4,  R0_step = 0.5,
  first_start_date = "2020-03-01",  last_start_date = "2020-03-10",
  day_step = 1,  replicates = 10, n_particles = 20,  forecast = 360,
  country = "Kenya",
  baseline_ICU_bed_capacity = 38,
  baseline_hosp_bed_capacity = 1893)

plot(out, "deaths", date_0 = max(covke$date), x_var = "date")
plot(out, "infections", date_0 = max(covke$date), x_var = "date")
plot(out, particle_fit = TRUE)

plot(out, particle_fit = TRUE) + 
  ggplot2::xlim(as.Date(c("2020-03-10","2020-07-03"))) + 
  ggplot2::ylim(c(0,15))


scenario0<-temo(out) %>%
  mutate(scenario_type = "scenario0")
write.csv(scenario0,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario0.csv")


# interventions 
change_date<-c("2020-03-20")
change_senario1<-c(0.8) # An 20% decline in contacts
change_senario2<-c(0.6) # An 40% decline in contacts
change_senario3<-c(0.5) # An 50% decline in contacts
change_senario4<-c(0.4) # An 60% decline in contacts
change_senario5<-c(0.2) # An 80% decline in contacts

# Scenario 1. 20% decline in contact
out1 <- calibrate(
  data = covke,R0_min = 1.5,R0_max = 4,R0_step = 0.5,first_start_date = "2020-03-01",
  last_start_date = "2020-03-10",day_step = 1,replicates = 10,n_particles = 20,
  forecast = 360, country = "Kenya",R0_change = change_senario1,
  date_R0_change = change_date,
  baseline_ICU_bed_capacity = 38,
  baseline_hosp_bed_capacity = 1893)

plot(out1, "deaths", date_0 = max(df$date), x_var = "date")
plot(out1, "infections", date_0 = max(df$date), x_var = "date")

scenario_1<-temo(out1)%>%
  mutate(scenario_type = "scenario_1")

write.csv(scenario_1,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario_1.csv")

# Scenario 2. 40% decline in contact
out2 <- calibrate(
  data = covke,R0_min = 1.5,R0_max = 4,R0_step = 0.5,first_start_date = "2020-03-01",
  last_start_date = "2020-03-10",day_step = 1,replicates = 10,n_particles = 20,
  forecast = 360, country = "Kenya",R0_change = change_senario2,
  date_R0_change = change_date,
  baseline_ICU_bed_capacity = 38,
  baseline_hosp_bed_capacity = 1893)


plot(out2, "deaths", date_0 = max(df$date), x_var = "date")
plot(out2, "infections", date_0 = max(df$date), x_var = "date")

scenario_2<-temo(out2)%>%
  mutate(scenario_type = "scenario_2")

write.csv(scenario_2,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario_2.csv")

# Scenario 3. 50% decline in contact
out3 <- calibrate(
  data = covke,R0_min = 1.5,R0_max = 4,R0_step = 0.5,first_start_date = "2020-03-01",
  last_start_date = "2020-03-10",day_step = 1,replicates = 10,n_particles = 20,
  forecast = 360, country = "Kenya",R0_change = change_senario3,
  date_R0_change = change_date,
  baseline_ICU_bed_capacity = 38,
  baseline_hosp_bed_capacity = 1893)


plot(out3, "deaths", date_0 = max(df$date), x_var = "date")
plot(out3, "infections", date_0 = max(df$date), x_var = "date")

scenario_3<-temo(out3) %>%
  mutate(scenario_type = "scenario_3")

write.csv(scenario_3,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario_3.csv")

# Scenario 4. 60% decline in contact
out4 <- calibrate(
  data = covke,R0_min = 1.5,R0_max = 4,R0_step = 0.5,first_start_date = "2020-03-01",
  last_start_date = "2020-03-10",day_step = 1,replicates = 10,n_particles = 20,
  forecast = 180, country = "Kenya",R0_change = change_senario4,
  date_R0_change = change_date,
  baseline_ICU_bed_capacity = 38,
  baseline_hosp_bed_capacity = 1893)


plot(out4, "deaths", date_0 = max(df$date), x_var = "date")
plot(out4, "infections", date_0 = max(df$date), x_var = "date")

scenario_4<-temo(out4) %>%
  mutate(scenario_type = "scenario_4")
write.csv(scenario_4,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario_4.csv")


# Scenario 5. 80% decline in contact
out5 <- calibrate(
  data = covke,R0_min = 1.5,R0_max = 4,R0_step = 0.5,first_start_date = "2020-03-01",
  last_start_date = "2020-03-10",day_step = 1,replicates = 10,n_particles = 20,
  forecast = 360, country = "Kenya",R0_change = change_senario5,
  date_R0_change = change_date,
  baseline_ICU_bed_capacity = 38,
  baseline_hosp_bed_capacity = 1893)


#plot(out5, "deaths", date_0 = max(df$date), x_var = "date")
#plot(out5, "infections", date_0 = max(df$date), x_var = "date")

scenario_5<-temo(out5)%>%
  mutate(scenario_type = "scenario_5")

write.csv(scenario_5,"D:\\NJUKI\\Prof-Achia-Work\\explicit_SEEIR_model\\scenario_5.csv")

#merging the scenarios datasets after calibrating the model
scenarios_data_calibrated <- data.frame(Reduce(function(x, y) merge(x, y, all=TRUE), 
                                    list(scenario0,scenario_1, scenario_2, scenario_3,
                                         scenario_4,scenario_5)))

head(scenarios_data_calibrated)
class(scenarios_data_calibrated)

scenarios_data_calibrated %>%
  glimpse() %>%
  names()


scenarios_data_calibrated$scenario_type <- as.factor(scenarios_data_calibrated$scenario_type)

#Daily deaths

theme_set(theme_tufte())
plots_data_calibrated <- scenarios_data_calibrated %>%
  group_by(scenario_type)

p3 <-ggplot(plots_data_calibrated, aes(x = time, y = deaths, colour = scenario_type)) +
  geom_line()+
  labs(y = "Daily Deaths",  x = "Time",
       title = "Daily Deaths in the \n Different Interventions(Calibrated)") +
  theme(plot.title = element_text(family = "Times New Roman", size = 15, 
                                  hjust = 0.5, face = "bold"),
        legend.position = "right",
        legend.title = element_text(family = "Times New Roman", size = rel(1.2)),
        legend.text = element_text(family = "Times New Roman", size = rel(1.0)),
        axis.title = element_text(family = "Times New Roman", size = rel(1.2), 
                                  hjust = 0.5, vjust = 10),
        axis.text = element_text(family = "Times New Roman", size = rel(1.0)),
        axis.line.y = element_line(colour = "black", size = 0.5),
        axis.line.x = element_line(colour = "black", size = 0.5))+
  scale_color_discrete(name = "Scenarios", labels = c("Scenario1", "Scenario2","Scenario3",
                                                      "Scenario4", "Scenario5", "Unmitigated"))

#Infections

p4 <-ggplot(plots_data_calibrated, aes(x = time, y = infections, colour = scenario_type)) +
  geom_line()+
  labs(y = "Infections",  x = "Time",
       title = "Infections in the \n Different Interventions(Calibrated)") +
  theme(plot.title = element_text(family = "Times New Roman", size = 15, 
                                  hjust = 0.5, face = "bold"),
        legend.position = "right",
        legend.title = element_text(family = "Times New Roman", size = rel(1.2)),
        legend.text = element_text(family = "Times New Roman", size = rel(1.0)),
        axis.title = element_text(family = "Times New Roman", size = rel(1.2), 
                                  hjust = 0.5, vjust = 10),
        axis.text = element_text(family = "Times New Roman", size = rel(1.0)),
        axis.line.y = element_line(colour = "black", size = 0.5),
        axis.line.x = element_line(colour = "black", size = 0.5)) +
  scale_color_discrete(name = "Scenarios", labels = c("Scenario1", "Scenario2","Scenario3",
                                                      "Scenario4",
                                                    "Scenario5", "Unmitigated"))
p3/p4

#making the plots interactive
library(plotly) 
ggplotly(p1)
ggplotly(p2)
ggplotly(p3)
ggplotly(p4)
names(scenario5)


stat0<-c(sum(scenario0$deaths),sum(scenario0$infections),
         max(scenario0$hosp_bed),max(scenario0$hospital_demand),max(scenario0$ICU_demand),
         max(scenario0$ICU_bed),max(scenario0$IOxGetLive),max(scenario0$IOxGetDie),
         max(scenario0$IOxNotGetLive),max(scenario0$IOxNotGetDie),max(scenario0$IMVGetLive),
         max(scenario0$IMVGetDie),max(scenario0$IMVNotGetDie),max(scenario0$IMVNotGetLive))


stat1<-c(sum(scenario_1$deaths),sum(scenario_1$infections),
max(scenario_1$hosp_bed),max(scenario_1$hospital_demand),max(scenario_1$ICU_demand),
max(scenario_1$ICU_bed),max(scenario_1$IOxGetLive),max(scenario_1$IOxGetDie),
max(scenario_1$IOxNotGetLive),max(scenario_1$IOxNotGetDie),max(scenario_1$IMVGetLive),
max(scenario_1$IMVGetDie),max(scenario_1$IMVNotGetDie),max(scenario_1$IMVNotGetLive))

stat2<-c(sum(is.na(scenario_2$deaths)),sum(is.na(scenario_2$infections)),
         max(is.na(scenario_2$hosp_bed)),max(is.na(scenario_2$hospital_demand)),max(is.na(scenario_2$ICU_demand)),
         max(is.na(scenario_2$ICU_bed)),max(is.na(scenario_2$IOxGetLive)),max(is.na(scenario_2$IOxGetDie)),
         max(is.na(scenario_2$IOxNotGetLive)),max(is.na(scenario_2$IOxNotGetDie)),max(is.na(scenario_2$IMVGetLive)),
         max(is.na(scenario_2$IMVGetDie)),max(is.na(scenario_2$IMVNotGetDie)),max(is.na(scenario_2$IMVNotGetLive)))

stat3<-c(sum(scenario_3$deaths),sum(scenario_3$infections),
         max(scenario_3$hosp_bed),max(scenario_3$hospital_demand),max(scenario_3$ICU_demand),
         max(scenario_3$ICU_bed),max(scenario_3$IOxGetLive),max(scenario_3$IOxGetDie),
         max(scenario_3$IOxNotGetLive),max(scenario_3$IOxNotGetDie),max(scenario_3$IMVGetLive),
         max(scenario_3$IMVGetDie),max(scenario_3$IMVNotGetDie),max(scenario_3$IMVNotGetLive))

stat4<-c(sum(scenario_4$deaths),sum(scenario_4$infections),
         max(scenario_4$hosp_bed),max(scenario_4$hospital_demand),max(scenario_4$ICU_demand),
         max(scenario_4$ICU_bed),max(scenario_4$IOxGetLive),max(scenario_4$IOxGetDie),
         max(scenario_4$IOxNotGetLive),max(scenario_4$IOxNotGetDie),max(scenario_4$IMVGetLive),
         max(scenario_4$IMVGetDie),max(scenario_4$IMVNotGetDie),max(scenario_4$IMVNotGetLive))


stat5<-c(sum(scenario_5$deaths),sum(scenario_5$infections),
         max(scenario_5$hosp_bed),max(scenario_5$hospital_demand),max(scenario_5$ICU_demand),
         max(scenario_5$ICU_bed),max(scenario_5$IOxGetLive),max(scenario_5$IOxGetDie),
         max(scenario_5$IOxNotGetLive),max(scenario_5$IOxNotGetDie),max(scenario_5$IMVGetLive),
         max(scenario_5$IMVGetDie),max(scenario_5$IMVNotGetDie),max(scenario_5$IMVNotGetLive))

cbind(stat0,stat1,stat2,stat3,stat4,stat5)
