# UPLOAD LIBRARY
library(data.table)
library(ggplot2)
library(lubridate)
library(leaflet)
library(dplyr)
library(tidyverse)
library(deSolve)
library(htmltools)
library(leaflet.extras)
library(mapview)
library(htmlwidgets)
library(IRdisplay)
library(readr)

# LOADING THE CSV FILE FOR CORONA VIRUS
corovirus <- read_csv("C:/Users/Delmafia91/Downloads/2019_nCoV_data.csv")
head(corovirus)

# set NA to 0
corovirus[is.na(corovirus)] <- 0

# QUICK LOOK AT THE DATA
head(corovirus)
str(corovirus)

# DELETE THE DUPLICATED ENTRYS BY DATE AND TIME OF ENTRY 

corovirus =
  corovirus %>% 
  mutate(`Last Update` = parse_date_time(`Last Update`,
                            c("mdy_HM","mdy"))) %>%
  group_by(`Province/State`,`Country`,`Last Update`) %>% 
  filter(Confirmed == max(Confirmed)) %>% 
  distinct

# PIPE THE DATA TO THE HUBEI PROVINCE (WUHAN), AND USE THE LAST DATA ENTRY OF THE DAY FOR PLOTTING

options(repr.plot.width = 26, repr.plot.height = 18)
corovirus %>% 
  filter(grepl('China',`Country`) & grepl('Hubei',`Province/State`))  %>%
  group_by(`Province/State`, d = day(`Last Update`)) %>% 
  filter(`Last Update` == max(`Last Update`)) %>%
  ggplot(aes(x = `Last Update`))+
  geom_line(aes(y=Confirmed, color = "Infected"))+
  geom_line(aes(y=Recovered, color = "Recovered"))+
  geom_line(aes(y=Deaths, color = "Deaths"))+
  
  geom_point(size = I(3), shape = 1, aes(y=Confirmed, color = "Infected"))+
  geom_point(size = I(3), shape = 1,aes(y=Recovered, color = "Recovered"))+
  geom_point(size = I(3), shape = 1,aes(y=Deaths, color = "Deaths"))+
  
  ylab(label="Count")+
  xlab(label="Date")+
  theme(legend.justification=c(1,0), legend.position=c(0.25,0.5))+
  theme(legend.title=element_text(size=12,face="bold"),
        legend.background = element_rect(fill='#FFFFFF',
                                         size=0.5,linetype="solid"),
        legend.text=element_text(size=10),
        legend.key=element_rect(colour="#FFFFFF",
                                fill='#C2C2C2',
                                size=0.25,
                                linetype="solid"))+
  scale_colour_manual("Compartments",
                      breaks=c("Infected","Recovered","Deaths"),
                      values=c("blue","green","black"))+
  labs(title = "Wuhan Coronavirus(2019-nCoV",
       subtitle = "Hubei Province, China")

# PLOT THE TOP TEN PROVINCES
# RETRIEVE NAMES TOP TEN PROVINCES

options(repr.plot.width = 36, 
        repr.plot.height = 18)
top_10 = 
  corovirus %>%
  filter(grepl('China', 
`Country`) & 
  `Last Update` == max(`Last Update`)) %>%
  top_n(10, Confirmed) %>%
  pull(`Province/State`)

corovirus %>%
  filter(grepl('China', `Country`) & 
           `Province/State` %in% top_10) %>%
  group_by(`Province/State`, d = day(`Last Update`)) %>%
  filter(`Last Update` == max(`Last Update`)) %>%
  ggplot(aes(x = `Last Update`, y = Confirmed,
             color = `Province/State`)) +
  geom_line() + 
  geom_point(size = I(1), shape = 2) +
  ylab(label = "Count") +
  xlab(label = "Date") +
  labs(title = "Wuhan Coronavirus 2019", 
       subtitle = "Top Ten Confirmed Cases By Province, China")

# Data piped to the last data entered for each province on each day and then summed to get a mainland total by day for plotting.
  
options(repr.plot.width = 36,
        repr.plot.height = 18)

corovirus %>%
  filter(grepl('China', `Country`)) %>%
  group_by(`Province/State`, d = day(`Last Update`)) %>%
  filter(`Last Update` == max(`Last Update`)) %>%
  group_by(d, m = month(`Last Update`), y=year(`Last Update`)) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(Date = ymd(paste(y, m, d))) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y=Confirmed, color = "Infected")) +
  geom_line(aes(y=Recovered, color = "Recovered")) +
  geom_line(aes(y=Deaths, color = "Deaths")) + 
  
  geom_point(size = I(2), shape = 2, 
            aes(y=Confirmed, color = "Infected")) +
  geom_point(size = I(2), shape = 2, 
            aes(y=Recovered, color = "Recovered")) +
  geom_point(size = I(2), shape = 2, 
            aes(y=Deaths, color = "Deaths")) +
  ylab(label = "Count") + 
  xlab(label = "Date") +
  theme(legend.justification = c(1,0), 
        legend.position = c(0.25, 0.5)) +
  theme(legend.title = element_text(size = 11, face = "bold"), 
        legend.background = element_rect(fill = '#FFFFFF',
                      size = 0.5, linetype = "solid"), 
        legend.text = element_text(size = 9), 
        legend.key = element_rect(colour = "#FFFFFF", 
                                  fill = '#C2C2C2', 
        size = 0.25, linetype = "solid")) +
  scale_colour_manual("Compartments", 
                      breaks = c("Infected", "Recovered", 
                                 "Deaths"), 
                values = c("blue", "red", "green")) +
  labs(title = "Wuhan Coronavirus 2019", 
       subtitle = "Mainland China")

# Create proviance lat and lon coordinates to join into the data set for creating an interactive map. 

provcoord = data.table(Province = c('Shanghai', 
'Beijing', 'Guangdong','Hubei','Tianjin','Chongqing',
'Liaoning','Sichuan','Jiangsu','Guizhou','Heilongjiang',
'Jilin','Zhejiang','Yunnan','Shanxi','Shandong',
'Fujian','Hunan','Gansu','Hebei','Guangxi',
'Xinjiang','Hainan','Anhui','Inner Mongolia',
'Qinghai','Ningxia','Tibet','Shaanxi','Henan','Jiangxi'), 
lng = c(121.458056, 116.388869,113.25,114.266667,117.176667,106.552778,123.432778,
        104.066667,118.777778,106.716667,126.65,125.322778,120.161419,102.718333,
        112.560278,116.997222,119.306111,112.966667,103.839868,114.478611,108.316667,
        87.630506,110.341667,117.280833,111.652222,101.75739,106.273056,91.1,108.928611,
        113.648611, 115.883333), 
lat = c(31.222222, 39.928819,23.116667, 30.583333,39.142222,29.562778,41.792222,30.666667,
        32.061667,26.583333,45.75,43.88,30.29365,25.038889,37.869444,36.668333,26.061389,
        28.2,36.057006,38.041389,22.816667,43.807347,20.045833,31.863889, 40.810556,
        36.625541,38.468056,29.65,34.258333,34.757778,28.683333))


# USING SEIR model. I HAVE NO CLUE IF THIS IS CORRECT, BUT INTERESTNG TO SEE A PREDICTED DURATION. BASED ON THE ACTIONS BEING TAKEN 
# I'M ASSUMING IT NOT ACCURATE, BUT I'M TRYING TO USE DIFFEQ PARAMETERS TO MATCH THE ACTUAL HUBEI PROVINCE DATA.
require(EpiDynamics)

SEIR.model.incubation.pop <- function(t, b, g, c ,population = 1E6, infect = 50){
  
  init <- c(S=1-infect/population,I=infect/population,R=0, E=3000/population)
  parameters <- c(bet=b,gamm=g,eta=c)
  time <- seq(0,t,by=t/(2*length(1:t)))
  eqn <- function(time,state,parameters){
    with(as.list(c(state,parameters)),{
      dS <- -bet*S*I
      dI <- eta*E-gamm*I
      dR <- gamm*I
      dE <- bet*S*I-eta*E
      return(list(c(dS,dI,dR,dE)))})}
  out<-ode(y=init,times=time,eqn,parms=parameters)
  out.dt<-as.data.table(out)
  
  out.dt[,S_pop := S*population]
  out.dt[,I_pop := I*population]
  out.dt[,R_pop := R*population]
  out.dt[,E_pop := E*population]
  
  #out.dt$S_pop <- out.dt$S*population
  #out.dt$I_pop <- out.dt$I*population
  #out.dt$R_pop <- out.dt$R*population
  #out.dt$E_pop <- out.dt$E*population
  
  options(repr.plot.width = 26, repr.plot.height = 18)
  
  corovirusAct <-
    corovirus %>% 
    filter(grepl('China',`Country`)& grepl('Hubei',`Province/State`))  %>%
    group_by(`Province/State`, d = day(`Last Update`))  %>% 
    filter(`Last Update` == max(`Last Update`)) %>%
    group_by( d, m = month(`Last Update`),y=year(`Last Update`)) %>%
    summarise_if(is.numeric,sum) %>%
    mutate(Date = ymd(paste(y,m,d))) %>%
    arrange(Date) %>%
    mutate(numDays = as.numeric(c(diff(Date),1))) %>%
    as.data.table() %>%
    mutate(numDays = cumsum(numDays)) %>%
    select(Confirmed,Recovered,numDays) %>%
    as.data.table()
  
  corovirusAct =  
    corovirusAct %>%
    mutate(type = rep("Coronavirus",times = nrow(corovirusAct)),
           Suspected = rep(0,times = nrow(corovirusAct)),
           Expected = rep(0,times = nrow(corovirusAct))
    ) %>% 
    as.data.table()
  
  out.dt  =  
    out.dt %>%
    select(numDays = time, Suspected = S_pop,Confirmed = I_pop,Recovered = R_pop, Expected=E_pop) %>%
    mutate(type = rep('Model',times = nrow(out.dt))) %>%
    rbind(corovirusAct)
  
  title <- bquote("SEIR Model: Basic")
  subtit <- bquote(list(beta==.(parameters[1]),~gamma==.(parameters[2]),~eta==.(parameters[3])))
  
  res<-ggplot(out.dt,aes(x=numDays))+
    ggtitle(bquote(atop(bold(.(title)),atop(bold(.(subtit))))))+
    #geom_line(aes(y=S_pop,colour="Susceptible"))+
    geom_line(aes(y=Confirmed,colour="Confirmed"))+
    geom_line(aes(y=Recovered,colour="Recovered"))+
    geom_line(aes(y=Expected,colour="Incubation"))+
    ylab(label="Count")+
    xlab(label="Time (days)")+
    facet_grid(.~type)+
    theme(legend.justification=c(1,0), legend.position=c(1,0.75))+
    theme(legend.title=element_text(size=12,face="bold"),
          legend.background = element_rect(fill='#FFFFFF',
                                           size=0.5,linetype="solid"),
          legend.text=element_text(size=10),
          legend.key=element_rect(colour="#FFFFFF",
                                  fill='#C2C2C2',
                                  size=0.25,
                                  linetype="solid"))+
    scale_colour_manual("Compartments",
                        breaks=c("Susceptible","Confirmed","Recovered","Incubation"),
                        values=c("blue","red","darkgreen","black"))
  
  res
}

SEIR.model.incubation.pop(20,1.1,1/7,1/6)
SEIR.model.incubation.pop(50,1.1,1/7,1/6)

# BASED ON THE DATA GIVEN FOR THE CORONA VIRUS OUTBREAK IN CHINA, WE CAN SEE THAT THE HIGHEST NUMBER OF THE CASE IS DEALING WITH PEOPLE BEING INFECTED BY THE VIRUS.
# EVEN THOUGH IT IS GOOD THAT ANYONE SHOULD PERISH FROM THIS VIRUS, NEVERTHELESS, THE NUMBER OF DEATHS IS MINIMAL COMPARED TO THOSE INFECTED.
# SOME ARE RECOVERING AT A SLOW PAST, WHICH IS IN A SENSE GOOD, BECAUSE IT SHOWS THAT THERE IS PROGRESS. EVEN THOUGH, THOSE BEING RECOVERED FROM THE VIRUS, IS SLIGHTLY
# ABOVE THOSE DYING FROM THE VIRUS, IT IS NOT AS BAD AS IT MAY HAVE BEEN MENTIONED ON THE NEWS. IT IS TRUE THAT A LOT OF PEOPLE ARE DYING FROM THIS VIRUS, BUT THE TRUTH
# IS THAT MORE PEOPLE ARE RECOVERING FROM IT, EVEN THOUGH THOSE BEING INFECTED IS VERY HIGH COMPARED TO THOSE RECOVERING OR DYING.