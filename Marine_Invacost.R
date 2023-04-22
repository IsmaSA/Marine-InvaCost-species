### Marine Invacost 

#df <- read_xlsx("InvaCost_database_v4.1.xlsx")
df <- read.csv2("InvaCost_database_v4.1.csv")

head(df)
colnames(df)
df<- df %>% filter(Ross=="Yes")

df1<- df %>% group_by(Species) %>% summarise(entradas=n())


####### expanded costs #####
df<- df %>% filter(Ross=="Yes")
df <- df[-which(is.na(df$Probable_starting_year_adjusted)), ]
df <- df[-which(is.na(df$Probable_ending_year_adjusted)), ]

expanded <- expandYearlyCosts(df, #your help file
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")

min(expanded$Impact_year)
max(expanded$Impact_year)

expanded<-expanded %>% filter(Impact_year <= "2021")
expanded<-expanded %>% filter(Impact_year >= "1912")             
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded <- expanded[!is.na(expanded$cost),]
expanded$cost_bil <- (expanded$cost/1000000000)
sum(expanded$cost_bil) # 8.56
nrow(expanded)


########### Observed vs potential ########

expanded %>% group_by(Implementation) %>% summarise(costes=sum(cost_bil))


########### low vs high  ########

expanded %>% group_by(Method_reliability) %>% summarise(costes=sum(cost_bil))


########### by species  ########

a<- expanded %>% group_by(Species) %>% summarise(costes=sum(cost_bil))


###### Type of cost   #####

expanded %>% group_by(Type_of_cost_merged) %>% summarise(costes=sum(cost_bil))


###### sector impacted   #####
expanded$Impacted_sector[expanded$Impacted_sector== "Agriculture/Environment/Fishery"] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Agriculture/Environment/Forestry"] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Agriculture/Fishery"] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Authorities-Stakeholders/Fishery"] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Environment/Fishery"] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Environment/Fishery/Public and social welfare"] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Environment/Health/Public and social welfare"] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Environment/Public and social welfare "] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Fishery/Public and social welfare"] <- "Diverse"
expanded$Impacted_sector[expanded$Impacted_sector== "Environment/Public and social welfare"] <- "Diverse"

expanded %>% group_by(Impacted_sector) %>% summarise(costes=sum(cost_bil))


##########Temporal plot over time ######

expanded_obs <- expanded[expanded$Implementation %in% c("Observed"),]
sum(expanded_obs$cost_bil) #3.47

global.raw.all <- summarizeCosts(expanded,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1912,
                                 maximum.year = 2020,
                                 year.breaks = seq(1910, 2020, by = 10))    #you may have to use other intervals, but 10 is like the common basis
global.raw.obs <- summarizeCosts(expanded_obs,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1912,
                                 maximum.year = 2020,
                                 year.breaks = seq(1910, 2020, by = 10))
global.raw.obs$average.cost.per.period$middle.years <- global.raw.obs$average.cost.per.period$initial_year +
  (global.raw.obs$average.cost.per.period$final_year -
     global.raw.obs$average.cost.per.period$initial_year) / 2
global.raw.all$average.cost.per.period$middle.years <- global.raw.all$average.cost.per.period$initial_year +
  (global.raw.all$average.cost.per.period$final_year -
     global.raw.all$average.cost.per.period$initial_year) / 2
all.costs <- rbind(data.frame(global.raw.obs$average.cost.per.period,
                              cost.type = "Observed"),
                   data.frame(global.raw.all$average.cost.per.period,
                              cost.type = "All"))
all.costs.per.year <- rbind(data.frame(global.raw.obs$cost.per.year,
                                       cost.type = "Observed"),
                            data.frame(global.raw.all$cost.per.year,
                                       cost.type = "All"))

p1 <-ggplot(all.costs) +
  ylab(paste0("Annual cost in US$")) +
  # Points
  geom_point(aes_string(x = "middle.years",
                        y = "annual_cost",
                        col = "cost.type"),
             shape = 15) +
  # Lines between points
  geom_line(aes_string(x = "middle.years",
                       y = "annual_cost",
                       col = "cost.type"),
            linetype = 2) +
  # Horizontal bars (year span)
  geom_segment(aes_string(x = "initial_year",
                          xend = "final_year",
                          y = "annual_cost",
                          yend = "annual_cost",
                          col = "cost.type")) +
  geom_point(data = all.costs.per.year,
             aes(x = year, y = cost,
                 size = number_estimates,
                 col = cost.type),
             alpha = .6) +
  xlab("Year") +
  scale_x_continuous(breaks = global.raw.obs$year.breaks) +
  scale_size_continuous(name = "Number of estimates\nper year",
                        breaks = c(1, 5, 20, 50)) +
  scale_color_brewer(name = "Cost estimations",
                     palette = "Dark2") + # Minimal theme
  scale_y_log10(breaks = 10^(-15:15), # y axis in log 10 with pretty labels
                labels = scales::comma) +
  annotation_logticks(sides = "l")+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
p1


############### Lollyplot #####
options(scipen = 999)
df<- expanded %>% group_by(Species) %>% summarise(cost=sum(cost))

ggplot(df, aes(x=Species, y=cost)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Species, 
                   xend=Species, 
                   y=0, 
                   yend=cost)) + 
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1,face = "italic")) + 
  scale_y_continuous(trans='log10') 


df %>%
  arrange(-cost) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Species=factor(Species, levels=Species)) %>% 
  ggplot(aes(x=Species, y=cost)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Species, 
                   xend=Species, 
                   y=0, 
                   yend=cost)) + 
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust=1,face = "italic")) + 
  scale_y_continuous(trans='log10') 



str(df)

######## Ballon plot ####
a<- expanded %>% group_by(Implementation) %>% summarise(cost= sum(cost_bil),
                                                        entradas=n()) %>% mutate(ID=1)
b<- expanded %>% group_by(Method_reliability) %>% summarise(cost= sum(cost_bil),
                                                            entradas=n()) %>% mutate(ID=2)

ggplot(a, aes(Implementation, cost)) +
  geom_point(aes(size=entradas)) + 
  scale_size_continuous("n", range = c(1, 10))  + theme_classic()

ggplot(b, aes(Method_reliability, cost)) +
  geom_point(aes(size=entradas)) + 
  scale_size_continuous("n", range = c(1, 10))  + theme_classic()


## Pie chart
 expanded

 
df <- getInvaCostVersion(version = "4.1") 
 
df <- df[-which(is.na(df$Probable_starting_year_adjusted)), ]
df <- df[-which(is.na(df$Probable_ending_year_adjusted)), ]

replace_mat <- rbind(c(".*/.*", "multiple"), 
                     c("variety", "multiple"))

df <- replace_names(df, "Species", 
                    replace_mat, to_NA = c("", "multiple"))

df <-  df[complete.cases(df$Species),] 



df<- df %>% filter(!Species =="Terrestrial")
df<- df %>% filter(Environment =="Terrestrial")
df<- df %>% filter(!Environment =="Aquatic/Terrestrial/Semi-aquatic")
df<- df %>% filter(!Environment =="Diverse/Unspecified")
df<- df %>% filter(!Environment =="Aquatic/Terrestrial")
df<- df %>% filter(!Environment =="Aquatic/Semi-aquatic")


expanded <- expandYearlyCosts(df, #your help file
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")

min(expanded$Impact_year)
max(expanded$Impact_year)

expanded<-expanded %>% filter(Impact_year <= "2021")
expanded<-expanded %>% filter(Impact_year >= "1912")             
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded <- expanded[!is.na(expanded$cost),]
expanded$cost_bil <- (expanded$cost/1000000000)
sum(expanded$cost_bil) # 8.56
nrow(expanded) 
 
 
 
#### Total economic cost == 11,008
#### Marine cost == 8.56
#### Terrestrial cost == 6874.89
### Aquatic cost == 197
### Diverse == 3927.55


## Single version
#### Total economic cost == 5777.118
#### Marine cost == 
#### Terrestrial cost == 5631.152
### Aquatic cost == 
### Diverse == 


# 3D Exploded Pie Chart
library(plotrix)
slices <- c(8.56, 6874.89, 197)
labels <- c("Marine", "Terrestrial", "Aquatic")
pie(slices,labels, 
      col= c("darkblue","orange3","deepskyblue2"))

pie3D(slices,radius=0.9,labels=labels,explode=0.1,
      col= c("darkblue","orange3","deepskyblue2"))


x <- c(2.85, 1.26, 1.05, 0.80,0.36)
labels <- c("Gyrodactylus salaris", "Petromyzon marinus", "Diverse/Unspecified", 
            "Didemnum vexillum","Prymnesium polylepis")
pie3D(x,radius=0.9,labels=labels,explode=0.1,
      col= c("darkblue","orange3","deepskyblue2", "gray87","red"))
      

pie(x,labels, 
    col= c("darkblue","orange3","deepskyblue2", "gray87","red"))

63- 6

x <- c(34,2)
labels <- c("a", "Petromyzon marinus")
pie3D(x,radius=0.9,labels=labels,explode=0.1,
      col= c("darkblue","orange3"))


pie(x,labels, 
    col= c("darkblue","orange3"))
