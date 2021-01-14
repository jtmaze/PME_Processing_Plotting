#Author: James Maze
#Purpose: Plotting PME data from F20 deployment

# 1. Load Packages and set data_dir --------------------------------------------------------

library("tidyverse")
library("lubridate")
library("readr")
library("lubridate")
library("zoo")
library("xts")
library("dygraphs")
library("htmlwidgets")

data_dir <- "C:/Users/James Maze/Desktop/Delmarva/Data/"

# 2. Read the data -----------------------------------------------------

#All of the data is read in separately, so I could keep track of Site_Ids. Worthwhile to automate. 

#DK Data
DK_SW_20200906_20200919 <- read_delim(paste0(data_dir,"DO/DK_SW_PME_20200906_20200919.txt"),
                                     delim = ",",
                                     skip = 9, 
                                     col_names = FALSE) %>% 
  add_column(Site_Id = "DK")

DK_SW_20200919_20201026 <- read_delim(paste0(data_dir,"DO/DK_SW_PME_20200919_20201026.txt"),
                                      delim = ",",
                                      skip = 9, 
                                      col_names = FALSE) %>% 
  add_column(Site_Id = "DK")

DK_SW_20201026_20201218 <- read_delim(paste0(data_dir,"DO/DK_SW_PME_20201026_20201218.txt"),
                                      delim = ",",
                                      skip = 9, 
                                      col_names = FALSE) %>% 
  add_column(Site_Id = "DK")

#TS Data

TS_SW_20200906_20200919 <- read_delim(paste0(data_dir,"DO/TS_SW_PME_20200906_20200919.txt"),
                                      delim = ",",
                                      skip = 9, 
                                      col_names = FALSE) %>% 
  add_column(Site_Id = "TS")

TS_SW_20200919_20201026 <- read_delim(paste0(data_dir,"DO/TS_SW_PME_20200919_20201026.txt"),
                                      delim = ",",
                                      skip = 9, 
                                      col_names = FALSE) %>% 
  add_column(Site_Id = "TS")

TS_SW_20201026_20201218 <- read_delim(paste0(data_dir,"DO/TS_SW_PME_20201026_20201218.txt"),
                                      delim = ",",
                                      skip = 9, 
                                      col_names = FALSE) %>% 
  add_column(Site_Id = "TS")

#ND Data

ND_SW_20200906_20200919 <- read_delim(paste0(data_dir,"DO/ND_SW_PME_20200906_20200919.txt"),
                                      delim = ",",
                                      skip = 9, 
                                      col_names = FALSE) %>% 
  add_column(Site_Id = "ND")

ND_SW_20200919_20201026 <- read_delim(paste0(data_dir,"DO/ND_SW_PME_20200919_20201026.txt"),
                                      delim = ",",
                                      skip = 9, 
                                      col_names = FALSE) %>% 
  add_column(Site_Id = "ND")

ND_SW_20201026_20201218 <- read_delim(paste0(data_dir,"DO/ND_SW_PME_20201026_20201218.txt"),
                                      delim = ",",
                                      skip = 9, 
                                      col_names = FALSE) %>% 
  add_column(Site_Id = "ND")


# 3. Combine the Data Frames ----------------------------------------------

PME_data_full_raw <- rbind(ND_SW_20201026_20201218, ND_SW_20200919_20201026, ND_SW_20200906_20200919,
                           TS_SW_20201026_20201218, TS_SW_20200919_20201026, TS_SW_20200906_20200919,
                           DK_SW_20201026_20201218, DK_SW_20200919_20201026, DK_SW_20200906_20200919) %>% 
  rename("Date_Time_EST" = `X3`, "Temp_C" = `X5`, "DO_mg_L" = `X6`, "DO_Sat" = `X7`, "Q" = `X8`) %>%
  select(Date_Time_EST, Temp_C, DO_mg_L, DO_Sat, Site_Id, Q) 

rm(ND_SW_20201026_20201218, ND_SW_20200919_20201026, ND_SW_20200906_20200919,
   TS_SW_20201026_20201218, TS_SW_20200919_20201026, TS_SW_20200906_20200919,
   DK_SW_20201026_20201218, DK_SW_20200919_20201026, DK_SW_20200906_20200919)

PME_data_full_raw <- PME_data_full_raw %>% 
  mutate("Temp_C" = as.numeric(PME_data_full_raw$Temp_C),
         "Date_Time_EST" = ymd_hms(Date_Time_EST, tz = "EST"),
         "DO_mg_L" = as.numeric(PME_data_full_raw$`DO_mg_L`), 
         "DO_Sat" = as.numeric(PME_data_full_raw$`DO_Sat`),
         "Q" = as.numeric(PME_data_full_raw$Q))



# 4. Clean and QAQC Data --------------------------------------------------

 
#Some of the MiniDOTs were off by a minute. For example, they might measure at 12:46 instead of 12:45. 
#To simplify the data, I rounded to the nearest 15 minute interval. 
PME_data_full_raw <- PME_data_full_raw %>% 
  mutate("Date_Time_EST" = round_date(Date_Time_EST, "15 minute")) 

#Remove times when sensors were out of the water. 
PME_data_full_raw <- PME_data_full_raw %>% 
  filter(Date_Time_EST >= "2020-09-06 16:15:00") %>% 
  filter(Date_Time_EST <= "2020-09-19 12:00:00" | Date_Time_EST >= "2020-09-19 16:30:00") %>% 
  filter(Date_Time_EST <= "2020-10-09 13:45:00" | Date_Time_EST >= "2020-10-09 15:15:00") %>% 
  filter(Date_Time_EST <= "2020-10-26 12:15:00" | Date_Time_EST >= "2020-10-26 17:30:00") %>% 
  filter(Date_Time_EST <= "2020-11-14 14:00:00" | Date_Time_EST >= "2020-11-14 16:30:00") %>% 
  filter(Date_Time_EST <= "2020-11-14 14:00:00" | Date_Time_EST >= "2020-11-14 16:00:00") %>% 
  filter(Date_Time_EST <= "2020-12-18 10:00:00")
  



# 5. Plot DO mg/L for all Sites -----------------------------------------------------------------

DO_mg_L<- PME_data_full_raw %>% 
  select(Date_Time_EST, DO_mg_L, Site_Id)

# Had to use a row number to keep pivot_wider from tweeking out
DO_mg_L <- DO_mg_L %>% 
  group_by(Site_Id) %>% 
  mutate(row = row_number())

#Pivot wider for dygraphs packages
DO_mg_L_xts <- pivot_wider(data = DO_mg_L, 
                          names_from = Site_Id, 
                          values_from = DO_mg_L) %>% 
  select(-`row`)
  
#Convert to xts
DO_mg_L_xts <- DO_mg_L_xts %>% 
  xts(DO_mg_L_xts, order.by = DO_mg_L_xts$Date_Time_EST)

#Generate dygraph
(Fall2020_DOmgL_Timeseries <- dygraph(DO_mg_L_xts, main = "Jackson Lane DO (mg/L) from 9/6 - 12/18") %>% 
    dyOptions(drawPoints = TRUE, pointSize = 1, connectSeparatedPoints = TRUE) %>% 
    dyRangeSelector())

#Save the dygraph
saveWidget(Fall2020_DOmgL_Timeseries, "Fall2020_DO_Timeseries.html")

# 6. Look for diel patterns in DO Saturation -----------------------------------------------

PME_data_full_raw <- PME_data_full_raw %>% 
  mutate(Hour = as.character(str_sub(Date_Time_EST, 12, 13))) 


# Full data set
Full_Hourly_Means_DO_Sat <- PME_data_full_raw %>% 
  group_by(Hour, Site_Id) %>% 
  summarise("DO_Sat" = mean(DO_Sat)) %>% 
  add_column(Fall = "All Data (9/6 - 12/18)")


#Early Fall data set

Early_Hourly_Means_DO_Sat <- PME_data_full_raw %>% 
  filter(Date_Time_Round <= "2020-10-20 12:00") %>% 
  group_by(Hour, Site_Id) %>% 
  summarise(DO_Sat = mean(DO_Sat)) %>% 
  add_column(Fall = "Early (9/6-10/20)")

#Late Fall data set

Late_Hourly_Means_DO_Sat <- PME_data_full_raw %>% 
  filter(Date_Time_Round > "2020-10-20 12:00") %>% 
  group_by(Hour, Site_Id) %>% 
  summarise(DO_Sat = mean(DO_Sat)) %>% 
  add_column(Fall = "Late (10/20 - 12/18)")

#rbind the Late and Early Data

Hourly_Mean_Comp <- rbind(Late_Hourly_Means_DO_Sat, Early_Hourly_Means_DO_Sat, Full_Hourly_Means_DO_Sat)

Hourly_DO <- ggplot(data = Hourly_Mean_Comp, 
       aes(x = Hour,
           y = DO_Sat, 
           color = Site_Id)) +
  geom_point(size = 3) +
  theme_bw() +
  facet_wrap(facets = vars(Fall)) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  labs(title = "Fall 2020 DO (% Sat) at Jackson Lane",
       x = "Hour",
       y = "DO % Saturation")

ggsave(filename = "F2020 Diel DO at Jackson Lane.png", plot = Hourly_DO)
  



