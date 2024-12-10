### Lake Evans Report###

library(here)
library(tidyverse)
library(viridis)
library(viridisLite)

############
### Data ###
############

LEvans.outflow <- read.csv(here("data", "LEvansUSGS.csv"))

LEvans.outflow <- LEvans.outflow %>% select(Location_Identifier, Location_Name, Location_Type, Location_Latitude, Location_Longitude, Activity_StartDate, Activity_StartTime, Result_Characteristic, Result_CharacteristicUserSupplied, Result_Measure, Result_MeasureUnit)

LEvans.outflow <- LEvans.outflow %>% pivot_wider(names_from = c(Result_CharacteristicUserSupplied, Result_MeasureUnit), values_from = Result_Measure)


# rename chems
names(LEvans.outflow) <- c("USGSSite", "Location", "Type", "lat", "lon", "Date", "Time", 
                "analytecode", "Bromide_mgL", "Fluoride_mgL", "agency", "Sulfate_mgL", "pH", "Chloride_mgL", "SPC", "NH4_DON_mgNL", "NO3_NO2_mgNL", "Hion_mgL", "NO3_mgNL", "NO2_mgNL", "NH4_mgNL", "NO2_mgNL2", "PO4_mgL", "DON_mgL", "SPC2", "dD_pmil", "d18O_pmil", "PO4_mgL2", "NO3_mgNL2", "DOC_mgL", "TDN_mgL", "DO_mgL", "Tair_C", "Twater_C", "NH4_mgNL3", "DO_pctsat", "baro_mmHG")

# Format concentrations as numeric & summarize to single row
LEvans.outflowsum <- LEvans.outflow %>% mutate(across(Bromide_mgL:baro_mmHG, as.numeric)) %>%
  summarize(across(where(is.numeric), list(mn = mean, SD = sd), na.rm = TRUE))

summarize(across(where(is.numeric),
                 list(mn = mean, SD = sd), na.rm = TRUE))
#Data Summary stuff from 02_datasummary Rscript

LEvans.chem <- read.csv(here("data", "ENSC140_lakechems.csv"))

LEvans.chemsum <- LEvans.chem %>% select(-rep) %>% group_by(Sample) %>%
  summarize(across(where(is.numeric),
                   list(mn = mean, SD = sd), na.rm = TRUE))

LEvans.chemsum <- LEvans.chemsum %>% mutate(Sample = ifelse(Sample == "S", 0, Sample)) %>%
  mutate(Sample = as.numeric(Sample))
#Data Plots for Lake Evans Chemical Analysis Results

NH4.pl <- LEvans.chemsum %>% ggplot(aes(x = NH4_mgNL_mn, y = Sample)) +
  geom_pointrange(aes(xmin = NH4_mgNL_mn - NH4_mgNL_SD,  xmax = NH4_mgNL_mn + NH4_mgNL_SD)) +
  geom_line() +
  scale_y_reverse()
print(NH4.pl+labs(y= "Depth in meters", x = "NH4 (mgN/L)")+ggtitle("Ammonia Concentration at Varying Depths "))

NO3NO2.pl <- LEvans.chemsum %>% ggplot(aes(x = NO3_NO2_mgNL_mn, y = Sample)) +
  geom_pointrange(aes(xmin = NO3_NO2_mgNL_mn - NO3_NO2_mgNL_SD,  xmax = NO3_NO2_mgNL_mn + NO3_NO2_mgNL_SD)) +
  geom_line() +
  scale_y_reverse()
print(NO3NO2.pl+labs(y= "Depth in meters", x = "NO3 and NO2 (mgN/L)")+ggtitle("Nitrate Concentration at Varying Depths "))

DOC.pl <- LEvans.chemsum %>% ggplot(aes(x = DOC_mgL_mn, y = Sample)) +
  geom_pointrange(aes(xmin = DOC_mgL_mn - DOC_mgL_SD,  xmax = DOC_mgL_mn + DOC_mgL_SD)) +
  geom_line() +
  scale_y_reverse()
print(DOC.pl+labs(y= "Depth in meters", x = "DOC (mg/L)")+ggtitle("Dissolved Organic Carbon Concentration at Varying Depths "))

TDN.pl <- LEvans.chemsum %>% ggplot(aes(x = TDN_mgL_mn, y = Sample)) +
  geom_pointrange(aes(xmin = TDN_mgL_mn - TDN_mgL_SD,  xmax = TDN_mgL_mn + TDN_mgL_SD)) +
  geom_line(orientation = "y") +
  scale_y_reverse()
print(TDN.pl+labs(y= "Depth in meters", x = "TDN (mg/L)")+ggtitle("Total Dissolved Nitrogen Concentration at Varying Depths "))
