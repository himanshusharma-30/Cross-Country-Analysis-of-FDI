rm(list=ls())

#reloading the existing libraries
library("rio")
library("moments")
library(dplyr)
library(ggplot2)
library("corrplot")
library(car)
library(tidyr)
library(GGally)
library(MASS)
library(lmtest)
library(PerformanceAnalytics)
library(pscl)
library(AER)
library(survival)
library(stargazer)
library(ggplot2)

setwd("C:/Users/himan/Downloads")
df<-read.csv("SDM_Project_FDIDataset.csv")
colnames(df)=tolower(make.names(colnames(df)))
df$fdi.value.<-as.numeric(df$fdi.value.)

#Checking NA values 
colSums(is.na(df)) 

df <- subset(df, countries != "Japan")

##------------------------------------------Corplot---------------------------------------

m1 <- mean(df$free_t_m, na.rm = TRUE)
df[is.na(df$free_t_m), "free_t_m"] <- m1



m2 <- mean(df$free_t_sd, na.rm = TRUE)
df[is.na(df$free_t_sd), "free_t_sd"] <- m2



m3 <- mean(df$free_f, na.rm = TRUE)
df[is.na(df$free_f), "free_f"] <- m3



m4 <- mean(df$reg_l_h, na.rm = TRUE)
df[is.na(df$reg_l_h), "reg_l_h"] <- m4



m4 <- mean(df$reg_l_h.1, na.rm = TRUE)
df[is.na(df$reg_l_h.1), "reg_l_h.1"] <- m4



#which(! complete.cases(df['reg_l_h']))
#[,'reg_l_h']


# Create a ggplot object area 1
d<-cor(df[,c(5:10)])
library(corrplot)
# Convert the correlation matrix to a data frame
df_corr <- as.data.frame(as.table(d))
names(df_corr) <- c("Variable 1", "Variable 2", "Correlation")

ggplot(df_corr, aes(x = `Variable 1`, y = `Variable 2`, fill = `Correlation`, label = round(`Correlation`, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Plot for Area 1  ") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


# Create a ggplot object area 2 
d1<-cor(df[,c(12:19)])
# Convert the correlation matrix to a data frame
df_corr <- as.data.frame(as.table(d1))
names(df_corr) <- c("Variable 1", "Variable 2", "Correlation")

ggplot(df_corr, aes(x = `Variable 1`, y = `Variable 2`, fill = `Correlation`, label = round(`Correlation`, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Plot for Area 2 ") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))


# Create a ggplot object area 4
d2<-cor(df[,c(21:24)])
# Convert the correlation matrix to a data frame
df_corr <- as.data.frame(as.table(d2))
names(df_corr) <- c("Variable 1", "Variable 2", "Correlation")

ggplot(df_corr, aes(x = `Variable 1`, y = `Variable 2`, fill = `Correlation`, label = round(`Correlation`, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Plot for Area 3 ") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# Create a ggplot object area 4
d3<-cor(df[,c(26:37)])
# Convert the correlation matrix to a data frame
df_corr <- as.data.frame(as.table(d3))
names(df_corr) <- c("Variable 1", "Variable 2", "Correlation")

ggplot(df_corr, aes(x = `Variable 1`, y = `Variable 2`, fill = `Correlation`, label = round(`Correlation`, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Plot for Area 4 ") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# Create a ggplot object area 5 
# Calculate correlation matrix
d4 <- cor(df[, c(39:56)])

# Convert the correlation matrix to a data frame
df_corr <- as.data.frame(as.table(d4))
names(df_corr) <- c("Variable 1", "Variable 2", "Correlation")

ggplot(df_corr, aes(x = `Variable 1`, y = `Variable 2`, fill = `Correlation`, label = round(`Correlation`, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Plot for Area 5 ") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

which(! complete.cases(df)) #return the number of non complete cases 

# Create a ggplot object labour force by education
# Calculate correlation matrix 
d5 <- cor(df[, c(57:60)])

# Convert the correlation matrix to a data frame
df_corr <- as.data.frame(as.table(d5))
names(df_corr) <- c("Variable 1", "Variable 2", "Correlation")

# Create a ggplot object
ggplot(df_corr, aes(x = `Variable 1`, y = `Variable 2`, fill = `Correlation`, label = round(`Correlation`, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Plot for]Labour Force by Education ") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

##--------------------------------Data Division-----------------------------------------

# Subset the data frame to only include negative FDI values
df_negative <- df[df$sign == "Negative",]

# Subset the data frame to only include non-negative FDI values
df_nonnegative <- df[df$sign == "Positive",]

##--------------------------------------------Histogram and Bar Graph--------------------------------------------
##---------------------------------------------------------------------------------------------------------------
#histogram of target: FDI positive
#histogram of target variable 
FDI.p<-ggplot(df_nonnegative, aes(x= log(fdi.value.) )) + 
  geom_histogram(aes(y= ..density..), colour= "white" , fill= "steelblue" )+
  geom_density(lwd = 0.6 ,linetype = 1 ,colour = 10 )  +  labs(title="Plot of Density Count  vs FDI ",
                                                               x ="FDI", y = "Density")
FDI.p

#histogram of target: FDI negative
#histogram of target variable 
FDI.n<-ggplot(df_negative, aes(x= log(fdi.value.) )) + 
  geom_histogram(aes(y= ..density..), colour= "white" , fill= "steelblue")+
  geom_density(lwd = 0.6 ,linetype = 1 ,colour = 10 )  +  labs(title="Plot of Density Count  vs Inward FDI ",
                                                               x ="Inward FDI", y = "Density")
FDI.n

#net fdi by year 
net_fdi <- df %>%
  group_by(year) %>%
  summarize(net_fdi = sum(ifelse(sign == "Positive", fdi.value., -fdi.value.)))


ggplot(net_fdi, aes(x = year, y = net_fdi, group = 1)) +
  geom_col() + geom_bar(stat = "identity", fill = "steelblue")+
  scale_fill_viridis_d() +
  labs(x = "Year", y = "Inward FDI", title = "Inward FDI by Year and Country")

#net fdi by country
net_fdi_country <- df %>%
  group_by(countries) %>%
  summarize(net_fdi = sum(ifelse(sign == "Positive", fdi.value., -fdi.value.)))

# order the data by net investment
net_fdi_country <- net_fdi_country[order(-net_fdi_country$net_fdi), ]


ggplot(net_fdi_country, aes(x = reorder(countries, net_fdi), y = net_fdi)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_fill_manual(values = c("#2C3E50", "#E74C3C", "#3498DB", "#F1C40F", "#27AE60")) +
  labs(x = "Country", y = "Total Inward FDI", title = "Total Inward FDI by Country") +
  theme_minimal() +
  coord_flip()

##----------------------------------------------Maps--------------------------------------------
##------------------------------------------------------------------------------------------------
library(maps)
library(mapdata)  
library(mapdata)
library(ggplot2)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Map of Net FDI by Country
# Merge net FDI data by country name
net_fdi_country <- df %>%
  group_by(countries) %>%
  summarize(net_fdi = sum(ifelse(sign == "Positive", fdi.value., -fdi.value.))) %>%
  mutate(iso_a3 = countrycode::countrycode(countries, "country.name", "iso3c"))

world_fdi <- left_join(world, net_fdi_country, by = c("iso_a3" = "iso_a3"))

# Plot map with net FDI highlighted
ggplot() +
  geom_sf(data = world_fdi, aes(fill = net_fdi)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_void() +
  ggtitle("Total Inward FDI by Country")

area1_country <- df %>%
  group_by(countries) %>%
  summarize(sog = (sum(sog)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(countries, "country.name", "iso3c"))

world_area1 <- left_join(world, area1_country, by = c("iso_a3" = "iso_a3"))

# Plot map with net SOG highlighted
ggplot() +
  geom_sf(data = world_area1, aes(fill = sog)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_void() +
  ggtitle("SOG Indicator by Country")

area2_country <- df %>%
  group_by(countries) %>%
  summarize(lspr = (sum(lspr)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(countries, "country.name", "iso3c"))

world_area2 <- left_join(world, area2_country, by = c("iso_a3" = "iso_a3"))

# Plot map with net Legal rights highlighted
ggplot() +
  geom_sf(data = world_area2, aes(fill = lspr)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_void() +
  ggtitle("Legal Rights Indicator by Country")

area3_country <- df %>%
  group_by(countries) %>%
  summarize(sm = (sum(sm)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(countries, "country.name", "iso3c"))

world_area3 <- left_join(world, area3_country, by = c("iso_a3" = "iso_a3"))

# Plot map with net Sound Money highlighted
ggplot() +
  geom_sf(data = world_area3, aes(fill = sm)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_void() +
  ggtitle("Sound Money Indicator by Country")

area4_country <- df %>%
  group_by(countries) %>%
  summarize(free = (sum(free)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(countries, "country.name", "iso3c"))

world_area4 <- left_join(world, area4_country, by = c("iso_a3" = "iso_a3"))

# Plot map with net Freedom of Trade highlighted
ggplot() +
  geom_sf(data = world_area4, aes(fill = free)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_void() +
  ggtitle("Freedom of Trade Indicator by Country")

area5_country <- df %>%
  group_by(countries) %>%
  summarize(reg = (sum(reg)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(countries, "country.name", "iso3c"))

world_area5 <- left_join(world, area5_country, by = c("iso_a3" = "iso_a3"))

# Plot map with net Regulation highlighted
ggplot() +
  geom_sf(data = world_area5, aes(fill = reg)) +
  scale_fill_viridis_c(option = "viridis") +
  theme_void() +
  ggtitle("Regulation indicator by Country")

##--------------------------------------Models-------------------------------------------------
##-----------------------------------------------------------------------------------------------

df_nonnegative <- df_nonnegative %>%
  mutate(iso_a3 = countrycode::countrycode(countries, "country.name", "iso3c"))

world_select <- world[, c("iso_a3", "economy", "income_grp")]
dp <- merge(world_select, df_nonnegative, by = c("iso_a3" = "iso_a3"))
dp$economy <- factor(dp$economy)
dp$economy <- relevel(dp$economy, ref = "1. Developed region: G7")
dp$log.fdi.value <- log(dp$fdi.value.)
df_nn <- pdata.frame(dp, index=c("countries", "year"))


#PLM models for positive values 
library(plm)

mpanel_p <- plm(df_nn$log.fdi.value ~ df_nn$sog_gi + df_nn$sog_tmitr + df_nn$lspr_ji + df_nn$lspr_ppr + df_nn$sm_f + df_nn$free_t_m +
            df_nn$free_f + df_nn$reg_c_o + df_nn$reg_b_s + df_nn$reg_b_i + df_nn$free_r + df_nn$reg_l_h + df_nn$buppsry+
            df_nn$try + df_nn$uppsry_ntry + economy,
          data = df_nn, model ="random")
summary(mpanel_p )

stargazer(mpanel_p, mpanel_pe, mpanel_pf, type='text', single.row=TRUE)


df_negative <- df_negative %>%
  mutate(iso_a3 = countrycode::countrycode(countries, "country.name", "iso3c"))
dn <- merge(world_select, df_nonnegative, by = c("iso_a3" = "iso_a3"))
dn$economy <- factor(dn$economy)
dn$economy <- relevel(dn$economy, ref = "1. Developed region: G7")
dn$log.fdi.value <- log(dn$fdi.value.)
df_n <- pdata.frame(dn, index=c("countries", "year"))


#PLM model for negative values 

mpanel_n <- plm(df_n$log.fdi.value ~ df_n$sog_gi + df_n$sog_tmitr + df_n$lspr_ji + df_n$lspr_ppr + df_n$sm_f + df_n$free_t_m +
                  df_n$free_f + df_n$reg_c_o + df_n$reg_b_s + df_n$reg_b_i + df_n$free_r + df_nn$reg_l_h + df_n$buppsry+
                  df_n$try + df_n$uppsry_ntry +economy,
                data = df_n, model ="random")
summary(mpanel_n)

#seperate models 
mpanel_pe <- plm(df_nn$log.fdi.value ~ df_nn$sog_gi + df_nn$sog_tmitr + df_nn$lspr_ji + df_nn$lspr_ppr + df_nn$sm_f + df_nn$free_t_m +
                  df_nn$free_f + df_nn$reg_c_o + df_nn$reg_b_s + df_nn$reg_b_i + df_nn$free_r + df_nn$reg_l_h + economy,
                data = df_nn, model ="random")
summary(mpanel_pe )

mpanel_pf <- plm(df_nn$log.fdi.value ~ df_n$buppsry+df_n$try + df_n$uppsry_ntry+ economy,
                 data = df_nn, model ="random")
summary(mpanel_pf )

#PLM Models within 
mpanel_pw <- plm(df_nn$log.fdi.value ~ df_nn$sog_gi + df_nn$sog_tmitr + df_nn$lspr_ji + df_nn$lspr_ppr + df_nn$sm_f + df_nn$free_t_m +
                  df_nn$free_f + df_nn$reg_c_o + df_nn$reg_b_s + df_nn$reg_b_i + df_nn$free_r + df_nn$reg_l_h + df_nn$buppsry+
                  df_nn$try + df_nn$uppsry_ntry + economy,
                data = df_nn, model ="within")
summary(mpanel_pw )

stargazer( mpanel_pe, mpanel_pf, mpanel_p,type='text', single.row=TRUE)

