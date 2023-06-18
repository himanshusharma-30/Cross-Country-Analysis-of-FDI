rm(list=ls())
library(rio)
library(moments)
library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lmtest)
library(plm)
library(car)
#importing the file
getwd()
setwd("C:/Users/Project")
d=import("FDI_Dataset.csv")
str(d)
#removing the index from the column
colnames(d) <- sub("^\\d+\\s*[A-Za-z]*\\s*", "", colnames(d))
#renaming the column BUPPSRY_UPPSRY_NTRY and TRY
new_names <- c("Labour force with primary education", "Labour force with secondary education", "Labour force with
tertiary education","FDI")
colnames(d)[colnames(d) %in% c("BUPPSRY", "UPPSRY_NTRY", "TRY","FDI(Value)")] <- new_names
colnames(d) <- gsub(" ", ".", colnames(d))
#removing negative values in FDI
d = subset(d, d$FDI >= 0)
#factorizing the variables
d$FDI=as.integer(d$FDI)
d$Countries=as.factor(d$Countries)
d$Year=as.factor(d$Year)
#checking null values by columns
colSums(is.na(d))
#performing log transform on FDI and checking skewness
d$log_FDI=log(d$FDI)
skewness(d$log_FDI)
#taking log of GDP
d$log_GDP=log(d$GDP)
#data visualizations
library(lattice)
#histogram of FDI
ggplot(data = d, aes(x = FDI)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Histogram of FDI", x = "FDI", y = "Frequency")
#histogram of Log of FDI
ggplot(data = d, aes(x = log_FDI)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Histogram of Log of FDI", x = "Log of FDI", y = "Frequency")
# Corrplot for predictor Variables
d6<-cor(d[,c(7,9,14,17,23,24,30,36,41,44,53,54,57,58,59,56,57,58,59,60,61)])
# Convert the correlation matrix to a data frame
df_corr <- as.data.frame(as.table(d6))
names(df_corr) <- c("Variable 1", "Variable 2", "Correlation")
# Create a ggplot object
ggplot(df_corr, aes(x = `Variable 1`, y = `Variable 2`, fill = `Correlation`, label = round(`Correlation`, 2))) +
  geom_tile(color = "white") +
  geom_text(size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1),
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Plot for Predictor Variables") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
#net fdi by country
net_fdi_country <- d %>%
  group_by(Countries) %>%
  summarize(net_fdi = sum(ifelse(Sign == "Positive", FDI , - FDI)))
# order the data by net investment
net_fdi_country <- net_fdi_country[order(-net_fdi_country$net_fdi), ]
ggplot(net_fdi_country, aes(x = reorder(Countries, net_fdi), y = net_fdi)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_fill_manual(values = c("#2C3E50", "#E74C3C", "#3498DB", "#F1C40F", "#27AE60")) +
  labs(x = "Country", y = "Total Inward FDI", title = "Total Inward FDI by Country") +
  theme_minimal() +
  coord_flip()
##----------------------------------------------Maps--------------------------------------------
##------------------------------------------------------------------------------------------------
#install.packages("maps")
#install.packages("mapdata")
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("sf")
#install.packages("countrycode")
#install.packages("patchwork")
library(maps)
library(mapdata)
library(mapdata)
library(ggplot2)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(patchwork)
# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
#Map: Size of govt
area1_country <- d %>%
  group_by(Countries) %>%
  summarize(sog = (sum(Size.of.Government)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(Countries, "country.name", "iso3c"))
world_area1 <- left_join(world, area1_country, by = c("iso_a3" = "iso_a3"))
ggplot() +
  geom_sf(data = world_area1, aes(fill = sog)) +
  scale_fill_gradientn(colors = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"),
                       na.value = "transparent", name = "Size of Governement") +
  theme_void() +
  ggtitle("Size of Govt Indicator by Country") +
  theme(legend.position = "right", plot.title = element_text(size = 15, face =
                                                               "bold"))
#Map: Legal rights
area2_country <- d %>%
  group_by(Countries) %>%
  summarize(lspr = (sum(Legal.System...Property.Rights)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(Countries, "country.name", "iso3c"))
world_area2 <- left_join(world, area2_country, by = c("iso_a3" = "iso_a3"))
ggplot() +
  geom_sf(data = world_area2, aes(fill = lspr)) +
  scale_fill_gradientn(colors = c("#f1eef6", "#d7b5d8", "#df65b0", "#dd1c77", "#980043"), na.value = "transparent", name =
                         "Legal Rights") +
  theme_void() +
  ggtitle("Legal Rights Indicator by Country") +
  theme(legend.position = "right", plot.title = element_text(size = 15, face =
                                                               "bold"))
#Map: Sound money
area3_country <- d %>%
  group_by(Countries) %>%
  summarize(sm = (sum(Sound.Money)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(Countries, "country.name", "iso3c"))
world_area3 <- left_join(world, area3_country, by = c("iso_a3" = "iso_a3"))
ggplot() +
  geom_sf(data = world_area3, aes(fill = sm)) +
  scale_fill_gradientn(colors = c("#f7fcf5", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081"),
                       na.value = "transparent", name = "Sound Money") +
  theme_void() +
  ggtitle("Sound Money Indicator by Country") +
  theme(legend.position = "right", plot.title = element_text(size = 15, face =
                                                               "bold"))
#Map: Freedom to trade
area4_country <- d %>%
  group_by(Countries) %>%
  summarize(free = (sum(Freedom.to.trade.internationally)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(Countries, "country.name", "iso3c"))
world_area4 <- left_join(world, area4_country, by = c("iso_a3" = "iso_a3"))

ggplot() +
  geom_sf(data = world_area4, aes(fill = free)) +
  scale_fill_gradientn(colors = c("#fce4f3", "#f8bbd0", "#f48fb1", "#f06292", "#e91e63", "#9c27b0", "#6a1b9a"), na.value =
                         "transparent", name = "Freedom to Trade") +
  theme_void() +
  ggtitle("Freedom to Trade International Indicator by Country") +
  theme(legend.position = "right", plot.title = element_text(size = 15, face =
                                                               "bold"))
#Map: Regulation
area5_country <- d %>%
  group_by(Countries) %>%
  summarize(reg = (sum(Regulation)/15)) %>%
  mutate(iso_a3 = countrycode::countrycode(Countries, "country.name", "iso3c"))
world_area5 <- left_join(world, area5_country, by = c("iso_a3" = "iso_a3"))
ggplot() +
  geom_sf(data = world_area5, aes(fill = reg)) +
  scale_fill_gradientn(colors = c("#e5f5e0", "#a1d99b", "#31a354", "#3182bd", "#08519c"), na.value = "transparent", name =
                         "Regulation") +
  theme_void() +
  ggtitle("Regulation Indicator by Country") +
  theme(legend.position = "right", plot.title = element_text(size = 15, face =
                                                               "bold"))
#Labor force by education
#install.packages("cowplot")
library(cowplot)
# Calculate means for each country and year
df_mean <- aggregate(d[,c("Labour.force.primary.education",
                          "Labour.force.secondary.education",
                          "Labour.force.tertiary.education")],
                     by = list(country = d$Countries, year = d$Year ),
                     FUN = mean)
# Create boxplots
p1 <- ggplot(df_mean, aes(x = country, y = Labour.force.primary.education, fill = country)) +
  geom_boxplot() +
  ggtitle("Avg Labour force with primary education") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1
p2 <- ggplot(df_mean, aes(x = country, y = Labour.force.secondary.education, fill = country)) +
  geom_boxplot() +
  ggtitle("Avg Labour force with secondary education") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p2
p3 <- ggplot(df_mean, aes(x = country, y = Labour.force.tertiary.education, fill = country)) +
  geom_boxplot() +
  ggtitle("Avg Labour force with tertiary education") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p3
# Combine plots using cowplot
plot_grid(p1, p2, p3, ncol = 3)
#------------------------------------------Models-----------------------------------
#models
#lmer models
library(lme4)
model_FDI= lmer(d$log_FDI~ d$log_GDP +d$Government.investment+ d$Top.marginal.income.tax.rate
                +d$Legal.enforcement.of.contracts+d$Protection.of.property.rights
                +d$Freedom.to.own.foreign.currency.bank.accounts+d$`Inflation:.Most.recent.year`
                +d$Capital.controls +d$Hiring.regulations.and.minimum.wage
                +d$Private.sector.credit+d$Regulatory.trade.barriers
                +d$Startinga..business+d$Impartial.Public.Administration
                +d$Labour.force.with.primary.education
                +d$Labour.force.with.secondary.education+d$Labour.force.with.tertiary.education
                +(1|d$Countries)+(1|d$Year),REML=FALSE
)
summary(model_FDI)
#checking multicollinearity
vif(model_FDI)
ranef(model_FDI)
#PLM models
dp <- pdata.frame(d, index=c("Countries", "Year"))
colSums(is.na(dp))
# plm random model with Economic Index and Labour Force
plm_FDI <- plm(dp$log_FDI ~ dp$log_GDP+dp$Government.investment+dp$Top.marginal.tax.rate
               +dp$Legal.enforcement.of.contracts+dp$Protection.of.property.rights
               +dp$Freedom.to.own.foreign.currency.bank.accounts+dp$Inflation..Most.recent.year
               +dp$Capital.controls+dp$Hiring.regulations.and.minimum.wage
               +dp$Private.sector.credit+dp$Regulatory.trade.barriers
               +dp$Startinga..business+dp$Impartial.Public.Administration
               +dp$Labour.force.with.primary.education+dp$Labour.force.with.secondary.education
               +dp$Labour.force.with.tertiary.education, index= c("Countries","Year"),data=dp,model="random"
)
summary(plm_FDI)
#checking multicollinearity
vif(plm_FDI)
plot(model_FDI)
# plm fixed effect model with Economic Index and Labour Force
plm_fe_FDI <- plm(dp$log_FDI ~ dp$log_GDP+dp$Government.investment+dp$Top.marginal.tax.rate
                  +dp$Legal.enforcement.of.contracts+dp$Protection.of.property.rights
                  +dp$Freedom.to.own.foreign.currency.bank.accounts+dp$Inflation..Most.recent.year
                  +dp$Capital.controls+dp$Hiring.regulations.and.minimum.wage
                  +dp$Private.sector.credit+dp$Regulatory.trade.barriers
                  +dp$Startinga..business+dp$Impartial.Public.Administration
                  +dp$Labour.force.with.primary.education+dp$Labour.force.with.secondary.education
                  +dp$Labour.force.with.tertiary.education, index= c("Countries","Year"),data=dp,model="within"
)
summary(plm_fe_FDI)
library(stargazer)
stargazer (model_FDI, plm_FDI, plm_fe_FDI, align=TRUE, no.space=TRUE, type = "text")