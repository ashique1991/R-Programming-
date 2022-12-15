########### Price Index Estimation ###########

######### Economic Data From Federal Reserve Bank of SL LUOIS ###########



######### Data Source Federal Reserve Bank of St. Louis USA ###################

######## Import the Package ############
library(fredr)
library(usethis)

#### Set Fred API Key #######
fredr_set_key("2d7a6dbebdb8b8136c0027fcc63ef735")

### Import Economic Data from Fred server ########

Plastic_Packaging_Products <-fredr(
  series_id = "WPU072A0101",
  observation_start = as.Date("2007-01-01"),
  observation_end = as.Date("2022-10-01")
)

Penuts_Price <- fredr(
  series_id = "WPU01830111",
  observation_start = as.Date("2007-01-01"),
  observation_end = as.Date("2022-10-01")
)
domestic_freight <- fredr(
  series_id = "PCU4841224841221",
  observation_start = as.Date("2007-01-01"),
  observation_end = as.Date("2022-10-01")
)

##### Check Properties of the Data set ######
head(Plastic_Packaging_Products )
is.na(Plastic_Packaging_Products$value)


####### Data Cleaning #####
#### Plastic_Packaging ##########
Plastic_Packaging <- data.frame(Plastic_Packaging_Products$date,(Plastic_Packaging_Products$value*30)/100)
colnames(Plastic_Packaging)[1] = "time"
colnames(Plastic_Packaging)[2] = "prices"
df1 <- data.frame(Plastic_Packaging,
                  prodID = paste0(1))
head(df1)
####Penuts Cost######
Penuts <- data.frame(Penuts_Price$date,(Penuts_Price$value*50)/100)
colnames(Penuts)[1] = "time"
colnames(Penuts)[2] = "prices"
df2 <- data.frame(Penuts,
                  prodID = paste0(2))
head(df2)
#### Freight Cost #######
freight <- data.frame(domestic_freight$date, (domestic_freight$value*20)/100)
colnames(freight)[1] = "time"
colnames(freight)[2] = "prices"
df3 <- data.frame(freight,
                  prodID = paste0(3))
head(df3)

#### Production Cost ######
All_cost <- data.frame(cbind((Plastic_Packaging_Products$value*30)/100,(Penuts_Price$value*50)/100,(domestic_freight$value*20)/100))
colnames(All_cost)[1] = "Packaging_Cost"
colnames(All_cost)[2] = "Penuts_Cost"
colnames(All_cost)[3] = "freight_Cost"
library(dplyr)
Production_cost <- All_cost %>%
  mutate(sum = rowSums(across(c(Packaging_Cost, Penuts_Cost,freight_Cost))))
head(Production_cost)

### Gross Profit Calculation ##########
profit <- data.frame(freight$time,(Production_cost$sum*40/100))
colnames(profit)[1] = "time"
colnames(profit)[2] = "prices"
df4 <- data.frame(profit,
                  prodID = paste0(4))
head(df4)
############# Combining the 4 data frame #########
Combined_data <- rbind(df1,df2,df3,df4)
head(Combined_data)
tail(Combined_data)
quantity = list(sample(150 : 340, size = 760, replace = T))
head(quantity)
Procucers_production_cost <- cbind(Combined_data,quantity)
colnames(Procucers_production_cost)[4] = "quantities"
head(Procucers_production_cost)
tail(Procucers_production_cost)
############ Index Calculation using Jevons method ###########
library(PriceIndices)
head(data_preparing(Procucers_production_cost, time="time",prices="prices",quantities="quantities"))
jevons(Procucers_production_cost, start="2007-02", end="2022-10")
jevons <- data.frame(jevons(Procucers_production_cost, start="2007-02", end="2022-10", interval=TRUE))

colnames(jevons)[1] = "bilateral unweighted price index"
date <- data.frame(Plastic_Packaging_Products$date)
Date <- data.frame(date[-190,])
colnames(Date)[1] = "time"
Jevons_Price_index <- cbind(Date,jevons)
head(Jevons_Price_index)

############ Index Calculation using lowe method #########
Lowe <-data.frame(lowe(Procucers_production_cost, start="2007-10", end="2022-10", base="2007-02", interval=TRUE))
colnames(Lowe)[1] = "bilateral unweighted price index (Lowe)"
Date_1 <- data.frame(date[-c(1,2,3,4,5,6,7,8,9),])
colnames(Date_1)[1] = "time"
Lowe_Price_index <- cbind(Date_1,Lowe)
head(Lowe_Price_index)




