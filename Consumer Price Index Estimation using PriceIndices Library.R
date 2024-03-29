########### Consumer Price Index Calculation using sample data from PriceIndices Library ############

########### Import Sugar Data set from PriceIndices Package###################
library(PriceIndices)
data("sugar")
unique(sugar$description)
head(sugar)
tail(sugar)

################## data_selecting ##################
subgroup1<-data_selecting(sugar, include=c("sugar"), must=c("cane"))
head(subgroup1)
unique(subgroup1$description)

############# Data Filtering ##############
######reject 1% of the lowest and 1% of the highest price changes
filter1<-data_filtering(sugar,start="2017-12-01",end="2020-11-01",
                        filters=c("extremeprices"),pquantiles=c(0.01,0.99))

#########to reject products with the price ratio being less than 0.5 or bigger than 2 ####
filter2<-data_filtering(sugar,start="2017-12-01",end="2020-11-01",
                        filters=c("extremeprices"),plimits=c(0.5,2))
nrow(filter1)
nrow(filter2)
##########  prices of sold products #####################
prices(sugar, period="2017-12")
prices(sugar, period="2018-12")
prices(sugar, period="2019-12")
prices(sugar, period="2020-9")

################ Histogram ###############3
ctg<-unique(sugar$description)
categories<-c(ctg[1],ctg[2],ctg[3])
sugar1<-dplyr::filter(sugar, sugar$description==categories[1])
sugar2<-dplyr::filter(sugar, sugar$description==categories[2])
sugar3<-dplyr::filter(sugar, sugar$description==categories[3])
sales_groups(datasets=list(sugar1,sugar2,sugar3),start="2018-12", end="2020-07")
sales_groups(datasets=list(sugar1,sugar2,sugar3),start="2018-12", end="2020-07", shares=TRUE)

sales_groups(datasets=list(sugar1,sugar2,sugar3),start="2018-12", end="2020-07", 
             barplot=TRUE, shares=TRUE, names=categories)

############### Pearson's correlation coefficient ###############
pqcor(sugar, period="2019-05")
pqcor(sugar, period="2019-05",figure=TRUE)

################# dissimilarity #####################

dissimilarity(sugar, period1="2017-12",period2="2020-11",type="pq")

dissimilarity_fig(sugar, start="2017-12",end="2020-11",type="pq",benchmark="start")

######################### bilateral unweighted price index calculation ##################
jevons(sugar, start="2017-12", end="2020-11")
index_bilateral_unweighted  <- data.frame(jevons(sugar, start="2017-12", end="2020-11", interval=TRUE))
#### Change Column Name ##########
colnames(index_bilateral_unweighted)[1] ="Unweighted_Price_Index"
head(index_bilateral_unweighted)

###### Create Data Frame for Unweighted Price Index ############
start_date <- as.Date("2017/12/01")
end_date <- as.Date("2020/11/30")
range <- seq(start_date, end_date,"months")
date <- data.frame(range)
colnames(date)[1] ="Date"
Unweighted_Price_Index <- cbind(date,index_bilateral_unweighted)
head(Unweighted_Price_Index)
#######
fisher(sugar, start="2017-12", end="2020-11")
lloyd_moulton(sugar, start="2017-12", end="2020-11", sigma=0.9)
unweighted_price_index_lowe<-data.frame(lowe(sugar, start="2018-12", end="2020-11", base="2017-12", interval=TRUE))
colnames(unweighted_price_index_lowe)[1] ="Lowe_Index"
head(unweighted_price_index_lowe)

########## Create New Data Frame with the Index #########
start_date_lowe <- as.Date("2018/12/01")
end_date_lowe <- as.Date("2020/11/30")
range_lowe <- seq(start_date_lowe, end_date_lowe,"months")
Date_lowe <- data.frame(range_lowe)
colnames(Date_lowe)[1] ="Date"
lowe_Price_Index <- cbind(Date_lowe,unweighted_price_index_lowe)
head(lowe_Price_Index)

############### chain price index calculation ########################

chain_price <- data.frame(chfisher(sugar, start="2017-12", end="2020-11", interval=TRUE))
colnames(chain_price)[1] ="Chain Price Index"
chain_Price_Index <- cbind(date,chain_price)
head(chain_Price_Index)

############### quality adjusted unit value index ##################
prodID<-base::unique(sugar$prodID)
values<-stats::runif(length(prodID),1,2)
v<-data.frame(prodID,values)
head(v)
QU(sugar, start="2017-12", end="2020-11", v)

################# General functions for price index calculations ################

price_index(sugar, start="2018-01", end="2020-11", formula="fisher")
price_index_General  <- data.frame(price_index(sugar, start="2018-01", end="2020-10",
                                               formula="tpd_splice",splice="movement",interval=TRUE))
colnames(price_index_General)[2] ="Generalized Price Index"
head(price_index_General)

###### Different Methods of Price Index Estimation in One specific Pipeline function #############
price_index_All <- data.frame(price_indices(sugar, start="2019-12", end="2020-08", bilateral=c("fisher"), 
                                            bindex=c("young"), base=c("2018-12"), 
                                            cesindex=c("agmean"), sigma=c(0.5), 
                                            fbmulti=c("geks", "gk"), fbwindow=c(9,9), 
                                            splicemulti=c("tpd_splice"),splicewindow=c(6), 
                                            splice=c("movement"), interval=TRUE))
head(price_index_All)


####################### Final Index ###################

g1<-dplyr::filter(sugar, sugar$description=="white sugar")
g2<-dplyr::filter(sugar, sugar$description=="powdered sugar")


Final_index_sugar <- data.frame(final_index(datasets=list(g1,g2), start="2017-12", end="2020-11", 
                       formula="chwalsh", 
                       aggrsets = "laspeyres", aggrret = "fisher", 
                       interval=TRUE))
head(Final_index_sugar)


















