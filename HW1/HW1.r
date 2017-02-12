
library(readr)
sales <- read_csv("sales.csv")

sales <- as.data.frame(sales)
head(sales)

library(plyr)
# group the original dataset usint multiple column name, here we use state and quarter
dataSummayByStates = ddply(sales,.(state, quarter),numcolwise(sum))
keeps <- c("state","quarter", "amount")
dataSummayByStates <- dataSummayByStates[keeps]
dataSummayByStates

library(ggplot2)
ggplot(dataSummayByStates, aes(factor(state), amount/1000000, fill = quarter)) + 
       geom_bar(stat="identity", position = "dodge") +
       scale_fill_brewer(palette = "Set1") +
       xlab("States") +
       ylab("Sales(millions)")+
       ggtitle("Sales by states")
     

dataSummary = ddply(sales,.(month),numcolwise(sum))
dataSummary

qqnorm(dataSummary$amount,main = "Q-Q Plot, amount")
qqline(dataSummary$amount)

qqnorm(log(dataSummary$amount),main = "Q-Q Plot, log(amount)")
qqline(log(dataSummary$amount))

heights <- tapply(sales$amount, sales$mo, mean)
# add Confidence interval to data
lower <-tapply(sales$amount, sales$mo, function(v) t.test(v)$conf.int[1])
upper <-tapply(sales$amount, sales$mo, function(v) t.test(v)$conf.int[2])

library(gplots)
barplot2(heights,
        plot.ci = TRUE, ci.l = lower, ci.u = upper, xpd = FALSE,
        main = "Mean sales amount by Month", 
        names.arg = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        col=c("grey"),
        ylab = "Sales Amount")
        

head(sales)

keep <- c("year","month", "state", "product",  "amount")
sales_revenue <- sales[keep]
sales_revenue <- as.data.frame(sales_revenue)
head(sales_revenue)

revenue_cube <- 
    tapply(sales_revenue$amount, 
           sales_revenue[,c("year", "month", "state", "product"),], 
           FUN=function(x){return(sum(x))})
dimnames(revenue_cube)


#Slice operation: compute the revenue for Laptop during January of 2013 in each state
revenue_cube['2013','Jan',,'Laptop']

#Dice operation: compute the revenue for the furniture products (Mattress and Chair) during the second quarter 
# (April, May and June) of 2014 in each state.
# the following line could be correctly displayed on RStudio as follows, but not on Jupyter notebook
revenue_cube['2014',c('Apr','May','Jun'),,c('Chair','Mattress')]
, , product = Chair

     state
month California New York Ontario Quebec Washington
  Apr      56160    56040   21960  25920      29400
  May      62640    64320   25200  39480      36960
  Jun      57960    43560   15480  18480      12720

, , product = Mattress

     state
month California New York Ontario Quebec Washington
  Apr      56000    47200      NA 127200         NA
  May      60000    15200      NA   2400      51200
  Jun     149600   111200      NA  59200     128800
# Rollup operation: compute the annual revenue for each product and collapse the state and month dimensions.
apply(revenue_cube, c("year", "product"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

# Drilldown operation: compute the annual and monthly revenue for each product and collapse the state dimension
apply(revenue_cube, c("year", "month", "product"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

, , product = Chair

      month
year      Apr    Aug    Dec    Feb    Jan    Jul    Jun    Mar    May    Nov    Oct    Sep
  2013 286320 125640 223920 159000 192960 204840 198720 237960 117120 137400 269160 259200
  2014 189480 204480 201120 201480 144720 237360 148200 247680 228600 174360 180960 224520

, , product = Laptop

      month
year      Apr    Aug    Dec    Feb    Jan    Jul    Jun    Mar     May    Nov    Oct    Sep
  2013 801000 895000 581000 558000 441000 614000 836000 622000 1087000 419000 827000 428000
  2014 648000 755000 623000 440000 638000 593000 896000 689000  581000 723000 892000 957000

, , product = Mattress

      month
year      Apr    Aug    Dec    Feb    Jan    Jul    Jun    Mar    May    Nov    Oct    Sep
  2013 280000 201600 374400 502400 443200 201600 148800 276800 425600 216800 212800 136000
  2014 230400 307200 243200 204800 312800 195200 448800 178400 128800 233600 490400 384800

, , product = Printer

      month
year     Apr   Aug   Dec   Feb   Jan   Jul    Jun    Mar   May    Nov    Oct   Sep
  2013 29600 67000 58200 93400 43400 35800  74800  71000 67600 100800 127400 17600
  2014 30600 55400 83200 65400 61800 71600 119600 105600 91600  73200  68000 46800

, , product = Tablet

      month
year      Apr    Aug    Dec    Feb    Jan    Jul    Jun    Mar    May    Nov    Oct    Sep
  2013 463500 455000 382500 414500 429500 720000 664500 492500 590500 595500 379000 571500
  2014 485000 481000 612500 448000 583500 564500 571500 571000 655000 701000 574500 504000

