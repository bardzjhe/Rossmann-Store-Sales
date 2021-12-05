library(readr)
library(dplyr) # filter, group_by...
library(lubridate) # process date data
library(ggplot2)
library(scales)
library(RColorBrewer)

# load data from .csv file
train <- read_csv("../input/train.csv", col_types = list(StateHoliday = col_character()))
test <- read_csv("../input/test.csv", col_types = list(StateHoliday = col_character()))
store <- read_csv("../input/store.csv")

# Add each store's average sales and customers to hthe store-level data frame
store <- left_join(store, summarize(group_by(train[train$Open == 1,], Store)
                              , AvgSales = mean(Sales, na.rm = TRUE) # remove null value within
                              , AvgCust = mean(Customers, na.rm = TRUE)), "Store")


store$Assortment[store$Assortment == "a"] <- "basic"
store$Assortment[store$Assortment == "b"] <- "extra"
store$Assortment[store$Assortment == "c"] <- "extended"
store$StTyAs <- paste("'", store$StoreType, "'", " store type - "
                   , store$Assortment, sep = "")

store$CompDistBin <- ntile(store$CompetitionDistance, 5)
store$CompDistBin <- paste(store$CompDistBin, "quintile")
store$CompDistBin[store$CompDistBin == "NA quintile"] <- NA

store$CompOpenDate <- ymd(paste(store$CompetitionOpenSinceYear
                             , store$CompetitionOpenSinceMonth
                             , "01"))

store$CompOpenDate <- as.Date(store$CompOpenDate)
store <- select(store
             , Store
             , StoreType
             , Assortment
             , StTyAs
             , CompDist = CompetitionDistance
             , CompDistBin
             , CompOpenDate
             , AvgSales
             , AvgCust)
store$AvgSpC <- store$AvgSales / store$AvgCust
store$CompDays <- as.numeric(as.Date("2015-09-17") - store$CompOpenDate)


int_TilePlot <- function(data, response, x, y
                         , r_lab = response, x_lab = x, y_lab = y
                         , r_scale = comma, x_scale = comma, y_scale = comma
                         , bins = 10, exp = 8){
  
  x_brk <- seq(min(data[[x]]), max(data[[x]]), length.out = bins)
  y_brk <- seq(min(data[[y]]), max(data[[y]]), length.out = bins)
  df <- data.frame(x_val = rep(x_brk, bins)
                   , y_val = rep(y_brk, each = bins)
                   , r_val = NA)
  df$x_z <- (df$x_val - mean(data[[x]])) / sd(data[[x]])
  df$y_z <- (df$y_val - mean(data[[y]])) / sd(data[[y]])
  data$x_z <- (data[[x]] - mean(data[[x]])) / sd(data[[x]])
  data$y_z <- (data[[y]] - mean(data[[y]])) / sd(data[[y]])
  for (i in 1:nrow(df)){
    dist <- numeric()
    for (j in 1:nrow(data)){
      dist <- append(dist, sqrt(abs(df$x_z[i] - data$x_z[j])^2
                                + abs(df$y_z[i] - data$y_z[j])^2))
    }
    adjDist <- abs(dist - max(dist))^exp
    wtdResp <- sum(adjDist * data[[response]]) / sum(adjDist)
    df$r_val[i] <- wtdResp
  }
  ggplot(df, aes(x = x_val, y = y_val, z = r_val)) +
    geom_tile(aes(fill = r_val)) +
    stat_contour(bins = 15) +
    scale_x_continuous(x_lab, labels = x_scale) +
    scale_y_continuous(y_lab, labels = y_scale) +
    scale_fill_continuous(r_lab, labels = r_scale) +
    theme_minimal()
}
int_TilePlot(store[store$StoreType != 'b' & !is.na(store$CompDist),]
             , "CompDist", "AvgSpC", "AvgCust"
             , "Competition Distance"
             , "Average Sales per Customer"
             , "Average # of Customers"
             , comma, dollar, comma)









