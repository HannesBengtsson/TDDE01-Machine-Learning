set.seed(1234567890)

#install.packages("geosphere")
library(geosphere)
setwd("~/Desktop/SKOLA/TDDE01/tdde01_labbar/lab3/")
stations <- read.csv('stations.csv', fileEncoding = "ISO-8859-1")
temps <- read.csv('temps50k.csv')
st <- merge(stations,temps,by='station_number')

#Manually set smoothing coefficients for the 3 kernels
h_distance <- 1000000 #Cites close are taken into account 
h_date <- 30 #Date is relevant if close in time
h_time <- 2 #Time is relevant when close in time
a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00","12:00:00","14:00:00","16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")
temp <- vector(length=length(times))

#Could be interesting to include trend as well
# dist_stations <- distHaversine(p1 = point_of_interest, p2 = stations_long_lat)
# Students’ code here
current_values <- st[st$date < date,]
point_of_interest <- matrix(data = c(b,a), ncol=2)
stations_long_lat <- matrix(data = c(current_values$longitude,current_values$latitude),ncol = 2)

dist_stations <- function (actual_station,other_stations){
  return(distHaversine(p1 = actual_station, 
                        p2 = other_stations))
}

dist_date <- function (actual_date,other_dates){
  return(difftime(time1 = as.Date(actual_date, format = '%Y-%m-%d'),
                  time2 = as.Date(other_dates, format = '%Y-%m-%d'), 
                  units = "days"))
}

dist_hours <- function(x1,x2){
  return (difftime(
    time1 = strptime(x1,format = '%H:%M:%S'), 
    time2 = strptime(x2, format = '%H:%M:%S'), 
    units = "hours"))
}

kernel_gaussian <- function(x,h){
  scaled <- x/h
  return(exp(-(scaled*scaled)))
}

#For the Gaussian kernel, λ is the standard deviation.
tmp_dist <- dist_stations(actual_station = point_of_interest, other_stations = stations_long_lat)
dist_kernel <- kernel_gaussian(x = tmp_dist, h = h_distance)
tmp_date <- as.numeric(dist_date(actual_date = date, other_dates = current_values$date))
date_kernel <- kernel_gaussian(x = tmp_date, h = h_date)

plot(tmp_dist,dist_kernel, xlab='Distance to target point', ylab = 'Value')
plot(tmp_date,date_kernel, xlab='Distance to target date', ylab = 'Value')

hour_kernel <- matrix(nrow = nrow(current_values), ncol = length(times))
predict_temperature_sum <- c()
predict_temperature_prod <- c()
for (i in 1:length(times)){
  hour_kernel[,i] <- kernel_gaussian(as.numeric(dist_hours(times[i],current_values$time)), h_time)
  
  temp_sum <- (dist_kernel + date_kernel + hour_kernel[,i])
  predict_temperature_sum <- append(predict_temperature_sum, 
                                    sum(((temp_sum)/sum(temp_sum)) 
                                        %*% current_values$air_temperature))
  
  ### task 2 in the same loop###
  temp_prod <- (dist_kernel * date_kernel * hour_kernel[,i])
  predict_temperature_prod <- append(predict_temperature_prod, 
                                     sum(((temp_prod)/sum(temp_prod)) 
                                         %*% current_values$air_temperature))
  
}
# sum the kernels:
x_axis <- c(4,6,8,10,12,14,16,18,20,22,24)
plot(x_axis,predict_temperature_sum)

plot(x_axis,predict_temperature_prod)