# 2023-06-14 RW
#
# get the data set from the Göttingen Research Online repository
# store a copy locally
# read the data set
# clean the date/time information
#

require(data.table)
require(lubridate)

require(dataverse)

require(vcd)
require(Cairo)
require(ggplot2)

# prepare a folder for storing the data files locally
data_dir <- file.path("..","..", "data")
if (!dir.exists(data_dir)) {dir.create(data_dir)}


# get the raw data from the dataverse
getDataFile <- function(data_file_name, data_dir){
  ups_file <- get_file_by_name(
    filename = data_file_name,
    dataset = "doi:10.25625/92H3K7",
    server = "data.goettingen-research-online.de",
    original = TRUE
  )
  writeBin(ups_file, file.path(data_dir, sub("tab$","csv",data_file_name)))
  print(file.path(data_dir, sub("tab$","csv",data_file_name)))
}

data_files6 <- c("UPS_A1.tab", "UPS_A2.tab", "UPS_A3.tab", "UPS_B1.tab", "UPS_B2.tab", "UPS_B3.tab")

for (data_file_name in data_files6) getDataFile(data_file_name, data_dir)


# read the raw data and process it
readUPSdata <- function(data_file_name11, data_source="unknown") {
  
  # read the time series
  power1B <- fread(file.path(data_dir, data_file_name11))
  
  # check for duplicates
  print(table(duplicated(power1B)))
  
  # check and remove summary lines
  power1B[grepl("Average",`Date Time`)]
  power1 <- power1B[!grepl("Average",`Date Time`)]
  power1[grepl("Average",`Date Time`)]
  
  head(power1$'Date Time(RAW)')
  print(power1[,range(`Date Time(RAW)`)])
  
  # https://kb.paessler.com/en/topic/1313-how-do-i-translate-prtg-timestamp-values-format-to-normal-time-format
  # number of days that have passed since 12/30/1899
  
  power1[, date2:=round_date(as_datetime(as_date(`Date Time(RAW)`, origin = "1899-12-30")), unit = "minute")]
  power1[,tz(date2)]
  # duplicate the date and force the time zone to GMT-1
  # https://en.wikipedia.org/wiki/Tz_database#Area
  power1[, date3 := force_tz(date2, tz="Etc/GMT-1")]
  power1[,tz(date3)]
  
  power1[, date2 := NULL]
  
  
  # check for missing data
  setkey(power1, date3)
  power1[, date3m1 := shift(date3,1, type="lag")]
  power1[, date3Diff := date3 - date3m1]
  power1[,table(date3Diff)]
  print(power1[,range(date3)])
  
  # kW instead of W
  power1[, ups_out := `ups output power(RAW)`/1000]
  
  power1[, data_source:=data_source]
  
  return(power1)
}


power_UPS_A1 <- readUPSdata("UPS_A1.csv", data_source="UPS_A1")

power_UPS_A2 <- readUPSdata("UPS_A2.csv", data_source="UPS_A2")

power_UPS_A3 <- readUPSdata("UPS_A3.csv", data_source="UPS_A3")


power_UPS_B1 <- readUPSdata("UPS_B1.csv", data_source="UPS_B1")

power_UPS_B2 <- readUPSdata("UPS_B2.csv", data_source="UPS_B2")

power_UPS_B3 <- readUPSdata("UPS_B3.csv", data_source="UPS_B3")

power1 <- rbind(power_UPS_A1, power_UPS_A2, power_UPS_A3,
                power_UPS_B1, power_UPS_B2, power_UPS_B3)


# double check DST
check1 <- ymd_hms("2023-03-25 22:00:00", tz = "Etc/GMT-1")
power1[check1 < date3 & date3 < check1 +dhours(6) & data_source=="UPS_A1"]

check2 <- ymd_hms("2020-10-24 22:00:00", tz = "Etc/GMT-1")
power1[check2 < date3 & date3 < check2 +dhours(6) & data_source=="UPS_A1"]

# check for NA
power1NA <- power1[is.na(ups_out),]
setkey(power1NA, date3)


# create a power sum for every hour

power1[, ups_out_sum:=sum(ups_out), by=date3]
power1[, ups_out_N:=.N, by=date3]
power1[, table(ups_out_N)]
# power1[ups_out_N==3]

# create a year_month combination
power1[, year_month:=paste0(year(date3), "-", month(date3))]
# fillin a 0
power1[, table(year_month)]
power1[, year_month:= sub("(-)(.$)","\\10\\2", year_month)]
power1[, table(year_month)]

# check for special events
power1[data_source=="UPS_A1" & !is.na(ups_out_sum), range(ups_out_sum)]
power1[data_source=="UPS_A1" & 10.0 < ups_out_sum,]

power75up <- power1[7.5 < ups_out_sum,]
setkey(power75up, date3)

save(power1, file=file.path(data_dir, "power1UAB.RData"))


# prepare a graphs

# check the power outage in detail
check2 <- ymd_hms("2022-12-19 12:00:00", tz = "Etc/GMT-1")
p2 <- ggplot(power1[check2 < date3 & date3 < check2 + dhours(96)], aes(x=date3, y=ups_out, color=data_source))
p2 + geom_point(size=0.1) + scale_x_datetime("date (GMT-1)")  +
  scale_y_continuous("ups power output in kW", limits=c(0,5))

