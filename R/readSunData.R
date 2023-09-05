# 2023-07-14 RW
# direct access to free available climate data from DWD Germany
# https://opendata.dwd.de/

require(data.table)
require(httr)
library(scales)

require(ggplot2)
require(lubridate)

# prepare a folder for storing the data files locally
data_dir <- file.path("..","..", "data")
if (!dir.exists(data_dir)) {dir.create(data_dir)}

# DWD Rostock Warnemünde
(today <- paste(as.POSIXlt(Sys.Date())$yday,paste(sample(seq(1:9),3), collapse =""),sep="-"))

link1 <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/solar/recent/10minutenwerte_SOLAR_04271_akt.zip"

get_link1 <- GET(link1)

# save a spare copy and extract the data file needed
if(status_code(get_link1)==200) {
  
  tmp1 = tempfile()
  writeBin(content(get_link1), tmp1)
  writeBin(content(get_link1), file.path(data_dir,paste("dwd",today,".zip", sep="")))

  tmplist <- unzip(tmp1, list=TRUE)
  tmp3 <- grep("produkt_zehn_min_sd",tmplist$Name, value=TRUE)
  
  print(tmp3)
  
  W1 <- as.data.table(read.csv2(unz(tmp1,tmp3), stringsAsFactors = FALSE))
  unlink(tmp1)
}

W1

# end of interval in UTC and use it as the key variable
W1[, date2 := ymd_hm(MESS_DATUM, tz="UTC")]
setkey(W1, date2)

# duplicate the date and fix the time zone to GMT-1
W1[, date3 := with_tz(date2, tz="Etc/GMT-1")]

# check the data set
W1[duplicated(date3)]


# DS_10 10min-sum of diffuse solar radiation J/cm^2
# GS_10 10min-sum of solar incoming radiation J/cm^2
# SD_10 10min-sum of sunshine duration h
# LS_10 10min-sum of longwave downward radiation J/cm^2

W1[,DS_10 := as.numeric(DS_10)]
W1[,GS_10 := as.numeric(GS_10)]
W1[,SD_10 := as.numeric(SD_10)]
W1[,LS_10 := as.numeric(LS_10)]


# recode missing values
W1[DS_10 == -999, DS_10 := NA]
W1[GS_10 == -999, GS_10 := NA]
W1[SD_10 == -999, SD_10 := NA]
W1[LS_10 == -999, LS_10 := NA]

# compute the power in W/m^2   1 m^2 = 10000 cm^2, 10 min = 600 s
W1[,DSp := DS_10*10000/600]
W1[,GSp := GS_10*10000/600]
W1[,SDp := SD_10*10000/600]
W1[,LSp := LS_10*10000/600]


W1[, lapply(.SD, range, na.rm=FALSE)]
W1[, lapply(.SD, range, na.rm=TRUE)]

# double check NA
W1[, lapply(lapply(.SD, is.na), table)]
W1[, table(is.na(LS_10))]

save(W1, file=file.path(data_dir, "localSunData.RData"))



now1 <- today()

heatcols <- c("DS"="red", "GS"="green", "LS"="blue")

p1 <- ggplot(W1[date3 > now1 - ddays(7)], aes(x=date3, y=DSp))
p1 + geom_point(alpha=0.2, aes(col="DS")) + geom_point(aes(y=GSp, col="GS"), alpha=0.2) +
  geom_point(aes(y=LSp, col="LS"), alpha=0.2) +
  scale_x_datetime("date", date_breaks="1 day", 
                   date_minor_breaks="4 hours", 
                   labels=date_format("%Y-%m-%d", tz="UTC")) +
  scale_y_continuous("DS GS LS in W / m^2") +
  scale_color_manual(name="Sun", values=heatcols) 

#


