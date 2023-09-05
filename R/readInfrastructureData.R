# 2023-07-14 RW
# 
# get the data set from the edmond dataverse repository
# store a copy locally
# read the data set
# clean the date/time information
#
# 
require(data.table)
require(lubridate)

require(dataverse)

# prepare a folder for storing the data files locally
data_dir <- file.path("..","..", "data")
if (!dir.exists(data_dir)) {dir.create(data_dir)}


data_file_name <- "power_consumption_MPIDR_2020_2022.csv"

power2 <- get_file_by_name(
  filename = data_file_name,
  dataset = "doi:10.17617/3.DHIBFN",
  server = "edmond.mpdl.mpg.de",
  original = TRUE
)

# store the file locally for later use
writeBin(power2, file.path(data_dir, data_file_name))

# read the time series
power3 <- fread(file.path(data_dir, data_file_name),
                encoding = "UTF-8", sep=";", dec=",", header=FALSE, skip=4, col.names=c("date1","energy1"))
# data shows energy consumption per 15 minutes (0.25 hours)
# power is energy per unit of time 
power3[,power1:=energy1 / 0.25]

# compute the power difference
power3[, dpower1:= c(0,diff(power1))]


# use the time zone CET (and DST)
power3[, date2 := dmy_hm(date1, tz="CET")]
# double check for daylight saving time, see e.g. 2020-03-29
power3[, dst2 := dst(date2)]

# duplicate the date and fix the time zone to GMT-1
power3[, date3 := with_tz(date2, tz="Etc/GMT-1")]

# resolve the time duplicates in the fall
power3[duplicated(date3, fromLast=TRUE)]
power3[duplicated(date3, fromLast=TRUE), date3 := date3 - dhours(1)]
power3[duplicated(date3, fromLast=TRUE)]

power3[, hour3 := hour(date3)]
power3[, minute3 := minute(date3)]
power3[, wday3 := wday(date3, label=TRUE)]

# double check DST
check1 <- ymd_hms("2020-03-28 22:00:00", tz = "Etc/GMT-1")
power3[check1 < date3 & date3 < check1 +dhours(6)]

power3[,table(duplicated(date3))]
power3[duplicated(date3)]

check2 <- ymd_hms("2020-10-25 00:00:00", tz = "Etc/GMT-1")
power3[check2 < date3 & date3 < check2 +dhours(6)]
# fixed

save(power3, file=file.path(data_dir, "power3Infrastructue.RData"))

