# 2023-07-18 RW
# data checks and some histograms
#
require(data.table)
require(lubridate)

require(vcd)
require(Cairo)
require(ggplot2)


data_dir <- file.path("..", "..", "data")

# prepare a folder for storing figures
figures_dir <- file.path("..","..", "figures")
if (!dir.exists(figures_dir)) {dir.create(figures_dir)}

load(file=file.path(data_dir, "power1UAB.RData"))

power1

# double check DST
check1 <- ymd_hms("2023-03-25 22:00:00", tz = "Etc/GMT-1")
power1[check1 < date3 & date3 < check1 + dhours(6) & data_source=="UPS_A1"]

check2 <- ymd_hms("2020-10-24 22:00:00", tz = "Etc/GMT-1")
power1[check2 < date3 & date3 < check2 + dhours(6) & data_source=="UPS_A1"]

# check for NA, no power or software out-of-order
power1NA <- power1[is.na(ups_out),]
setkey(power1NA, date3)


# create a power sum for every hour

power1[, ups_out_sum:=sum(ups_out), by=date3]
power1[, ups_out_N:=.N, by=date3]
power1[, table(ups_out_N)]
# power1[ups_out_N==3]

# create a year_month combination for better order
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


# prepare the histogram


h1 <- ggplot(power1[data_source=="UPS_A1" & !is.na(ups_out_sum) & year_month!="2024-09"],
             aes(x=ups_out_sum, group=year_month, color=as.factor(year_month)))
h2 <- h1 + geom_histogram(binwidth=0.025) + scale_y_log10() + 
  xlab("total ups power output in kW") + facet_wrap( ~year_month) +
  theme(legend.position="none")

h2

# save it as an PDF image
CairoPDF(file.path(figures_dir ,"UPShistogram.pdf"), bg="transparent")
h2
dev.off()

# save it as an SVG image
CairoSVG(file.path(figures_dir ,"UPShistogram.svg"), width = 9, height = 6, bg="transparent")
h2
dev.off()
