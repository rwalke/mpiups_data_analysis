# 2023-07-18 RW
# example month December 2022
# 
require(data.table)
require(lubridate)
require(scales)
require(tsbox)

require(vcd)
require(Cairo)
require(ggplot2)


data_dir <- file.path("..", "..", "data")

# prepare a folder for storing figures
figures_dir <- file.path("..","..", "figures")
if (!dir.exists(figures_dir)) {dir.create(figures_dir)}


load(file=file.path(data_dir, "power1UAB.RData"))

power1

# create sub data sets
p_UPS_A1 <- power1[data_source=="UPS_A1",.(date3, ups_out)]
p_UPS_A2 <- power1[data_source=="UPS_A2",.(date3, ups_out)]
p_UPS_A3 <- power1[data_source=="UPS_A3",.(date3, ups_out)]
p_UPS_B1 <- power1[data_source=="UPS_B1",.(date3, ups_out)]
p_UPS_B2 <- power1[data_source=="UPS_B2",.(date3, ups_out)]
p_UPS_B3 <- power1[data_source=="UPS_B3",.(date3, ups_out)]

# create a sub set for the sum, but for complete series (N==6) only
p_UPS_T <- unique(power1[ups_out_N==6,.(date3, ups_out_sum)])

# add 50% more (including non UPS power)
p_UPS_T[,ups_out_sum_1.5 :=ups_out_sum * 1.5]
# add additional 30% more (including non UPS power and including PUE 1.3)
p_UPS_T[,ups_out_sum_1.95 :=ups_out_sum * 1.5 *1.3]


# load infrastructe power consumption data

load(file=file.path(data_dir, "power3Infrastructue.RData"))

power3
# reduce to the time series
p_Infrastructure <- power3[,.(date3, power1)]


# load local Sun data 
load(file=file.path(data_dir, "localSunData.RData"))

W1

# reduce to the time series
p_SUN_LSp <- W1[,.(date3, LSp)]
p_SUN_GSp <- W1[,.(date3, GSp)]
p_SUN_DSp <- W1[,.(date3, DSp)]

ts_plot(p_SUN_LSp, p_SUN_GSp, p_SUN_DSp)



# prepare the graph
start1 <- "2022-12-01"
end1 <- "2022-12-31"

heatbreaks <- c("Institute", "+95%", "+50%", "UPS Sum", "Sun GS 100m\U00B2")

heatcols <- c("UPS Sum"="red","+50%"="red","+95%"="red","Institute"="gray","Sun GS 100m\U00B2"="orange")


g1 <- ggplot(data = p_Infrastructure, aes(x=date3, y=power1, col="Institute"))
g2 <- g1 + geom_line() +
  geom_line(data=p_UPS_T, aes(x=date3, y=ups_out_sum, col="UPS Sum")) +
  geom_line(data=p_UPS_T, aes(x=date3, y=ups_out_sum_1.5, col="+50%"), linetype = "dotted") +
  geom_line(data=p_UPS_T, aes(x=date3, y=ups_out_sum_1.95, col="+95%"), linetype = "dotted") +
  geom_line(data = p_SUN_GSp, aes(x=date3, y=GSp/10, col="Sun GS 100m\U00B2"), linetype = "dashed") +
  scale_color_manual(name="power", breaks=heatbreaks, values=heatcols) +
  scale_x_datetime("date (GMT-1)", limits=c(as.POSIXct(ymd(start1)),as.POSIXct(ymd(end1))), date_breaks="1 week", 
                   date_minor_breaks="1 day", 
                   labels=date_format("%Y-%m-%d", tz="Etc/GMT-1")) + 
  scale_y_continuous(expression("power consumption in kW, sun radiation in kW per 100m\U00B2"), limits=c(0,75)) +
  theme(legend.justification=c(1,1), legend.position=c(0.95,0.90), legend.direction = "vertical", 
        plot.background = element_rect(fill = "transparent", colour = NA))
g2
# save it as an PDF image
CairoPDF(file.path(figures_dir ,"month202212.pdf"), bg="transparent")
g2
dev.off()

# save it as an SVG image
CairoSVG(file.path(figures_dir ,"month202212.svg"), width = 9, height = 6, bg="transparent")
g2
dev.off()
