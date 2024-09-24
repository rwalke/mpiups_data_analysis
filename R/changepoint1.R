# 2023-07-18 RW
# example with the changepoint library and tsbox
# 
require(data.table)
require(lubridate)
require(scales)
require(tsbox)

require(vcd)
require(Cairo)
require(ggplot2)
require(ggfortify) # for autoplot if needed

require(changepoint)

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

ts_plot(p_UPS_A1, p_UPS_A2, p_UPS_A3, p_UPS_B1, p_UPS_B2, p_UPS_B3, p_UPS_T)


detectChangePoints <- function(p_time1) {
  
  # replace the NA's by 0, changepoint does not work with NA's
  # and replace the column names by default column names time and value
  p_time1n <- copy(ts_default(p_time1))
  p_time1n[, value := ifelse(is.na(value), 0, value)]
  
  # use the BinSeq method to find changepoints
  p_time1n_cptBS <- cpt.mean(ts_ts(p_time1n), method="BinSeg", Q=20, class=TRUE)
  
  cp1 <- data.table(time3=c(p_time1n[,min(time)], p_time1n[cpts(p_time1n_cptBS), time]),
                    pmean=param.est(p_time1n_cptBS)$mean)
  r1 <- p_time1n[cpts(p_time1n_cptBS), time]
  cp1F <- data.table(time3=c(p_time1n[,min(time)], sort(c(r1, r1 -dhours(1))), p_time1n[,max(time)]),
                     pmean=rep(param.est(p_time1n_cptBS)$mean, each=2))
  cpR <- list(cp=cp1, cpF=cp1F, cptBS=p_time1n_cptBS)
  return(cpR)
  
}

cpA1 <- detectChangePoints(p_UPS_A1)
cpA1$cp
plot(cpA1$cptBS)
ts_plot("UPS_A1"=p_UPS_A1, "CP"=cpA1$cpF)
#
cpA2 <- detectChangePoints(p_UPS_A2)
cpA2$cp
plot(cpA2$cptBS)
ts_plot("UPS_A2"=p_UPS_A2, "CP"=cpA2$cpF)
#
cpA3 <- detectChangePoints(p_UPS_A3)
cpA3$cp
plot(cpA3$cptBS)
ts_plot("UPS_A3"=p_UPS_A3, "CP"=cpA3$cpF)

cpB1 <- detectChangePoints(p_UPS_B1)
cpB1$cp
plot(cpB1$cptBS)
ts_plot("UPS_B1"=p_UPS_B1, "CP"=cpB1$cpF)
#
cpB2 <- detectChangePoints(p_UPS_B2)
cpB2$cp
plot(cpB2$cptBS)
ts_plot("UPS_B2"=p_UPS_B2, "CP"=cpB2$cpF)
#
cpB3 <- detectChangePoints(p_UPS_B3)
cpB3$cp
plot(cpB3$cptBS)
ts_plot("UPS_B3"=p_UPS_B3, "CP"=cpB3$cpF)

cpBT <- detectChangePoints(p_UPS_T)
cpBT$cp
plot(cpBT$cptBS)
ts_plot("UPS_T"=p_UPS_T, "CP"=cpBT$cpF)

#

heatcols <- c("Sum"="red","A1"="gray","A2"="brown","A3"="green","B1"="orange","B2"="blue","B3"="purple")


g1 <- ggplot(data=p_UPS_T, aes(x=date3, y=ups_out_sum))
g2 <- g1 + geom_line() + geom_line(data = cpBT$cpF, aes(x=time3, y=pmean, col="Sum")) +
  geom_line(data = cpA1$cpF, aes(x=time3, y=pmean, col="A1")) +
  geom_line(data = cpA2$cpF, aes(x=time3, y=pmean, col="A2")) +
  geom_line(data = cpA3$cpF, aes(x=time3, y=pmean, col="A3")) +
  geom_line(data = cpB1$cpF, aes(x=time3, y=pmean, col="B1"), linetype = "dashed") +
  geom_line(data = cpB2$cpF, aes(x=time3, y=pmean, col="B2"), linetype = "dashed") +
  geom_line(data = cpB3$cpF, aes(x=time3, y=pmean, col="B3"), linetype = "dashed") +
  scale_color_manual(name="UPS", values=heatcols) +
  scale_x_datetime("date (GMT-1)", date_breaks="1 year", 
                   date_minor_breaks="1 month", 
                   labels=date_format("%Y-%m-%d", tz="Etc/GMT-1")) + 
  scale_y_continuous("power consumption in kW") +
  theme(legend.justification=c(0,1), legend.position = "inside", legend.position.inside =c(0.05,0.9), legend.direction = "horizontal", 
        plot.background = element_rect(fill = "transparent", colour = NA))
g2

# save it as an PDF image
CairoPDF(file.path(figures_dir ,"UPS1.pdf"), bg="transparent")
g2
dev.off()

# save it as an SVG image
CairoSVG(file.path(figures_dir ,"UPS1.svg"), width = 9, height = 6, bg="transparent")
g2
dev.off()
