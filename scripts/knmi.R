library(dplyr)
library(tidyr)
library(ggplot2)
library(animation)
library(plotly)

url <- "data/knmi.txt"
temp_dat <- read.table(url, skip = 13, sep = ",", header = TRUE)
colnames(temp_dat) <- c("STN", "YYYY", seq(1:12), "YEAR")

temp_dat_monthly <- temp_dat %>%
  select(-starts_with("STN")) %>%
  select(-starts_with("YEAR")) %>%
  filter(YYYY != 2019) %>%
  gather(month, temperature, -YYYY) %>%
  rename(year = YYYY)

temp_dat_monthly$month <- temp_dat_monthly$month %>% as.numeric
summary(temp_dat_monthly)
temp_dat_monthly <- temp_dat_monthly %>% group_by(month) %>% mutate(anomaly = (temperature - mean(temperature)) * 0.1)

mo <- months(seq(as.Date("1910/1/1"), as.Date("1911/1/1"), "months"))
mo <- gsub("(^...).*", "\\1", mo)

saveGIF({

  for(i in 1901:2018){
    print(ggplot(temp_dat_monthly %>% filter(year <= i), 
                 aes(x=month, y=anomaly, color=year, group=year)) +
            geom_line() +
            scale_color_gradient(low="blue", high="red", limits=c(1901, 2018), guide="none") +
            geom_hline(yintercept=0, color="white", lty=2) +
            geom_hline(yintercept=2, color="white", lty=2) +
            coord_polar() +
            annotate(x=1, y=-2, geom="label", label=i, fill="white") +
            annotate(x=1, y=0, geom="label", label="0C", fill="white", label.size=0) +
            annotate(x=1, y=2, geom="label", label="2C", fill="white", label.size=0) +
            ggtitle(expression(atop("AFWIJKINGEN MAANDTEMPERATUREN", atop(italic("KONINKLIJK NEDERLANDS METEOROLOGISCH INSTITUUT (KNMI)"), "")))) +
            scale_x_continuous(labels=mo, breaks=1:13) +
            scale_y_continuous(labels=NULL, breaks=NULL, limits = c(min(temp_dat_monthly$anomaly), max(temp_dat_monthly$anomaly))) +
            ylab("") + xlab("")
          
    )}
}, movie.name = "climate.gif", interval=0.1)

p <- ggplot(temp_dat_monthly, 
       aes(x=month, y=anomaly, color=year, group=year)) +
  geom_line() +
  scale_color_gradient(low="green", high="red", limits=c(1901, 2018), guide="none") +
  geom_hline(yintercept=0, color="white", lty=2) +
  geom_hline(yintercept=2, color="white", lty=2) 
  ggtitle(expression(atop("AFWIJKINGEN MAANDTEMPERATUREN", atop(italic("KONINKLIJK NEDERLANDS METEOROLOGISCH INSTITUUT (KNMI)"), "")))) +
  scale_x_continuous(labels=mo, breaks=1:13) +
  scale_y_continuous(labels=NULL, breaks=NULL, limits = c(min(temp_dat_monthly$anomaly), max(temp_dat_monthly$anomaly))) +
  ylab("") + xlab("")

ggplotly(p)