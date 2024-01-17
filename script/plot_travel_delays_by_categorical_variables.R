install.packages('gghalves')
install.packages('gridExtra')
install.packages('ggh4x')
install.packages("rlang")
install.packages('cowplot')

library(ggplot2)
library(gghalves)
library(gridExtra)
library(ggh4x)
library(cowplot)



setwd("path_to_the_script") # change the path to the script

var<-read.csv('../data/total_delay_per_crash.csv',header=T) # the most updated variable table
var$FRC <- as.factor(var$FRC)
head(var)

data<-iris
head(data)

#####
# FRC

lookup_table <- c('Main'=1, 'Secondary'=2, 'Local'=3, 'Minor'=4)
var$FRC_string <- as.factor(lookup_table[var$FRC_string])


p_frc<-ggplot(var,aes(x=FRC_string,
                   y=traveldelay_15min_sum,
                   fill=FRC_string,
                   color=FRC_string))+
  force_panelsizes(rows = unit(3, "in"),
                   cols = unit(3, "in"))

mycolor<-c('#f2997a','#f2997a','#f2997a','#f2997a') 
p_frc0<-p_frc+scale_color_manual(values=rev(mycolor))+scale_fill_manual(values=rev(mycolor))

p_frc1<-p_frc0+geom_half_violin()

p_frc2<-p_frc0+geom_half_violin(position=position_nudge(x=0.1,y=0),
                        side='R',adjust=1.2,trim=F,color=NA,alpha=0.8)

p_frc3<-p_frc2+geom_point()

p_frc4<-p_frc2+geom_point(aes
                  (x = as.numeric(FRC_string)-0.1,
                    y = traveldelay_15min_sum,
                    color = FRC_string),
                  position = position_jitter(width =0.03),size =0.2, shape = 20)

p_frc5<-p_frc4+geom_boxplot(outlier.shape = NA, 
                    width =0.1,
                    alpha=0.7)

p_frc6 <- p_frc5+theme_bw()

p_frc6 <- p_frc6 + labs(x = "Functional road class", y = "Estimated travel delays (vehicle-hours)")

p_frc6 <- p_frc6 + theme(legend.position = "none")

ggsave("../output/raincloud_FRCstring.png", 
       plot = p_frc6, width = 5, height = 4)


#####
# Severity

lookup_table <- c('Serious'=1, 'Slight'=2)
var$SEVERITY <- as.factor(lookup_table[var$SEVERITY])


p_severity<-ggplot(var,aes(x=SEVERITY,
                  y=traveldelay_15min_sum,
                  fill=SEVERITY,
                  color=SEVERITY))+
  force_panelsizes(rows = unit(3, "in"),
                   cols = unit(3, "in"))

mycolor<-c('#6fa8dc','#6fa8dc') 
p_severity0<-p_severity+scale_color_manual(values=rev(mycolor))+scale_fill_manual(values=rev(mycolor))

p_severity1<-p_severity0+geom_half_violin()

p_severity2<-p_severity0+geom_half_violin(position=position_nudge(x=0.1,y=0),
                        side='R',adjust=1.2,trim=F,color=NA,alpha=0.8)

p_severity3<-p_severity2+geom_point()

p_severity4<-p_severity2+geom_point(aes
                  (x = as.numeric(SEVERITY)-0.1,
                    y = traveldelay_15min_sum,
                    color = SEVERITY),
                  position = position_jitter(width =0.03),size =0.2, shape = 20)

p_severity5<-p_severity4+geom_boxplot(outlier.shape = NA, 
                    width =0.1,
                    alpha=0.7)

p_severity6 <- p_severity5+theme_bw()

p_severity6 <- p_severity6 + labs(x = "Crash severity", y = "Estimated travel delays (vehicle-hours)")

p_severity6 <- p_severity6 + theme(legend.position = "none")

ggsave("../output/raincloud_severity.png", 
       plot = p_severity6, width = 5, height = 4)


#####
# ST_LGT

lookup_table <- c('good'=1, 'bad'=2)
var$ST_LGT_good <- as.factor(lookup_table[var$ST_LGT_good])


p_light<-ggplot2::ggplot(var,aes(x=ST_LGT_good,
                  y=traveldelay_15min_sum,
                  fill=ST_LGT_good,
                  color=ST_LGT_good))+
        force_panelsizes(rows = unit(3, "in"),
                         cols = unit(3, "in"))

mycolor<-c('#abc59f','#abc59f') 
p_light0<-p_light+scale_color_manual(values=rev(mycolor))+scale_fill_manual(values=rev(mycolor))

p_light1<-p_light0+geom_half_violin()

p_light2<-p_light0+geom_half_violin(position=position_nudge(x=0.1,y=0),
                        side='R',adjust=1.2,trim=F,color=NA,alpha=0.8)

p_light3<-p_light2+geom_point()

p_light4<-p_light2+geom_point(aes
                  (x = as.numeric(ST_LGT_good)-0.1,
                    y = traveldelay_15min_sum,
                    color = ST_LGT_good),
                  position = position_jitter(width =0.03),size =0.2, shape = 20)

p_light5<-p_light4+geom_boxplot(outlier.shape = NA, 
                    width =0.1,
                    alpha=0.7)

p_light6 <- p_light5+theme_bw()

p_light6 <- p_light6 + labs(x = "Light conditions", y = "Estimated travel delays (vehicle-hours)")

p_light6 <- p_light6 + theme(legend.position = "none")

ggsave("../output/raincloud_lightconditions.png", 
       plot = p_light6, width = 5, height = 4)


#####
# Collision type

lookup_table <- c('single-vehicle'=1, 'vehicle-vehicle'=2, 'vehicle-object'=3, 'vehicle-pedestrian'=4, 'mixed'=5)
var$collision <- as.factor(lookup_table[var$collision])


p_collisiontype<-ggplot(var,aes(x=collision,
                  y=traveldelay_15min_sum,
                  fill=collision,
                  color=collision))+
  force_panelsizes(rows = unit(3, "in"),
                   cols = unit(3, "in"))

mycolor<-c('#588B8B','#588B8B','#588B8B','#588B8B','#588B8B') 
p_collisiontype0<-p_collisiontype+scale_color_manual(values=rev(mycolor))+scale_fill_manual(values=rev(mycolor))

p_collisiontype1<-p_collisiontype0+geom_half_violin()

p_collisiontype2<-p_collisiontype0+geom_half_violin(position=position_nudge(x=0.1,y=0),
                        side='R',adjust=1.2,trim=F,color=NA,alpha=0.8)

p_collisiontype3<-p_collisiontype2+geom_point()

p_collisiontype4<-p_collisiontype2+geom_point(aes
                  (x = as.numeric(collision)-0.1,
                    y = traveldelay_15min_sum,
                    color = collision),
                  position = position_jitter(width =0.03),size =0.2, shape = 20)

p_collisiontype5<-p_collisiontype4+geom_boxplot(outlier.shape = NA,
                    width =0.1,
                    alpha=0.7)

p_collisiontype6 <- p_collisiontype5+theme_bw()

p_collisiontype6 <- p_collisiontype6 + labs(x = "Collision type", y = "Estimated travel delays (vehicle-hours)")

p_collisiontype6 <- p_collisiontype6 + theme(legend.position = "none")

ggsave("../output/raincloud_collisiontype.png", 
       plot = p_collisiontype6, width = 5, height = 4)


#####
# day type

lookup_table <- c('weekday'=1, 'weekend'=2)
var$daytype <- as.factor(lookup_table[var$daytype])


p_daytype<-ggplot(var,aes(x=daytype,
                  y=traveldelay_15min_sum,
                  fill=daytype,
                  color=daytype))+
  force_panelsizes(rows = unit(3, "in"),
                   cols = unit(3, "in"))

mycolor<-c('#B67C7E','#B67C7E') 
p_daytype0<-p_daytype+scale_color_manual(values=rev(mycolor))+scale_fill_manual(values=rev(mycolor))

p_daytype1<-p_daytype0+geom_half_violin()

p_daytype2<-p_daytype0+geom_half_violin(position=position_nudge(x=0.1,y=0),
                        side='R',adjust=1.2,trim=F,color=NA,alpha=0.8)

p_daytype3<-p_daytype2+geom_point()

p_daytype4<-p_daytype2+geom_point(aes
                  (x = as.numeric(daytype)-0.1,
                    y = traveldelay_15min_sum,
                    color = daytype),
                  position = position_jitter(width =0.03),size =0.2, shape = 20)

p_daytype5<-p_daytype4+geom_boxplot(outlier.shape = NA, 
                    width =0.1,
                    alpha=0.7)

p_daytype6 <- p_daytype5+theme_bw()

p_daytype6 <- p_daytype6 + labs(x = "Day of the week", y = "Estimated travel delays (vehicle-hours)")

p_daytype6 <- p_daytype6 + theme(legend.position = "none")

ggsave("../output/raincloud_daytype.png", 
       plot = p_daytype6, width = 5, height = 4)



#####
# Region

lookup_table <- c('HKI'=1, 'KL'=2, 'NT'=3)
var$Region <- as.factor(lookup_table[var$Region])


p_region<-ggplot(var,aes(x=Region,
                  y=traveldelay_15min_sum,
                  fill=Region,
                  color=Region))+
  force_panelsizes(rows = unit(3, "in"),
                   cols = unit(3, "in"))

mycolor<-c('#9f9aa4','#9f9aa4','#9f9aa4') 
p_region0<-p_region+scale_color_manual(values=rev(mycolor))+scale_fill_manual(values=rev(mycolor))

p_region1<-p_region0+geom_half_violin()

p_region2<-p_region0+geom_half_violin(position=position_nudge(x=0.1,y=0),
                        side='R',adjust=1.2,trim=F,color=NA,alpha=0.8)

p_region3<-p_region2+geom_point()

p_region4<-p_region2+geom_point(aes
                  (x = as.numeric(Region)-0.1,
                    y = traveldelay_15min_sum,
                    color = Region),
                  position = position_jitter(width =0.03),size =0.2, shape = 20)

p_region5<-p_region4+geom_boxplot(outlier.shape = NA, 
                    width =0.1,
                    alpha=0.7)

p_region6 <- p_region5+theme_bw()

p_region6 <- p_region6 + labs(x = "Region", y = "Estimated travel delays (vehicle-hours)")

p_region6 <- p_region6 + theme(legend.position = "none")

ggsave("../output/raincloud_Region.png", 
       plot = p_region6, width = 5, height = 4)
