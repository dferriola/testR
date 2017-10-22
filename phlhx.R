labels=c('12','1','2','3','4','5','6','7','8','9','10','11','12','1','2','3','4','5','6','7','8','9','10','11'))
labels=c('12','1','2','3','4','5','6','7','8','9','10','11','12','1','2','3','4','5','6','7','8','9','10','11'))
Mode(dui$Hour)
mode.dui.hour=Mode(dui$Hour)
arson=subset(a,subset=Text_General_Code=="Arson")
mode.arson.hour=Mode(arson$Hour)
View(armr)
armr=subset(a,subset=Text_General_Code=="Robbery Firearm")
mode.armr.hour=Mode(armr$Hour)
fraud=subset(a,subset=Text_General_Code=="Fraud")
mode.fraud.hour=Mode(fraud$Hour)
weaponviolations=subset(a,subset=Text_General_Code=="Weapons Violations")
mode.weaponsviolations.hour=Mode(weaponviolations$Hour)
rape=subset(a,subset=Text_General_Code=="Rape")
mode.rape.hour=Mode(rape$Hour)
homicide.criminal=subset(a,subset=Text_General_Code=="Homicide - Criminal")
mode.homicide.criminal.hour=Mode(homicide.criminal$Hour)
public.drunkenness=subset(a,subset=Text_General_Code=="Public Drunkenness")
mode.public.drunkenness.criminal.hour=Mode(public.drunkenness$Hour)
prostitution=subset(a,subset=Text_General_Code=="Prostitution and Commercialized Vice")
mode.prost.hour=Mode(prostitution$Hour)
Thefts=subset(a,subset=Text_General_Code=="Thefts")
mode.thefts.hour=Mode(Thefts$Hour)
vandals=subset(a,subset=Text_General_Code=="Vandalism/Criminal Mischief")
mode.vandals.hour=Mode(vandals$Hour)
robbery.nfirearm=subset(a,subset=Text_General_Code=="Robbery No Firearm")
mode.robberynf.hour=Mode(robbery.nfirearm$Hour)
narcD=subset(a,subset=Text_General_Code=="Narcotic / Drug Law Violations")
mode.narc.hour=Mode(narcD$Hour)
b&e=subset(a,subset=Text_General_Code=="Burglary Residential")
b.e=subset(a,subset=Text_General_Code=="Burglary Residential")
mode.burgRes.hour=Mode(b.e$Hour)
homeless=subset(a,subset=Text_General_Code=="Vagrancy/Loitering")
mode.vagloit.hour=Mode(homeless$Hour)
modeHour=as.data.frame(c(mode.armr.hour,mode.dui.hour))
View(modeHour)
Mode(a$Hour)
Mode(a$Hour, a$Text_General_Code)
table(a$Hour,a$Text_General_Code)
hours.crimes=table(a$Hour,a$Text_General_Code)
summary(hours.crimes)
quantile(hours.crimes)
hours.crimes2=as.data.frame(hours.crimes)
View(hours.crimes2)
AggravatedAssualt.FireArm=subset(hours.crimes2,subset=Text_General_Code=="Aggravated Assault Firearm
")
AggravatedAssualt.FireArm=subset(hours.crimes2,subset=Text_General_Code=="Aggravated Assault Firearm")
AggravatedAssualt.FireArm=subset(hours.crimes2,subset=Var2=="Aggravated Assault Firearm")
View(AggravatedAssualt.FireArm)
plot(AggravatedAssualt.FireArm$Var1,AggravatedAssualt.FireArm$Freq)
AggravatedAssualt.NFireArm=subset(hours.crimes2,subset=Var2=="Aggravated Assault No Firearm")
plot(AggravatedAssualt.NFireArm$Var1,AggravatedAssualt.NFireArm$Freq)
aanfplot=plot(AggravatedAssualt.NFireArm$Var1,AggravatedAssualt.NFireArm$Freq)
aawf=plot(AggravatedAssualt.FireArm$Var1,AggravatedAssualt.FireArm$Freq)
arson.hours=subset(hours.crimes2,subset=Var2=="Aggravated Assault No Firearm")
arsonplot=plot(arson.hours$Var1,arson.hours$Freq)
burglary.nonres=subset(hours.crimes2,subset=Var2=="Burglary Non-Residential")
burgnonresplot=plot(burglary.nonres$Var1,burglary.nonres$Freq)
burglary.res=subset(hours.crimes2,subset=Var2=="Burglary Residential")
burgresplot=plot(burglary.res$Var1,burglary.res$Freq)
dconductplot=subset(hours.crimes2,subset=Var2=="Disorderly Conduct")
dconductplot=plot(dconductplot$Var1,dconductplot$Freq)
dui.h=subset(hours.crimes2,subset=Var2=="DRIVING UNDER THE INFLUENCE")
duiplot=plot(dui.h$Var1,dui.h$Freq)
library (plyr)
library (dplyr)
library (plyr)
library (ggplot)
library (ggplot2)
hour=c([0:24])
hour=c(0:24)
qplot(arson.hours)
qplot(arson.hours, binwidth=30)
ggplot(AggravatedAssualt.FireArm)
ggplot(AggravatedAssualt.FireArm,hour)
View(AggravatedAssualt.FireArm)
ggplot(AggravatedAssualt.FireArm, aes(x=Var1, y=Freq))
ggplot(AggravatedAssualt.FireArm, aes(x=Var1, y=Freq))+geom_point()
ggplot(AggravatedAssualt.FireArm, aes(x=Var1, y=Freq))+geom_line()
ggplot(AggravatedAssualt.FireArm, aes(x=Var1, y=Freq))+geom_point()
table(AggravatedAssualt.FireArm)
cor(AggravatedAssualt.FireArm$Freq~.)
cor(AggravatedAssualt.FireArm$Freq~Var1)
cor(AggravatedAssualt.FireArm$Freq~AggravatedAssault.FireArm$Var1)
barplot(table(AggravatedAssualt.FireArm))
class(Var1)
class(AggravatedAssault.Firearm$Var1)
class(AggravatedAssault.FireArm$Var1)
class(AggravatedAssualt.FireArm$Var1)
aa1=as.numeric(AggravatedAssualt.FireArm$Var1)
aa1=as.numeric(AggravatedAssualt.FireArm$Freq)
aahour=rbind(hour,aa1)
View(aahour)
colnames(aa1)=hour
rownames(aahour)=hour
hour=c(0:23)
rownames(aahour)=hour
View(aahour)
aahour=aa1
aahour=rbind(hour,aa1)
View(aahour)
rownames(aahour)=hour
qplot(aahour, aa1, geom="bar", stat="identity")
qplot(aahour, aa1, geom="bar")
ggplot(aahour, aes(x=hour, y=aaq)) + geom_line() + geom_point()
ggplot(aahour, aes(x=hour, y=aa1)) + geom_line() + geom_point()
ggplot(aahour, aes(x=hour, y=aa1)) + geom_line()
aahour=as.data.frame(aahour)
ggplot(aahour, aes(x=hour, y=aa1)) + geom_line()
aa2=aahour$aa1
ggplot(aahour, aes(x=hour, y=aa1)) + geom_line()
require(reshape)
#Recreate your data frame
user <- gl(3, 1)
Meas1 <- c(0.7, 0.3, 0.3)
Meas2 <- c(0.7, 0.3, 0.3)
Meas3 <- c(0.2, 0.4, 0.4)
group <- c(3, 2, 2)
df <- data.frame(user=user, Meas1=Meas1, Meas2=Meas2, Meas3=Meas3, group=group)
#'melt' the data frame into long format
dfm <- melt(df, id.vars=c("user", "group"))
ggplot(dfm, aes(x=as.numeric(variable), y=value, colour=user)) + geom_line()
barplot(aahour)
ggplot(hours.crimes2, aes(x=hours.crimes2$Var1, y=hours.crimes2$Freq, fill=hours.crimes2$Var2)) + geom_area()
View(disorderlyConduct)
View(homicide.criminal)
homicide.criminal$UCR_General=NULL
table(homicide.criminal)
summary(homicide.criminal)
summary(homicide.criminal$Month)
hist(homicide.criminal$Hour)
qplot(homicide.criminal$Month)
homicideplot=qplot(homicide.criminal$Month)
qplot(arson$Hour)
qplot(arson$Month)
arsonmonth=qplot(arson$Month)
ggplot(data=homicide.criminal, aes(x=homicide.criminal$Month)) +
geom_bar(stat="identity")
arsonmonth=qplot(arson$Month)
arsonmonth
View(disorderlyConduct)
dcmonth=qplot(disorderlyConduct$Month)
dcmonth
duimonth=qploy(dui$Month)
duimonth=qplot(dui$Month)
duimonth
duihour=qplot(dui$Hour)
duihour
duihour=qplot(dui$Hour,binwidth=1000)
duihour
duihour=qplot(dui$Hour,binwidth=100)
duihour
duihour=qplot(dui$Hour,binwidth=10)
duihour
duihour=qplot(dui$Hour,binwidth=1)
duihour
ggplot(dui, aes(x=Hour)) + geom_freqpoly(binwidth=4)
ggplot(dui, aes(x=Hour)) + geom_freqpoly(binwidth=1)
freqpolygondui=ggplot(dui, aes(x=Hour)) + geom_freqpoly(binwidth=1)
ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black")
source('~/.active-rstudio-document')
ggplot(dui, aes(x=Month)) + geom_histogram(fill="white", colour="black")
ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black")
ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=20)
ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=15)
ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=10)
ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=5)
ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
philduihour + scale_x_continuous(breaks=c(0, 1,2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
labels=c('12am','1am','2am','3am','4am','5am','6am','7am','8am','9am','10am','11am','12pm','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm','9pm','10pm','11pm',))
philduihour=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
philduihour + scale_x_continuous(breaks=c(0, 1,2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
labels=c('12am','1am','2am','3am','4am','5am','6am','7am','8am','9am','10am','11am','12pm','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm','9pm','10pm','11pm',))
philduihour + scale_x_continuous(breaks=c(0, 1,2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
labels=c('12am','1am','2am','3am','4am','5am','6am','7am','9am','10am','11am','12pm','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm','9pm','10pm','11pm',))
philduihour=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
philduihour + scale_x_continuous(breaks=c(0, 1,2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
labels=c('12am','1am','2am','3am','4am','5am','6am','7am','8am','9am','10am','11am','12pm','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm','9pm','10pm','11pm'))
philduihour + scale_x_continuous(breaks=c(0, 1,2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
labels=c('12','1','2','3','4','5','6','7','8','9','10','11','12','1','2','3','4','5','6','7','8','9','10','11'))
philduihour=philduihour + scale_x_continuous(breaks=c(0, 1,2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
labels=c('12','1','2','3','4','5','6','7','8','9','10','11','12','1','2','3','4','5','6','7','8','9','10','11'))
library(ggthemes)
philduihour+theme_economist() + scale_colour_economist()
philecon=philduihour+theme_economist() + scale_colour_economist()
philduihour+theme_solarized(light = FALSE) +
scale_colour_solarized("red")
duisolar=philduihour+theme_solarized(light = FALSE) +
scale_colour_solarized("red")
p1=philduihour
p1+theme_wsj() + scale_colour_wsj("colors6", "")
dui_wsj=p1+theme_wsj() + scale_colour_wsj("colors6", "")
dui_wsj=theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank())
dui_wsj
dui_wsj=p1+theme_wsj() + scale_colour_wsj("colors6", "")
dui_wsj+theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank())
dui1=dui_wsj+theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank())
dui_wsj+theme(panel.grid.major = element_blank()
)
dui_wsj
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
"#0072B2", "#D55E00", "#CC79A7")
p1+scale_fill_manual(values=cb_palette)
philduihour=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#56B4E9", colour="black",bins=24)
philduihour
philduihour=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#56B4E9", colour="black",bins=24,position="dodge")
philduihour
philduihour+theme(panel.background = element_rect(fill="lightblue"),)
philduihour+theme(panel.background = element_rect(fill="lightblue")
)
philduihour+theme(panel.background = element_rect(fill="#009E73")
)
philduihour=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#009E73", colour="white",bins=24,position="dodge")
philduihour
philduihour2=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#999999", colour="white",bins=24,position="dodge")
philduihour2
philduihour2=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="0072B2", colour="#999999",bins=24,position="dodge")
philduihour2
philduihour2=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#0072B2", colour="#999999",bins=24,position="dodge")
philduihour2
philduihour2=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#0072B2", colour="#999999",bins=24,position="dodge")+theme((panel.background = element_rect(fill="#009E73"))
)
philduihour2=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#0072B2", colour="#999999",bins=24,position="dodge")+theme(panel.background = element_rect(fill="#009E73"))
philduihour2
philduihour2=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#0072B2", colour="#999999",bins=24,position="dodge")+theme(panel.background = element_rect(fill="#56B4E9"))
philduihour2
philduihour2=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#0072B2", colour="white",bins=24,position="dodge")+theme(panel.background = element_rect(fill="#56B4E9"))
philduihour2
philduihour2=ggplot(dui, aes(x=Hour)) + geom_histogram(fill="#0072B2", colour="black",bins=24,position="dodge")+theme(panel.background = element_rect(fill="#56B4E9"))
philduihour2
theme_wsj <- function(base_size = 12,
color = "brown",
base_family = "sans",
title_family = "mono") {
colorhex <- ggthemes_data$wsj$bg[color]
(theme_foundation(base_size = base_size, base_family = base_family) +
theme(line = element_line(linetype = 1, colour = "black"),
rect = element_rect(fill = colorhex, linetype = 0, colour = NA),
text = element_text(colour = "black"),
title = element_text(family = title_family,
size = rel(2)),
axis.title = element_blank(),
axis.text = element_text(face = "bold", size = rel(1)),
axis.text.x = element_text(colour = NULL),
axis.text.y = element_text(colour = NULL),
axis.ticks = element_line(colour = NULL),
axis.ticks.y = element_blank(),
axis.ticks.x = element_line(colour = NULL),
axis.line = element_line(),
axis.line.y = element_blank(),
legend.background = element_rect(),
legend.position = "top",
legend.direction = "horizontal",
legend.box = "vertical",
panel.grid = element_line(colour = NULL, linetype = 3),
panel.grid.major = element_line(colour = "black"),
panel.grid.major.x = element_blank(),
panel.grid.minor = element_blank(),
plot.title = element_text(hjust = 0, face = "bold"),
plot.margin = unit(c(1, 1, 1, 1), "lines"),
strip.background = element_rect()))
}
philduihour2+wsj_theme()
philduihour2+theme_wsj()
phil4=philduihour2+theme_wsj()
p1
p1+scale_fill_manual(values=cb_palette)
p1+theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank())
p3=p1+theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank())
p3+theme_fivethirtyeight()
538=p3+theme_fivethirtyeight()
fivethirty=p3+theme_fivethirtyeight()
p3+theme_linedraw()
line=p3+theme_linedraw()
p3+theme_minimal()
min=p3+theme_minimal()
minimal=p3+theme_minimal()
p3+theme_economist_white()
p3+theme_tufte()
p3+theme_calc()
p3+theme_pander()
p3+theme_pander(fill="#E69F00")
p3+theme_pander(colours()="#E69F00")
p3+theme_pander(colour="#E69F00")
p3+theme_update()
p3+theme_bw()
p3+theme_dark()
p3+theme_excel()
p3+theme_void()
p3+theme_solid()
p3+theme_solarized_2()
p3+theme_solarized()
p3+theme_solarized_2()
p3+theme_wsj()
p3+theme_solarized_2()
p3+theme_set()
p3+theme_fivethirtyeight()
b1=p3+theme_fivethirtyeight()
b1+theme(axis.text.x = element_text(angle = 45, hjust = 1))
b1+theme(axis.text.x = element_text(angle = 25, hjust = 1))
b1+theme(axis.text.x = element_text(angle = 15, hjust = 1))
b1+theme(axis.text.x = element_text(angle = 5, hjust = 1))
b1+theme(axis.text.x = element_text(angle = -5, hjust = 1))
b1+theme(axis.text.x = element_text(angle = -2, hjust = 1))
b1+theme(axis.text.x = element_text(angle = -1, hjust = 1))
b1
View(disorderlyConduct)
dconduct=ggplot(disorderlyConduct, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=20)
dconuct
conduct
dconduct
dconduct+theme_fivethirtyeight()
hoursb=scale_x_continuous(breaks=c(0, 1,2, 3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
labels=c('12','1','2','3','4','5','6','7','8','9','10','11','12','1','2','3','4','5','6','7','8','9','10','11'))
dconduct+hoursb
dconduct=dconduct+hoursb
dconduct+theme_fivethirtyeight()
dconduct=dconduct+theme_fivethirtyeight()
dconduct=ggplot(rape, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=20)
dconduct=dconduct+theme_fivethirtyeight()
rape=ggplot(rape, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=20)
rape
rape+hoursb
rape=rape+hoursb
rape+theme_fivethirtyeight()
rape=ggplot(rape, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
library(ggplot2)
rape=ggplot(rape, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
rape=ggplot(rape, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=20)
rape
armr2=ggplot(armr,aes(x=Hour))+geom_histogram(fill="white",clour="black",bins=24)
armr2=ggplot(armr,aes(x=Hour))+geom_histogram(fill="white",colour="black",bins=24)
armr
armr2
armr3<-armr2+hoursb
armr3
armr3+theme_fivethirtyeight()
armr3<-armr3+theme_fivethirtyeight()
homicide=ggplot(homicide.criminal, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
homicide
homicide+hoursb
homicide=homicide+hoursb+theme_fivethirtyeight()
homicide
homeless=ggplot(homeless, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
homeless+hoursb
homeless+theme_fivethirtyeight()
homeless=homeless+theme_fivethirtyeight()+hoursb
homeless
prostitution=ggplot(prostitution, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
prositution+theme_fivethirtyeight()+hooursb
prositution+theme_fivethirtyeight()+hoursb
prostitution+theme_fivethirtyeight()+hoursb
prostitution=prostitution+theme_fivethirtyeight()+hoursb
thefts=ggplot(Thefts, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
thefts=thefts+theme_fivethirtyeight()+hoursb
thefts
fraud=ggplot(fraud, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)+theme_fivethirtyeight()+hoursb
fraud
public.drunkenness=ggplot(public.drunkenness, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)
public.drunkenness+hoursb+theme_fivethirtyeight()
pd=public.drunkenness+hoursb+theme_fivethirtyeight()
pd
burglary.res=ggplot(burglary.res, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)+theme_fivethirtyeight()+hoursb
burglary.res
ggplot(burglary.res, aes(x=Hour)) + geom_histogram(fill="white", colour="black",bins=24)+theme_fivethirtyeight()+hoursb
burglary.res
prostitution
thefts
homicide
thefts
prostitution
View(cl)
View(disorderlyConduct)
dconduct
dconduct+hoursb+theme_fivethirtyeight()
install.packages('ggplot2movies')
library(ggplot2movies)
library(ggplot2)
p1=ggplot(movies, aes(x=rating))
print(p1+geom_histogram())
print(p1+geom_histogram(.1))
print(p1+geom_histogram(binwidth = 1))
print(p1+geom_histogram(binwidth = .01))
print(p1+geom_histogram(binwidth = .1))
p1=ggplot(arson5, aes(x=Month))
print(p1+geom_histogram(binwidth = .1))
print(p1+geom_histogram(binwidth = 1))
ggplot(a,aes(x=Hour,y=UCR_General))
print(p1+geom_point())
ggplot(a,aes(x=Hour,y=factor(UCR_General))
)
print(p1+geom_point())
df=mtcars
p1=ggplot(df,aes(x=wt,y=mpg))
print(p1+geom_point())
solar=read.csv(file.choose(),headers=T)
solar=read.csv(file.choose(),header=T)
head(oec)
head(solar)
summary(solar)
View(solar)
ggplot(solar %>% filter(DiscoveryYear > 1991),
aes(x = DiscoveryYear)) +
geom_bar(stat="count") +
scale_x_continuous(breaks = seq(1991, 2016, 1)) +
labs(x = "Discovery Year", y = "Number of Planets Discovered") +
theme_solarized_2() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(solar %>% filter(DiscoveryYear > 1900),
aes(x = DiscoveryYear)) +
geom_bar(stat="count") +
scale_x_continuous(breaks = seq(1991, 2016, 1)) +
labs(x = "Discovery Year", y = "Number of Planets Discovered") +
theme_solarized_2() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(solar %>% filter(DiscoveryYear > 1900),
aes(x = DiscoveryYear)) +
geom_bar(stat="count") +
scale_x_continuous(breaks = seq(1900, 2016, 1)) +
labs(x = "Discovery Year", y = "Number of Planets Discovered") +
theme_solarized_2() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(solar %>% filter(DiscoveryYear > 1987),
aes(x = DiscoveryYear)) +
geom_bar(stat="count") +
scale_x_continuous(breaks = seq(1900, 2016, 1)) +
labs(x = "Discovery Year", y = "Number of Planets Discovered") +
theme_solarized_2() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
1987
nyd=read.csv(file.choose(),header=T)
names(nyd)
summary(nyd)
View(nyd)
library(dplyr)
nyd$Region.Description=NULL
nyd$Region.Code=NULL
nyf=filter(nyd, Year==2003, Gender.Code==2)
View(nyf)
totalnyf=filter(nyd, Selected.Cause.of.Death=='Total', Gender.Code==2)
View(totalnyf)
View(nyf)
totalnyf=filter(nyd, Gender.Code==2)
View(totalnyf)
summarise()
diabnyf=filter(nyd, Cause.code==3, Gender.Code==2)
diabnyf=filter(nyd, Cause.Code==3, Gender.Code==2)
View(diabnyf)
sb=read.csv(file.choose(), header=T)
View(sb)
sb$Brand=NULL
sb$Store.Number
sb$Store.Number=NULL
sb$Phone.Number=NULL
sb$Street.Combined=NULL
sb=unique(sb)
sb$Street.1=NULL
sb$Name=NULL
sb$Street.2=NULL
sb$Street.3=NULL
sb$Current.Timezone.Offset=NULL
sb$Country.Subdivision=NULL
sb$Store.ID=NULL
sb$Postal.Code=NULL
sb$First.Seen=NULL
sb$Ownership.Type=NULL
sb$Timezone=NULL
summary(sb$Country)
summary(sb$City)
ggplot(sb, aes(x=city))
ggplot(sb, aes(x=City))
ed=read.csv(file.choose(), header=T)
View(ed)
ed=read.csv(file.choose(), header=T)
ed=read.csv(file.choose(), header=T)
View(ed)
ed=read.csv(file.choose(), header=T)
ed=read.csv(file.choose(), header=T)
ed$Failure..=as.numeric(ed$Failure..)
ed$Failure../100
ed$Failure../10
ed=read.csv(file.choose(), header=T)
ed=read.csv(file.choose(), header=T)
View(diabnyf)
View(nyd)
View(nyf)
ed=read.csv(file.choose(), header=T)
ed=unique(ed)
names(ed)
ed$Complaint.ID
ed$Complaint.ID=NULL
names(ed)
View(sb)
sb=unique(sb)
summary(sb)
datasets()
flight_edges <- read.delim("~/Downloads/chimps_16091-2010-08-03_17-08-31/flight_edges.tsv", header=FALSE)
View(flight_edges)
colnames(flight_edges)[1]='Short_name'
colnames(flight_edges)[2]='origin'
colnames(flight_edges)[3]='dest'
colnames(flight_edges)[4]='ori'
colnames(flight_edges)[5]='passengers'
colnames(flight_edges)[6]='seats_avail'
flight_edges$V7=NULL
flight_edges$Short_name=NULL
flight_edges$origin=NULL
summary(flight_edges)
summary(ed)
hflights=tbl_df(hflights)
mutate(hflights, loss=hflights$ArrTime-hflights$DepDelay)
View(hflights)
h1=mutate(hflights, loss=hflights$ArrTime-hflights$DepDelay)
h1
View(h1)
mean(h1$loss,na.rm = T)
f1=select(hflights, starts_with("Cancel"), DepDelay)
f1
a1=select(hflights, TailNum, contains("Delay"))
a1
arrange(a1, DepDelay)
arrange(a1, DepDelay, ArrDelay)
a1=filter(a1,!is.na(DepDelay))
summarise(a1,min=min(DepDelay), max=max(DepDelay),avg=mean(DepDelay),med=median(DepDelay))
a2=summarise(a1,min=min(DepDelay), max=max(DepDelay),avg=mean(DepDelay),med=median(DepDelay))
a2
df=read.csv(file.choose(),header = T)
df=tbl_df(df)
df
View(df)
df$INCIDENT_NUMBER=NULL
View(df)
df$NIBRS_CODE=NULL
df$UCR_HIERARCHY=NULL
df$ATT_COMP=NULL
df$LMPD_DIVISION=NULL
View(df)
df$CITY=NULL
df$ID=NULL
q1=select(df, CRIME_TYPE,contains("ASSAULT"))
q1
q1=select(df, TailNum ,contains("ASSAULT"))
q1=select(df, CRIME_TYPE,contains("ASSAULT"))
View(q1)
View(df)

