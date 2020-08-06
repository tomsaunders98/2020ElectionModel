library(tidyverse)
library(bsts)

### Load Poll

Polls2008 <- read_csv("2008Polls.csv") # Polls taking from RealClearPolitics
#https://www.realclearpolitics.com/epolls/2008/president/us/general_election_mccain_vs_obama-225.html#!


### Wrangle Data


opinion <- Polls2008
opinion <- opinion %>%
  rename(House = Poll, "Dems" = "Obama (D)", "GOP" = "McCain (R)") %>%
  mutate(
    PublishedDate = stringr::str_split(Date, " ", n = 3, simplify = TRUE)[,3],
    PublishedDate = as.Date(paste(PublishedDate, "/2008", sep=""), format = "%m/%d/%Y"),
    House = as.factor(House),
    Company_Code = as.numeric(House),
    Sample = as.numeric(gsub("[a-zA-Z ]", "", Sample)),
    Dems = as.numeric(Dems),
    GOP = as.numeric(GOP),
    squaresample = sqrt(Sample)
  ) %>%
  drop_na(Sample) %>%
  subset(PublishedDate < as.Date("11/4/2008", format = "%m/%d/%Y")) %>%
  arrange(PublishedDate) 
  

###Carry out DLMs for Republicans and Democrats


#Democrats
Dems = opinion$Dems
tsDems = ts(Dems)
DemsPrior = SdPrior(sigma.guess=50)
ss = AddLocalLevel(y=Dems, sigma.prior = DemsPrior) 
model = bsts(Dems, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)
Demtrend = data.frame(model$state.contributions)
x=colMeans(Demtrend)
Demdata = data.frame(Dem=x, Demlb=x - 1.96*summary(model)$residual.sd, 
                   Demub=x + 1.96*summary(model)$residual.sd, 
                   date=opinion$PublishedDate, house=opinion$House)

# Republicans
GOP = opinion$GOP
tsGOP = ts(GOP)
GOPPrior = SdPrior(sigma.guess=50)
ss = AddLocalLevel(y=GOP, sigma.prior = GOPPrior) 
model = bsts(GOP, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)
GOPtrend = data.frame(model$state.contributions)
x=colMeans(GOPtrend)
GOPdata = data.frame(GOP=x, GOPlb=x - 1.96*summary(model)$residual.sd, 
                     GOPub=x + 1.96*summary(model)$residual.sd, 
                     date=opinion$PublishedDate, house=opinion$House)

### Plot point prediction for each party


ggplot() +
  geom_point(data=opinion, aes(x=PublishedDate, y=Dems, colour="firebrick3", alpha=0.8)) +
  geom_point(data=opinion, aes(x=PublishedDate, y=GOP, colour="cornflowerblue", alpha=0.8)) +
  geom_line(data=Demdata,aes(x=date, y=Dem, colour="firebrick3", alpha = 1)) +
  geom_line(data=GOPdata,aes(x=date, y=GOP, colour="cornflowerblue", alpha = 1)) +
  geom_label_repel() +
  geom_line(aes(y=52.9, x=GOPdata$date,  colour="firebrick3"),linetype="dotted") +
  geom_line(aes(y=45.6, x=GOPdata$date,  colour="cornflowerblue"),linetype="dotted") +
  annotate("text", x=as.Date("2008-03-01"), y=53.5, label= "Actual Dem Vote %: 52.9, Predicted: 51.5 %", colour="cornflowerblue") + 
  annotate("text", x=as.Date("2008-03-01"), y=46, label= "Actual GOP Vote %: 45.6, Predicted: 44.2%", colour="firebrick3") + 
  ggtitle("DLM for 2008 Presidential Race") +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%B",             limits=c(as.Date("2008-01-01"), 
                        as.Date("2008-11-03"))) +
  theme(strip.text=element_text(size=18, face="bold")) +
  theme(axis.title.x=element_blank()) + 
  theme(strip.background=element_rect(fill="skyblue")) +
  labs(y="Result (%)") +
  theme(legend.position="none") +
  theme(axis.title.y=element_text(size=22, vjust=1, face="bold")) +
  theme(axis.text.x=element_text(size=16, vjust=0.5, angle=30)) +
  theme(plot.title=element_text(vjust=1, size=24, face="bold"))


### Illustrative Normal Distribution of Dem percentage of winning

Mean <- Demdata[which.max(Demdata$date), "Dem"]
UB <- Demdata[which.max(Demdata$date), "Demub"]
sd <- UB - Mean

x=seq(0,100,length=200)
y=dnorm(x, mean=Mean, sd=sd)
plot(x,y,type="l", lwd=2, col="blue")
x=seq(50,100,length=200)
y=dnorm(x, mean=Mean, sd=sd)
polygon(c(50,x,100),c(0,y,0),col="gray")
Prob = 1-pnorm(50, mean=Mean, sd=sd)



