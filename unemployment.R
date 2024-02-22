library(tidyverse)
library(janitor)
library(broom)

#change in turnout ~ change in unemployment rate 
#load the dataset
unemploy_2020<-readRDS("unemployment_rate_by_county.rds") |> filter(year==2020)
unemploy_2016<-readRDS("unemployment_rate_by_county.rds") |> filter(year==2016)
vap_2016<-readRDS("age_data_by_county_2016.rds") |> mutate(total_vap=age_18_to_34+age_35_to_44
                                                           +age_45_to_54+age_55_to_64+age_65_to_74+age_75_plus)
vap_2020<-readRDS("age_data_by_county_2020.rds") |> mutate(total_vap=age_18_to_34+age_35_to_44
                                                       +age_45_to_54+age_55_to_64+age_65_to_74+age_75_plus)

vote_2020<-readRDS("election_data_president_2012_2020.rds") |> filter(year==2020) |> 
  left_join(vap_2020,by="fips") |> mutate(vote_rate_2020=total_vote/total_vap) |> 
  select("state.x","fips","state_name.x","name","total_vap","vote_rate_2020")

vote_2016<-readRDS("election_data_president_2012_2020.rds") |> filter(year==2016) |> 
  left_join(vap_2016,by="fips") |> mutate(vote_rate_2016=total_vote/total_vap) |> 
  select("state.x","fips","state_name.x","name","total_vap","vote_rate_2016")

change_unemploy<-left_join(unemploy_2016,unemploy_2020,by="fips") |> mutate(change_unemployrate=unemploymnt_rate.y-unemploymnt_rate.x)
change_turnout<-left_join(vote_2016,vote_2020,by="fips") |> mutate(change_vote_rate=vote_rate_2020-vote_rate_2016)

master<-left_join(change_unemploy,change_turnout,by="fips") |> 
  select("fips","state_name.x.x","county_name.x","change_unemployrate","change_vote_rate")

#average change in unemployment rate 
change_unemploy |> summarise(mean=mean(change_unemployrate, na.rm = T))
change_unemploy_2012|> summarise(mean=mean(change_unemployrate, na.rm = T))

#independent: change_unemployrate
#dependent:change_vote_rate

#regression: 
lm(master$change_vote_rate~master$change_unemployrate) |> summary()
lm(master$change_vote_rate~master$change_unemployrate) |> tidy(conf.int=T, conf.level=.95)
#scatter plot

ggplot(master, aes(x=change_unemployrate, y=change_vote_rate))+
  geom_point(pch=21,alpha = 0.7, color = "black")+
  geom_smooth(method="lm", se=F, color="#dd1c77", lwd=1)+
  theme_grey()+
  labs(title="Relationship between Change in Unemployment Rate and \nChange in Voter Turnout Rate by County (2016-2020)", x="Change in Unemployment Rate", y="Change in Voter Turnout Rate")+
  theme( text = element_text( family = "Arial", size = 14))+
  scale_x_continuous(limits = c(-10, 15,5))+
  scale_y_continuous(limits=c(-.25,0.75))+
  theme(plot.title=element_text(face="bold", size=14))

ggsave("reg1.png")
#map 
library(glue)
library(sf)
library(leaflet)
unemploy<-change_unemploy |> select("fips","county_name.x","change_unemployrate")
geo<-readRDS("usa_county_shapefile.rds")

geo_unemploy<-geo |> left_join(unemploy,by="fips")
state_bounds <- geo_unemploy |> group_by(state) |> 
  summarize(geometry=st_union(geometry))

ggplot(geo_unemploy) +
  geom_sf(aes(fill=change_unemployrate), linewidth=.1, col="white") +
  scale_fill_distiller(palette="RdPu", direction=1,name="Percent Change (%)",limit=c(-10,15))+
  theme_void()+
  labs(title="Change in Unemployment Rate Per County (2016-2020)")+
  geom_sf(data=state_bounds, fill=NA, color="black", linewidth=.15)+
  theme( text = element_text( family = "Arial",size = 12))+
  theme(plot.title=element_text(face="bold",size=14))

ggsave("2016.png")


#difference in voter turnout from 2012 to 2016 ~ unemployment rate 
unemploy_2012<-readRDS("unemployment_rate_by_county.rds") |> filter(year==2012)
vap_2012<-readRDS("age_data_by_county_2012.rds") |> mutate(total_vap=age_18_to_34+age_35_to_44
                                                           +age_45_to_54+age_55_to_64+age_65_to_74+age_75_plus)
vote_2012<-readRDS("election_data_president_2012_2020.rds") |> filter(year==2012) |> 
  left_join(vap_2012,by="fips") |> mutate(vote_rate_2012=total_vote/total_vap) |> 
  select("state.x","fips","state_name.x","name","total_vap","vote_rate_2012")

change_unemploy_2012<-left_join(unemploy_2012,unemploy_2016,by="fips") |> mutate(change_unemployrate=unemploymnt_rate.y-unemploymnt_rate.x)
change_turnout_2012<-left_join(vote_2012,vote_2016,by="fips") |> mutate(change_vote_rate=vote_rate_2016-vote_rate_2012)

master1<-left_join(change_unemploy_2012,change_turnout_2012,by="fips") |> 
  select("fips","state_name.x.x","county_name.x","change_unemployrate","change_vote_rate")

lm(master1$change_vote_rate~master1$change_unemployrate) |> summary()
lm(master1$change_vote_rate~master1$change_unemployrate) |> tidy(conf.int=T, conf.level=.95)

ggplot(master1, aes(x=change_unemployrate, y=change_vote_rate))+
  geom_point(pch=21,alpha = 0.7, color = "black")+
  geom_smooth(method="lm", se=F, color="#dd1c77", lwd=1)+
  theme_grey()+
  labs(title="Relationship between Change in Unemployment Rate and \nChange in Voter Turnout Rate by County (2012-2016)", x="Change in Unemployment Rate", y="Change in Voter Turnout Rate")+
  theme( text = element_text( family = "Arial", size = 14))+
  scale_x_continuous(limits = c(-10, 15,5))+
  scale_y_continuous(limits=c(-.25,0.75))+
  theme(plot.title=element_text(face="bold",size=14))

ggsave("reg2.png")  

#map or unemployment rate change in 2012-2016
unemploy1<-change_unemploy_2012 |> select("fips","county_name.x","change_unemployrate")
geo<-readRDS("usa_county_shapefile.rds")

geo_unemploy1<-geo |> left_join(unemploy1,by="fips")
state_bounds <- geo_unemploy1 |> group_by(state) |> 
  summarize(geometry=st_union(geometry))

ggplot(geo_unemploy1) +
  geom_sf(aes(fill=change_unemployrate), linewidth=.1, col="white") +
  scale_fill_distiller(palette="RdPu", direction=1,name="Percent Change (%)",limit=c(-10,15))+
  theme_void()+
  labs(title="Change in Unemployment Rate Per County (2012-2016)")+
  geom_sf(data=state_bounds, fill=NA, color="black", linewidth=.15)+
  theme( text = element_text( family = "Arial",size = 12))+
  theme(plot.title=element_text(face="bold",size=14))

ggsave("2012.png")

