library(tidyverse)
library(tidycensus)

#Election data from https://github.com/tonmcg/US_County_Level_Election_Results_08-16
election_data<-read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/US_County_Level_Presidential_Results_08-16.csv") %>%
  pivot_longer(total_2008:oth_2016,names_to="var",values_to="count") %>%
  separate(var,into=c("vote","year"),sep="_") %>%
  pivot_wider(names_from=vote,values_from=count)

#Census regions
regions<-read_csv("setup/Counties_State_Regions_XY.csv") %>%
  mutate(fips_code=substr(CTY_TXT,2,6))

#Census data
# vars<-load_variables(year=2010,dataset="acs5",cache=TRUE)
# var_tables<-vars %>%
#   filter(substr(name,1,6) %in% c("B01001","B03002","B24041","B19013","B15003","B16010"))
# write_csv(var_tables,"setup/census_vars.csv")
vars_sel<-read_csv("setup/census_vars.csv")

data2008<-get_acs(geography="county",variables=vars_sel[vars_sel$y2008==1,]$name,year=2010) %>%
  mutate(year=2008)
data2012<-get_acs(geography="county",variables=vars_sel[vars_sel$y2012==1,]$name,year=2014)%>%
  mutate(year=2012)
data2016<-get_acs(geography="county",variables=vars_sel[vars_sel$y2012==1,]$name,year=2018)%>%
  mutate(year=2016)

var_order<-unique(vars_sel$varname)

data_all <-bind_rows(data2008,data2012,data2016) %>%
  left_join(vars_sel %>% rename(variable=name)) %>%
  select(GEOID,year,varname,estimate) %>%
  pivot_wider(names_from=varname,values_from=estimate,values_fill=0) 
names(data_all)

data_all$ed_lesshs <- rowSums(data_all[,c(9,28:41)], na.rm = TRUE)
data_all$ed_hsdiploma<-rowSums(data_all[,c(42,10)], na.rm = TRUE)
data_all$ed_lessba<-rowSums(data_all[,c(11,43,44)], na.rm = TRUE)
data_all$ed_graddeg<-rowSums(data_all[,c(45:46,13)], na.rm = TRUE)

data_all1<-data_all %>%
  select(GEOID,year,all_of(var_order),ed_lesshs,ed_hsdiploma,ed_lessba,ed_ba,ed_graddeg,medinc) %>%
  select(GEOID:indpop_pubadm,ed_totpop,ed_lesshs,ed_hsdiploma,ed_lessba,ed_ba,ed_graddeg,medinc)

#Combine data
combined<-election_data %>%
  left_join(regions) %>%
  left_join(data_all1 %>% 
              rename(fips_code=GEOID) %>% 
              mutate(year=as.character(year))) %>%
  select(fips_code,CTY_TXT,county,State,Region,Division,year,total:oth,totpop:medinc) %>%
  rename(region=Region,
         state=State,
         division=Division) %>%
  rename(gisjn_cty=CTY_TXT)

combined[is.na(combined)]<-0
combined$totpop_ind<-rowSums(combined[18:29],na.rm=TRUE)

#write_csv(combined,"data/elections0816_demog.csv",na = "0")
combined<-combined %>%
  mutate(dem_pct=dem/total*100,
         gop_pct=gop/total*100,
         oth_pct=oth/total*100,
         wht_pop_pct=wht_pop/totpop*100,
         afam_pop_pct=afam_pop/totpop*100,
         asn_pop_pct=asn_pop/totpop*100,
         hisp_pop_pct=hisp_pop/totpop*100,
         other_pop_pct=100-(wht_pop_pct+afam_pop_pct+asn_pop_pct+hisp_pop_pct),
         indpop_ag_pct=indpop_ag/totpop_ind*100,
         indpop_const_pct=indpop_const/totpop_ind*100,
         indpop_manuf_pct=indpop_manuf/totpop_ind*100,
         indpop_trade_pct=indpop_trade/totpop_ind*100,
         indpop_retail_pct=indpop_retail/totpop_ind*100,
         indpop_transport_pct=indpop_transport/totpop_ind*100,
         indpop_info_pct=indpop_info/totpop_ind*100,
         indpop_prof_pct=indpop_prof/totpop_ind*100,
         indpop_ed_pct=indpop_ed/totpop_ind*100,
         indpop_arts_pct=indpop_arts/totpop_ind*100,
         indpop_serv_pct=indpop_serv/totpop_ind*100,
         indpop_pubadm_pct=indpop_pubadm/totpop_ind*100,
         ed_lesshs_pct=ed_lesshs/ed_totpop*100,
         ed_hsdiploma_pct=ed_hsdiploma/ed_totpop*100,
         ed_lessba_pct=ed_lessba/ed_totpop*100,
         ed_ba_pct=ed_ba/ed_totpop*100,
         ed_graddeg_pct=ed_graddeg/ed_totpop*100) %>%
  filter(totpop>0 & dem_pct<100 & gop_pct<100)

write_csv(combined,"data/elections0816_demog_pct.csv")
