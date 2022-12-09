#### INTAKE DATA SURVEY #####
##Load packages
library(tidyverse)
library(readxl)
library(table1)
library(likert)
library(RColorBrewer)

##Load data (text) and question key
setwd("C:/Users/lexis/Documents/Masters/Intake Data/Data")
intake<-read.csv("Intake Data - July 6 2022.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

#Add respondent ID
intake_id<-intake %>% 
  mutate(resp_id = 1:n())%>% 
  select(resp_id, everything())

#Add location region
region_data<-read.csv("regions.csv", stringsAsFactors = FALSE)
intake_region = merge(x=intake_id,y=region_data,by="state_prov",all.x=TRUE)

intake_region<-intake_region%>%
  dplyr::rename(region = ï..region)

#Join with Updated surrender categories
surr_cat<-read.csv("sb_eagan.csv", stringsAsFactors = FALSE)

#Separate out order
intake_order<- intake_region%>%
  separate(order, c("order_1","order_2", "order_3", "order_4"), sep="-Datainput")

#Change order to scenario number
intake_order$order_1[intake_order$order_1=="Scenario#1"] <- 1
intake_order$order_1[intake_order$order_1=="Scenario#2"] <- 2
intake_order$order_1[intake_order$order_1=="Scenario#3"] <- 3
intake_order$order_1[intake_order$order_1=="Scenario#4"] <- 4

intake_order$order_2[intake_order$order_2=="Scenario#1"] <- 1
intake_order$order_2[intake_order$order_2=="Scenario#2"] <- 2
intake_order$order_2[intake_order$order_2=="Scenario#3"] <- 3
intake_order$order_2[intake_order$order_2=="Scenario#4"] <- 4

intake_order$order_3[intake_order$order_3=="Scenario#1"] <- 1
intake_order$order_3[intake_order$order_3=="Scenario#2"] <- 2
intake_order$order_3[intake_order$order_3=="Scenario#3"] <- 3
intake_order$order_3[intake_order$order_3=="Scenario#4"] <- 4

intake_order$order_4[intake_order$order_4=="Scenario#1"] <- 1
intake_order$order_4[intake_order$order_4=="Scenario#2"] <- 2
intake_order$order_4[intake_order$order_4=="Scenario#3"] <- 3
intake_order$order_4[intake_order$order_4=="Scenario#4"] <- 4

#Want the total number of reasons that the person listed by row
intake_sum<-intake_order%>%
  mutate(s1_sum = rowSums(!is.na(across(starts_with('s1_reason')))))%>%
  mutate(s2_sum = rowSums(!is.na(across(starts_with('s2_reason')))))%>%
  mutate(s3_sum = rowSums(!is.na(across(starts_with('s3_reason')))))%>%
  mutate(s4_sum = rowSums(!is.na(across(starts_with('s4_reason')))))

#Filter - finished 
intake_complete <- intake_sum %>% mutate_all(na_if,"") %>%
  filter(finished == "TRUE")
  
#Filter - responsible for intake
intake_complete_resp <- intake_complete %>% mutate_all(na_if,"") %>%
  filter(responsible == "Yes")

##Remove two respondents who were cat-only organizations
intake_complete_cat <- intake_complete_resp %>%
  filter(response_id != "R_3dQSHhuAyiJ4w9h")%>%
  filter(response_id != "R_sj12xMthdRGezUl")

#Filter - organization does intake through owner surrender
intake_filtered <- intake_complete_cat %>% mutate_all(na_if,"") %>%
  filter(owner_surrender == "Yes")

#average duration
mean(intake_filtered$duration_sec)/60
intake_filtered %>% 
  summarise(sd = (sd(duration_sec, na.rm = T))/60,
            mean = (mean(duration_sec, na.rm = T))/60,
            range = paste(((min(duration_sec, na.rm = T))/60), "-", ((max(duration_sec, na.rm = T)))/60),
            n = sum(!is.na(duration_sec)), 
            med = (median(duration_sec)))

#Location of orgs
state<-intake_filtered%>%
  group_by(state_prov)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))

region<-intake_filtered%>%
  group_by(region)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))

#Type of organizations 
org_order <- c('Municipally (governmentally) operated', 'Private with municipal contract(s)', 
  'Private rescue with shelter facility', 'Private shelter',
  'Private foster-based rescue', 'Other:')
ggplot(intake_filtered, aes(x = factor(org_type, level = org_order))) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+labs(
    x = "Organization type",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

org_type<-intake_filtered%>%
  group_by(org_type_recode)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))
write.csv(region, 'intake_region.csv')


org_recode_order <- c('Municipally (governmentally) operated', 'Private with municipal contract(s)', 'Private shelter/rescue', 'Other')
ggplot(intake_filtered, aes(x = factor(org_type_recode, level = org_recode_order))) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+labs(
    x = "Organization type",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#How long have you worked?
work_order <- c('Less than six months', '6 months - 1 year', 
                 '> 1 -  2 years', '> 2 - 5 years',
                '5 + years')
ggplot(intake_filtered,aes(x = factor(long_work, level = work_order))) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+labs(
    x = "Organization type",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_work<-intake_filtered%>%
  group_by(long_work)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))
write.csv(long_work, 'intake_long_work.csv')

#Type of intake
intake_order <- c('Limited intake (only when space in the shelter is available for that animal)',
                'Appointment-based', 'Open intake (no appointment)',
                'Other:')
ggplot(intake_filtered,aes(x = factor(intake_type, level = intake_order))) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+labs(
    x = "Organization type",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

intake_recode_order <- c('Limited intake (only when space in the shelter is available for that animal)',
                  'Appointment-based', 'Open intake (no appointment)',
                  'Mixed intake types')
ggplot(intake_filtered,aes(x = factor(intake_type_recode, level = intake_recode_order))) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+labs(
    x = "Organization type",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

intake_type<-intake_filtered%>%
  group_by(intake_type_recode)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))
write.csv(long_work, 'intake_long_work.csv')

#Yearly dog intake
ggplot(intake_filtered, aes(yearly_dog_intake)) +
  geom_histogram(bins=1000)+
  labs(
    x = "Yearly dog intake",
    y = "Count",
  ) +
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  geom_vline(aes(xintercept=mean(yearly_dog_intake), color="red"),
             linetype="dashed")

intake_filtered %>% 
  summarise(sd = sd(yearly_dog_intake),
            mean = mean(yearly_dog_intake),
            min = min(yearly_dog_intake),
            med = median(yearly_dog_intake),
            max = max(yearly_dog_intake),
            n = sum(!is.na(yearly_dog_intake)))

#% intake is owner surrender
ggplot(intake_filtered, aes(percent_owner_surrender)) +
  geom_histogram(bins=18)+
  labs(
    x = "Percent of yearly dog intake that is owner surrendered",
    y = "Count",
  ) +
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_vline(aes(xintercept=mean(percent_owner_surrender), color="red"),
             linetype="dashed")

intake_filtered %>% 
  summarise(sd = sd(percent_owner_surrender),
            mean = mean(percent_owner_surrender),
            min = min(percent_owner_surrender),
            med = median(percent_owner_surrender),
            max = max(percent_owner_surrender),
            n = sum(!is.na(percent_owner_surrender)))

#Shelter Software used
soft<-intake_filtered%>%
  group_by(shelter_soft)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))

soft_order <- c('Petpoint', 'ShelterBuddy', 'Chameleon', 
                'Shelterluv', 'Animal Shelter Manager', 'Petstablished',
                'Other:', 'Do not use shelter software')
ggplot(intake_filtered, aes(x = factor(shelter_soft, level = soft_order))) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,0.5))+labs(
    x = "Shelter Software",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

soft_order <- c('Petpoint', 'ShelterBuddy', 'Chameleon', 
                'Shelterluv', 'Animal Shelter Manager','Other', 'Do not use shelter software')
ggplot(intake_filtered, aes(x = factor(shelter_soft_recode1, level = soft_order))) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,0.5))+labs(
    x = "Shelter Software",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

soft<-intake_filtered%>%
  group_by(shelter_soft_recode1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))

#Shelter software by org type
intake_filtered$shelter_soft_recode1<- factor(intake_filtered$shelter_soft_recode1, levels = c("ShelterBuddy","Petpoint",  "Chameleon", "Shelterluv", "Other", "Do not use shelter software"))
soft_org<-ggplot(intake_filtered, aes(x = factor(org_type_recode, level = org_recode_order), fill = shelter_soft_recode1)) +
  geom_bar(aes(y=..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0,0.5))+labs(
    x = "",
    y = "Percent",
  ) +
  labs(fill="Shelter software")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values = c("#D7DECF",  "#DBA8A7","#F4E7CD", "#C8D5E5", "#FADAC7","#E4DCF9" ))

tiff('soft_org.tiff', units="in", width=11, height=7, res=300, compression = 'lzw')
soft_org
dev.off()

soft<-intake_filtered%>%
  group_by(shelter_soft_recode1, org_type_recode)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))

write.csv(soft, "soft_org_prop.csv")

#Fisher's exact test

fisher.test(intake_filtered$shelter_soft_recode1, intake_filtered$org_type_recode, simulate.p.value=TRUE)


#Intake type and organization type
fisher.test(intake_filtered$intake_type_recode, intake_filtered$org_type_recode, simulate.p.value=TRUE)


#One-way ANOVA
dog_aov <- aov(yearly_dog_intake ~ org_type_recode, data = intake_filtered)
summary(dog_aov)

os_aov <- aov(percent_owner_surrender ~ org_type_recode, data = intake_filtered)
summary(os_aov)

#Intake type by org type
intake_org<-ggplot(intake_filtered,aes(x = factor(org_type_recode, level = org_recode_order), fill = intake_type_recode)) +
  geom_bar(aes(y=..count../sum(..count..)), position="fill") +
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  scale_x_discrete(labels=c('Municipally operated', 'Private with municipal contracts', 'Private shelter/rescue'))+
  labs(fill="Intake type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(labels = c("Appointment-based intake", "Limited intake", "Mixed intake types", "Open intake"), values = c("#D7DECF",  "#F4E7CD", "#C8D5E5", "#FADAC7" ))
intake_org

tiff('intake_org.tiff', units="in", width=9, height=7, res=300, compression = 'lzw')
intake_org
dev.off()

intake_org<-intake_filtered%>%
  group_by(intake_type_recode, org_type_recode)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(desc(n))

write.csv(intake_org, "intake_org.csv")

#Percent owner surrender by org type
owner_surr_org<-intake_filtered %>% 
  group_by(org_type_recode)%>%
  summarise(sd = sd(percent_owner_surrender),
            mean = mean(percent_owner_surrender),
            min = min(percent_owner_surrender),
            med = median(percent_owner_surrender),
            max = max(percent_owner_surrender),
            n = sum(!is.na(percent_owner_surrender)))

ggplot(intake_filtered) +
  aes(x = percent_owner_surrender, color=org_type_recode, fill=org_type_recode) +
  geom_density(alpha = 0.3) +
  labs(
    x = "Percent of intake that is owner surrender",
    y = "Proportion",
  ) +
  theme_bw()+
  labs(fill="Organization type", colour = "Organization type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Yearly dog intake by org type
yearly_org<-intake_filtered %>% 
  group_by(org_type_recode)%>%
  summarise(sd = sd(yearly_dog_intake),
            mean = mean(yearly_dog_intake),
            min = min(yearly_dog_intake),
            med = median(yearly_dog_intake),
            max = max(yearly_dog_intake),
            n = sum(!is.na(duration_sec)))

ggplot(intake_filtered) +
  aes(x = yearly_dog_intake, color=org_type_recode, fill=org_type_recode) +
  geom_density(alpha = 0.3) +
  labs(
    x = "Percentage of intake that is owner surrender",
    y = "Proportion",
  ) +
  theme_bw()+
  labs(fill="Organization type", colour = "Organization type")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Yearly dog intake by intake type
yearly_intake<-intake_filtered %>% 
  group_by(intake_type)%>%
  summarise(sd = sd(yearly_dog_intake),
            mean = mean(yearly_dog_intake),
            min = min(yearly_dog_intake),
            med = median(yearly_dog_intake),
            max = max(yearly_dog_intake),
            n = sum(!is.na(duration_sec)))


#Plot the average number of reasons by scenario 
avg_reasons<-intake_filtered %>%
  select(contains("sum"))%>%
  pivot_longer(s1_sum:s4_sum,names_to = "scenario", values_to = "sum")

avg_reasons$scenario[avg_reasons$scenario=="s1_sum"] <- 1
avg_reasons$scenario[avg_reasons$scenario=="s2_sum"] <- 2
avg_reasons$scenario[avg_reasons$scenario=="s3_sum"] <- 3
avg_reasons$scenario[avg_reasons$scenario=="s4_sum"] <- 4

reasons_scenario<-ggplot(avg_reasons) +
  aes(x = scenario,
    y = sum, fill = scenario) +
  geom_boxplot() +
  geom_jitter(width = 0.15)+
  labs(
    x = "Scenario",
    y = "Number of reasons",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values = c("#D7DECF",  "#F4E7CD", "#C8D5E5", "#FADAC7" ))
  

tiff('reasons_scenario.tiff', units="in", width=12, height=7, res=300, compression = 'lzw')
reasons_scenario
dev.off()

ggplot_build(reasons_scenario)$data

cor(as.numeric(avg_reasons$scenario), avg_reasons$sum)

avg_reasons %>% 
  summarise(mean = mean(sum, na.rm = T),
            sd = sd(sum, na.rm = T),
            min = min(sum, na.rm = T),
            med = median(sum, na.rm = T),
            max = max(sum, na.rm = T),
            n = sum(!is.na(sum)))

avg_reasons %>% 
  group_by(scenario)%>%
  summarise(mean = mean(sum, na.rm = T),
            sd = sd(sum, na.rm = T),
            min = min(sum, na.rm = T),
            med = median(sum, na.rm = T),
            max = max(sum, na.rm = T),
            n = sum(!is.na(sum)))

avg_aov <- aov(sum ~ scenario, data = avg_reasons)
summary(avg_aov)

##histogram version - number of reasons TOTAL
ggplot(avg_reasons) +
  aes(x = sum) +
  geom_histogram(bins=6) +
  labs(
    x = "",
    y = "Number of reasons",
  ) +
  scale_x_continuous(breaks = c(1:6))+
  scale_y_continuous(breaks = seq(from = 0, to = 150, by = 5), expand = c(0,0))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Number of human and animal reasons
reason<-intake_filtered %>%
  select(contains("reason"))%>%
  select(-c(reason_accurate))%>%
  pivot_longer(contains("reason"),names_to = "scenario", values_to = "reason")%>%
  drop_na()

reason_cat<-merge(x=reason,y=surr_cat,by.x="reason", by.y="surrender_reason",all.x=TRUE)

cat_count<-reason_cat%>%
  mutate(rank = case_when(grepl("reason1", scenario) ~ '1',
                        grepl("reason2", scenario) ~ '2',
                        grepl("reason3", scenario) ~ '3',
                        grepl("reason4", scenario) ~ '4',
                        grepl("reason5", scenario) ~ '5',
                        grepl("reason6", scenario) ~ '6'))

ggplot(cat_count)+
  geom_histogram(aes(x = rank, fill = surrender_category),alpha = 0.5, position = "identity", stat = "count", bins=6) +
  labs(
    x = "",
    y = "",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  facet_wrap(~surrender_category)

#Human and animal individual
reason_indv<-intake_filtered %>%
  select(contains("reason"),resp_id)%>%
  select(-c(reason_accurate))%>%
  pivot_longer(contains("reason"),names_to = "scenario", values_to = "reason")%>%
  drop_na()

reason_indv_cat<-merge(x=reason_indv,y=surr_cat,by.x="reason", by.y="surrender_reason",all.x=TRUE)

reason_indv_sum<-reason_indv_cat%>%
  group_by(resp_id)%>%
  summarise(human = sum(surrender_category == 'Human'),
            animal = sum(surrender_category == 'Animal'))%>%
  summarise(prop_human = (human/(human+animal)),
            prop_animal = (animal/(human+animal)))

summary(reason_indv_sum$prop_human)
sd(reason_indv_sum$prop_human)
  
ggplot(reason_indv_sum)+
  geom_histogram(aes(x = prop_human), bins = 20) +
  labs(
    x = "",
    y = "",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Primary reasons, Human and animal individual 
reason1_indv<-intake_filtered %>%
  select(contains("reason1"),resp_id)%>%
  pivot_longer(contains("reason1"),names_to = "scenario", values_to = "reason")%>%
  drop_na()

reason1_indv_cat<-merge(x=reason1_indv,y=surr_cat,by.x="reason", by.y="surrender_reason",all.x=TRUE)

reason1_indv_sum<-reason1_indv_cat%>%
  group_by(resp_id)%>%
  summarise(human = sum(surrender_category == 'Human'),
            animal = sum(surrender_category == 'Animal'))%>%
  summarise(prop_human = (human/(human+animal)),
            prop_animal = (animal/(human+animal)))

summary(reason1_indv_sum$prop_human)
sd(reason1_indv_sum$prop_human)

ggplot(reason_indv_sum)+
  geom_histogram(aes(x = prop_human), bins = 20) +
  labs(
    x = "",
    y = "",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


#Number of reasons by order 
reason_order<-intake_filtered %>%
  select(contains("order"), contains("sum"))%>%
  pivot_longer(contains("sum"),names_to = "scenario", values_to = "responses")

reason_order$scenario[reason_order$scenario=="s1_sum"] <- 1
reason_order$scenario[reason_order$scenario=="s2_sum"] <- 2
reason_order$scenario[reason_order$scenario=="s3_sum"] <- 3
reason_order$scenario[reason_order$scenario=="s4_sum"] <- 4

reason_order_new <- reason_order %>%
  mutate(order = case_when(reason_order$scenario == reason_order$order_1 ~ '1',
                           reason_order$scenario == reason_order$order_2 ~ '2',
                           reason_order$scenario == reason_order$order_3 ~ '3',
                           reason_order$scenario == reason_order$order_4 ~ '4'))

ggplot(reason_order_new) +
  aes(x = order,
      y = responses) +
  geom_boxplot() +
  geom_jitter(width = 0.15)+
  labs(
    x = "order",
    y = "Number of reasons",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

cor(as.numeric(reason_order_new$order), reason_order_new$responses)

reason_order_new %>% 
  group_by(order)%>%
  summarise(mean = mean(responses, na.rm = T),
            sd = sd(responses, na.rm = T),
            min = min(responses, na.rm = T),
            med = median(responses, na.rm = T),
            max = max(responses, na.rm = T),
            n = sum(!is.na(responses)))

ord_aov <- aov(responses ~ order, data = reason_order_new)
summary(ord_aov)

#Number of UNIQUE REASONS for each scenario - TOTAL
all_reasons<-intake_filtered %>%                    
  select(contains("reason"), -reason_accurate)%>%
  pivot_longer(s1_reason1:s4_reason10,names_to = "rank", values_to = "reason")%>%
  mutate(scenario = case_when(grepl("s1", rank) ~ '1',
                              grepl("s2", rank) ~ '2',
                              grepl("s3", rank) ~ '3',
                              grepl("s4", rank) ~ '4'))

unique_reasons<-all_reasons%>%
  group_by(scenario)%>%       
  summarise(Unique_Elements = n_distinct(na.omit(reason)))

unique_reasons

#Number of UNIQUE REASONS for each scenario - BY RANK
all_reasons<-intake_filtered %>%                    
  select(contains("reason"), -reason_accurate)%>%
  pivot_longer(s1_reason1:s4_reason10,names_to = "rank", values_to = "reason")%>%
  mutate(scenario = case_when(grepl("s1", rank) ~ '1',
                              grepl("s2", rank) ~ '2',
                              grepl("s3", rank) ~ '3',
                              grepl("s4", rank) ~ '4',
                              grepl("s5", rank) ~ '5',
                               grepl("s6", rank) ~ '6'))%>%
  mutate(rank = case_when(grepl("reason1", rank) ~ '1',
                          grepl("reason2", rank) ~ '2',
                          grepl("reason3", rank) ~ '3',
                          grepl("reason4", rank) ~ '4',
                          grepl("reason5", rank) ~ '5',
                          grepl("reason6", rank) ~ '6'))

rank_reasons<-all_reasons%>%
  group_by(scenario, rank)%>%       
  summarise(Unique_Elements = n_distinct(na.omit(reason)))

write.csv(rank_reasons, "rank_reasons.csv")

#Mean number of unique reasons by rank
rank_reasons%>%
  group_by(scenario)%>%       
  summarise(mean = mean(Unique_Elements, na.rm = T),
            sd = sd(Unique_Elements, na.rm = T),
            min = min(Unique_Elements, na.rm = T),
            med = median(Unique_Elements, na.rm = T),
            max = max(Unique_Elements, na.rm = T),
            n = sum(!is.na(Unique_Elements)))

#Number of UNIQUE BREEDS for each scenario
unique_breeds<-intake_filtered %>%                    
  select(contains("breed"), -reason_accurate)%>%
  pivot_longer(s1_breed1:s4_breed2,names_to = "rank", values_to = "breed")%>%
  mutate(scenario = case_when(grepl("s1", rank) ~ '1',
                              grepl("s2", rank) ~ '2',
                              grepl("s3", rank) ~ '3',
                              grepl("s4", rank) ~ '4'))%>%
  group_by(rank, scenario)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(breed)))

unique_breeds

#Number of UNIQUE COLOURS for each scenario
unique_colours<-intake_filtered %>%                    
  select(contains("colour"))%>%
  pivot_longer(s1_colour1:s4_colour2,names_to = "rank", values_to = "colour")%>%
  mutate(scenario = case_when(grepl("s1", rank) ~ '1',
                              grepl("s2", rank) ~ '2',
                              grepl("s3", rank) ~ '3',
                              grepl("s4", rank) ~ '4'))%>%
  group_by(rank, scenario)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

unique_colours


#Scenario 1: 
#1 - Behaviour (too active) 
#2 - Life change (new baby/divorce/relation etc.) 
#3 - Behaviour (growls/lunges/claws/hisses)
#4 - No time for pet/too much responsibility

# Scenario 1 - Surrender reason
long_rank_s1<-intake_filtered %>%
  select(contains("s1_reason"))%>%
  pivot_longer(s1_reason1:s1_reason6,names_to = "rank", values_to = "reason")

long_rank_s1$rank<-factor(long_rank_s1$rank, ordered = TRUE, 
                          levels = c("s1_reason1", "s1_reason2", "s1_reason3", "s1_reason4",
                                     "s1_reason5", "s1_reason6"))

long_rank_s1$rank <- recode_factor(long_rank_s1$rank, s1_reason1 = "1", s1_reason2 = "2", s1_reason3 = "3", s1_reason4 = "4",
                                  s1_reason5 = "5", s1_reason6 = "6")

long_rank_s1_sum<-long_rank_s1%>%
  group_by(rank, reason)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))
long_rank_s1_sum
  
ggplot(data=long_rank_s1_sum, aes(x=rank, y=frequency, fill=reason))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 1 - Combinations of reasons
long_combo_s1<-intake_filtered %>%
  select(contains("s1_reason"), resp_id)
combo_s1<-long_combo_s1%>%
  count(s1_reason1,s1_reason2,s1_reason3,s1_reason4, s1_reason5)

no_order_combo_s1<-combo_s1 %>%
  mutate(comblist = apply(.[1:5], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>%
  select(comblist, n) %>%
  unnest(comblist) %>%
  group_by(comblist) %>%
  summarise(x = sum(n))

write.csv(no_order_combo_s1, "no_order_combo_s1.csv")

#Scenario 1 - Surrender human/animal reason
long_rank_s1_cat = merge(x=long_rank_s1,y=surr_cat,by.x="reason", by.y="surrender_reason",all.x=TRUE)

long_rank_s1_cat_sum<-long_rank_s1_cat%>%
  group_by(rank, surrender_category)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=long_rank_s1_cat_sum, aes(x=rank, y=frequency, fill=surrender_category))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason category")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_s1_cat_meanrank<-long_rank_s1_cat %>%
  select(surrender_category, rank)%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(surrender_category) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
long_s1_cat_meanrank

#Scenario 1 - Reason Grouping 1
long_rank_s1_surr1_sum<-long_rank_s1_cat%>%
  group_by(rank, surrender_1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=long_rank_s1_surr1_sum, aes(x=rank, y=frequency, fill=surrender_1))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason category")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_s1_surr1_meanrank<-long_rank_s1_cat %>%
  select(surrender_1, rank)%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(surrender_1) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
long_s1_surr1_meanrank

#Scenario 1 - Number of unique reasons by rank
long_rank_s1%>%
  summarise(Unique_Elements = n_distinct(na.omit(reason)))

s1_unique<-long_rank_s1%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(reason)))
s1_unique

#Scenario 1 - Average Ranking and Frequency
s1_meanrank<-intake_filtered %>%
  select(contains("s1_reason"))%>%
  pivot_longer(s1_reason1:s1_reason10,names_to = "rank", values_to = "reason")%>%
  mutate(rank = case_when(grepl("reason1", rank) ~ '1',
                              grepl("reason2", rank) ~ '2',
                              grepl("reason3", rank) ~ '3',
                              grepl("reason4", rank) ~ '4',
                              grepl("reason5", rank) ~ '5',
                              grepl("reason6", rank) ~ '6'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(reason) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s1_meanrank

write.csv(s1_meanrank,"s1_meanrank.csv")

#Scenario 1 - Breed
breed_s1<-intake_filtered %>%
  select(s1_breed1,s1_breed2)%>%
  pivot_longer(s1_breed1:s1_breed2, names_to = "rank", values_to = "breed")
breed_s1$breed<-str_squish(breed_s1$breed)

breed_s1$rank[breed_s1$rank=="s1_breed1"] <- "Primary Breed"
breed_s1$rank[breed_s1$rank=="s1_breed2"] <- "Secondary Breed"

breed_s1_sum<-breed_s1%>%
  group_by(rank, breed)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=breed_s1_sum, aes(x=rank, y=frequency, fill=breed))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 1 - Breed (by Primary and Secondary)
breed1_s1<-intake_filtered%>%
  group_by(s1_breed1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

breed2_s1<-intake_filtered%>%
  group_by(s1_breed2)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

long_breed_s1<-intake_filtered %>%
  select(contains("s1_breed"), resp_id)%>%
  count(s1_breed1, s1_breed2)

#Scenario 1 - Unique number of breeds
breed_s1%>%
  summarise(Unique_Elements = n_distinct(na.omit(breed)))

s1_breed_unique<-breed_s1%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(breed)))
s1_breed_unique

#Scenario 1 - Frequency breed
s1_meanbreed<-intake_filtered %>%
  select(contains("s1_breed"))%>%
  pivot_longer(s1_breed1:s1_breed2,names_to = "rank", values_to = "breed")%>%
  mutate_all(str_squish)%>%
  mutate(rank = case_when(grepl("breed1", rank) ~ '1',
                          grepl("breed2", rank) ~ '2'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(breed) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s1_meanbreed

write.csv(s1_meanbreed,"s1_meanbreed.csv")


#Scenario 1 - Colour
colour_s1<-intake_filtered %>%
  select(s1_colour1, s1_colour2)%>%
  pivot_longer(s1_colour1:s1_colour2, names_to = "rank", values_to = "colour")

colour_s1$rank[colour_s1$rank=="s1_colour1"] <- "Primary Colour"
colour_s1$rank[colour_s1$rank=="s1_colour2"] <- "Secondary Colour"

colour_s1_sum<-colour_s1%>%
  group_by(rank, colour)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=colour_s1_sum, aes(x=rank, y=frequency, fill=colour))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 1 - Unique number of colours
colour_s1%>%
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

s1_colour_unique<-colour_s1%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

s1_colour_unique

#Scenario 1 - Frequency colour
s1_meancolour<-intake_filtered %>%
  select(contains("s1_colour"))%>%
  pivot_longer(s1_colour1:s1_colour2,names_to = "rank", values_to = "colour")%>%
  mutate(rank = case_when(grepl("colour1", rank) ~ '1',
                          grepl("colour2", rank) ~ '2'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(colour) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s1_meancolour

write.csv(s1_meancolour,"s1_meancolour.csv")

#Scenario 1 - Colour (by Primary and Secondary)
colour1_s1<-intake_filtered%>%
  group_by(s1_colour1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

colour2_s1<-intake_filtered%>%
  group_by(s1_colour2)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

long_colour_s1<-intake_filtered %>%
  select(contains("s1_colour"), resp_id)%>%
  count(s1_colour1, s1_colour2)

#Scenario 2: 
#1 - Life Change (human illness/injury etc)
#2 - Behaviour (destructive)
#3 - Can't afford vet expenses for procedure/illness
#4 - Pet has anxiety

# Scenario 2 - Surrender reason
long_rank_s2<-intake_filtered %>%
  select(contains("s2_reason"))%>%
  pivot_longer(s2_reason1:s2_reason6,names_to = "rank", values_to = "reason")

long_rank_s2$rank<-factor(long_rank_s2$rank, ordered = TRUE, 
                          levels = c("s2_reason1", "s2_reason2", "s2_reason3", "s2_reason4",
                                     "s2_reason5", "s2_reason6"))

long_rank_s2$rank <- recode_factor(long_rank_s2$rank, s2_reason1 = "1", s2_reason2 = "2", s2_reason3 = "3", s2_reason4 = "4",
                                   s2_reason5 = "5", s2_reason6 = "6")

long_rank_s2_sum<-long_rank_s2%>%
  group_by(rank, reason)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))
long_rank_s2_sum

ggplot(data=long_rank_s2_sum, aes(x=rank, y=frequency, fill=reason))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 2 - Combinations of reasons
long_combo_s2<-intake_filtered %>%
  select(contains("s2_reason"), resp_id)
combo_s2<-long_combo_s2%>%
  count(s2_reason1,s2_reason2,s2_reason3,s2_reason4, s2_reason5, s2_reason6)

no_order_combo_s2<-combo_s2 %>%
  mutate(comblist = apply(.[1:5], 1, function(x) {
    x <- sort(na.omit(x))
    unlist(sapply(seq_along(x), function(y)
      list(combn(x, y,
                 FUN = function(l)
                   list(toString(l))
      ))))
  })) %>%
  select(comblist, n) %>%
  unnest(comblist) %>%
  group_by(comblist) %>%
  summarise(x = sum(n))

write.csv(no_order_combo_s2, "no_order_combo_s2.csv")

#Scenario 2 - Surrender human/animal reason
long_rank_s2_cat = merge(x=long_rank_s2,y=surr_cat,by.x="reason", by.y="surrender_reason",all.x=TRUE)

long_rank_s2_cat_sum<-long_rank_s2_cat%>%
  group_by(rank, surrender_category)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=long_rank_s2_cat_sum, aes(x=rank, y=frequency, fill=surrender_category))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason category")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_s2_cat_meanrank<-long_rank_s2_cat %>%
  select(surrender_category, rank)%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(surrender_category) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
long_s2_cat_meanrank

#Scenario 2 - Reason Grouping 1
long_rank_s2_surr1_sum<-long_rank_s2_cat%>%
  group_by(rank, surrender_1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=long_rank_s2_surr1_sum, aes(x=rank, y=frequency, fill=surrender_1))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason category")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_s2_surr1_meanrank<-long_rank_s2_cat %>%
  select(surrender_1, rank)%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(surrender_1) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
long_s2_surr1_meanrank

#Scenario 2 - Number of unique reasons by rank
long_rank_s2%>%
  summarise(Unique_Elements = n_distinct(na.omit(reason)))

s2_unique<-long_rank_s2%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(reason)))
s2_unique

#Scenario 2 - Average Ranking and Frequency
s2_meanrank<-intake_filtered %>%
  select(contains("s2_reason"))%>%
  pivot_longer(s2_reason1:s2_reason10,names_to = "rank", values_to = "reason")%>%
  mutate(rank = case_when(grepl("reason1", rank) ~ '1',
                          grepl("reason2", rank) ~ '2',
                          grepl("reason3", rank) ~ '3',
                          grepl("reason4", rank) ~ '4',
                          grepl("reason5", rank) ~ '5',
                          grepl("reason6", rank) ~ '6'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(reason) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s2_meanrank

write.csv(s2_meanrank,"s2_meanrank.csv")

#Scenario 2 - Breed
breed_s2<-intake_filtered %>%
  select(s2_breed1,s2_breed2)%>%
  pivot_longer(s2_breed1:s2_breed2, names_to = "rank", values_to = "breed")
breed_s2$breed<-str_squish(breed_s2$breed)

breed_s2$rank[breed_s2$rank=="s2_breed1"] <- "Primary Breed"
breed_s2$rank[breed_s2$rank=="s2_breed2"] <- "Secondary Breed"

breed_s2_sum<-breed_s2%>%
  group_by(rank, breed)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=breed_s2_sum, aes(x=rank, y=frequency, fill=breed))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 2 - Unique number of breeds
breed_s2%>%
  summarise(Unique_Elements = n_distinct(na.omit(breed)))

s2_breed_unique<-breed_s2%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(breed)))
s2_breed_unique

#Scenario 2 - Frequency breed
s2_meanbreed<-intake_filtered %>%
  select(contains("s2_breed"))%>%
  pivot_longer(s2_breed1:s2_breed2,names_to = "rank", values_to = "breed")%>%
  mutate_all(str_squish)%>%
  mutate(rank = case_when(grepl("breed1", rank) ~ '1',
                          grepl("breed2", rank) ~ '2'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(breed) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s2_meanbreed

write.csv(s2_meanbreed,"s2_meanbreed.csv")

#Scenario 2 - Breed (by Primary and Secondary)
breed1_s2<-intake_filtered%>%
  group_by(s2_breed1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

breed2_s2<-intake_filtered%>%
  group_by(s2_breed2)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

long_breed_s2<-intake_filtered %>%
  select(contains("s2_breed"), resp_id)%>%
  count(s2_breed1, s2_breed2)

#Scenario 2 - Colour
colour_s2<-intake_filtered %>%
  select(s2_colour1, s2_colour2)%>%
  pivot_longer(s2_colour1:s2_colour2, names_to = "rank", values_to = "colour")

colour_s2$rank[colour_s2$rank=="s2_colour1"] <- "Primary Colour"
colour_s2$rank[colour_s2$rank=="s2_colour2"] <- "Secondary Colour"

colour_s2_sum<-colour_s2%>%
  group_by(rank, colour)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=colour_s2_sum, aes(x=rank, y=frequency, fill=colour))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 2 - Unique number of colours
colour_s2%>%
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

s2_colour_unique<-colour_s2%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

s2_colour_unique

long_colour_s2<-intake_filtered %>%
  select(contains("s2_colour"), resp_id)%>%
  count(s2_colour1, s2_colour2)

#Scenario 2 - Frequency colour
s2_meancolour<-intake_filtered %>%
  select(contains("s2_colour"))%>%
  pivot_longer(s2_colour1:s2_colour2,names_to = "rank", values_to = "colour")%>%
  mutate(rank = case_when(grepl("colour1", rank) ~ '1',
                          grepl("colour2", rank) ~ '2'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(colour) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s2_meancolour

write.csv(s2_meancolour,"s2_meancolour.csv")

#Scenario 3: 
#1 - Unwanted litter
#2 - Behaviour (too vocal)
#3 - Moving/evicted - Can't take pet
#4 - Behaviour (housetraining/spraing/marking)

# Scenario 3 - Surrender reason
long_rank_s3<-intake_filtered %>%
  select(contains("s3_reason"))%>%
  pivot_longer(s3_reason1:s3_reason6,names_to = "rank", values_to = "reason")

long_rank_s3$rank<-factor(long_rank_s3$rank, ordered = TRUE, 
                          levels = c("s3_reason1", "s3_reason2", "s3_reason3", "s3_reason4",
                                     "s3_reason5", "s3_reason6"))

long_rank_s3$rank <- recode_factor(long_rank_s3$rank, s3_reason1 = "1", s3_reason2 = "2", s3_reason3 = "3", s3_reason4 = "4",
                                   s3_reason5 = "5", s3_reason6 = "6")

long_rank_s3_sum<-long_rank_s3%>%
  group_by(rank, reason)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))
long_rank_s3_sum

ggplot(data=long_rank_s3_sum, aes(x=rank, y=frequency, fill=reason))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 3 - Surrender human/animal reason
long_rank_s3_cat = merge(x=long_rank_s3,y=surr_cat,by.x="reason", by.y="surrender_reason",all.x=TRUE)

long_rank_s3_cat_sum<-long_rank_s3_cat%>%
  group_by(rank, surrender_category)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=long_rank_s3_cat_sum, aes(x=rank, y=frequency, fill=surrender_category))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason category")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_s3_cat_meanrank<-long_rank_s3_cat %>%
  select(surrender_category, rank)%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(surrender_category) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
long_s3_cat_meanrank

#Scenario 2 - Reason Grouping 1
long_rank_s2_surr1_sum<-long_rank_s2_cat%>%
  group_by(rank, surrender_1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=long_rank_s2_surr1_sum, aes(x=rank, y=frequency, fill=surrender_1))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason category")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_s2_surr1_meanrank<-long_rank_s2_cat %>%
  select(surrender_1, rank)%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(surrender_1) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
long_s2_surr1_meanrank

#Scenario 3 - Number of unique reasons by rank
long_rank_s3%>%
  summarise(Unique_Elements = n_distinct(na.omit(reason)))

s3_unique<-long_rank_s3%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(reason)))
s3_unique

#Scenario 3 - Average Ranking and Frequency
s3_meanrank<-intake_filtered %>%
  select(contains("s3_reason"))%>%
  pivot_longer(s3_reason1:s3_reason10,names_to = "rank", values_to = "reason")%>%
  mutate(rank = case_when(grepl("reason1", rank) ~ '1',
                          grepl("reason2", rank) ~ '2',
                          grepl("reason3", rank) ~ '3',
                          grepl("reason4", rank) ~ '4',
                          grepl("reason5", rank) ~ '5',
                          grepl("reason6", rank) ~ '6'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(reason) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s3_meanrank

write.csv(s3_meanrank,"s3_meanrank.csv")

#Scenario 3 - Breed
breed_s3<-intake_filtered %>%
  select(s3_breed1,s3_breed2)%>%
  pivot_longer(s3_breed1:s3_breed2, names_to = "rank", values_to = "breed")
breed_s3$breed<-str_squish(breed_s3$breed)

breed_s3$rank[breed_s3$rank=="s3_breed1"] <- "Primary Breed"
breed_s3$rank[breed_s3$rank=="s3_breed2"] <- "Secondary Breed"

breed_s3_sum<-breed_s3%>%
  group_by(rank, breed)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=breed_s3_sum, aes(x=rank, y=frequency, fill=breed))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 3 - Breed (by Primary and Secondary)
breed1_s3<-intake_filtered%>%
  group_by(s3_breed1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

breed2_s3<-intake_filtered%>%
  group_by(s3_breed2)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

long_breed_s3<-intake_filtered %>%
  select(contains("s3_breed"), resp_id)%>%
  count(s3_breed1, s3_breed2)

#Scenario 3 - Unique number of breeds
breed_s3%>%
  summarise(Unique_Elements = n_distinct(na.omit(breed)))

s3_breed_unique<-breed_s3%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(breed)))
s3_breed_unique

#Scenario 3 - Frequency breed
s3_meanbreed<-intake_filtered %>%
  select(contains("s3_breed"))%>%
  pivot_longer(s3_breed1:s3_breed2,names_to = "rank", values_to = "breed")%>%
  mutate_all(str_squish)%>%
  mutate(rank = case_when(grepl("breed1", rank) ~ '1',
                          grepl("breed2", rank) ~ '2'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(breed) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s3_meanbreed

write.csv(s3_meanbreed,"s3_meanbreed.csv")

#Scenario 3 - Colour
colour_s3<-intake_filtered %>%
  select(s3_colour1, s3_colour2)%>%
  pivot_longer(s3_colour1:s3_colour2, names_to = "rank", values_to = "colour")

colour_s3$rank[colour_s3$rank=="s3_colour1"] <- "Primary Colour"
colour_s3$rank[colour_s3$rank=="s3_colour2"] <- "Secondary Colour"

colour_s3_sum<-colour_s3%>%
  group_by(rank, colour)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=colour_s3_sum, aes(x=rank, y=frequency, fill=colour))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 3 - Unique number of colours
colour_s3%>%
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

s3_colour_unique<-colour_s3%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

s3_colour_unique

long_colour_s3<-intake_filtered %>%
  select(contains("s3_colour"), resp_id)%>%
  count(s3_colour1, s3_colour2)

#Scenario 3 - Frequency colour
s3_meancolour<-intake_filtered %>%
  select(contains("s3_colour"))%>%
  pivot_longer(s3_colour1:s3_colour2,names_to = "rank", values_to = "colour")%>%
  mutate(rank = case_when(grepl("colour1", rank) ~ '1',
                          grepl("colour2", rank) ~ '2'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(colour) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s3_meancolour

write.csv(s3_meancolour,"s3_meancolour.csv")

#Scenario 4
#1 - Behaviour (Keeps escaping)
#2 - Allergies (Human allergy to pet)
#3 - Behaviour (growls/lunges etc)
#4 - Can't afford spay/neuter

# Scenario 4 - Surrender reason
long_rank_s4<-intake_filtered %>%
  select(contains("s4_reason"))%>%
  pivot_longer(s4_reason1:s4_reason6,names_to = "rank", values_to = "reason")

long_rank_s4$rank<-factor(long_rank_s4$rank, ordered = TRUE, 
                          levels = c("s4_reason1", "s4_reason2", "s4_reason3", "s4_reason4",
                                     "s4_reason5", "s4_reason6"))

long_rank_s4$rank <- recode_factor(long_rank_s4$rank, s4_reason1 = "1", s4_reason2 = "2", s4_reason3 = "3", s4_reason4 = "4",
                                   s4_reason5 = "5", s4_reason6 = "6")

long_rank_s4_sum<-long_rank_s4%>%
  group_by(rank, reason)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))
long_rank_s4_sum

ggplot(data=long_rank_s4_sum, aes(x=rank, y=frequency, fill=reason))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 4 - Surrender human/animal reason
long_rank_s4_cat = merge(x=long_rank_s4,y=surr_cat,by.x="reason", by.y="surrender_reason",all.x=TRUE)

long_rank_s4_cat_sum<-long_rank_s4_cat%>%
  group_by(rank, surrender_category)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=long_rank_s4_cat_sum, aes(x=rank, y=frequency, fill=surrender_category))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason category")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_s4_cat_meanrank<-long_rank_s4_cat %>%
  select(surrender_category, rank)%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(surrender_category) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
long_s4_cat_meanrank

#Scenario 4 - Number of unique reasons by rank
long_rank_s4%>%
  summarise(Unique_Elements = n_distinct(na.omit(reason)))

s4_unique<-long_rank_s4%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(reason)))
s4_unique

#Scenario 4 - Average Ranking and Frequency
s4_meanrank<-intake_filtered %>%
  select(contains("s4_reason"))%>%
  pivot_longer(s4_reason1:s4_reason10,names_to = "rank", values_to = "reason")%>%
  mutate(rank = case_when(grepl("reason1", rank) ~ '1',
                          grepl("reason2", rank) ~ '2',
                          grepl("reason3", rank) ~ '3',
                          grepl("reason4", rank) ~ '4',
                          grepl("reason5", rank) ~ '5',
                          grepl("reason6", rank) ~ '6'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(reason) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s4_meanrank

write.csv(s4_meanrank,"s4_meanrank.csv")

#Scenario 3 - Reason Grouping 1
long_rank_s4_surr1_sum<-long_rank_s4_cat%>%
  group_by(rank, surrender_1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=long_rank_s4_surr1_sum, aes(x=rank, y=frequency, fill=surrender_1))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "Rank of reason",
    y = "Proportion",
  ) +
  labs(fill = "Surrender reason category")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

long_s4_surr1_meanrank<-long_rank_s4_cat %>%
  select(surrender_1, rank)%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(surrender_1) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
long_s4_surr1_meanrank

#Scenario 4 - Breed
breed_s4<-intake_filtered %>%
  select(s4_breed1,s4_breed2)%>%
  pivot_longer(s4_breed1:s4_breed2, names_to = "rank", values_to = "breed")
breed_s4$breed<-str_squish(breed_s4$breed)

breed_s4$rank[breed_s4$rank=="s4_breed1"] <- "Primary Breed"
breed_s4$rank[breed_s4$rank=="s4_breed2"] <- "Secondary Breed"

breed_s4_sum<-breed_s4%>%
  group_by(rank, breed)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=breed_s4_sum, aes(x=rank, y=frequency, fill=breed))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 4 - Unique number of breeds
breed_s4%>%
  summarise(Unique_Elements = n_distinct(na.omit(breed)))

s4_breed_unique<-breed_s4%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(breed)))
s4_breed_unique

#Scenario 4 - Frequency breed
s4_meanbreed<-intake_filtered %>%
  select(contains("s4_breed"))%>%
  pivot_longer(s4_breed1:s4_breed2,names_to = "rank", values_to = "breed")%>%
  mutate_all(str_squish)%>%
  mutate(rank = case_when(grepl("breed1", rank) ~ '1',
                          grepl("breed2", rank) ~ '2'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(breed) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s4_meanbreed

write.csv(s4_meanbreed,"s4_meanbreed.csv")


#Scenario 4 - Breed (by Primary and Secondary)
breed1_s4<-intake_filtered%>%
  group_by(s4_breed1)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

breed2_s4<-intake_filtered%>%
  group_by(s4_breed2)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

long_breed_s4<-intake_filtered %>%
  select(contains("s4_breed"), resp_id)%>%
  count(s4_breed1, s4_breed2)

#Scenario 4 - Colour
colour_s4<-intake_filtered %>%
  select(s4_colour1, s4_colour2)%>%
  pivot_longer(s4_colour1:s4_colour2, names_to = "rank", values_to = "colour")

colour_s4$rank[colour_s4$rank=="s4_colour1"] <- "Primary Colour"
colour_s4$rank[colour_s4$rank=="s4_colour2"] <- "Secondary Colour"

colour_s4_sum<-colour_s4%>%
  group_by(rank, colour)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))

ggplot(data=colour_s4_sum, aes(x=rank, y=frequency, fill=colour))+
  geom_bar(stat="identity")+
  geom_text(aes(label=frequency), color="white", size=3, position = position_stack(0.5))+
  scale_y_continuous(expand = c(0,0))+
  labs(
    x = "",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Scenario 4 - Unique number of colours
colour_s4%>%
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

s4_colour_unique<-colour_s4%>%
  group_by(rank)%>%           
  summarise(Unique_Elements = n_distinct(na.omit(colour)))

s4_colour_unique

long_colour_s4<-intake_filtered %>%
  select(contains("s4_colour"), resp_id)%>%
  count(s4_colour1, s4_colour2)

#Scenario 4 - Frequency colour
s4_meancolour<-intake_filtered %>%
  select(contains("s4_colour"))%>%
  pivot_longer(s4_colour1:s4_colour2,names_to = "rank", values_to = "colour")%>%
  mutate(rank = case_when(grepl("colour1", rank) ~ '1',
                          grepl("colour2", rank) ~ '2'))%>%
  mutate(rank = as.numeric(rank))%>%
  drop_na()%>%
  group_by(colour) %>%
  summarize(mean = mean(rank, na.rm=TRUE),sd=sd(rank), n = n())%>%
  arrange(desc(n))
s4_meancolour

write.csv(s4_meancolour,"s4_meancolour.csv")

###PRIMARY REASON ANALYSIS
require(lme4)
primary_reason<-intake_filtered%>%
  select(resp_id, org_type_recode, s1_reason1, s2_reason1, s3_reason1, s4_reason1, order_1, order_2, order_3, order_4)

primary_order<-primary_reason %>%
  select(everything())%>%
  pivot_longer(contains("order"),names_to = "order", values_to = "scenario")%>%
  pivot_wider(names_from = "scenario", values_from = "order")%>%
  rename(s1_order="1", s2_order="2", s3_order ="3", s4_order="4")

primary_replace<-primary_order%>%
  mutate_at(vars(contains("order")),
         ~case_when(. == 'order_1' ~ 1,
                    . == 'order_2' ~ 2,
                    . == 'order_3' ~ 3,
                    . == 'order_4' ~ 4))

primary_long<-primary_replace%>%
  pivot_longer(contains("reason"),names_to = "scenario", values_to = "reason")%>%
  pivot_longer(contains("order"),names_to = "scenario2", values_to = "order")%>%
  mutate(scenario = case_when(grepl("s1", scenario) ~ 1,
                    grepl("s2", scenario) ~ 2,
                    grepl("s3", scenario) ~ 3,
                    grepl("s4", scenario) ~ 4,))%>%
  mutate(scenario2 = case_when(grepl("s1", scenario2) ~ 1,
                              grepl("s2", scenario2) ~ 2,
                              grepl("s3", scenario2) ~ 3,
                              grepl("s4", scenario2) ~ 4,))%>%
  filter(scenario == scenario2)%>%
  select(-scenario2)

primary_model<-merge(x=primary_long,y=surr_cat,by.x="reason", by.y="surrender_reason",all.x=TRUE)
primary_model$surrender_category[(primary_model$surrender_category == "Human")]<-1
primary_model$surrender_category[(primary_model$surrender_category == "Animal")]<-0
primary_model$surrender_category<-as.factor(primary_model$surrender_category)
summary(primary_model$surrender_category)

#Descriptive- Primary reason
primary_freq<-primary_model%>%
  group_by(reason, scenario)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(scenario, desc(n))

write.csv(primary_freq, "primary_freq.csv")

primary_HA<-primary_model%>%
  group_by(surrender_category, scenario)%>%
  summarise(n = n()) %>%  
  mutate(frequency = n / sum(n))%>%
  mutate(across(frequency, round, 3))%>%
  arrange(scenario, desc(n))
write.csv(primary_HA, "primary_HA.csv")


#Mixed Logit
primary_model$scenario<-as.factor(primary_model$scenario)
summary(primary_model$scenario)
primary_model$org_type_recode<-as.factor(primary_model$org_type_recode)
summary(primary_model$org_type_recode)

primary_result <- glmer(surrender_category ~ scenario + order + org_type_recode + (1|resp_id), data = primary_model, family = binomial)

print(primary_result)
se <- sqrt(diag(vcov(primary_result)))
(tab <- cbind(Est = fixef(primary_result), LL = fixef(primary_result) - 1.96 * se, UL = fixef(primary_result) + 1.96 *
                se))
exp(tab)

###ATTITUDES REASONING QUESTIONS ANALYSIS
#How well do you feel the surrender reason inputted into your shelter software accurately reflects the owner's decision?
ggplot(intake_filtered) +
  aes(x = reason_accurate) +
  geom_histogram(binwidth=10) +
  labs(
    x = "Accuracy of surrender reason in software",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_vline(aes(xintercept=mean(reason_accurate), color="red"),
             linetype="dashed")

intake_filtered %>% 
  summarise(mean = mean(reason_accurate, na.rm = T),
            sd = sd(reason_accurate, na.rm = T),
            min = min(reason_accurate, na.rm = T),
            med = median(reason_accurate, na.rm = T),
            max = max(reason_accurate, na.rm = T),
            n = sum(!is.na(reason_accurate)))

#What percent are human versus animal 
human_animal<-intake_filtered %>%
  select(percent_human,percent_animal)%>%
  pivot_longer(contains("percent"),names_to = "type", values_to = "percent")

human_animal$type[(human_animal$type == "percent_human")]<-"Human"
human_animal$type[(human_animal$type == "percent_animal")]<-"Animal"

percent_human_animal<-ggplot(human_animal) +
  aes(x = type,
      y = percent, fill = type) +
  geom_boxplot() +
  geom_jitter(width = 0.15)+
  labs(
    x = "",
    y = "Percentage",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values = c("#D7DECF",  "#F4E7CD"))

tiff('percent_human_animal.tiff', units="in", width=11, height=7, res=300, compression = 'lzw')
percent_human_animal
dev.off()

intake_filtered %>% 
  summarise(mean = mean(percent_human, na.rm = T),
            sd = sd(percent_human, na.rm = T),
            min = min(percent_human, na.rm = T),
            med = median(percent_human, na.rm = T),
            max = max(percent_human, na.rm = T),
            n = sum(!is.na(percent_human)))

intake_filtered %>% 
  summarise(mean = mean(percent_animal, na.rm = T),
            sd = sd(percent_animal, na.rm = T),
            min = min(percent_animal, na.rm = T),
            med = median(percent_animal, na.rm = T),
            max = max(percent_animal, na.rm = T),
            n = sum(!is.na(percent_animal)))



#Likert scale questions 
intake_likert<-intake_filtered%>%
  select(more_than_one, owner_provide_one, owner_truth, assistance_change, difficult_change)
intake_likert[] <- lapply(intake_likert, factor, levels = c("Strongly Disagree","Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree"),
                    ordered = TRUE)
names(intake_likert) <- c("There is often more than one underlying reason leading to the surrender of the dog to a shelter", 
                          "During the intake interview, owners will often provide only one reason for surrender",
                          "Owners will often not report the true underlying reason for the surrender", 
                          "It is often possible to provide assistance to the owner to change their mind about surrendering to the shelter",
                          "When owners have decided to surrender and brought the dog into the shelter, it is difficult to change their mind")

summary(intake_likert)

likert_result = likert(intake_likert)

likert_plot = plot(likert_result)

p1 = plot(likert(intake_likert), colors = c("#DFC07C", "#ECD7AC", "#F7EFDE", "#E5E6E7", "#EBEFE7", "#CDD6C2", "#AFBD9E"), legend.position="bottom")

p2<-p1+ theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 8))+
  guides(fill=guide_legend("", nrow=1))
p2

tiff('likert_plot_v2.tiff', units="in", width=12, height=7, res=300, compression = 'lzw')
p2
dev.off()
     
plot(likert_result,
     type="density",
     facet = TRUE,
     bw = 0.5)

#Group by org type 
#Private shelter/rescue
intake_likert_private<-intake_filtered%>%
  select(more_than_one, owner_provide_one, owner_truth, assistance_change, difficult_change, org_type_recode)%>%
  filter(org_type_recode == 'Private shelter/rescue')
intake_likert_private[] <- lapply(intake_likert_private, factor, levels = c("Strongly Disagree","Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree"),
                          ordered = TRUE)
names(intake_likert_private) <- c("There is often more than one underlying reason leading to the surrender of the dog to a shelter", 
                          "During the intake interview, owners will often provide only one reason for surrender",
                          "Owners will often not report the true underlying reason for the surrender", 
                          "It is often possible to provide assistance to the owner to change their mind about surrendering to the shelter",
                          "When owners have decided to surrender and brought the dog into the shelter, it is difficult to change their mind")

summary(intake_likert_private)

likert_result_private = likert(intake_likert_private)
likert_result_private

plot(likert_result,
     type="bar")

plot(likert_result,
     type="density",
     facet = TRUE,
     bw = 0.5)
intake_likert_private<-intake_likert_private%>%
  select(-last_col())%>%
  mutate_if(is.factor,as.numeric)
  

summary(intake_likert_private)
intake_likert_private%>%
  summarise_all(funs(sd(., na.rm = FALSE)))

#Private with municipal contract
intake_likert_contract<-intake_filtered%>%
  select(more_than_one, owner_provide_one, owner_truth, assistance_change, difficult_change, org_type_recode)%>%
  filter(org_type_recode == 'Private with municipal contract(s)')
intake_likert_contract[] <- lapply(intake_likert_contract, factor, levels = c("Strongly Disagree","Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree"),
                                  ordered = TRUE)
names(intake_likert_contract) <- c("There is often more than one underlying reason leading to the surrender of the dog to a shelter", 
                                  "During the intake interview, owners will often provide only one reason for surrender",
                                  "Owners will often not report the true underlying reason for the surrender", 
                                  "It is often possible to provide assistance to the owner to change their mind about surrendering to the shelter",
                                  "When owners have decided to surrender and brought the dog into the shelter, it is difficult to change their mind")

summary(intake_likert_contract)

likert_result_contract = likert(intake_likert_contract)
likert_result_contract

plot(likert_result_contract,
     type="bar")

plot(likert_result_contract,
     type="density",
     facet = TRUE,
     bw = 0.5)
intake_likert_contract<-intake_likert_contract%>%
  select(-last_col())%>%
  mutate_if(is.factor,as.numeric)


summary(intake_likert_contract)
intake_likert_contract%>%
  summarise_all(funs(sd(., na.rm = FALSE)))

#Municipally (governmentally) operated
intake_likert_government<-intake_filtered%>%
  select(more_than_one, owner_provide_one, owner_truth, assistance_change, difficult_change, org_type_recode)%>%
  filter(org_type_recode == 'Municipally (governmentally) operated')
intake_likert_government[] <- lapply(intake_likert_government, factor, levels = c("Strongly Disagree","Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree"),
                                   ordered = TRUE)
names(intake_likert_government) <- c("There is often more than one underlying reason leading to the surrender of the dog to a shelter", 
                                   "During the intake interview, owners will often provide only one reason for surrender",
                                   "Owners will often not report the true underlying reason for the surrender", 
                                   "It is often possible to provide assistance to the owner to change their mind about surrendering to the shelter",
                                   "When owners have decided to surrender and brought the dog into the shelter, it is difficult to change their mind")

summary(intake_likert_government)

likert_result_government = likert(intake_likert_government)
likert_result_government

plot(likert_result_government,
     type="bar")

plot(likert_result_government,
     type="density",
     facet = TRUE,
     bw = 0.5)
intake_likert_government<-intake_likert_government%>%
  select(-last_col())%>%
  mutate_if(is.factor,as.numeric)


summary(intake_likert_government)
intake_likert_government%>%
  summarise_all(funs(sd(., na.rm = FALSE)))

##Kruskal-Wallis Test 
likert_org<-intake_filtered%>%
  select(more_than_one, owner_provide_one, owner_truth, assistance_change, difficult_change, org_type_recode)
likert_org$org_type_recode<-as.factor(likert_org$org_type_recode)
likert_org[1:5] <- lapply(likert_org, factor, levels = c("Strongly Disagree","Disagree", "Somewhat Disagree", "Neutral", "Somewhat Agree", "Agree", "Strongly Agree"),
                                     ordered = TRUE)

kruskal.test(more_than_one ~ org_type_recode, data = likert_org)
kruskal.test(owner_provide_one ~ org_type_recode, data = likert_org)
kruskal.test(owner_truth ~ org_type_recode, data = likert_org)
kruskal.test(assistance_change ~ org_type_recode, data = likert_org)
kruskal.test(difficult_change ~ org_type_recode, data = likert_org)

#How accurate are these scenarios?
ggplot(intake_filtered) +
  aes(x = scenarios_reflect) +
  geom_density() +
  labs(
    x = "Accuracy of the survey's scenarios",
    y = "Proportion",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_vline(aes(xintercept=mean(scenarios_reflect), color="red"),
             linetype="dashed")

intake_filtered %>% 
  summarise(mean = mean(scenarios_reflect, na.rm = T),
            sd = sd(scenarios_reflect, na.rm = T),
            min = min(scenarios_reflect, na.rm = T),
            med = median(scenarios_reflect, na.rm = T),
            max = max(scenarios_reflect, na.rm = T),
            n = sum(!is.na(scenarios_reflect)))

intake_filtered %>% 
  group_by(org_type_recode)%>%
  summarise(mean = mean(scenarios_reflect, na.rm = T),
            sd = sd(scenarios_reflect, na.rm = T),
            min = min(scenarios_reflect, na.rm = T),
            med = median(scenarios_reflect, na.rm = T),
            max = max(scenarios_reflect, na.rm = T),
            n = sum(!is.na(scenarios_reflect)))

accuracy <- ggplot(intake_filtered) +
  aes(y = scenarios_reflect, x = org_type_recode, fill = org_type_recode) +
  geom_boxplot() +
  geom_jitter(width = 0.15)+
  labs(
    x = "",
    y = "Accuracy of the survey's scenarios",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values = c("#FADAC7", "#D7DECF",  "#C8D5E5"))

tiff('accuracy.tiff', units="in", width=11, height=7, res=300, compression = 'lzw')
accuracy
dev.off()

reflect_aov <- aov(scenarios_reflect ~ org_type_recode, data = intake_filtered)
summary(reflect_aov)

#Qualitative analysis
intake_qual<-read.csv("intake_qualitative_final.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
intake_qual<-intake_qual%>%
  dplyr::rename(resp_id = ï..resp_id)

#resp_id and org type
org_id<-intake_filtered%>%
  select(resp_id, org_type_recode, long_work)

#When faced with complex reasons to surrender an animal, how do you choose what the primary reason for surrender is?
qual_primary<-intake_qual%>%
  select(select_multiple_L:help_adopters_L)%>%
  mutate_if(is.integer, as.factor)
table1(~. , data=qual_primary, overall="Total")

qual_primary_id<-intake_qual%>%
  select(resp_id, choose_primary, select_multiple_L:help_adopters_L)

qual_primary_id<-merge(x=qual_primary_id,y=org_id,by="resp_id",all.x=TRUE)

qual_primary_id%>%
  filter(select_multiple_L == '1')

qual_compare<-qual_primary_id%>%
  select(-c(choose_primary, resp_id))%>%
  mutate_if(is.integer, as.factor)

table1(~.|org_type_recode , data=qual_compare, overall="Total")

#Describe the typical scenarios you are faced with during owner surrender. How are the scenarios in this survey different or similar to your experiences during intake?
qual_explain<-intake_qual%>%
  select(shelter_practices_L:truth_L)%>%
  mutate_if(is.integer, as.factor)
table1(~. , data=qual_explain, overall="Total")

qual_explain<-intake_qual%>%
  select(shelter_practices_L:truth_L)

qual_explain_id<-intake_qual%>%
  select(resp_id, explain_scenarios, shelter_practices_L:truth_L)

qual_explain_id<-merge(x=qual_explain_id,y=org_id,by="resp_id",all.x=TRUE)

quotes<-qual_explain_id%>%
  filter(owner_related_L == '1')

#intake themes plot
intake_themes_plot<-read.csv("intake_themes_plot.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
intake_themes_plot<-intake_themes_plot%>%
  dplyr::rename(category = ï..category)
primary_themes_plot<-intake_themes_plot%>%
  filter(category=='choose_primary')

primary_themes_plot<-ggplot(data=primary_themes_plot, aes(x=reorder(theme, -percent), y=percent))+
  geom_bar(stat="identity", fill = '#C8D5E5')+
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
  labs(
    x = "Theme",
    y = "Frequency",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

tiff('primary_themes_plot.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')
primary_themes_plot
dev.off()

experience_plot<-intake_themes_plot%>%
  filter(category=='experience')

experience_plot<-ggplot(data=experience_plot, aes(x=reorder(theme, -percent), y=percent))+
  geom_bar(stat="identity", fill = '#C8D5E5')+
  scale_y_continuous(labels = scales::percent, expand = c(0,0))+
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15))+
  labs(
    x = "Theme",
    y = "Frequency",
  ) +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

tiff('experience_plot.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')
experience_plot
dev.off()
