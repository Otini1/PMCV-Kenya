# Load the required packages
library(haven)
library(DT)
library(shinythemes)
library(plotly)
library(waffle)
library(magrittr)
library(hrbrthemes)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggExtra)
library(sitools)
library(gridExtra)
library(readxl)
library(tidyr)
library(writexl)
library(stringr)
library(tidyverse)
library(haven)
library(dplyr)
library(treemap)
library(flexdashboard)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(waffle)
library(shinyWidgets)
library(scales)
library(forcats)
library(fresh)
library(broom)
library(ggpubr)
library(fpp2)
library(htmltools)
library(grid)
theme_update(plot.title = element_text(hjust = 0.5, face="bold"), legend.position = "bottom",  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text= element_text(size=12))

#library(semantic.dashboard)

SCT_Post <- read_excel("Kenya_SCT_Post.xlsx", sheet = "Data")
SCT_Post <-  SCT_Post[!is.na(SCT_Post$Year),] 

SCT_OBS <- read_excel("Kenya_SCT_OBS_Data.xlsx", sheet = "data")
SCT_OBS <-  SCT_OBS[!is.na(SCT_OBS$Year),] 

CHEW_POST <- read_excel("Kenya _CHEW_Post_Data.xlsx", sheet = "Data")
CHEW_POST <-  CHEW_POST[!is.na(CHEW_POST$Year),] 

TT_POST <-  read_excel("Kenya_TT_Pre_Post.xlsx",  sheet = "Data")
TT_POST <-  TT_POST[!is.na(TT_POST$Year),] 

TT_OBS <- read_excel("Kenya_TT_OBS.xlsx",  sheet = "Data")
TT_OBS <-  TT_OBS[!is.na(TT_OBS$Year),] 


DD_Comm <- read_excel("Kenya_DD_Comm.xlsx", sheet = "Sheet2")
DD_Comm <-  DD_Comm[!is.na(DD_Comm$rows),] 

DD_Main <- read_excel("Kenya_DD_Main.xlsx", sheet = "Sheet2")
#DD_Main <-  DD_Main[!is.na(DD_Comm$rows),] 

###CES
ces_data=read.csv("KE W1_CES_Data.csv")

ces_data$surveyarea=ces_data$county
ces_data$cluster=ces_data$p011_sbunit_name
ces_data$gender=ces_data$p202_resp_sex
ces_data$enrol_status=ces_data$p203b_enrolled_status
ces_data$trttype=ifelse(ces_data$p101_drug_type=="ALB Only", "STH", ifelse(ces_data$p101_drug_type=="PZQ Only", "SCH", "NA"))
#reported coverage
ces_data$reported_trt=ces_data$surveyarea
ces_data$reported_trt=ifelse(ces_data$reported_trt=="Bomet", "82%", ifelse(ces_data$reported_trt=="Bungoma", "87%", "NA" ))


#####NIGERIA DATA

Nigeria_TT_PrePost <- read_excel("Nigeria_TT_PrePost.xlsx", sheet = "Sheet1")


Nigeria_TT_Obs <- read_excel("Nigeria_TT_Obs.xlsx", 
                             sheet = "TT_OBS")

Nigeria_TT_Obs <-  Nigeria_TT_Obs[!is.na(Nigeria_TT_Obs$Year),] 

Nigeria_TTPrePost_Extra <-read_excel("Nigeria_TT_PrePost.xlsx", sheet = "PrePost Extra Analysis")
Nigeria_TTPrePost_Extra <-  Nigeria_TTPrePost_Extra[!is.na(Nigeria_TTPrePost_Extra$Year),] 

Nigeria_Extraanalysis <- read_excel("Nigeria_TT_Obs.xlsx", 
                                    sheet = "TT_Extra_Analysis")
Nigeria_Extraanalysis <-  Nigeria_Extraanalysis[!is.na(Nigeria_Extraanalysis$Year),] 

Nigeria_DD_Main <- read_excel("Nigeria_DD_Main.xlsx", sheet = "Sheet2")
Nigeria_DD_Main <-  Nigeria_DD_Main[!is.na(Nigeria_DD_Main$Year),]

Nigeria_DD_Com <- read_excel("Nigeria_DD_Comm.xlsx", sheet = "Sheet2",range = "a1:BD10000")


Nigeria_SCT_Post <- read_excel("Nigeria_LGA_Post_Dashboard.xlsx",  sheet = "Data")
Nigeria_SCT_Post <-  Nigeria_SCT_Post[!is.na(Nigeria_SCT_Post$Rowsa),] 

Nigeria_LGAT_OBS <- read_excel("Nigeria_LGA_OBs.xlsx", sheet = "data")
Nigeria_LGAT_OBS <-  Nigeria_LGAT_OBS[!is.na(Nigeria_LGAT_OBS$Year),] 

Nigeria_FLHF_POST <-read_excel("Nigeria_LGA_FLHF_Post_Dashboard.xlsx", sheet = "Data")
Nigeria_FLHF_POST <-  Nigeria_FLHF_POST[!is.na(Nigeria_FLHF_POST$Rowsa),] 


####PAKISTAN DATA 

#import data
tt_obs=read.csv("ICT_TT_OBS_Data.csv")
tt_obs$Region=tt_obs$prov
Pakistan_tt_data=read.csv("TT_PRE_POST.csv")

##Subsetting data into Pre and Post
Pakistan_tt_data_pre <- subset(as.data.frame(Pakistan_tt_data),  Pakistan_tt_data$survey=="Pre")
Pakistan_tt_data_post <- subset(as.data.frame(Pakistan_tt_data),  Pakistan_tt_data$survey=='Post')


####KENYA ANALYSIS
#####TT OBS
obs_sheets <- TT_OBS %>%
  group_by(Year, Wave) %>%
  summarise(`Attendance sheets` = round(mean((tto106_attendance_sheet == "Yes"))*100),
            `DtW Attendance sheets` =  round(mean((tto107_attendance_sheet_dtw == "Yes"))*100))

obs_att_sheet <- gather(obs_sheets, 'Variables', 'Values',`Attendance sheets`:`DtW Attendance sheets`)
obs_att_sheet <-  obs_att_sheet[!is.na(obs_att_sheet$Values),] 

obs_sheets_county <- TT_OBS %>%
  group_by(Year, Wave, tto012_county) %>%
  summarise(`Attendance sheets` = round(mean(as.numeric(tto106_attendance_sheet== "Yes"))*100),
            `DtW Attendance sheets` =  round(mean(as.numeric(tto107_attendance_sheet_dtw== "Yes"))*100))

obs_att_sheet_county <- gather(obs_sheets_county, 'Variables', 'Values',`Attendance sheets`:`DtW Attendance sheets`)
obs_att_sheet_county <-  obs_att_sheet_county[!is.na(obs_att_sheet_county$Values),] 

coverage_general <- TT_OBS %>%
  select(Year, Wave, tto012_county, Transmission, Prevention, `Effect of worms`)

coverage_general <- gather(coverage_general,'Variables', 'Values', Transmission:`Effect of worms`)
coverage_general <-  coverage_general[!is.na(coverage_general$Values),] 

coverage_sth <- TT_OBS %>%
  select(Year, Wave, STH_Agegroup,STH_Drug,STH_Drug_Dosage,Drug_storage,Drugs_free_safe)%>%
  mutate(`STH Age Group` = STH_Agegroup,
         `STH Drug` = STH_Drug,
         `STH Drug Dosage` = STH_Drug_Dosage,
         `STH Drug storage` = Drug_storage,
         `Drugs are Free and Safe` = Drugs_free_safe)
coverage_sth <- gather(coverage_sth,'Variables', 'Values', `STH Age Group`:`Drugs are Free and Safe`)
coverage_sth <-  coverage_sth[!is.na(coverage_sth$Values),] 

coverage_sch <- TT_OBS %>%
  select(Year, Wave, SCH_Agegroup, SCH_Agegroup,SCH_Drug,SCH_Drug_Dosage)%>%
  mutate(`SCH Age Group` = SCH_Agegroup,
         `SCH Drug` = SCH_Drug,
         `SCH Drug Dosage` = SCH_Drug_Dosage)
coverage_sch <- gather(coverage_sch,'Variables', 'Values', `SCH Age Group`:  `SCH Drug Dosage`)
coverage_sch <-  coverage_sch[!is.na(coverage_sch$Values),]


obs_tt_roles <- TT_OBS %>%
  select(Year, Wave, tto012_county, Role_1, Role_2,Role_3,Role_4,Role_5)
obs_tt_roles <- gather(obs_tt_roles,'Variables', 'Values', Role_1:Role_5)
obs_tt_roles <-  obs_tt_roles[!is.na(obs_tt_roles$Values),]

obs_chew_roles <- TT_OBS %>%
  select(Year, Wave, tto012_county, Chew_role_1, Chew_role_2,Chew_role_3,Chew_role_4,Chew_role_5)
obs_chew_roles <- gather(obs_chew_roles,'Variables', 'Values', Chew_role_1:Chew_role_5) 
obs_chew_roles <-  obs_chew_roles[!is.na(obs_chew_roles$Values),]

obs_HT_roles <- TT_OBS %>%
  select(Year, Wave, tto012_county, HT_role_1, HT_role_2,HT_role_3,HT_role_4,HT_role_5)
obs_HT_roles <- gather(obs_HT_roles,'Variables', 'Values', HT_role_1:HT_role_5) 
obs_HT_roles <-  obs_HT_roles[!is.na(obs_HT_roles$Values),]

obs_poles <- TT_OBS %>%
  select(Year, Wave, tto012_county, Pole_1, Pole_2,Pole_3)
obs_poles <- gather(obs_poles,'Variables', 'Values', Pole_1:Pole_3) 

obs_not_treat <- TT_OBS %>%
  select(Year, Wave, tto012_county, Not_treat_1, Not_treat_2,Not_treat_3, Not_treat_4, Not_treat_5)
obs_not_treat <- gather(obs_not_treat,'Variables', 'Values', Not_treat_1:Not_treat_5) 

obs_sideeffects <- TT_OBS %>%
  select(Year, Wave, tto012_county, Side_effects_1, Side_effects_2,Side_effects_3, Side_effects_4, Side_effects_5, Side_effects_6)
obs_sideeffects <- gather(obs_sideeffects,'Variables', 'Values', Side_effects_1:Side_effects_6) 

obs_materials <- TT_OBS %>%
  select(Year, Wave, tto012_county, Materials_1, Materials_2,Materials_3)
obs_materials <- gather(obs_materials,'Variables', 'Values', Materials_1:Materials_3)


####TT POST-

sth_knowledge_all <- TT_POST %>%
  filter(STH_Numbers == 1) %>%
  group_by(Year, Wave, source_file,Type) %>%
  summarise(Worms_treated = (round(sum(!is.na(Correct_sth_worms))/sum(STH_Numbers)*100)),
            Correct_Drugs = (round(sum(!is.na(Correct_sth_drugs))/sum(STH_Numbers)*100)),
            Correct_age_group = (round(sum(!is.na(Correct_sth_age_group))/sum(STH_Numbers)*100)),
            Correct_dosage = (round(sum(!is.na(Correct_sth_dosage)) /sum(STH_Numbers)*100)))


sch_knowledge_all <- TT_POST %>%
  filter(SCH_Numbers == 1) %>%
  group_by(Year, Wave, source_file, Type) %>%
  summarise(#Worms_treated = (round(sum(!is.na(Correct_sch_worms))/sum(SCH_Numbers)*100)),
    Correct_Drugs = (round(sum(!is.na(Correct_sch_drugs))/sum(SCH_Numbers)*100)),
    Correct_age_group = (round(sum(!is.na(Correct_sch_age_group))/sum(SCH_Numbers)*100)),
    Correct_dosage = (round(sum(!is.na(Correct_sch_dosage)) /sum(SCH_Numbers)*100)))




knowledge_all <- rbind(sch_knowledge_all, sth_knowledge_all)
knowledge_all <-knowledge_all%>%
  select(Year, Wave, source_file,Type, Worms_treated,Correct_Drugs, Correct_age_group, Correct_dosage )
knowledge_all <-  knowledge_all[!is.na(knowledge_all$Year),]
knowledge_all_1 <- gather(knowledge_all, "Variables", "values", Worms_treated:Correct_dosage)
knowledge_all_1 <-  knowledge_all_1[!is.na(knowledge_all_1$Year),] 


sth_knowledge_cty <- TT_POST %>%
  filter(STH_Numbers == 1) %>%
  group_by(Year, Wave, source_file, Type, ttp012_county) %>%
  summarise(Worms_treated = (round(sum(!is.na(Correct_sth_worms))/sum(STH_Numbers)*100)),
            Correct_Drugs = (round(sum(!is.na(Correct_sth_drugs))/sum(STH_Numbers)*100)),
            Correct_age_group = (round(sum(!is.na(Correct_sth_age_group))/sum(STH_Numbers)*100)),
            Correct_dosage = (round(sum(!is.na(Correct_sth_dosage)) /sum(STH_Numbers)*100)))



sch_knowledge_cty <- TT_POST %>%
  filter(SCH_Numbers == 1) %>%
  group_by(Year, Wave, source_file, Type, ttp012_county) %>%
  summarise(#Worms_treated = (round(sum(!is.na(Correct_sch_worms))/sum(SCH_Numbers)*100)),
    Correct_Drugs = (round(sum(!is.na(Correct_sch_drugs))/sum(SCH_Numbers)*100)),
    Correct_age_group = (round(sum(!is.na(Correct_sch_age_group))/sum(SCH_Numbers)*100)),
    Correct_dosage = (round(sum(!is.na(Correct_sch_dosage)) /sum(SCH_Numbers)*100)))

knowledge_cty <- rbind(sch_knowledge_cty, sth_knowledge_cty)
knowledge_cty <-knowledge_cty%>%
  select(Year, Wave, source_file,ttp012_county, Type, Worms_treated,Correct_Drugs, Correct_age_group, Correct_dosage )


knowledge_cty_1 <- gather(knowledge_cty, "Variables", "values", Worms_treated:Correct_dosage)
knowledge_cty_1 <-  knowledge_cty_1[!is.na(knowledge_cty_1$Year),] 

####SIDE EFFECTS
side_effets1<- TT_POST %>%
  select(source_file,Type, STH_Numbers, Year, Wave,Side_Effects1, Side_Effects2, Side_Effects3, Side_Effects4, Side_Effects5, Side_Effects6, Side_Effects7, Side_Effects8)
sth_sideeffects <- gather(side_effets1, "Variables", "Values", Side_Effects1:Side_Effects8)
sth_sideeffects <-  sth_sideeffects[!is.na(sth_sideeffects$Values),] 


side_effets2<- TT_POST %>%
  select(source_file, Type, SCH_Numbers, Year, Wave,Side_SCH_Effects1, Side_SCH_Effects2, Side_SCH_Effects3, Side_SCH_Effects4, Side_SCH_Effects5, Side_SCH_Effects6, Side_SCH_Effects7, Side_SCH_Effects8)
sch_sideeffects <- gather(side_effets2, "Variables", "Values", Side_SCH_Effects1:Side_SCH_Effects8)
sch_sideeffects <-  sch_sideeffects[!is.na(sch_sideeffects$Values),] 

###MATERIALS
tt_mats<- TT_POST %>%
  select(source_file,Type, STH_Numbers, Year, Wave,Mat_1, Mat_2, Mat_3, Mat_4, Mat_5)
tt_materials <- gather(tt_mats, "Variables", "Values", Mat_1:Mat_5)
tt_materials <-  tt_materials[!is.na(tt_materials$Values),] 

####CHEW POST
role_chew <- gather(CHEW_POST, Variables, Values, Chew_1:Chew_5)

role_chew <- role_chew %>%
  select(Year, Wave, sctp012_county_name, Variables, Values)
role_chew <-  role_chew[!is.na(role_chew$Values),]

sth_side_effect <- gather(CHEW_POST, Variables, Values, STH_Sideeffects_1:STH_Sideeffects_8)

sth_side_effect <- sth_side_effect %>%
  select(Year, Wave, sctp012_county_name, Variables, Values)
sth_side_effect <-  sth_side_effect[!is.na(sth_side_effect$Values),]


sch_side_effect <- gather(CHEW_POST, Variables, Values, SCH_Sideeffects_1:SCH_Sideeffects_8)
sch_side_effect <- sch_side_effect %>%
  select(Year, Wave, sctp012_county_name, Variables, Values)
sch_side_effect <-  sch_side_effect[!is.na(sch_side_effect$Values),]


####SCT OBS
not_trt  <- gather(SCT_OBS, Variables, Values2, Not_treat_1:Not_treat_6)

not_trt <- not_trt %>%
  select(Year, Wave, scto012_county, Variables, Values2)

not_trt <-  not_trt[!is.na(not_trt$Values2),]
not_trt$Values = str_wrap(not_trt$Values2, width = 10)

chew_roles <- gather(SCT_OBS, Variables, Values, Chew_1:Chew_8)

chew_roles <- chew_roles %>%
  select(Year, Wave, scto012_county, Variables, Values)
chew_roles <-  chew_roles[!is.na(chew_roles$Values),]

HT_roles <- gather(SCT_OBS, Variables, Values, HT_1:HT_6)

HT_roles <- HT_roles %>%
  select(Year, Wave, scto012_county, Variables, Values)
HT_roles <-  HT_roles[!is.na(HT_roles$Values),]

Qst <- gather(SCT_OBS, Variables, Values, Question_1:Question_11)

Qst <- Qst %>%
  select(Year, Wave, scto012_county, Variables, Values)
Qst <-  Qst[!is.na(Qst$Values),]

topics <- SCT_OBS %>%
  select(Year, Wave, scto012_county, info_worms,info_drugs,info_drugs_admin,info_forms,info_forms_sec,info_forms_prac,info_conduct_tt)%>%
  mutate(`Information on prevention and control of worms`= info_worms,
         `Information on drugs` = info_drugs,
         `Information on drugs administration` = info_drugs_admin,
         `Information on monitoring forms` = info_forms,
         `Information on sections of forms` = info_forms_sec,
         `Practice of all sections of  the forms` = info_forms_prac,
         `Conducting teacher training sessions` = info_conduct_tt)
topics2 <- gather(topics, Variables, Values,`Information on prevention and control of worms`: `Conducting teacher training sessions`, factor_key=TRUE) 



Comm_parents <- gather(DD_Comm, Variables, Values, 'Primary School Teacher1':Radio1)

Comm_parents <- Comm_parents %>%
  select(Year, Wave, pdc012_county, Variables, Values)%>%
  mutate(Role = "Parents")
Comm_parents <-  Comm_parents[!is.na(Comm_parents$Values),]


Comm_CHV <- gather(DD_Comm, Variables, Values, 'Primary School Teacher':Radio)

Comm_CHV <- Comm_CHV %>%
  select(Year, Wave, pdc012_county, Variables, Values)%>%
  mutate(Role = "Chew")
Comm_CHV <-  Comm_CHV[!is.na(Comm_CHV$Values),]

Comm <- rbind(Comm_CHV, Comm_parents)

cols=c("Pre" = "indianred3", "Post" = "grey50") 

Comm_parents2 <- gather(DD_Comm, Variables, Values, Radio2:'School Children2')

Comm_parents2 <- Comm_parents2 %>%
  select(Year, Wave, pdc012_county, Variables, Values)
Comm_parents2 <-  Comm_parents2[!is.na(Comm_parents2$Values),]


key_messages <- gather(DD_Main, Variables, Values, 'Free Deworming medicine':'Deworming can improve health and/or education')

key_messages <- key_messages %>%
  select(Year, Wave, ddm012_county, Variables, Values)

key_messages <-  key_messages[!is.na(key_messages$Values),]


sensitization_acts <- gather(DD_Main, Variables, Values, 'Conduct Health Education in Classrooms':'Announce deworming message in school assembly')

sensitization_acts <- sensitization_acts %>%
  select(Year, Wave, ddm012_county, Variables, Values)

sensitization_acts <-  sensitization_acts[!is.na(sensitization_acts$Values),]



non_enrolled_reach <- gather(DD_Main, Variables, Values, Radio:Churches)

non_enrolled_reach <- non_enrolled_reach %>%
  select(Year, Wave, ddm012_county, Variables, Values)

non_enrolled_reach <-  non_enrolled_reach[!is.na(non_enrolled_reach$Values),]

trt_num_all <- 
  DD_Main %>%
  filter(  !is.na(ddm009a_num_children))%>%
  group_by(Year, Wave)%>%
  dplyr:: summarise(Males = round(sum(as.numeric(ddm401_male_present), na.rm = T)),
                    Females = round(sum(as.numeric(ddm402_fmale_present), na.rm = T)),
                    Totals = Males+Females,
                    perc_male = paste(round(Males/Totals *100),"%", sep=""),
                    perc_female = paste(round(Females/Totals *100),"%", sep=""),
                    ##Took ALB
                    Male_took = round(sum(as.numeric(ddm403_male_took_alb), na.rm = T)),
                    Female_took= round(sum(as.numeric(ddm404_fmale_took_alb), na.rm = T)),
                    Total_took = Male_took+Female_took,
                    perc_male_took = paste(round(Male_took/Total_took *100),"%", sep=""),
                    perc_female_took = paste(round(Female_took/Total_took *100),"%", sep=""),
                    perc_took = paste(round(Total_took/Totals *100),"%", sep=""),
                    ##Teacher Observe
                    Male_observe = round(sum(as.numeric(ddm405_male_obsrvdtook_alb), na.rm = T)),
                    Female_observe = round(sum(as.numeric(ddm406_fmale_obsrvdtook_alb), na.rm = T)),
                    Total_observe = Male_observe + Female_observe,
                    perc_male_observe = paste(round(Male_observe/Total_observe *100),"%", sep=""),
                    perc_female_observe = paste(round(Female_observe/Total_observe *100),"%", sep=""),
                    perc_observe = paste(round(Total_observe/Total_took *100),"%", sep="")
  )%>%
  ungroup()

trt_num_sc <- 
  DD_Main %>%
  filter(  !is.na(ddm009a_num_children))%>%
  group_by(Year, Wave)%>%
  dplyr:: summarise(Males = round(sum(as.numeric(ddm401_male_present), na.rm = T)),
                    Females = round(sum(as.numeric(ddm402_fmale_present), na.rm = T)),
                    Totals = Males+Females,
                    perc_male = paste(round(Males/Totals *100),"%", sep=""),
                    perc_female = paste(round(Females/Totals *100),"%", sep=""),
                    ##Took ALB
                    Male_took = round(sum(as.numeric(ddm403_male_took_alb), na.rm = T)),
                    Female_took= round(sum(as.numeric(ddm404_fmale_took_alb), na.rm = T)),
                    Total_took = Male_took+Female_took,
                    perc_male_took = paste(round(Male_took/Total_took *100),"%", sep=""),
                    perc_female_took = paste(round(Female_took/Total_took *100),"%", sep=""),
                    perc_took = paste(round(Total_took/Totals *100),"%", sep=""),
                    ##Teacher Observe
                    Males_observe = round(sum(as.numeric(ddm405_male_obsrvdtook_alb), na.rm = T)),
                    Female_observe = round(sum(as.numeric(ddm406_fmale_obsrvdtook_alb), na.rm = T)),
                    Total_observe = Males_observe + Female_observe,
                    perc_male_observe = paste(round(Males_observe/Total_observe *100),"%", sep=""),
                    perc_female_observe = paste(round(Female_observe/Total_observe *100),"%", sep=""),
                    perc_observe = paste(round(Total_observe/Total_took *100),"%", sep="")
  )%>%
  ungroup()

####NIGERIA ANALYSIS

ng_trt_num_all <- 
  Nigeria_DD_Main %>%
  filter(  !is.na(dwo200k_num_male_reg))%>%
  group_by(State,Year, Round)%>%
  dplyr:: summarise(Males = round(sum(as.numeric(dwo200a_num_male_present), na.rm = T)),
                    Females = round(sum(as.numeric(dwo200b_num_female_present), na.rm = T)),
                    Totals = Males+Females,
                    perc_male = paste(round(Males/Totals *100),"%", sep=""),
                    perc_female = paste(round(Females/Totals *100),"%", sep=""),
                    ##Took ALB
                    Male_took = round(sum(as.numeric(dwo200c_num_male_took_alb), na.rm = T)),
                    Female_took= round(sum(as.numeric(dwo200d_num_female_took_alb), na.rm = T)),
                    Total_took = Male_took+Female_took,
                    perc_male_took = paste(round(Male_took/Total_took *100),"%", sep=""),
                    perc_female_took = paste(round(Female_took/Total_took *100),"%", sep=""),
                    perc_took = paste(round(Total_took/Totals *100),"%", sep=""),
                    ##Teacher Observe
                    Male_observe = round(sum(as.numeric(dwo200e_num_male_obsrvd_alb), na.rm = T)),
                    Female_observe = round(sum(as.numeric(dwo200f_num_fmale_obsrvd_alb), na.rm = T)),
                    Total_observe = Male_observe + Female_observe,
                    perc_male_observe = paste(round(Male_observe/Total_observe *100),"%", sep=""),
                    perc_female_observe = paste(round(Female_observe/Total_observe *100),"%", sep=""),
                    perc_observe = paste(round(Total_observe/Total_took *100),"%", sep="")
  )%>%
  ungroup()

ng_Comm_parents <- gather(Nigeria_DD_Com, Variables, Values, Poster:'Teacher from a local school')

ng_Comm_parents <- ng_Comm_parents %>%
  select(State, Year, Round,  Variables, Values)%>%
  mutate(Role = "Parents")
ng_Comm_parents <-  ng_Comm_parents[!is.na(ng_Comm_parents$Values),]


ng_Comm_CHV <- gather(Nigeria_DD_Com, Variables, Values, 'Primary School Teacher2':'Junior secondary School Teacher 2')

ng_Comm_CHV <- ng_Comm_CHV %>%
  select(State, Year, Round,  Variables, Values)%>%
  mutate(Role = "Chew")
ng_Comm_CHV <-  ng_Comm_CHV[!is.na(ng_Comm_CHV$Values),]

ng_Comm <- rbind(ng_Comm_CHV, ng_Comm_parents)

cols=c("Pre" = "indianred3", "Post" = "grey50") 

ng_Comm_parents2 <- gather(Nigeria_DD_Com, Variables, Values, 'Radio 1':'Town Hall Meeting1')

ng_Comm_parents2 <- ng_Comm_parents2 %>%
  select(State, Year, Round,  Variables, Values)
ng_Comm_parents2 <-  ng_Comm_parents2[!is.na(ng_Comm_parents2$Values),]



####CHEW POST
ng_role_chew <- gather(Nigeria_FLHF_POST, Variables, Values, Chew_1:Chew_5)

ng_role_chew <- ng_role_chew %>%
  select(State, Year, Round, Variables, Values)
ng_role_chew <-  ng_role_chew[!is.na(ng_role_chew$Values),]

ng_sth_side_effect <- gather(Nigeria_FLHF_POST, Variables, Values, STH_Sideeffects_1:STH_Sideeffects_8)

ng_sth_side_effect <- ng_sth_side_effect %>%
  select(State, Year, Round, Variables, Values)
ng_sth_side_effect <-  ng_sth_side_effect[!is.na(ng_sth_side_effect$Values),]


ng_sch_side_effect <- gather(Nigeria_FLHF_POST, Variables, Values, SCH_Sideeffects_1:SCH_Sideeffects_8)
ng_sch_side_effect <- ng_sch_side_effect %>%
  select(State, Year, Round, Variables, Values)
ng_sch_side_effect <-  ng_sch_side_effect[!is.na(ng_sch_side_effect$Values),]


####SCT OBS
ng_not_trt  <- gather(Nigeria_LGAT_OBS, Variables, Values2, Not_treat_1:Not_treat_6)

ng_not_trt <- ng_not_trt %>%
  select(State, Year, Round, Variables, Values2)

ng_not_trt <-  ng_not_trt[!is.na(ng_not_trt$Values2),]
ng_not_trt$Values = str_wrap(ng_not_trt$Values2, width = 10)

ng_chew_roles <- gather(Nigeria_LGAT_OBS, Variables, Values, Chew_1:Chew_8)

ng_chew_roles <- ng_chew_roles %>%
  select(State, Year, Round, Variables, Values)
ng_chew_roles <-  ng_chew_roles[!is.na(ng_chew_roles$Values),]

ng_HT_roles <- gather(Nigeria_LGAT_OBS, Variables, Values, HT_1:HT_6)

ng_HT_roles <- ng_HT_roles %>%
  select(State, Year, Round, Variables, Values)
ng_HT_roles <-  ng_HT_roles[!is.na(ng_HT_roles$Values),]

ng_post_materials <- gather(Nigeria_SCT_Post, Variables, Values, `Teacher Training Handout`:`Level 2/ FLHF Summary Form`)

ng_post_materials <- ng_post_materials %>%
  select(State, Year, Round, Variables, Values)
ng_post_materials <-  ng_post_materials[!is.na(ng_post_materials$Values),]

ng_Qst <- gather(Nigeria_LGAT_OBS, Variables, Values, Question_1:Question_11)

ng_Qst <- ng_Qst %>%
  select(State, Year, Round, Variables, Values)
ng_Qst <-  ng_Qst[!is.na(ng_Qst$Values),]

ng_topics <- Nigeria_LGAT_OBS %>%
  select(State, Year, Round, info_worms,info_drugs,info_drugs_admin,info_forms,info_forms_sec,info_forms_prac,info_conduct_tt)%>%
  mutate(`Information on prevention and control of worms`= info_worms,
         `Information on drugs` = info_drugs,
         `Information on drugs administration` = info_drugs_admin,
         `Information on monitoring forms` = info_forms,
         `Information on sections of forms` = ,
         `Practice of all sections of  the forms` = info_forms_sec,
         `Conducting teacher training sessions` = info_conduct_tt)
ng_topics2 <- gather(ng_topics, Variables, Values,`Information on prevention and control of worms`: `Conducting teacher training sessions`, factor_key=TRUE) 




ng_colors <- c("#60BC68","#F9A33A", "#A4A4A4", "#F05854" )


ng_late <- Nigeria_TTPrePost_Extra %>%
  select(State, Year, Round, `Invited late`,	'Had to go to class/school first,',	'Had to travel  long distance')

ng_late1 <- gather(ng_late, Variables, values, `Invited late`,	'Had to go to class/school first,',	'Had to travel  long distance', factor_key=TRUE)
ng_late1 <-  ng_late1[!is.na(ng_late1$values),]

ng_x <- Nigeria_TT_PrePost %>%
  filter(source_file =="Pre") %>%
  mutate(LGA_Name = ttp012_lga_name ,
         Ward_Name = ttp013_ward_name)%>%
  select(State, LGA_Name, Ward_Name,Year, Round, ttp011_sth_schisto)

ng_x$LGA_Name <- str_to_title(ng_x$LGA_Name)
ng_x$Ward_Name <- str_to_title(ng_x$Ward_Name)


ng_x <- distinct(ng_x)

ng_no_wards <- ng_x %>%
  group_by(State, LGA_Name, Year, Round) %>%
  summarise('No of Wards' = n())

ng_trt_type  <- ng_x %>%
  group_by(State,Year, Round, ttp011_sth_schisto)%>%
  summarise('TREATMENT TYPES' = n())

ng_respondents  <- Nigeria_TT_PrePost %>%
  group_by(State, Year, Round, source_file)%>%
  summarise('RESPONDENTS' = n())%>%
  ungroup()


ng_training_attendance <- Nigeria_TT_Obs %>%
  group_by(State,Year, Round)%>%
  summarise(Teacher_Attendance = (round(sum(tto1202_teachers_present, na.rm = T)/sum(as.numeric(tto1201_teachers_invited),  na.rm = T)*100)),
            School_Attendance = (round(sum(tto1206_school_present, na.rm = T)/sum(as.numeric(tto1207_school_invited),  na.rm = T)*100)),
            Teacher_on_Time = (round(sum(tto103a_head_count, na.rm = T)/sum(as.numeric(tto1202_teachers_present),  na.rm = T)*100)))%>%
  ungroup()


ng_dd_summary <- Nigeria_DD_Main %>%
  filter(!is.na(Year))%>%
  group_by(State, Year, Round)%>%
  summarise (Tablets = paste(round(sum(!is.na(Num_tablets))/sum(!is.na(dwo218_num_mbendzol))*100),"%",sep=""),
             Poles = paste(round(sum(!is.na(Tablet_Pole))/sum(!is.na(dwo215_tablet_pole))*100),"%",sep=""),
             Tablet_Disposal =  paste(round(sum(!is.na(Disposal))/sum(!is.na(dwo223_use_spoiled_tab))*100),"%",sep=""),
             Sick_Deworm =  paste(round(sum(!is.na(Asked_sick))/sum(!is.na(Year))*100),"%",sep=""),
             Registers =  paste(round(sum(!is.na(Register))/sum(!is.na(dwo212_teacher_register))*100),"%",sep=""),
             Transfers = paste(round( sum(!is.na(Transfer))/sum(!is.na(dwo213_transfer_register))*100),"%",sep=""),
             Eat =  paste(round(sum(!is.na(Eaten))/sum(!is.na(dwo201a_asked_if_eaten))*100),"%",sep=""),
             Hand_wash =  paste(round(sum(!is.na(Handwashing))/sum(!is.na(Year))*100),"%",sep=""),
             Health =  paste(round(sum(!is.na(Health_texts))/sum(!is.na(dwo206_health_msg))*100),"%",sep=""))%>%
  ungroup()


ng_tp <- Nigeria_Extraanalysis %>%
  select(State,Year, Round,`SCH numbers`, `STH numbers`, `Worms Topic calculation`, `Side Effects Calculation`, `Target Population`, `Reporting Forms Calculation`, `Roles and Responsibilites`, `Community Sensitization` )

ng_data_long4 <- gather(ng_tp, Variables, values, `Worms Topic calculation`:`Community Sensitization`, factor_key=TRUE)
#ng_data_long4$values <- factor(ng_data_long4$values, levels = c("Not Covered", "Covered"))
ng_colot_progress <- c("#60BC68", "#60BC68","#60BC68","#60BC68","#F05854")


ng_actions <- 
  Nigeria_DD_Main %>%
  #filter(State == input$State & Year == input$years2 & Round == input$Round & !is.na(dwo230_obsrv_side_effect)) %>%  
  group_by(State, Year, Round, dwo706_extr_tabs_actn)%>%
  summarise(Deworm =round(n()/sum(!is.na(Nigeria_DD_Main$dwo706_extr_tabs_actn))*100),
            lbs = paste(round(n()/sum(!is.na(Nigeria_DD_Main$dwo706_extr_tabs_actn))*100),"%",sep=""))
ng_actions$label <- paste(ng_actions$dwo706_extr_tabs_actn, ng_actions$lbs, sep = "\n")


ng_methods <- Nigeria_Extraanalysis %>%
  select(State, Year , Round, `Lecture based presentation`,Discussion,'Group work',Demonstrations,'Role play')

ng_methods1 <- gather(ng_methods, Variables, values, `Lecture based presentation`:`Role play`, factor_key=TRUE)


ng_hear_deworming <- Nigeria_DD_Com %>%
  select(State, Year , Round, Poster,`Town Announcer`,	`My child told me`,	`It was announced in church`,	`Radio`,	`Speaker mounted van`,	`Teacher from a local school`)

ng_hear_deworming1 <- gather(ng_hear_deworming, Variables, values, Poster:`Teacher from a local school`)


ng_dd_side_effects <- Nigeria_DD_Main %>%
  select(State, Year , Round, Headache,	Nausea,	`Abdominal discomfort`,	Fainting,	Vomiting)

ng_dd_side_effects1 <- gather(ng_dd_side_effects, Variables, values, Headache:Vomiting)

ng_Sufficiency <- Nigeria_DD_Main %>%
  select(State, Year , Round, Drugs,	Forms)

Sufficiency1 <- gather(ng_Sufficiency, Variables, values,  Drugs,	Forms)

#########################KNOWLEDGE
ng_sth_knowledge <- Nigeria_TTPrePost_Extra %>%
  filter(`STH numbers` == 1) %>%
  group_by(State, Year, Round, source_file) %>%
  summarise('Worms treated' = (round(sum(`Correct Worms`, na.rm = T)/sum(`STH numbers`)*100)),
            'Correct Drugs' = (round(sum(`Correct STH drug`, na.rm = T)/sum(`STH numbers`)*100)),
            'Correct age group' = (round(sum(`Correct STH age group`, na.rm = T)/sum(`STH numbers`)*100)),
            'Correct dosage' = (round(sum(`Correct STH Dosage`, na.rm = T) /sum(`STH numbers`)*100)))

ng_sch_knowledge <- Nigeria_TTPrePost_Extra %>%
  filter(`SCH Numbers` == 1) %>%
  group_by(State, Year, Round, source_file) %>%
  summarise('Worms treated' = (round(sum(`Correct Worms`, na.rm = T)/sum(`SCH Numbers`)*100)),
            'Correct Drugs' = (round(sum(`Correct ScH drug`, na.rm = T)/sum(`SCH Numbers`)*100)),
            'Correct age group' = (round(sum(`Correct ScH age group`, na.rm = T)/sum(`SCH Numbers`)*100)),
            'Correct dosage' = (round(sum(`Correct SCH Dosage`, na.rm = T) /sum(`SCH Numbers`)*100)))

ng_sth_knowledge$Type <- "STH"
ng_sch_knowledge$Type <- "SCH"
ng_knowledge <- rbind(ng_sth_knowledge, ng_sch_knowledge)
ng_knowledge1 <- gather(ng_knowledge, "Variables", "values", 'Worms treated':'Correct dosage')

########################Materials

#Nigeria_TT_Obs <- mutate(Nigeria_TT_Obs,
                         #posters = case_when(
                           #tto1110_posters_dist_sch < 4 ~ "Yes, to all the schools",
                           #tto1110_posters_dist_sch < 4 ~ "No")
#)

Nigeria_TT_PrePost <- Nigeria_TT_PrePost%>%
  mutate(  Chew_Contact = case_when(
    ttp214b_contact_chew == 1 ~ "Yes",
    ttp214b_contact_chew == 0 ~ "No")
  )


#Nigeria_TT_Obs$posters <- " "
#Nigeria_TT_Obs[Nigeria_TT_Obs$tto1110_posters_dist_sch < 4, "posters"] <- "Yes, to all the schools"
#Nigeria_TT_Obs[Nigeria_TT_Obs$tto1110_posters_dist_sch > 3,'posters'] <- "No"

ng_materials <- Nigeria_TT_Obs %>%
  select(State, Year, Round, tto1106_drugs_dist_sch,tto1107_forms_dist_sch, tto1108_tapes_dist_sch,tto1109_register_dist_sch,tto1110_posters_dist_sch )

ng_materials1 <- gather(ng_materials, Variables, values, tto1106_drugs_dist_sch:tto1110_posters_dist_sch, factor_key=TRUE)


##ng_materials1$values[which(is.na(ng_materials1$values ))] <- "No"

#ng_topic_coverage$values <- factor(ng_topic_coverage$values, levels = c("Not Covered", "Covered"))

ng_pole1 <- Nigeria_TT_Obs %>%
  group_by(State, Year, Round, tto504_tabletpole_explained) %>%
  summarise(`Tablet pole demonstration` = (n()))

ng_pole1 <-  ng_pole1[!is.na(ng_pole1$tto504_tabletpole_explained),] 

###################Contact of CHEW
ng_contacts <- Nigeria_TT_PrePost %>%
  group_by(State, Year, Round, ttp214b_contact_chew) %>%
  summarise(Contacts = paste(round(n()/nrow(ng_tp)*100),"%",sep=""))

ng_late <- Nigeria_TTPrePost_Extra %>%
  select(State, Year, Round, `Invited late`,	'Had to go to class/school first,',	'Had to travel  long distance')

ng_late1 <- gather(ng_late, Variables, values, `Invited late`,	'Had to go to class/school first,',	'Had to travel  long distance', factor_key=TRUE)



ng_summaryforms <- Nigeria_TTPrePost_Extra %>%
  filter(source_file == "Post") %>%
  group_by(State, Year, Round, `Summary form`) %>%
  summarise(`Summary Forms` = paste(round(n()/!is.na(source_file)*100),"%",sep=""))
ng_summaryforms  <- unique(ng_summaryforms )

ng_supplies <- Nigeria_TTPrePost_Extra %>%
  filter(source_file == "Post") %>%
  group_by(State, Year, Round) %>%
  summarise(`Enough Forms` = paste(round(sum(Forms)/n() *100),"%",sep=""),
            `Enough Drugs` = paste(round(sum(Drugs)/n() *100),"%",sep=""))

####THE APP


function(input, output, session) {
  
  observeEvent(input$dtw_Kenya,{
    ## Switch active tab to 'Page 2'
    updateTabsetPanel(session, "navbar",
                      selected = "dtw1")
  })
  
  observeEvent(input$dtw_Nigeria,{
    ## Switch active tab to 'Page 2'
    updateTabsetPanel(session, "navbar",
                      selected = "dtw2")
  })
  
  observeEvent(input$dtw_Pakistan,{
    ## Switch active tab to 'Page 2'
    updateTabsetPanel(session, "navbar",
                      selected = "dtw3")
  }) 
  
  observeEvent(input$dtw_Kenya,{
    updateTabsetPanel(session = session, inputId = "dsw", selected = "dtw1")
  })
  observeEvent(input$accelerator_dashhboards,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "Accelerator")
  })
  observeEvent(input$dtw_dashboards,{
    updateTabsetPanel(session = session, inputId = "navbar", selected = "DTW Kenya")
  })
  
  
  output$SCTPost_title <- renderText({
    paste(input$years, " Wave ",input$Wave, "SCT POST DASHBOARDS")
  })
  
  output$SCTOBS_title <- renderText({
    paste(input$years, " Wave ",input$Wave, "SCT OBS DASHBOARDS")
  })
  
  output$SCTCHEW_title <- renderText({
    paste(input$years, " Wave ",input$Wave, "SCT CHEW DASHBOARDS")
  }) 
  
  
  sc <- reactive ({
    SCT_Post %>%
      mutate(County = sctp012_county)%>%
      group_by(County)%>%
      filter(Wave == input$Wave & Year == input$years)%>%
      dplyr:: summarise(Number_of_Sub_Counties = n())
  })
  
  output$subcounties <- renderDataTable({
    sc()
  })
  
  ###FEEDBACK
  feedback_all <- reactive ({
    SCT_Post %>%
      mutate(Feedback = sctp304_feedback,
             County = sctp012_county,
             Sub_County = sctp013_sub_county)%>%
      group_by(County, Sub_County, Feedback)%>%
      filter(Wave == input$Wave & Year == input$years)%>%
      select(County, Sub_County, Feedback)
  })
  
  feedback_sc <- reactive ({
    SCT_Post %>%
      mutate(Feedback = sctp304_feedback,
             County = sctp012_county,
             Sub_County = sctp013_sub_county)%>%
      group_by(County, Sub_County, Feedback)%>%
      filter(Wave == input$Wave & Year == input$years & SCT_Post$sctp012_county_name == input$County ) %>%
      select(County, Sub_County, Feedback)
  })
  
  output$fb <- renderDataTable({
    
    {(feedback_all())}
    
  })
  
  
  
  
  county_rows <- reactive({
    SCT_Post %>%
      dplyr::filter(SCT_Post$sctp012_county == input$County & Year == input$years & Wave == input$Wave & !is.na(Treatment))
  })
  
  rows <- reactive({
    SCT_Post %>%
      filter(Year == input$years & Wave == input$Wave & !is.na(Treatment))
    
  })
  
  
  
  ###TREATMENTTYPES
  
  trt_all <- reactive ({
    x<- SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave & !is.na(Treatment))%>%
      group_by(Year, Wave,Treatment)%>%
      dplyr:: summarise(trt = (n()/nrow(rows()))*100)%>%
      ungroup()%>%
      mutate(Labels = paste(Treatment, "(",trt,"%)"))%>%
      select(Labels, trt)%>%
      tibble::deframe()
  })
  
  
  sc_trt <- reactive ({
    SCT_Post %>%
      filter((SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave & !is.na(Treatment)))%>%
      group_by(Year, Treatment)%>%
      dplyr::  summarise(trt = (n()/nrow(county_rows()))*100)%>%
      ungroup()%>%
      mutate(Labels = paste(Treatment, "(",trt,"%)"))%>%
      select(Labels, trt)%>%
      tibble::deframe()
    
    
  })
  
  
  output$trt_type <- renderPlot({
    if (input$County == 'Kenya')
    {return(waffle(trt_all(),rows = 10,
                   colors = c('#FE6F87',"grey", "grey"),
                   title = "Treatment Types",
                   legend_pos = "bottom")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    
    waffle(sc_trt(),rows = 10,
           colors = c('#FE6F87','grey'),
           title = "Treatment Types",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  
  
  
  #### STH DRUGS
  sthdrugs_all <- reactive ({
    SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave & !is.na(Treatment))%>%
      group_by(Year, Wave,`STH Drug`)%>%
      dplyr:: summarise(drug = paste(round(n()/nrow(rows())*100),"%", sep=""))%>%
      ungroup()%>%
      filter(`STH Drug` == "Albendazole")%>%
      mutate(Drugs = HTML(paste(drug, sep ="")))%>%
      select(drug)
  })
  
  sc_sthdrug <- reactive ({
    SCT_Post %>%
      filter((SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave & !is.na(Treatment)))%>%
      group_by(Year, `STH Drug`)%>%
      dplyr::  summarise(drug = round(n()/nrow(county_rows())*100))%>%
      ungroup()%>%
      filter(SCT_Post$`STH Drug` == "Albendazole")%>%
      mutate(Drugs = paste( drug,"%"))%>%
      select(Drugs)
  })
  
  output$sth_drugs <- renderInfoBox ({
    if (input$County == 'Kenya')  
    {return (infoBox(
      h4("Correct STH Drug"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",  sthdrugs_all()$drug),
      icon = icon("prescription-bottle-alt"),
      color = 'aqua'
    ))}
    
    infoBox(
      h4("Correct STH Drug"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", sc_sthdrug()$Drugs),
      icon = icon("prescription-bottle-alt")
    )
    
  })     
  
  
  ####STH DOSAGE
  
  sthdosage_all <- reactive ({
    SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave & !is.na(Treatment))%>%
      group_by(Year, Wave,`STH Dosage`)%>%
      dplyr:: summarise(dosage = round(n()/nrow(rows())*100))%>%
      ungroup()%>%
      filter(`STH Dosage` == "One Tablet Per Child")%>%
      mutate(Drugs = paste( dosage,"%"))%>%
      select(Drugs)
  })
  
  
  
  sc_sthdsage <- reactive ({
    SCT_Post %>%
      filter((SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave & !is.na(Treatment)))%>%
      group_by(Year, `STH Dosage`)%>%
      dplyr::  summarise(dosage = round(n()/nrow(county_rows())*100))%>%
      ungroup()%>%
      filter(`STH Dosage` == "One Tablet Per Child")%>%
      mutate(Drugs = paste( dosage,"%"))%>%
      select(Drugs)
  })
  
  output$sth_dosage <- renderInfoBox({
    if (input$County == 'Kenya')  
    {return (infoBox(
      h4("Correct STH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",  sthdosage_all()$Drugs),
      icon = icon("capsules")
    ))}
    
    infoBox(
      h4("Correct STH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", sc_sthdsage()$Drugs),
      icon = icon("capsules")
    )
    
  })     
  
  
  
  #STH AGE GROUP
  sthage_all <- reactive ({
    SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave & !is.na(Treatment))%>%
      group_by(Year, Wave,`STH Age`)%>%
      dplyr:: summarise(age = round(n()/nrow(rows())*100))%>%
      ungroup()%>%
      dplyr:: filter(`STH Age` == "2-14 years")%>%
      dplyr::mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
  })
  
  sc_sthage <- reactive ({
    SCT_Post %>%
      filter((SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave & !is.na(Treatment)))%>%
      group_by(Year, `STH Age`)%>%
      dplyr::  summarise(age = round(n()/nrow(county_rows())*100))%>%
      ungroup()%>%
      dplyr:: filter(`STH Age` == "2-14 years")%>%
      mutate(Drugs = paste( age,"%"))%>%
      select(Drugs)
  })
  
  output$sth_age <- renderInfoBox({
    if (input$County == 'Kenya')  
    {return (infoBox(
      h4("Correct STH Age Group"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",  sthage_all()$Drugs),
      icon = icon("calendar-week"),
      color = "maroon"
    ))}
    
    infoBox(
      h4("Correct STH Age Group"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", sc_sthage()$Drugs),
      icon = icon("calendar-week")
    )
    
  })
  
  
  ###SICK CHILD RECEIVE DRUGS 
  sick_all <- reactive ({
    SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave & !is.na(Treatment))%>%
      group_by(Year, Wave,`Sick children`)%>%
      dplyr:: summarise(sick = round(n()/nrow(rows())*100))%>%
      mutate(Labels = paste(`Sick children`, "(",sick,"%)"))%>%
      ungroup()%>%
      select(Labels, sick)%>%
      tibble::deframe()
  })
  
  sc_sick <- reactive ({
    SCT_Post %>%
      filter((SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave & !is.na(Treatment)))%>%
      group_by(Year, `Sick children`)%>%
      dplyr::  summarise(sick = round(n()/nrow(county_rows())*100))%>%
      mutate(Labels = paste(`Sick children`, "(",sick,"%)"))%>%
      ungroup()%>%
      select(Labels, sick)%>%
      tibble::deframe()
  })
  
  
  output$sick <- renderPlot({
    if (input$County == 'Kenya')
    {return(waffle(sick_all(),rows = 10,
                   colors = c('#FE6F87',"grey", "grey"),
                   title = "Should Sick Children deworm",
                   legend_pos = "bottom")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    
    waffle(sc_sick(),rows = 10,
           colors = c('#FE6F87','grey'),
           title = "Should Sick Children deworm",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  
  #### SCH DRUGS
  schdrugs_all <- reactive ({
    SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave & !is.na(SCH)  & !is.na(Treatment) & !is.na(`SCH Drug`))%>%
      group_by(Year, Wave,`SCH Drug`)%>%
      dplyr:: summarise(drug = (n()/sum(!is.na(rows()$`SCH Drug`)))*100)%>%
      ungroup()%>%
      dplyr:: filter(`SCH Drug` == "Praziquantel")%>%
      mutate(Drugs = paste(`SCH Drug`," ", drug,"%"))%>%
      select(Drugs)
  })
  
  
  sc_schdrug <- reactive ({
    SCT_Post %>%
      filter(SCT_Post$sctp012_county == input$County & !is.na(SCH) & SCT_Post$Year == input$years & Wave == input$Wave & !is.na(SCH)  & !is.na(Treatment) & !is.na(`SCH Drug`))%>%
      group_by(Year, `SCH Drug`)%>%
      dplyr::  summarise(drug = (n()/sum(!is.na(county_rows()$`SCH Drug`)))*100)%>%
      ungroup()%>%
      dplyr::  filter(`SCH Drug` == "Praziquantel")%>%
      mutate(Drugs = paste(`SCH Drug`," ", drug,"%"))%>%
      select(Drugs)
  })
  
  
  output$sch_drugs <- renderInfoBox({
    validate(
      need(schdrugs_all()$drug, 'No SCHISTO Data at the Moment'))
    if (input$County == 'Kenya')  
    {return (infoBox(
      h4("Correct SCH Drug"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", sthdrugs_all()$Drugs),
      icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4( "Correct SCH Drug"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", sc_sthdrug()$Drugs),
      icon = icon("prescription-bottle-alt")
    )
    
  })
  
  
  ####SCH DOSAGE
  
  schdosage_all <- reactive ({
    SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave   & !is.na(Treatment) & !is.na(`SCH Drug`))%>%
      group_by(Year, Wave,`SCH Dosage`)%>%
      dplyr:: summarise(dosage = (n()/sum(!is.na(rows()$`SCH Dosage`)))*100)%>%
      ungroup()%>%
      dplyr:: filter(`SCH Dosage` == "According to tablet pole")%>%
      mutate(Drugs = paste(`SCH Dosage`," ", dosage,"%"))%>%
      select(Drugs)
    
  })
  
  sc_schdsage <- reactive ({
    SCT_Post %>%
      filter((SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave  & !is.na(Treatment) & !is.na(`SCH Drug`)))%>%
      group_by(Year, `SCH Dosage`)%>%
      dplyr::  summarise(dosage = (n()/sum(!is.na(county_rows()$`SCH Dosage`)))*100)%>%
      ungroup()%>%
      dplyr::  filter(`SCH Dosage` == "According to tablet pole")%>%
      mutate(Drugs = paste(`SCH Dosage`," ", dosage,"%"))%>%
      select(Drugs)
    
    
  })
  
  output$sch_dosage <- renderInfoBox({
    validate(
      need(schdrugs_all()$drug, 'No SCHISTO Data at the Moment')) 
    if (input$County == 'Kenya')  
    {return (infoBox(
      h4("Correct SCH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", schdosage_all()$Drugs),
      icon = icon("capsules")
    ))}
    
    infoBox(
      h4("Correct SCH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold;;", sc_schdsage()$Drugs),
      icon = icon("capsules")
    )
    
  })
  
  
  #SCH AGE GROUP
  
  
  schage_all <- reactive ({
    validate(
      need(schdrugs_all()$drug, 'No SCHISTO Data at the Moment'))
    
    SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave   & !is.na(Treatment) & !is.na(`SCH Drug`))%>%
      group_by(Year, Wave,`SCH Age`)%>%
      dplyr:: summarise(age =(n()/sum(!is.na(rows()$`SCH Age`)))*100)%>%
      ungroup()%>%
      dplyr:: filter(`SCH Age` == "6-14 years")%>%
      mutate(Drugs = paste(`SCH Age`," ", age,"%"))%>%
      select(Drugs)
    
  })
  
  sc_schage <- reactive ({
    SCT_Post %>%
      filter((SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave  & !is.na(Treatment) & !is.na(`SCH Drug`)))%>%
      group_by(Year, `SCH Age`)%>%
      dplyr::  summarise(age = (n()/sum(!is.na(county_rows()$`SCH Ahe`)))*100)%>%
      ungroup()%>%
      dplyr:: filter(`SCH Age` == "6-14 years")%>%
      mutate(Drugs = paste(`SCH Age`," ", age,"%"))%>%
      select(Drugs)
    
    
  })
  output$sch_age <- renderInfoBox({
    if (input$County == 'Kenya')  
    {return (infoBox(
      h4("Correct SCH Age Group?"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", schage_all()$Drugs),
      icon = icon("calendar-week")
    ))}
    
    infoBox(
      h4("Correct SCH Age Group?"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", sc_schage()$Drugs),
      icon = icon("calendar-week")
    )
    
  })
  
  
  
  ####TRAINING DATA
  formp_all <- reactive ({
    SCT_Post %>%
      filter(!is.na(date1)& Year == input$years & Wave == input$Wave   & !is.na(Treatment))%>%
      group_by(Year, Wave,`Updating Form`)%>%
      dplyr:: summarise(formP = round(n()/sum(!is.na(rows()$date1))*100))%>%
      ungroup()%>%
      mutate(Labels = paste(`Updating Form`, "(",formP,"%)"))%>%
      ungroup()%>%
      select(Labels, formP)%>%
      tibble::deframe()
  })
  
  sc_formp <- reactive ({
    SCT_Post %>%
      filter((!is.na(date1) & SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave  & !is.na(Treatment) ))%>%
      group_by(Year, `Updating Form`)%>%
      dplyr::  summarise(formP = round(n()/sum(!is.na(county_rows()$date1)))*100)%>%
      mutate(Labels = paste(`Updating Form`, "(",formP,"%)"))%>%
      ungroup()%>%
      select(Labels, formP)%>%
      tibble::deframe()
  })
  
  
  output$FormP <- renderPlot({
    if (input$County == 'Kenya')
    {return(waffle(formp_all(),rows = 10,
                   colors = c('#FE6F87',"grey", "grey"),
                   title = "When to Update the Form",
                   legend_pos = "bottom")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    waffle(sc_formp(),rows = 10,
           colors = c('#FE6F87','grey'),
           title = "When to Update the Form",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  ####TRAINING DATA
  noforms_all <- reactive ({
    SCT_Post %>%
      filter( Year == input$years & Wave == input$Wave   & !is.na(Treatment))%>%
      group_by(Year, Wave,`Number of forms`)%>%
      dplyr:: summarise(no_forms = (n()/sum(!is.na(rows()$Year)))*100)%>%
      mutate(Labels = paste(`Number of forms`, "(",no_forms,"%)"))%>%
      ungroup()%>%
      select(Labels, no_forms)%>%
      tibble::deframe()
  })
  
  sc_noforms <- reactive ({
    SCT_Post %>%
      filter((SCT_Post$sctp012_county == input$County & SCT_Post$Year == input$years & Wave == input$Wave  & !is.na(Treatment) ))%>%
      group_by(Year, `Number of forms`)%>%
      dplyr::  summarise(no_forms = (n()/sum(!is.na(county_rows()$Year)))*100)%>%
      mutate(Labels = paste(`Number of forms`, "(",no_forms,"%)"))%>%
      ungroup()%>%
      select(Labels, no_forms)%>%
      tibble::deframe()
  })
  
  
  output$No_Forms <- renderPlot({
    if (input$County == 'Kenya')
    {return(waffle(formp_all(),rows = 10,
                   colors = c('#FE6F87',"grey", "grey"),
                   title = "Number of forms to fill",
                   legend_pos = "bottom")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    waffle(sc_formp(),rows = 10,
           colors = c('#FE6F87','grey'),
           title = "Number of forms to fill",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  
  ####SCT OBS
  sc_obs <- reactive ({
    SCT_OBS %>%
      mutate(County = scto012_county)%>%
      group_by(County)%>%
      filter(Wave == input$Wave2 & Year == input$years33)%>%
      dplyr:: summarise(Number_of_Sub_Counties = n())
  })
  
  output$Subcs <- DT ::  renderDataTable({
    sc_obs()
  })
  ####ROWS
  kenya_rows <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)
  }) 
  
  county_rows <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)
  })
  
  #####COVID
  trainers_mask_all <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year, Wave, scto110_trainers_wear_mask)%>%
      dplyr:: summarise(  trainers_mask = round(n()/sum(!is.na(kenya_rows()$Year))*100))
  })
  
  trainers_mask_county <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year, Wave, scto110_trainers_wear_mask, scto012_county)%>%
      dplyr:: summarise(  trainers_mask = round(n()/sum(!is.na(county_rows()$Year))*100))
  })
  
  output$trainers_masks <- renderPlot ({
    if (input$County2 == 'Kenya') 
    {return(ggplot(data = trainers_mask_all(), 
                   aes(x = 2, y = trainers_mask, fill= scto110_trainers_wear_mask ))+
              geom_bar(stat = "identity")+
              ggtitle("Were all the trainers wearing protective face \n masks during the training?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(trainers_mask,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = trainers_mask_county(), 
            aes(x = 2, y = trainers_mask, fill= scto110_trainers_wear_mask ))+
        geom_bar(stat = "identity")+
        ggtitle("Were all the trainers wearing protective face \n masks during the training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(trainers_mask,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  
  participants_mask_all <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year, Wave, scto111_partc_wear_mask)%>%
      dplyr:: summarise(  participants_mask = round(n()/sum(!is.na(kenya_rows()$Year))*100))
  })
  
  participants_mask_county <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year, Wave, scto111_partc_wear_mask, scto012_county)%>%
      dplyr:: summarise(  participants_mask = round(n()/sum(!is.na(county_rows()$Year))*100))
  })
  
  output$participants_masks <- renderPlot ({
    if (input$County2 == 'Kenya') 
    {return(ggplot(data = participants_mask_all(), 
                   aes(x = 2, y = participants_mask, fill= scto111_partc_wear_mask ))+
              geom_bar(stat = "identity")+
              ggtitle("Were all the participants wearing protective face \n masks during the training?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(participants_mask,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = participants_mask_county(), 
            aes(x = 2, y = participants_mask, fill= scto111_partc_wear_mask ))+
        geom_bar(stat = "identity")+
        ggtitle("Were all the participants wearing protective face \n masks during the training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(participants_mask,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  ####HAND SANITIZER AVAILABEL
  sanitizer_all <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year, Wave, scto113_sanitizer_available)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(kenya_rows()$Year))*100))
  })
  
  sanitizer_county <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year, Wave, scto113_sanitizer_available, scto012_county)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(county_rows()$Year))*100))
  })
  
  output$sanitizer <- renderPlot ({
    if (input$County2 == 'Kenya') 
    {return(ggplot(data = sanitizer_all(), 
                   aes(x = 2, y = sanitizer, fill= scto113_sanitizer_available ))+
              geom_bar(stat = "identity")+
              ggtitle("Was hand sanitizer or a hand washing facility \n available at the training?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = sanitizer_county(), 
            aes(x = 2, y = sanitizer, fill= scto113_sanitizer_available ))+
        geom_bar(stat = "identity")+
        ggtitle("Was hand sanitizer or a hand washing \n facility available at the training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  
  ####SANITIZE VANUE
  sanitize_venue_all <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year, Wave, scto114_venue_sanitized)%>%
      dplyr:: summarise(  sanitize_venue = round(n()/sum(!is.na(kenya_rows()$Year))*100))
  })
  
  sanitize_venue_county <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year, Wave, scto114_venue_sanitized, scto012_county)%>%
      dplyr:: summarise(  sanitize_venue = round(n()/sum(!is.na(county_rows()$Year))*100))
  })
  
  output$sanitize_venue <- renderPlot ({
    if (input$County2 == 'Kenya') 
    {return(ggplot(data = sanitize_venue_all(), 
                   aes(x = 2, y = sanitize_venue, fill= scto114_venue_sanitized ))+
              geom_bar(stat = "identity")+
              ggtitle("Was the training venue cleaned/sanitized \n before this training?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(sanitize_venue,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = sanitize_venue_county(), 
            aes(x = 2, y = sanitize_venue, fill= "grp_train_admin:scto114_venue_sanitized" ))+
        geom_bar(stat = "identity")+
        ggtitle("Was the training venue cleaned/sanitized before this training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(sanitize_venue,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  ###Participants wearing Masks  
  parts_mask_all <- reactive({
    SCT_OBS%>%
      group_by(scto112_num_partc_wear)%>%
      filter(Wave == input$Wave2 & Year == input$years2 & !is.na(scto112_num_partc_wear))%>%
      dplyr:: summarise( parts_mask = n()/sum(!is.na(kenya_rows()$scto112_num_partc_wear))*100)
  })
  
  parts_mask <- reactive({
    SCT_OBS%>%
      group_by(scto112_num_partc_wear)%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2 & !is.na(scto112_num_partc_wear))%>%
      dplyr:: summarise( parts_mask = n()/sum(!is.na(county_rows()$scto112_num_partc_wear))*100)
  })
  
  output$num_parts_mask <- renderPlot({
    if (input$County2 == 'Kenya') 
    {return ( ggplot(data = parts_mask_all(), aes(x = scto112_num_partc_wear, y =  parts_mask, fill=scto112_num_partc_wear))+
                geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
                ggtitle("How many participants were wearing protective \n face masks during the training?")+
                scale_fill_manual(values = c('#FE6F87', '#5E5F5F', 'grey'), aesthetics = "fill") +
                #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
                geom_text(aes(label = paste(round( parts_mask),"%",sep=""),  hjust = "right", size = 4),
                          colour = "white",
                          position = position_dodge(width = .0),
                          size = 5,
                          show.legend = FALSE,
                          fontface="bold") +
                theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
                theme(axis.text.y = element_text(color="BlACK",size=14)) +
                theme(axis.title.x = element_blank(), 
                      axis.ticks.x =  element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.x = element_blank(),
                      #axis.text.y = element_blank(), 
                      # axis.ticks.y = element_blank(),
                      plot.margin = unit(c(1,1,1,0), "mm"),
                      legend.position = "none",
                      panel.border = element_blank()) +
                coord_flip()+
                removeGrid())}
    ( ggplot(data = parts_mask(), aes(x = scto112_num_partc_wear, y =  parts_mask, fill=scto112_num_partc_wear))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("How many participants were wearing protective \n face masks during the training?")+
        scale_fill_manual(values = c('#FE6F87', '#5E5F5F', 'grey'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round( parts_mask),"%",sep=""), hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid())
  })
  
  ###ATTENDANCE SHEET 
  att_sheet_all <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year, Wave, attendance_sheet)%>%
      dplyr:: summarise(Attendance = n()/sum(!is.na(kenya_rows()$Year))*100)
  })
  
  att_sheet <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year, Wave, attendance_sheet, scto012_county)%>%
      dplyr:: summarise(Attendance = n()/sum(!is.na(county_rows()$Year))*100)
  })
  
  
  output$attendance <- renderPlot({
    if (input$County2 == 'Kenya') 
    {return( ggplot(data = att_sheet_all(), aes(x = attendance_sheet, y = att_sheet_all()$Attendance, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("Is there an attendance sheet?")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(att_sheet_all()$Attendance),"%",sep=""),  hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    { ggplot(data = att_sheet(), aes(x = attendance_sheet, y = Attendance, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("Is there an attendance sheet?")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Attendance),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black", size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  #####BOOKLETS GIVEN
  
  
  booklets_given_all <- reactive({
    SCT_OBS%>%
      group_by(scto107_given)%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      dplyr:: summarise(Given = n()/sum(!is.na(kenya_rows()$Year))*100)
  })
  
  booklets_given <- reactive({
    SCT_OBS%>%
      group_by(scto107_given)%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      dplyr:: summarise(Given = n()/sum(!is.na(county_rows()$Year))*100)
  })
  
  output$booklets <- renderPlot({
    if (input$County2 == 'Kenya') 
    {return ( ggplot(data = booklets_given_all(), aes(x = scto107_given, y = Given, fill=scto107_given))+
                geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
                ggtitle("Was the Sub county Training Booklet distributed to \n all the participants?")+
                scale_fill_manual(values = c('#FE6F87', '#5E5F5F', '#FE6F87'), aesthetics = "fill") +
                #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
                geom_text(aes(label = paste(round(Given),"%",sep=""),  hjust = "right", size = 4),
                          colour = "white",
                          position = position_dodge(width = .0),
                          size = 5,
                          show.legend = FALSE,
                          fontface="bold") +
                theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
                theme(axis.text.y = element_text(color="BlACK",size=14)) +
                theme(axis.title.x = element_blank(), 
                      axis.ticks.x =  element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.x = element_blank(),
                      #axis.text.y = element_blank(), 
                      # axis.ticks.y = element_blank(),
                      plot.margin = unit(c(1,1,1,0), "mm"),
                      legend.position = "none",
                      panel.border = element_blank()) +
                coord_flip()+
                removeGrid())}
    ( ggplot(data = booklets_given(), aes(x = scto107_given, y = Given, fill=scto107_given))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("Was the Sub county Training Booklet distributed \n to all the participants?")+
        scale_fill_manual(values = c('#FE6F87', '#5E5F5F', '#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Given),"%",sep=""), hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid())
  })
  ####ATTENDANCE RATES
  
  attrate_1 <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      dplyr:: summarise(Day1 = round(mean(as.numeric(scto702_total_partcipants_day1), na.rm = T)))
  })
  
  sc_attrate_1 <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      dplyr:: summarise(Day1 = round(mean(as.numeric(scto702_total_partcipants_day1))))
  })
  
  attrate_2 <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      dplyr:: summarise(Day2 = round(mean(as.numeric(scto1202_total_partcipants_day2))))
  })
  
  sc_attrate_2 <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      dplyr:: summarise(Day2 = round(mean(as.numeric(scto1202_total_partcipants_day2))))
  })
  
  
  output$box1 <- renderInfoBox({
    if (input$County2 == 'Kenya')  
    {return (infoBox(
      h4("Average # of participants attending training on day 1"),
      tags$p(style = "font-size: 30px;", attrate_1()$Day1)
      #icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4("Average # of participants attending training on day 1"),
      tags$p(style = "font-size: 30px;", sc_attrate_1()$Day1)
      #icon = icon("prescription-bottle-alt")
    )
    
  })
  
  
  output$box2 <- renderInfoBox({
    if (input$County2 == 'Kenya') 
    {return(infoBox(
      h4("Average # of participants attending training on day 2"),
      tags$p(style = "font-size: 30px;", attrate_2()$Day2)
      #icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4("Average # of participants attending training on day 2"),
      tags$p(style = "font-size: 30px;", sc_attrate_2()$Day2)
      #icon = icon("prescription-bottle-alt")
    )
  })
  
  ###attendance rate at start and end
  
  attrate_start <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      dplyr:: summarise(Day1 =paste(round(mean(attendance_rate_start)*100),"%", sep=""))
  })
  
  sc_attrate_start <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      dplyr:: summarise(Day1 = paste(round(mean(attendance_rate_start)*100),"%", sep=""))
  })
  
  attrate_end <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      dplyr:: summarise(Day2 = paste(round(mean(attendance_rate_1)*100),"%", sep=""))
  })
  
  sc_attrate_end <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      dplyr:: summarise(Day2 = paste(round(mean(attendance_rate_1)*100),"%", sep=""))
  })
  
  output$box3 <- renderInfoBox({
    if (input$County2 == 'Kenya') 
    {return(infoBox(
      h4("Attendance Rate on day 1 at the start"),
      tags$p(style = "font-size: 30px;", attrate_start()$Day1)
      #icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4("Attendance Rate on day 1 at the start"),
      tags$p(style = "font-size: 30px;", sc_attrate_start()$Day1)
      #icon = icon("prescription-bottle-alt")
    )
  })
  
  output$box4 <- renderInfoBox({
    if (input$County2 == 'Kenya') 
    {return(infoBox(
      h4("Attendance Rate on day 1 at the end"),
      tags$p(style = "font-size: 30px;", attrate_end()$Day2)
      #icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4("Attendance Rate on day 1 at the end"),
      tags$p(style = "font-size: 30px;", sc_attrate_end()$Day2)
      #icon = icon("prescription-bottle-alt")
    )
  })
  ####TOPIC COVERAGE
  tpoiccoverage_all <- reactive({
    topics2%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Variables,Values) %>%
      dplyr:: summarise(coverage = round(n()/nrow(kenya_rows())*100))
  })
  
  topiccoverage_county <- reactive({
    topics2%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Variables,Values) %>%
      dplyr:: summarise(coverage = (round(n()/nrow(county_rows())*100)))
  })
  
  output$coverage <- renderPlot ({
    
    if (input$County2 == 'Kenya') 
    {return(ggplot(data = tpoiccoverage_all(), aes(stringr :: str_wrap(Variables), y = coverage, fill= Values))+
              geom_bar(stat="identity") +
              ggtitle("Topic Coverage")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F','grey','dark grey'))+
              aes(stringr::str_wrap(Variables, 25), coverage) + xlab(NULL)+
              geom_text(aes(label = paste(tpoiccoverage_all()$coverage,"%",sep=""), hjust = "right"),
                        colour = "white",
                        #position = position_dodge(width = -.3),
                        position = position_stack(vjust = 0.5),
                        size = 5,
                        fontface="bold",
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="bottom") +
              removeGrid())}
    (ggplot(data = topiccoverage_county(), aes(stringr :: str_wrap(Variables), y = coverage, fill= Values))+
        geom_bar(stat="identity") +
        ggtitle("Topic Coverage")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        geom_text(aes(label = paste(topiccoverage_county()$coverage,"%",sep=""), hjust = "left"),
                  colour = fct_rev(Grade),
                  position = position_dodge(width = .2),
                  size = 5,
                  fontface="bold",
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  #####NOT TREATED
  not_trt_all <- reactive({
    not_trt%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(not_trt = round(n()/nrow(kenya_rows())*100))
  })
  
  not_trt_county <- reactive({
    not_trt%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(not_trt = (round(n()/nrow(county_rows())*100)))
  })
  
  output$not_treated <- renderPlot({
    if (input$County2 == 'Kenya') 
    {return( ggplot(data = not_trt_all(), aes(stringr :: str_wrap( Values), y = not_trt, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(Values, 30), not_trt) + xlab(NULL)+
               ggtitle("Which children should not be treated under the program?")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(not_trt),"%",sep=""),  hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = not_trt_county(), aes(x = Values, y = not_trt, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(Values, 30), not_trt) + xlab(NULL)+
        ggtitle("Which children should not be treated under the program?")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(not_trt),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  #####CHEW ROLES
  chew_roles_all <- reactive({
    chew_roles%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(chew_roles = round(n()/nrow(kenya_rows())*100))
  })
  
  chew_roles_county <- reactive({
    chew_roles%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(chew_roles = (round(n()/nrow(county_rows())*100)))
  })
  
  output$Roles_chew <- renderPlot({
    if (input$County2 == 'Kenya') 
    {return( ggplot(data = chew_roles_all(), aes(x = Values, y = chew_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
               ggtitle("Roles of CHEW")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = chew_roles_county(), aes(x = Values, y = chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
        ggtitle("Roles of CHEW")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  #####HT ROLES
  HT_roles_all <- reactive({
    HT_roles%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(HT_roles = round(n()/nrow(kenya_rows())*100))
  })
  
  HT_roles_county <- reactive({
    HT_roles%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(HT_roles = (round(n()/nrow(county_rows())*100)))
  })
  
  output$Roles_HT <- renderPlot({
    if (input$County2 == 'Kenya') 
    {return( ggplot(data = HT_roles_all(), aes(x = Values, y = HT_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(Values, 30), HT_roles) + xlab(NULL)+
               ggtitle("What is the responsibility of the Head Teacher/ Health Teacher? ")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(HT_roles),"%",sep=""),  hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = HT_roles_county(), aes(x = Values, y = HT_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(Values, 30), HT_roles) + xlab(NULL)+
        ggtitle("What is the responsibility of the Head Teacher/ Health Teacher? ")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(HT_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  #####DRUG STATUS
  drug_availability_all <- reactive({
    SCT_OBS%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year,scto402_drug_availability) %>%
      dplyr:: summarise(drug_availability = round(n()/nrow(kenya_rows())*100))
  })
  
  drug_availability_county <- reactive({
    HT_roles%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year, scto402_drug_availability) %>%
      dplyr:: summarise(drug_availability = (round(n()/nrow(county_rows())*100)))
  })
  
  output$Drug_Availability <- renderPlot({
    if (input$County2 == 'Kenya') 
    {return( ggplot(data = drug_availability_all(), aes(x = scto402_drug_availability, y = drug_availability, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(scto402_drug_availability, 30), drug_availability) + xlab(NULL)+
               ggtitle("What was the deworming medicine availability status discussed \n by the SCMOH? ")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(drug_availability),"%",sep=""),  hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = drug_availability_county(), aes(x = scto402_drug_availability, y = drug_availability, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("What was the deworming medicine availability status discussed \n by the SCMOH? ")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        aes(stringr::str_wrap(scto402_drug_availability, 30), drug_availability) + xlab(NULL)+
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(drug_availability),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  
  #####Questions_asked
  Questions_asked_all <- reactive({
    Qst%>%
      filter(Wave == input$Wave2 & Year == input$years2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(Questions_asked = round(n()/nrow(kenya_rows())*100))
  })
  
  Questions_asked_county <- reactive({
    Qst%>%
      filter(Wave == input$Wave2 & Year == input$years2 & scto012_county == input$County2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(Questions_asked = (round(n()/nrow(county_rows())*100)))
  })
  
  output$Questions_Asked <- renderPlot({
    if (input$County2 == 'Kenya') 
    {return( ggplot(data = Questions_asked_all(), aes(x = Values, y = Questions_asked, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(Values, 30), Questions_asked) + xlab(NULL)+
               ggtitle("Areas that participants asked the most questions")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(Questions_asked),"%",sep=""), hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = Questions_asked_county(), aes(x = Values, y = Questions_asked, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(Values, 30), Questions_asked) + xlab(NULL)+
        ggtitle("Areas that participants asked the most questions")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Questions_asked),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  ####CHEW POST
  chew_sc <- reactive ({
    CHEW_POST %>%
      mutate(County = sctp012_county_name)%>%
      group_by(County)%>%
      filter(Wave == input$Wave3 & Year == input$years3)%>%
      dplyr:: summarise(Number_of_Sub_Counties = n())
  })
  
  output$chew_subcounties <- renderDataTable({
    sc()
  })
  
  
  
  chew_county_rows <- reactive({
    CHEW_POST %>%
      dplyr::filter(CHEW_POST$sctp012_county_name == input$County3 & Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs))
  })
  
  chew_rows <- reactive({
    CHEW_POST %>%
      filter(Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs))
    
  })
  ####TREATMENTTYPES
  
  chew_trt_all <- reactive ({
    x<- CHEW_POST %>%
      filter( Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs))%>%
      group_by(Year, Wave,sctp011_ct_obs)%>%
      dplyr:: summarise(trt = (n()/nrow(chew_rows()))*100)
    x <-  x[!is.na(x$trt),] 
  })
  
  chew_sc_trt <- reactive ({
    CHEW_POST %>%
      filter((CHEW_POST$sctp012_county_name == input$County3 & CHEW_POST$Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs)))%>%
      group_by(Year, sctp011_ct_obs)%>%
      dplyr::  summarise(trt = (n()/nrow(chew_county_rows()))*100)
    
    
  })
  
  output$chew_trt_type <- renderPlot({
    if (input$County3 == 'Kenya') 
    {return( ggplot(data = chew_trt_all(), aes(x = sctp011_ct_obs, y = trt, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("What treatment is this training focused on?")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(trt),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = chew_sc_trt(), aes(x = sctp011_ct_obs, y = trt, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("What treatment is this training focused on?")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(trt),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  #### STH DRUGS
  
  chew_sthdrugs_all <- reactive ({
    CHEW_POST %>%
      filter( Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs))%>%
      group_by(Year, Wave,`STH Drug`)%>%
      dplyr:: summarise(drug = (n()/nrow(chew_rows()))*100)%>%
      ungroup()%>%
      filter(`STH Drug` == "Albendazole")%>%
      mutate(Drugs = HTML(paste(drug, sep ="%")))%>%
      select(Drugs)
  })
  
  chew_sc_sthdrugs <- reactive ({
    CHEW_POST %>%
      filter((CHEW_POST$sctp012_county_name == input$County3 & CHEW_POST$Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs)))%>%
      group_by(Year, `STH Drug`)%>%
      dplyr::  summarise(drug = (n()/nrow(chew_county_rows()))*100)%>%
      ungroup()%>%
      filter(`STH Drug` == "Albendazole")%>%
      mutate(Drugs = HTML(paste(drug,"%")))%>%
      select(Drugs)
    
    
  })
  
  output$chew_sth_drugs <- renderInfoBox({
    if (input$County3 == 'Kenya')  
    {return (infoBox(
      h4("Correct STH Drug"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", chew_sthdrugs_all()$Drugs),
      icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4("Correct STH Drug"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", chew_sc_sthdrugs()$Drugs),
      icon = icon("prescription-bottle-alt")
    )
    
  })
  
  
  
  ####STH DOSAGE
  
  chew_sthdosage_all <- reactive ({
    CHEW_POST %>%
      filter( Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs))%>%
      group_by(Year, Wave,`STH Dosage`)%>%
      dplyr:: summarise(dosage = round(n()/nrow(chew_rows())*100))%>%
      filter(`STH Dosage` == "One Tablet Per Child")%>%
      mutate(Drugs = paste( dosage,"%"))%>%
      select(Drugs)
    
    
  })
  
  chew_sc_sthdsage <- reactive ({
    CHEW_POST %>%
      filter((CHEW_POST$sctp012_county_name == input$County3 & CHEW_POST$Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs)))%>%
      group_by(Year, `STH Dosage`)%>%
      dplyr::  summarise(dosage = (n()/nrow(chew_county_rows()))*100)%>%
      filter(`STH Dosage` == "One Tablet Per Child")%>%
      mutate(Drugs = paste(dosage,"%"))%>%
      select(Drugs)
    
    
  })
  
  
  output$chew_sth_dosage <- renderInfoBox({
    if (input$County3 == 'Kenya')  
    {return (infoBox(
      h4("Correct STH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", chew_sthdosage_all()$Drugs),
      icon = icon("capsules")
    ))}
    
    infoBox(
      h4("Correct STH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", chew_sc_sthdsage()$Drugs),
      icon = icon("capsules")
    )
    
  })
  
  
  #STH AGE GROUP
  
  
  chew_sthage_all <- reactive ({
    CHEW_POST %>%
      filter( Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs))%>%
      group_by(Year, Wave,`STH Age`)%>%
      dplyr:: summarise(age =round(n()/nrow(chew_rows())*100))%>%
      ungroup()%>%
      dplyr:: filter(`STH Age` == "2-14 Years")%>%
      dplyr::mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
    
  })
  
  chew_sc_sthage <- reactive ({
    CHEW_POST %>%
      filter((CHEW_POST$sctp012_county_name == input$County3 & CHEW_POST$Year == input$years3 & Wave == input$Wave3 & !is.na(sctp011_ct_obs)))%>%
      group_by(Year, `STH Age`)%>%
      dplyr::  summarise(age = round(n()/nrow(chew_county_rows())*100))%>%
      ungroup()%>%
      dplyr:: filter(`STH Age` == "2-14 Years")%>%
      dplyr::mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
    
    
  })
  
  
  output$chew_sth_age <- renderInfoBox({
    if (input$County3 == 'Kenya')  
    {return (infoBox(
      h4("Correct STH Age Group"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",  chew_sthage_all()$Drugs),
      icon = icon("calendar-week")
    ))}
    
    infoBox(
      h4("Correct STH Age Group"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", chew_sc_sthage()$Drugs),
      icon = icon("calendar-week")
    )
    
  })
  
  
  
  ###STH SIDE EFFECTS
  sth_side_effect_all <- reactive ({
    sth_side_effect %>%
      filter( Year == input$years3 & Wave == input$Wave3 )%>%
      group_by(Year, Wave,Values)%>%
      dplyr:: summarise(Side_Effects = (n()/nrow(chew_rows()))*100)
    
  })
  
  sc_sth_side_effect <- reactive ({
    sth_side_effect %>%
      filter((sth_side_effect$sctp012_county_name == input$County3 & sth_side_effect$Year == input$years3 & Wave == input$Wave3 ))%>%
      group_by(Year, Values)%>%
      dplyr::  summarise(Side_Effects = (n()/nrow(chew_county_rows()))*100)
    
    
  })
  output$sth_side_effects <- renderPlot({
    if (input$County3 == 'Kenya') 
    {return( ggplot(data = sth_side_effect_all(), aes(x = Values, y = Side_Effects, fill= Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("What are the likely expected side effects of STH treatment?")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(Side_Effects),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = sc_sth_side_effect(), aes(x = Values, y = Side_Effects, fill= Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("What are the likely expected side effects of STH treatment?")+
        scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Side_Effects),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  }) 
  
  ###sch SIDE EFFECTS
  sch_side_effect_all <- reactive ({
    sch_side_effect %>%
      filter( Year == input$years3 & Wave == input$Wave3 )%>%
      group_by(Year, Wave,Values)%>%
      dplyr:: summarise(Side_Effects = (n()/nrow(chew_rows()))*100)
    
  })
  
  sc_sch_side_effect <- reactive ({
    sch_side_effect %>%
      filter((sch_side_effect$sctp012_county_name == input$County3 & sch_side_effect$Year == input$years3 & Wave == input$Wave3 ))%>%
      group_by(Year, Values)%>%
      dplyr::  summarise(Side_Effects = (n()/nrow(chew_county_rows()))*100)
    
    
  })
  output$sch_side_effects <- renderPlot({
    validate(
      need(sch_side_effect_all()$Side_Effects, 'No SCHISTO Data at the Moment'))
    if (input$County3 == 'Kenya') 
    {return( ggplot(data = sch_side_effect_all(), aes(x = Values, y = Side_Effects, fill= Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("What are the likely expected side effects of sch treatment?")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(Side_Effects),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = sc_sch_side_effect(), aes(x = Values, y = Side_Effects, fill= Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("What are the likely expected side effects of sch treatment?")+
        scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Side_Effects),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  }) 
  
  #### SCH DRUGS
  
  chew_schdrugs_all <- reactive ({
    CHEW_POST %>%
      filter( Year == input$years3 & Wave == input$Wave3   & !is.na(sctp011_ct_obs) & !is.na(`SCH Drug`))%>%
      group_by(Year, Wave,`SCH Drug`)%>%
      dplyr:: summarise(drug = (n()/sum(!is.na(chew_rows()$`SCH Drug`)))*100)%>%
      ungroup()%>%
      dplyr::  filter(`SCH Drug` == "Praziquantel")%>%
      mutate(Drugs = paste(`SCH Drug`," ", drug,"%"))%>%
      select(Drugs)
    
  })
  
  chew_sc_schdrug <- reactive ({
    CHEW_POST %>%
      filter(CHEW_POST$sctp012_county_name == input$County3  & CHEW_POST$Year == input$years3 & Wave == input$Wave3 & !is.na(SCH)  & !is.na(sctp011_ct_obs) & !is.na(`SCH Drug`))%>%
      group_by(Year, `SCH Drug`)%>%
      dplyr::  summarise(drug = (n()/sum(!is.na(chew_county_rows()$`SCH Drug`)))*100)%>%
      ungroup()%>%
      dplyr::  filter(`SCH Drug` == "Praziquantel")%>%
      mutate(Drugs = paste(`SCH Drug`," ", drug,"%"))%>%
      select(Drugs)
  })
  
  
  output$chew_sch_drugs <- renderInfoBox({
    validate(
      need(sch_side_effect_all()$Side_Effects, 'No SCHISTO Data at the Moment'))
    if (input$County3 == 'Kenya')  
    {return (infoBox(
      h4("What drug will be used to treat STH?"),
      tags$p(style = "font-size: 30px;", chew_schdrugs_all()$Drugs),
      icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4( "What drug will be used to treat STH?"),
      tags$p(style = "font-size: 30px;", chew_sc_schdrug()$Drugs),
      icon = icon("prescription-bottle-alt")
    )
    
  })
  
  
  
  
  
  ####SCH DOSAGE
  
  chew_schdosage_all <- reactive ({
    CHEW_POST %>%
      filter( Year == input$years3 & Wave == input$Wave3   & !is.na(sctp011_ct_obs) & !is.na(`SCH Drug`))%>%
      group_by(Year, Wave,`SCH Dosage`)%>%
      dplyr:: summarise(dosage = (n()/sum(!is.na(chew_rows()$`SCH Dosage`)))*100)%>%
      ungroup()%>%
      dplyr::  filter(`SCH Dosage` == "According to tablet pole")%>%
      mutate(Drugs = paste(`SCH Dosage`," ", dosage,"%"))%>%
      select(Drugs)
  })
  
  chew_sc_schdsage <- reactive ({
    CHEW_POST %>%
      filter((CHEW_POST$sctp012_county_name == input$County3 & CHEW_POST$Year == input$years3 & Wave == input$Wave3  & !is.na(sctp011_ct_obs) & !is.na(`SCH Drug`)))%>%
      group_by(Year, `SCH Dosage`)%>%
      dplyr::  summarise(dosage = (n()/sum(!is.na(chew_county_rows()$`SCH Dosage`)))*100)%>%
      ungroup()%>%
      dplyr::  filter(`SCH Dosage` == "According to tablet pole")%>%
      mutate(Drugs = paste(`SCH Dosage`," ", dosage,"%"))%>%
      select(Drugs)
    
    
  })
  
  output$chew_sch_dosage <- renderInfoBox({
    validate(
      need(sch_side_effect_all()$Side_Effects, 'No SCHISTO Data at the Moment')) 
    if (input$County3 == 'Kenya')  
    {return (infoBox(
      h4("What will be the STH dosage?"),
      tags$p(style = "font-size: 30px;", chew_schdosage_all()$Drugs),
      icon = icon("capsules")
    ))}
    
    infoBox(
      h4("What will be the STH dosage?"),
      tags$p(style = "font-size: 30px;", chew_sc_schdsage()$Drugs),
      icon = icon("capsules")
    )
    
  })
  
  
  
  
  
  #SCH AGE GROUP
  
  
  chew_schage_all <- reactive ({
    CHEW_POST %>%
      filter( Year == input$years3 & Wave == input$Wave3   & !is.na(sctp011_ct_obs) & !is.na(`SCH Drug`))%>%
      group_by(Year, Wave,`SCH Age`)%>%
      dplyr:: summarise(age =(n()/sum(!is.na(chew_rows()$`SCH Age`)))*100)
    dplyr:: summarise(age = (n()/nrow(rows()))*100)%>%
      ungroup()%>%
      dplyr:: filter(`SCH Age` == "6-14 years")%>%
      mutate(Drugs = paste(`SCH Age`," ", age,"%"))%>%
      select(Drugs)
    
    
  })
  
  
  chew_sc_schage <- reactive ({
    CHEW_POST %>%
      filter((CHEW_POST$sctp012_county_name == input$County3 & CHEW_POST$Year == input$years3 & Wave == input$Wave3  & !is.na(sctp011_ct_obs) & !is.na(`SCH Drug`)))%>%
      group_by(Year, `SCH Age`)%>%
      dplyr::  summarise(age = (n()/sum(!is.na(chew_county_rows()$`SCH Ahe`)))*100)
    dplyr:: summarise(age = (n()/nrow(rows()))*100)%>%
      ungroup()%>%
      dplyr:: filter(`SCH Age` == "6-14 years")%>%
      mutate(Drugs = paste(`SCH Age`," ", age,"%"))%>%
      select(Drugs)
    
    
    
  })
  
  
  output$chew_sch_age <- renderInfoBox({
    validate(
      need(sch_side_effect_all()$Side_Effects, 'No SCHISTO Data at the Moment')) 
    if (input$County3 == 'Kenya')  
    {return (infoBox(
      h4("What is the age-group for STH treatment?"),
      tags$p(style = "font-size: 30px;", chew_schage_all()$Drugs),
      icon = icon("calendar-week")
    ))}
    
    infoBox(
      h4("What is the age-group for STH treatment?"),
      tags$p(style = "font-size: 30px;", chew_sc_schage()$Drugs),
      icon = icon("calendar-week")
    )
    
  })
  
  
  
  ###sch SIDE EFFECTS
  role_chew_all <- reactive ({
    role_chew %>%
      filter( Year == input$years3 & Wave == input$Wave3 )%>%
      group_by(Year, Wave,Values)%>%
      dplyr:: summarise(Role_Chew = (n()/nrow(chew_rows()))*100)
    
  })
  
  sc_role_chew <- reactive ({
    role_chew %>%
      filter((sch_side_effect$sctp012_county_name == input$County3 & sch_side_effect$Year == input$years3 & Wave == input$Wave3 ))%>%
      group_by(Year, Values)%>%
      dplyr::  summarise(Role_Chew = (n()/nrow(chew_county_rows()))*100)
    
    
  })
  output$Role_Chew <- renderPlot({
    if (input$County3 == 'Kenya') 
    {return( ggplot(data = role_chew_all(), aes(x = Values, y = Role_Chew, fill= Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("CHEW responsibilities during NSBDP")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(Role_Chew),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = sc_role_chew(), aes(x = Values, y = Role_Chew, fill= Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("CHEW responsibilities during NSBDP")+
        scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Role_Chew),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  }) 
  ######TT PRE POST
  #### STH DRUGS
  #####CORRECT WORMS 
  gg2_all <- reactive ({sth_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "POST" ) 
  })
  
  gg2_all2 <- reactive ({sth_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "PRE") 
  })
  
  gg2_sc <- reactive ({sth_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4 & source_file == "POST" ) 
  })
  
  gg2_sc2 <- reactive ({sth_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4 & source_file == "PRE" ) 
  })
  
  
  output$sthworms_tt <-renderPlotly({
    if (input$County4 == 'Kenya') 
    {return(    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg2_all()$Worms_treated[[1]],
      title = list(text = "Worms Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg2_all2()$Worms_treated[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg2_all2()$Worms_treated[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    )}
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg2_sc()$Worms_treated[[1]],
      title = list(text = "Worms Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg2_sc2()$Worms_treated[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg2_sc2()$Worms_treated[[1]]), color = "#D7837F")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  
  gg3_all <- reactive ({sch_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & source_file == "POST" ) 
  })
  gg3_all2 <- reactive ({sch_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & source_file == "PRE" ) 
  })
  
  gg3_sc <- reactive ({sch_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & ttp012_county == input$County4 & source_file == "POST" ) 
  })
  gg3_sc2 <- reactive ({sch_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & ttp012_county == input$County4 & source_file == "PRE" ) 
  })
  
  
  
  output$sChworms_tt <-renderPlotly({
    validate(
      need(gg3_all()$Year, 'No SCHISTO Data at the Moment'))
    if (input$County4 == 'Kenya') 
    {return(plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg3_all()$Worms_treated[[1]],
      title = list(text = "Worms Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg3_all2()$Worms_treated[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg3_all2()$Worms_treated[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    )}
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg3_sc()$Worms_treated[[1]],
      title = list(text = "Worms Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg3_sc2()$Worms_treated[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg3_sc2()$Worms_treated[[1]]), color = "#D7837F")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  
  #####CORRECT DRUGS
  ###STH
  gg4_all <- reactive ({sth_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "POST") 
  })
  
  gg4_all2 <- reactive ({sth_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "PRE" ) 
  })
  
  gg4_sc <- reactive ({sth_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4 & source_file == "POST" ) 
  })
  
  gg4_sc2 <- reactive ({sth_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4 & source_file == "PRE" ) 
  })
  
  output$sthdrugs_tt <-renderPlotly({
    validate(
      need(gg4_all()$Year, 'No SCHISTO Data at the Moment'))
    if (input$County4 == 'Kenya') 
    {return(plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg4_all()$Correct_Drugs[[1]],
      title = list(text = "Drugs Used"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg4_all2()$Correct_Drugs[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg4_all2()$Correct_Drugs[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    )}
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg4_sc()$Correct_Drugs[[1]],
      title = list(text = "Drugs Used"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg4_sc2()$Correct_Drugs[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg4_sc2()$Correct_Drugs[[1]]), color = "#D7837F")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  
  gg5_all <- reactive ({sch_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & source_file == "POST" ) 
  })
  
  gg5_all2 <- reactive ({sch_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY"  & source_file == "PRE") 
  })
  
  gg5_sc <- reactive ({sch_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & ttp012_county == input$County4 & source_file == "POST") 
  })
  
  gg5_sc2 <- reactive ({sch_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & ttp012_county == input$County4 & source_file == "PRE") 
  })
  
  
  
  output$schdrugs_tt <-renderPlotly({
    validate(
      need(gg5_all()$Year, 'No SCHISTO Data at the Moment'))
    if (input$County4 == 'Kenya') 
    {return(plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg5_all()$Correct_Drugs[[1]],
      title = list(text = "Drugs Used"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg5_all2()$Correct_Drugs[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg5_all2()$Correct_Drugs[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    )}
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg5_sc()$Correct_Drugs[[1]],
      title = list(text = "Drugs Used"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg5_sc2()$Correct_Drugs[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg5_sc2()$Correct_Drugs[[1]]), color = "#D7837F")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  ####### CORRECT AGE GROUP
  ###SCH
  
  gg7_all <- reactive ({sch_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & source_file == "POST" ) 
  })
  gg7_all2 <- reactive ({sch_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & source_file == "PRE" ) 
  })
  
  gg7_sc <- reactive ({sch_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & ttp012_county == input$County4 & source_file == "POST")
  })
  gg7_sc2 <- reactive ({sch_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type != "STH ONLY" & ttp012_county == input$County4 & source_file == "PRE")
  })
  
  output$schage_tt <-renderPlotly({
    validate(
      need(gg7_all()$Year, 'No SCHISTO Data at the Moment'))
    if (input$County4 == 'Kenya') 
    {return(plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg7_all()$Correct_age_group[[1]],
      title = list(text = "Age Group Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg7_all2()$Correct_age_group[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg7_all2()$Correct_age_group[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    )}
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg7_sc()$Correct_age_group[[1]],
      title = list(text = "Age Group Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg7_sc2()$Correct_age_group[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg7_sc2()$Correct_age_group[[1]]), color = "#D7837F")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })   
  
  ###STH
  gg6_all <- reactive ({sth_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "POST"  ) 
  })
  
  gg6_all2 <- reactive ({sth_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "PRE" ) 
  })
  
  gg6_sc <- reactive ({sth_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4  & source_file == "POST" ) 
  })
  
  gg6_sc2 <- reactive ({sth_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4 & source_file == "PRE"  ) 
  })
  
  
  output$sthage_tt <-renderPlotly({
    validate(
      need(gg6_all()$Year, 'No SCHISTO Data at the Moment'))
    if (input$County4 == 'Kenya') 
    {return(plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg6_all()$Correct_age_group[[1]],
      title = list(text = "Age Group Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg6_all2()$Correct_age_group[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg6_all2()$Correct_age_group[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    )}
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg6_sc()$Correct_age_group[[1]],
      title = list(text = "Age Group Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg6_sc2()$Correct_age_group[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg6_sc2()$Correct_age_group[[1]]), color = "#D7837F")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  }) 
  
  
  #####CORRECT DOSAGE
  ###STH
  
  gg8_all <- reactive ({sth_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "POST" ) 
  })
  
  gg8_all2 <- reactive ({sth_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "PRE" ) 
  })
  
  gg8_sc <- reactive ({sth_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4 & source_file == "POST") 
  })
  
  gg8_sc2 <- reactive ({sth_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4 & source_file == "PRE" ) 
  })
  
  output$sthdose_tt <-renderPlotly({
    validate(
      need(gg8_all()$Year, 'No SCHISTO Data at the Moment'))
    if (input$County4 == 'Kenya') 
    {return(plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg8_all()$Correct_dosage[[1]],
      title = list(text = "Dosage Given"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg8_all2()$Correct_dosage[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg8_all2()$Correct_dosage[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    )}
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg8_sc()$Correct_dosage[[1]],
      title = list(text = "Dosage Given"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg8_sc2()$Correct_dosage[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg8_sc2()$Correct_dosage[[1]]), color = "#D7837F")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  }) 
  
  ###SCH
  gg9_all <- reactive ({sch_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "POST" ) 
  })
  
  gg9_all2 <- reactive ({sch_knowledge_all%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & source_file == "PRE" ) 
  })
  
  gg9_sc <- reactive ({sch_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4  & source_file == "POST"  ) 
  })
  
  gg9_sc2 <- reactive ({sch_knowledge_all_cty%>%
      filter( Year == input$years4 & Wave == input$Wave4 & Type == "STH ONLY" & ttp012_county == input$County4 & source_file == "PRE" ) 
  })
  
  
  output$schdose_tt <-renderPlotly({
    validate(
      need(gg9_all()$Year, 'No SCHISTO Data at the Moment'))
    if (input$County4 == 'Kenya') 
    {return(plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg9_all()$Correct_dosage[[1]],
      title = list(text = "Dosage Given"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg9_all2()$Correct_dosage[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg9_all2()$Correct_dosage[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    )}
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg9_sc()$Correct_dosage[[1]],
      title = list(text = "Dosage Given"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg9_sc2()$Correct_dosage[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg9_sc2()$Correct_dosage[[1]]), color = "#D7837F")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  }) 
  
  
  
  ###ROW DETAILS
  tt_rows <- reactive ({
    TT_POST %>%
      filter(Year == input$years4 & Wave == input$Wave4)
  })
  
  sc_tt_rows <- reactive ({
    TT_POST %>%
      filter(Year == input$years4 & Wave == input$Wave4 & ttp012_county == input$County4) 
  })
  
  tt_sch_num <- reactive ({
    TT_POST %>%
      filter(Year == input$years4 & Wave == input$Wave4 & (SCH_Numbers) == 1)
  })
  
  sc_tt_sch_num <- reactive ({
    TT_POST %>%
      filter(Year == input$years4 & Wave == input$Wave4 & ttp012_county == input$County4 & (SCH_Numbers) == 1)
  })
  
  tt_sth_num <- reactive ({
    TT_POST %>%
      filter(Year == input$years4 & Wave == input$Wave4 & (STH_Numbers) == 1)
  })
  
  sc_tt_sth_num <- reactive ({
    TT_POST %>%
      filter(Year == input$years4 & Wave == input$Wave4 & ttp012_county == input$County4 & (STH_Numbers) == 1)
  })
  
  sth_sideeffects_all <- reactive({
    sth_sideeffects%>%
      filter(Wave == input$Wave4 & Year == input$years4)%>%
      dplyr::group_by(source_file,Variables,Values) %>%
      dplyr:: summarise(side_effects = round(n()/sum(!is.na(tt_rows()$ttp207_sth_sideeffects_1))*100))
  })
  
  sth_sideeffects_cnt <- reactive({ 
    sth_sideeffects%>%
      dplyr::filter(Wave == input$Wave4 & Year == input$years4 & ttp012_county == input$County4 )%>%
      dplyr::group_by(sth_sideeffects$Year, sth_sideeffects$Wave, source_file,Variables,Values) %>%
      dplyr:: summarise(side_effects = round(n()/sum(!is.na(sc_tt_rows()$ttp207_sth_sideeffects_1))*100))
  })
  
  output$sth_side_effects1 <- renderPlot ({
    if (input$County4 == 'Kenya') 
    {return(ggplot(data = sth_sideeffects_all(), aes(x = Values, y = side_effects, fill= source_file))+
              geom_bar(stat="identity") +
              ggtitle("STH Side Effects")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="bottom") +
              removeGrid())}
    (ggplot(data = sth_sideeffects_cnt(), aes(x=Values), y = side_effects, fill= source_file))+
      geom_bar(stat="identity") +
      ggtitle("STH Side Effects")+
      scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
      geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                colour = "white",
                position = position_stack(vjust = 0.1),
                fontfface="bold",
                size = 4,
                show.legend = FALSE) +
      coord_flip()+
      theme_bw()+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.ticks.x =  element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            panel.border = element_blank()) +
      removeGrid()
  })
  
  
  sch_sideeffects_all <- reactive({
    sch_sideeffects%>%
      filter(Wave == input$Wave4 & Year == input$years4)%>%
      group_by(Year,Wave, source_file,Variables,Values) %>%
      dplyr:: summarise(side_effects = round(n()/sum(tt_sch_num()$SCH_Numbers)*100))
  })
  
  sch_sideeffects_cnt <- reactive({
    sch_sideeffects%>%
      filter(Wave == input$Wave4 & Year == input$years4 & ttp012_county == input$County4 )%>%
      group_by(Year,Wave, source_file,Variables,Values) %>%
      dplyr::summarise(side_effects = round(n()/sum(sc_tt_sch_num()$SCH_Numbers)*100))
  })
  
  
  output$sch_side_effects1 <- renderPlot ({
    validate(
      need(gg3_all()$Year, 'No SCHISTO Data at the Moment'))
    
    if (input$County4 == 'Kenya') 
    {return(ggplot(data = sch_sideeffects_all(), aes(stringr :: str_wrap(Values), y = side_effects, fill= source_file))+
              geom_bar(stat="identity") +
              ggtitle("SCH Side Effects")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="bottom") +
              removeGrid())}
    (ggplot(data = sch_sideeffects_cnt(), aes(stringr :: str_wrap(Values), y = side_effects, fill= source_file))+
        geom_bar(stat="identity") +
        ggtitle("SCH Side Effects")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  
  ####CHEW CONTACTS
  Chews_all <- reactive({
    TT_POST %>%
      filter(!is.na(Chew_Contact)& Wave == input$Wave4 & Year == input$years4)%>%
      group_by(Year, Wave, Chew_Contact) %>%
      summarise(contacts = round(n()/sum(!is.na(tt_rows()$Chew_Contact))*100))
  })
  
  Chews_county <- reactive({
    TT_POST %>%
      filter(!is.na(Chew_Contact) & Wave == input$Wave4 & Year == input$years4 & ttp012_county == input$County4)%>%
      group_by(Year, Wave, Chew_Contact) %>%
      summarise(contacts = round(n()/sum(!is.na(sc_tt_rows()$Chew_Contact))*100))
  })
  
  output$chew_contacts <- renderPlot ({
    if (input$County4 == 'Kenya') 
    {return(ggplot(data = Chews_all(), 
                   aes(x = 2, y = contacts, fill= Chew_Contact ))+
              geom_bar(stat = "identity")+
              ggtitle("Do you have contacts of the CHEW?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(contacts,"%", sep = ""),hjust = "centre"), col = "white",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('#5E5F5F','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    ggplot(data = Chews_county(), 
           aes(x = 2, y = contacts, fill= Chew_Contact ))+
      geom_bar(stat = "identity")+
      ggtitle("Do you have contacts of the CHEW?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(contacts,"%", sep = ""),hjust = "centre"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c('#5E5F5F', '#FE6F87'))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  
  
  ####AEO CONTACTS
  AEOs_all <- reactive({
    TT_POST %>%
      filter(!is.na(AEO_Contact)& Wave == input$Wave4 & Year == input$years4)%>%
      group_by(Year, Wave, AEO_Contact) %>%
      dplyr::  summarise(contacts = round(n()/sum(!is.na(tt_rows()$AEO_Contact))*100))
  })
  
  AEOs_county <- reactive({
    TT_POST %>%
      filter(!is.na(AEO_Contact)& Wave == input$Wave4 & Year == input$years4 & ttp012_county == input$County4)%>%
      group_by(Year, Wave, AEO_Contact) %>%
      dplyr::  summarise(contacts = round(n()/sum(!is.na(sc_tt_rows()$AEO_Contact))*100))
  })
  
  output$AEO_contacts <- renderPlot ({
    if (input$County4 == 'Kenya') 
    {return(ggplot(data = AEOs_all(), 
                   aes(x = 2, y = contacts, fill= AEO_Contact ))+
              geom_bar(stat = "identity")+
              ggtitle("Do you have contacts of the AEO?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(contacts,"%", sep = ""),hjust = "centre"), col = "white",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('#5E5F5F', '#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    ggplot(data = AEOs_county(), 
           aes(x = 2, y = contacts, fill= AEO_Contact ))+
      geom_bar(stat = "identity")+
      ggtitle("Do you have contacts of the AEO?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = past4e(contacts,"%", sep = ""),hjust = "centre"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c('#5E5F5F','#FE6F87'))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  
  ####DEWORM SICK CHILDREN
  Sick_all <- reactive({
    TT_POST %>%
      filter(!is.na(Deworm_Sick)& Wave == input$Wave4 & Year == input$years4)%>%
      group_by(Year, Wave, Deworm_Sick) %>%
      dplyr:: summarise(Sick = round(n()/sum(!is.na(tt_rows()$Deworm_Sick))*100))
  })
  
  Sick_county <- reactive({
    TT_POST %>%
      filter(!is.na(Deworm_Sick) & Wave == input$Wave4 & Year == input$years4 & ttp012_county == input$County4)%>%
      group_by(Year, Wave, Deworm_Sick) %>%
      dplyr:: summarise(Sick = round(n()/sum(!is.na(sc_tt_rows()$Deworm_Sick))*100))
  })
  
  output$Deworm_sick <- renderPlot ({
    if (input$County4 == 'Kenya') 
    {return(ggplot(data = Sick_all(), 
                   aes(x = 2, y = Sick, fill= Deworm_Sick ))+
              geom_bar(stat = "identity")+
              ggtitle("Do you deworm sick children?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(Sick,"%", sep = ""),hjust = "centre"), col = "white",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('#FE6F87', '#5E5F5F'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    {ggplot(data = Sick_county(), 
            aes(x = 2, y = Sick, fill= Deworm_Sick ))+
        geom_bar(stat = "identity")+
        ggtitle("Do you deworm sick children?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(Sick,"%", sep = ""),hjust = "centre"), col = "white",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('#FE6F87', '#5E5F5F'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5)}
  })
  
  
  ###MATERIALS GIVEN
  tt_materials_all <- reactive({
    tt_materials%>%
      filter(Wave == input$Wave4 & Year == input$years4)%>%
      group_by(Year,Wave,Variables,Values) %>%
      dplyr:: summarise(tt_materials = round(n()/sum(!is.na(tt_rows()$ttp227a_mat_received))*100))
  })
  
  tt_materials_cnt <- reactive({
    tt_materials%>%
      filter(Wave == input$Wave4 & Year == input$years4 & ttp012_county == input$County4 )%>%
      group_by(Year,Wave,Variables,Values) %>%
      dplyr::summarise(tt_materials = round(n()/sum(!is.na(sc_tt_rows()$STH_Numbers)*100)))
  })
  
  
  output$tt_materials_given <- renderPlot ({
    if (input$County4 == 'Kenya') 
    {return(ggplot(data = tt_materials_all(), aes(x = Values, y = tt_materials, fill = "Year"))+
              geom_bar(stat="identity" , width = 0.4) +
              ggtitle("Materials Given")+
              scale_fill_manual(values =  c('#FE6F87'))+
              geom_text(aes(label = paste(tt_materials,"%",sep=""), hjust = "right"),
                        colour = "black",
                        position = position_dodge(width = .0),
                        vjust= 0,
                        size = 4,
                        show.legend = FALSE,
                        fontface="bold") +
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              #theme(axis.text.y = element_text(color="BlACK",size=10)) +
              theme(axis.text.x = element_text(color="BlACK",size=10)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.y = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    legend.title = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="none") +
              removeGrid())}
    (ggplot(data = tt_materials_cnt(), aes(x=Values), y = tt_materials, fill = "Year"))+
      geom_bar(stat="identity", width = 0.4) +
      ggtitle("Materials Given")+
      scale_fill_manual(values =  c('#FE6F87'))+
      geom_text(aes(label = paste(tt_materials,"%",sep=""), hjust = "centre"),
                colour = fct_rev(Grade),
                position = position_dodge(width = .0),
                fontface="bold",
                size = 4,
                show.legend = FALSE) +
      theme_bw()+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=10)) +
      #theme(axis.text.y = element_text(color="BlACK",size=10)) +
      theme(axis.title.x = element_blank(), 
            #axis.text.x = element_blank(),
            axis.ticks.x =  element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            panel.border = element_blank(),
            legend.title= element_blank()) +
      removeGrid()
  })
  
  
  ###POSTERS GIVEN
  tt_posters_all <- reactive({
    TT_POST%>%
      filter(Wave == input$Wave4 & Year == input$years4 & !is.na(Num_Posters))%>%
      group_by(Year,Wave,Num_Posters) %>%
      dplyr:: summarise(Posters = round(n()/sum(!is.na(tt_rows()$Num_Posters))*100))
  })
  
  tt_posters_county <- reactive({
    TT_POST%>%
      filter(Wave == input$Wave4 & !is.na(Num_Posters) & Year == input$years4 & ttp012_county == input$County4 )%>%
      group_by(Year,Wave,Num_Posters) %>%
      dplyr::summarise(Posters = round(n()/sum(!is.na(tt_rows()$Num_Posters))*100))
  })
  
  
  output$tt_posters <- renderPlot ({
    if (input$County4 == 'Kenya') 
    {return(ggplot(data = tt_posters_all(), aes(x = Num_Posters, y = Posters, fill = "Num_Posters"))+
              geom_bar(stat="identity", width = 0.4) +
              ggtitle("Posters Given")+
              scale_fill_manual(values =  c('#FE6F87','grey'))+
              geom_text(aes(label = paste(Posters,"%",sep=""), hjust = "right"),
                        colour = "black",
                        position = position_dodge(width = .0),
                        vjust= 0,
                        size = 4,
                        show.legend = FALSE,
                        fontface="bold") +
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.x = element_text(color="BlACK",size=10)) +
              theme(axis.text.y = element_blank()) +
              theme(axis.title.x = element_blank(), 
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(),
                    legend.title = element_blank(),
                    legend.key = element_blank(),
                    legend.position="none")+
              removeGrid())}
    (ggplot(data = tt_posters_county(), aes(x=Num_Posters), y = Posters, fill = "Num_Posters"))+
      geom_bar(stat="identity", width = 0.4) +
      ggtitle("Posters Given")+
      scale_fill_manual(values =  c('#FE6F87', 'grey'))+
      geom_text(aes(label = paste(Posters,"%",sep=""), hjust = "centre"),
                colour = fct_rev(Grade),
                position = position_dodge(width = .0),
                fontface="bold",
                size = 4,
                show.legend = FALSE) +
      theme_bw()+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=10)) +
      theme(axis.text.y = element_blank()) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            panel.border = element_blank(),
            legend.position="bottom") +
      removeGrid()
  })
  
  
  ####ENOUGH POSTERS
  
  enough_posters_all <- reactive({
    TT_POST %>%
      filter(!is.na(enough_posters)& Wave == input$Wave4 & Year == input$years4)%>%
      group_by(Year, Wave, enough_posters) %>%
      dplyr::  summarise(posters = round(n()/sum(!is.na(tt_rows()$enough_posters))*100))
  })
  
  enough_posters_county <- reactive({
    TT_POST %>%
      filter(!is.na(enough_posters)& Wave == input$Wave4 & Year == input$years4 & ttp012_county == input$County4)%>%
      group_by(Year, Wave, enough_posters) %>%
      dplyr::  summarise(posters = round(n()/sum(!is.na(sc_tt_rows()$enough_posters))*100))
  })
  
  output$enough_posters <- renderPlot ({
    
    if (input$County4 == 'Kenya') 
    {return(ggplot(data = enough_posters_all(), 
                   aes(x = 2, y = posters, fill= enough_posters ))+
              geom_bar(stat = "identity")+
              ggtitle("Are the Posters enough?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(posters,"%", sep = ""),hjust = "centre"), col = "white",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('#5E5F5F', '#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title = element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    ggplot(data = enough_posters_county(), 
           aes(x = 2, y = posters, fill= enough_posters ))+
      geom_bar(stat = "identity")+
      ggtitle("Are the Posters enough?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(posters,"%", sep = ""),hjust = "centre"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c('#5E5F5F','#FE6F87'))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title = element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  tt_sc <- reactive ({
    TT_POST %>%
      mutate(County = ttp012_county)%>%
      group_by(County)%>%
      filter(Wave == input$Wave4 & Year == input$years4)%>%
      dplyr:: summarise(Number_of_Sub_Counties = n())
  })
  
  output$tt_sucbcounties <- renderDataTable({
    tt_sc()
  })
  
  tt_type <- reactive ({
    TT_POST %>%
      filter(Wave == input$Wave4 & Year == input$years4)%>%
      group_by(Type)%>%
      dplyr:: summarise(Treatment_Type = n())
  })
  
  output$tt_type <- renderDataTable({
    tt_type()
  })
  
  tt_Respondents <- reactive ({
    TT_POST %>%
      filter(Wave == input$Wave4 & Year == input$years4)%>%
      group_by(source_file)%>%
      dplyr:: summarise(Respondents = n())
  })
  
  output$tt_Respondents <- renderDataTable({
    tt_Respondents()
  })
  
  
  
  ##########TT OBS
  tt_obs_type <- reactive ({
    TT_OBS %>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Type)%>%
      dplyr:: summarise(Treatment_Type = n())
  })
  
  output$tt_obs_type <- renderDataTable({
    tt_obs_type()
  })
  
  
  tt_obs_attendance_all <-  reactive ({
    TT_OBS %>%
      filter(Wave == input$Wave5 & Year == input$years5 )%>%
      dplyr:: summarise(Attendance = round(mean(tto103a_participants_start)))
    
  })
  
  tt_obs_attendance_county <-  reactive ({
    TT_OBS %>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)%>%
      dplyr:: summarise(Attendance =round(mean(tto103a_participants_start)))
    
  })
  
  output$obs_Attendance <- renderInfoBox({
    if (input$County5 == 'Kenya') 
    {return(infoBox(
      h4("Average # of participants attending training on day 1"),
      tags$p(style = "font-size: 30px;", tt_obs_attendance_all()$Attendance)
      #icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4("Average # of participants attending training on day 1"),
      tags$p(style = "font-size: 30px;", tt_obs_attendance_county()$Attendance)
      #icon = icon("prescription-bottle-alt")
    )
  })
  
  
  ####TOPIC COVERAGE GENERAL
  obs_all <- reactive ({
    TT_OBS %>%
      filter( Year == input$years5 & Wave == input$Wave5 )
  })
  
  obs_county <- reactive ({
    TT_OBS %>%
      filter((Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5))
  })
  
  #####COVID
  tt_trainers_mask_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year, Wave, trainers_masked)%>%
      dplyr:: summarise(  trainers_mask = round(n()/sum(!is.na(obs_all()$Year))*100))
  })
  
  tt_trainers_mask_county <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)%>%
      group_by(Year, Wave, trainers_masked, tto012_county)%>%
      dplyr:: summarise(  trainers_mask = round(n()/sum(!is.na(obs_county()$Year))*100))
  })
  
  output$tt_trainers_masks <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = tt_trainers_mask_all(), 
                   aes(x = 2, y = trainers_mask, fill= trainers_masked ))+
              geom_bar(stat = "identity")+
              ggtitle("Were all the trainers wearing protective face \n masks during the training?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(trainers_mask,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = tt_trainers_mask_county(), 
            aes(x = 2, y = trainers_mask, fill= trainers_masked ))+
        geom_bar(stat = "identity")+
        ggtitle("Were all the trainers wearing protective face \n masks during the training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(trainers_mask,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  
  tt_participants_mask_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year, Wave, Parts_masked)%>%
      dplyr:: summarise(  participants_mask = round(n()/sum(!is.na(obs_all()$Year))*100))
  })
  
  tt_participants_mask_county <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)%>%
      group_by(Year, Wave, Parts_masked, tto012_county)%>%
      dplyr:: summarise(  participants_mask = round(n()/sum(!is.na(obs_county()$Year))*100))
  })
  
  output$tt_participants_masks <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = tt_participants_mask_all(), 
                   aes(x = 2, y = participants_mask, fill= Parts_masked ))+
              geom_bar(stat = "identity")+
              ggtitle("Were all the participants wearing protective face \n masks during the training?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(participants_mask,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = tt_participants_mask_county(), 
            aes(x = 2, y = participants_mask, fill= Parts_masked ))+
        geom_bar(stat = "identity")+
        ggtitle("Were all the participants wearing protective face \n masks during the training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(participants_mask,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  ####HAND SANITIZER AVAILABEL
  tt_sanitizer_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year, Wave, Sanitizers)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(obs_all()$Year))*100))
  })
  
  tt_sanitizer_county <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)%>%
      group_by(Year, Wave, Sanitizers, tto012_county)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(obs_county()$Year))*100))
  })
  
  output$tt_sanitizer <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = tt_sanitizer_all(), 
                   aes(x = 2, y = sanitizer, fill= Sanitizers ))+
              geom_bar(stat = "identity")+
              ggtitle("Was hand sanitizer or a hand washing facility \n available at the training?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = tt_sanitizer_county(), 
            aes(x = 2, y = sanitizer, fill= Sanitizers ))+
        geom_bar(stat = "identity")+
        ggtitle("Was hand sanitizer or a hand washing \n facility available at the training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  
  ####SANITIZE VANUE
  tt_sanitize_venue_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & !is.na(Venue_cleaned))%>%
      group_by(Year, Wave, Venue_cleaned)%>%
      dplyr:: summarise(  sanitize_venue = round(n()/sum(!is.na(obs_all()$Year))*100))
  })
  
  tt_sanitize_venue_county <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5 )%>%
      group_by(Year, Wave, Venue_cleaned, tto012_county)%>%
      dplyr:: summarise(  sanitize_venue = round(n()/sum(!is.na(obs_county()$Year))*100))
  })
  
  output$tt_sanitize_venue <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = tt_sanitize_venue_all(), 
                   aes(x = 2, y = sanitize_venue, fill= Venue_cleaned ))+
              geom_bar(stat = "identity")+
              ggtitle("Was the training venue cleaned/sanitized \n before this training?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(sanitize_venue,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','maroon', '#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = tt_sanitize_venue_county(), 
            aes(x = 2, y = sanitize_venue, fill= Venue_cleaned ))+
        geom_bar(stat = "identity")+
        ggtitle("Was the training venue cleaned/sanitized before this training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(sanitize_venue,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  tt_parts_mask_all <- reactive({
    TT_OBS%>%
      group_by(Number_Parts_masked)%>%
      filter(Wave == input$Wave5 & Year == input$years5 & !is.na(Number_Parts_masked))%>%
      dplyr:: summarise( parts_mask = round(n()/sum(!is.na(obs_all()$Number_Parts_masked))*100))
  })
  
  tt_parts_mask <- reactive({
    TT_OBS%>%
      group_by(Number_Parts_masked)%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5 & !is.na(Number_Parts_masked))%>%
      dplyr:: summarise( parts_mask = round(n()/sum(!is.na(obs_county()$Number_Parts_masked))*100))
  })
  
  output$tt_num_parts_mask <- renderPlot({
    if (input$County5 == 'Kenya') 
    {return ( ggplot(data = tt_parts_mask_all(), aes(x = Number_Parts_masked, y =  parts_mask, fill=Number_Parts_masked))+
                geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
                ggtitle("How many participants were wearing protective \n face masks during the training?")+
                scale_fill_manual(values = c('#FE6F87', '#5E5F5F', 'grey'), aesthetics = "fill") +
                #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
                geom_text(aes(label = paste(round( parts_mask),"%",sep=""),  hjust = "right", size = 4),
                          colour = "white",
                          position = position_dodge(width = .0),
                          size = 5,
                          show.legend = FALSE,
                          fontface="bold") +
                theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
                theme(axis.text.y = element_text(color="BlACK",size=14)) +
                theme(axis.title.x = element_blank(), 
                      axis.ticks.x =  element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.x = element_blank(),
                      #axis.text.y = element_blank(), 
                      # axis.ticks.y = element_blank(),
                      plot.margin = unit(c(1,1,1,0), "mm"),
                      legend.position = "none",
                      panel.border = element_blank()) +
                coord_flip()+
                removeGrid())}
    ( ggplot(data = tt_parts_mask(), aes(x =Number_Parts_masked, y =  parts_mask, fill=Number_Parts_masked))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("How many participants were wearing protective \n face masks during the training?")+
        scale_fill_manual(values = c('#FE6F87', '#5E5F5F', 'grey'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round( parts_mask),"%",sep=""), hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid())
  })
  
  
  ####ATTENDANCE SHEETS
  obs_sheets_all <- reactive ({
    obs_att_sheet %>%
      filter( Year == input$years5 & Wave == input$Wave5 )
  })
  
  obs_sheets_county <- reactive ({
    obs_att_sheet_county %>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)
  })
  
  output$obs_attendance_sheets <- renderPlot({
    if (input$County5 == 'Kenya') 
    {return( ggplot(data = obs_sheets_all(), aes(x = Variables, y = Values, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("Presence of Attendance sheet")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(Values),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = obs_sheets_county(), aes(x = Variables, y = Values, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("Presence of Attendance sheet")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Values),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  #####TOPIC COVERAGE
  
  coverage_general_all <- reactive({
    coverage_general%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      dplyr::group_by(Variables,Values) %>%
      dplyr:: summarise(coverage_general = round(n()/sum(!is.na(obs_all()$Year))*100))
    
  })
  
  coverage_general_cnt <- reactive({
    coverage_general%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)%>%
      dplyr::group_by(Variables,Values) %>%
      dplyr:: summarise(coverage_general = round(n()/sum(!is.na(obs_county()$Year))*100))
    
  })
  
  
  output$coverage_general1 <- renderPlot ({
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = coverage_general_all(), aes(x = Variables, y = coverage_general, fill= Values))+
              geom_bar(stat="identity") +
              ggtitle("Coverage on Health Education: \n What are Worms(General)")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F', 'grey'))+
              geom_text(aes(label = paste(coverage_general,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.3),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="bottom") +
              removeGrid())}
    (ggplot(data = coverage_general_cnt(), aes(x=Variables), y = coverage_general, fill= Values))+
      geom_bar(stat="identity") +
      ggtitle("Coverage on Health Education: \n What are Worms(General)")+
      scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
      geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                colour = "white",
                position = position_stack(vjust = 0.1),
                fontface="bold",
                size = 4,
                show.legend = FALSE) +
      coord_flip()+
      theme_bw()+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.ticks.x =  element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            panel.border = element_blank()) +
      removeGrid()
  })
  
  
  ####STH COVERAGE
  coverage_sth_all <- reactive({
    coverage_sth%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      dplyr::group_by(Variables,Values) %>%
      dplyr:: summarise(coverage_sth = round(n()/sum(!is.na(obs_all()$Year))*100))
    
  })
  
  coverage_sth_cnt <- reactive({
    coverage_sth%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)%>%
      dplyr::group_by(Variables,Values) %>%
      dplyr:: summarise(coverage_sth = round(n()/sum(!is.na(obs_county()$Year))*100))
    
  })
  
  
  output$coverage_sth1 <- renderPlot ({
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = coverage_sth_all(), aes(x = Variables, y = coverage_sth, fill= Values))+
              geom_bar(stat="identity") +
              ggtitle("Coverage on Health Education: \n What are Worms(STH)")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F','grey'))+
              geom_text(aes(label = paste(coverage_sth,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="bottom") +
              removeGrid())}
    (ggplot(data = coverage_sth_cnt(), aes(x=Variables), y = coverage_sth, fill= Values))+
      geom_bar(stat="identity") +
      ggtitle("Coverage on Health Education: \n What are Worms(STH)")+
      scale_fill_manual(values =  c('#FE6F87', '#5E5F5F', 'grey'))+
      geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                colour = "white",
                position = position_stack(vjust = 0.1),
                fontface="bold",
                size = 4,
                show.legend = FALSE) +
      coord_flip()+
      theme_bw()+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.ticks.x =  element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            panel.border = element_blank()) +
      removeGrid()
  })
  
  
  ####SCH COVERAGE
  coverage_sch_all <- reactive({
    coverage_sch%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      dplyr::group_by(Variables,Values) %>%
      dplyr:: summarise(coverage_sch = round(n()/sum(obs_all()$SCH_Numbers)*100))
    
  })
  
  coverage_sch_cnt <- reactive({
    coverage_sch%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)%>%
      dplyr::group_by(Variables,Values) %>%
      dplyr:: summarise(coverage_sch = round(n()/sum(obs_county()$SCH_Numbers)*100))
    
  })
  
  
  output$coverage_sch1 <- renderPlot ({
    validate(
      need(coverage_sch_all()$coverage_sch, 'No SCHISTO Data at the Moment'))
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = coverage_sch_all(), aes(x = Variables, y = coverage_sch, fill= Values))+
              geom_bar(stat="identity") +
              ggtitle("Coverage on Health Education: \n What are Worms(SCH)")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F','grey'))+
              geom_text(aes(label = paste(coverage_sch,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="bottom") +
              removeGrid())}
    (ggplot(data = coverage_sch_cnt(), aes(x=Variables), y = coverage_sch, fill= Values))+
      geom_bar(stat="identity") +
      ggtitle("Coverage on Health Education: \n What are Worms(SCH)")+
      scale_fill_manual(values =  c('#FE6F87', '#5E5F5F','grey'))+
      geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                colour = "white",
                position = position_stack(vjust = 0.1),
                fontface="bold",
                size = 4,
                show.legend = FALSE) +
      coord_flip()+
      theme_bw()+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.ticks.x =  element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            panel.border = element_blank()) +
      removeGrid()
  })
  
  #####FORM FILLING
  
  obs_form_all <- reactive ({
    TT_OBS %>%
      group_by(Year, Wave,Form_Filling)%>%
      dplyr:: summarise(Forms = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( Year == input$years5 & Wave == input$Wave5 & !is.na(Year))
    
  })
  
  obs_form_county <- reactive ({
    TT_OBS %>%
      group_by(Year, Wave, Form_Filling)%>%
      dplyr::  summarise(Forms = round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5 & !is.na(Year))
    
    
  })
  
  output$obs_Form_Filling <- renderPlot({
    if (input$County5 == 'Kenya') 
    {return( ggplot(data = obs_form_all(), aes(x = Form_Filling, y = Forms, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("Forms filled")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(Forms),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=12)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     #axis.text.x = element_blank(),
                     axis.text.y = element_blank(), 
                     axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               removeGrid())}
    
    {ggplot(data = obs_form_county(), aes(x = Form_Filling, y = Forms, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("Forms filled")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Forms),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=12)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              #axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  
  #####PRACTICE SESSIONS
  
  practice_all <- reactive ({
    TT_OBS %>%
      group_by(Year, Wave,Practice_sections)%>%
      dplyr:: summarise(practices = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( Year == input$years5 & Wave == input$Wave5 & !is.na(Year))
    
  })
  
  practice_county <- reactive ({
    TT_OBS %>%
      group_by(Year, Wave, Practice_sections)%>%
      dplyr::  summarise(practices = round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)
    
    
  })
  
  output$obs_Practice_sections <- renderPlot({
    if (input$County5 == 'Kenya') 
    {return( ggplot(data = practice_all(), aes(x = Practice_sections, y = practices, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("Practical Sessions")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(practices),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=12)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     #axis.text.x = element_blank(),
                     axis.text.y = element_blank(), 
                     axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               
               removeGrid())}
    
    {ggplot(data = practice_county(), aes(x = Practice_sections, y = practices, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("Practical Sessions")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(practices),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=12)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              #axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  #####ROLES
  ###TT
  obs_tt_roles_all <- reactive ({
    obs_tt_roles %>%
      group_by(Year, Wave,Variables,Values)%>%
      dplyr:: summarise(tt_roles = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( Year == input$years5 & Wave == input$Wave5 & !is.na(Values))
    
  })
  
  obs_tt_roles_county <- reactive ({
    obs_tt_roles %>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5)%>%
      group_by(Year, Wave, Variables,Values)%>%
      dplyr::  summarise(tt_roles = round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5 & !is.na(Values))
    
    
  })
  
  output$obs_tt_roles <- renderPlot({
    if (input$County5 == 'Kenya') 
    {return( ggplot(data = obs_tt_roles_all(), aes(x = Values, y = tt_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(Values, 30), tt_roles) + xlab(NULL)+
               ggtitle("Teacher Roles Discussed")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(tt_roles),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=12)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = obs_tt_roles_county(), aes(x = Values, y = tt_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("Teacher Roles Discussed")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(tt_roles),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=12)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              #axis.text.x = element_blank(), 
              #axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  ###chew
  obs_chew_roles_all <- reactive ({
    obs_chew_roles %>%
      group_by(Year, Wave,Variables,Values)%>%
      dplyr:: summarise(chew_roles = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5  & !is.na(Values))
    
  })
  
  obs_chew_roles_county <- reactive ({
    obs_chew_roles %>%
      #filter(Wave == input$Wave5 & Year == input$years5 & chewo012_county == input$years5)%>%
      group_by(Year, Wave, Variables,Values)%>%
      dplyr::  summarise(chew_roles = round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5 & !is.na(Values))
    
    
  })
  
  output$obs_chew_roles <- renderPlot({
    if (input$County5 == 'Kenya') 
    {return( ggplot(data = obs_chew_roles_all(), aes(x = Values, y = chew_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("CHEW Roles Discussed")+
               aes(stringr::str_wrap(Values, 20), chew_roles) + xlab(NULL)+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=12)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = obs_chew_roles_county(), aes(x = Values, y = chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("CHEW Roles Discussed")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        aes(stringr::str_wrap(Values, 20), chew_roles) + xlab(NULL)+
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=12)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              #axis.text.x = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  ###HT
  obs_HT_roles_all <- reactive ({
    obs_HT_roles %>%
      #filter( Year == input$years5 & Wave == input$Wave5 )%>%
      group_by(Year, Wave,Variables,Values)%>%
      dplyr:: summarise(HT_roles = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5  & !is.na(Values))
    
  })
  
  obs_HT_roles_county <- reactive ({
    obs_HT_roles %>%
      # filter(Wave == input$Wave5 & Year == input$years5 & chewo012_county == input$years5)%>%
      group_by(Year, Wave, Variables,Values)%>%
      dplyr::  summarise(HT_roles = round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county== input$County5 & !is.na(Values))
    
    
  })
  
  output$obs_HT_roles <- renderPlot({
    if (input$County5 == 'Kenya') 
    {return( ggplot(data = obs_HT_roles_all(), aes(x = Values, y = HT_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               ggtitle("HT Roles Discussed")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               aes(stringr::str_wrap(Values, 30), HT_roles) + xlab(NULL)+
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(HT_roles),"%",sep=""),  hjust = "right", size = 4),
                         #colour = fct_rev(Grade)),
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=12)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = obs_HT_roles_county(), aes(x = Values, y = HT_roles, fill=Values))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("HT Roles Discussed")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        aes(stringr::str_wrap(Values, 30), HT_roles) + xlab(NULL)+
        geom_text(aes(label = paste(round(HT_roles),"%",sep=""),  hjust = "right", size = 4),
                  #colour = fct_rev(Grade)),
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=12)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              #axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  
  ###SIDE EFFECTS
  obs_sideeffects_all <- reactive({
    obs_sideeffects%>%
      #filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year,Wave,Variables,Values) %>%
      dplyr:: summarise(side_effects = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5 & !is.na(Values))
  })
  
  obs_sideeffects_cnt <- reactive({
    obs_sideeffects%>%
      group_by(Year,Wave,Variables,Values) %>%
      dplyr::summarise(side_effects =round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter(Wave == input$Wave5 & Year == input$years5 & chewo012_county == input$years5 & !is.na(Values))
  })
  
  
  output$obs_side_effects1 <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_sideeffects_all(), aes(stringr :: str_wrap(Values), y = side_effects, fill= Year))+
              geom_bar(stat="identity") +
              ggtitle("Side Effects")+
              aes(stringr::str_wrap(Values, 30), side_effects) + xlab(NULL)+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=12)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="bottom") +
              removeGrid())}
    (ggplot(data = obs_sideeffects_cnt(), aes(stringr :: str_wrap(Values), y = side_effects, fill= Year))+
        geom_bar(stat="identity") +
        ggtitle("Side Effects")+
        aes(stringr::str_wrap(Values, 30), side_effects) + xlab(NULL)+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        geom_text(aes(label = paste(side_effects,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  
  ###NOT TREATED
  obs_not_treat_all <- reactive({
    obs_not_treat%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year,Wave,Variables,Values) %>%
      dplyr:: summarise(Not_treat = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( !is.na(Values))
  })
  
  obs_not_treat_cnt <- reactive({
    obs_not_treat%>%
      filter(Wave == input$Wave5 & Year == input$years5 & chewo012_county == input$years5 )%>%
      group_by(Year,Wave,Variables,Values) %>%
      dplyr::summarise(Not_treat =round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter( !is.na(Values))
  })
  
  
  output$obs_not_treat <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_not_treat_all(), aes(stringr :: str_wrap(Values), y = Not_treat, fill= Year))+
              geom_bar(stat="identity") +
              ggtitle("Who should not be treated?")+
              aes(stringr::str_wrap(Values, 30), Not_treat) + xlab(NULL)+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(Not_treat,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=12)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="none") +
              removeGrid())}
    (ggplot(data = obs_not_treat_cnt(), aes(stringr :: str_wrap(Values), y = Not_treat, fill= Year))+
        geom_bar(stat="identity") +
        ggtitle("Who should not be treated?")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        aes(stringr::str_wrap(Values, 30), Not_treat) + xlab(NULL)+
        geom_text(aes(label = paste(Not_treat,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=12)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  
  
  
  ###MATERIALS
  obs_materials_all <- reactive({
    obs_materials%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year,Wave,Variables,Values) %>%
      dplyr:: summarise(Materials = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( !is.na(Values))
  })
  
  obs_materials_cnt <- reactive({
    obs_materials%>%
      filter(Wave == input$Wave5 & Year == input$years5 & chewo012_county == input$years5 )%>%
      group_by(Year,Wave,Variables,Values) %>%
      dplyr::summarise(Materials =round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter( !is.na(Values))
  })
  
  
  output$obs_materials <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_materials_all(), aes(stringr :: str_wrap(Values), y = Materials, fill= Year))+
              geom_bar(stat="identity") +
              ggtitle("Materials Distributed")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              aes(stringr::str_wrap(Values, 20), Materials) + xlab(NULL)+
              geom_text(aes(label = paste(Materials,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="none") +
              removeGrid())}
    (ggplot(data = obs_materials_cnt(), aes(stringr :: str_wrap(Values), y = Materials, fill= Year))+
        geom_bar(stat="identity") +
        ggtitle("Materials Distributed")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        aes(stringr::str_wrap(Values, 20), Materials) + xlab(NULL)+
        geom_text(aes(label = paste(Materials,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  
  ###CHEW CHECKLIST
  obs_chewchecklist_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year,Wave,Chew_Checklist) %>%
      dplyr:: summarise(chewchecklist = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( !is.na(Chew_Checklist))
  })
  
  obs_chewchecklist_cnt <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county == input$years5 )%>%
      group_by(Year,Wave,Chew_Checklist) %>%
      dplyr::summarise(chewchecklist =round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter( !is.na(Chew_Checklist))
  })
  
  
  output$obs_chewchecklist <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_chewchecklist_all(), aes(stringr :: str_wrap(Chew_Checklist), y = chewchecklist, fill= Year))+
              geom_bar(stat="identity") +
              ggtitle("CHEW Checklist")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(chewchecklist,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="bottom") +
              removeGrid())}
    (ggplot(data = obs_chewchecklist_cnt(), aes(stringr :: str_wrap(Chew_Checklist), y = chewchecklist, fill= Year))+
        geom_bar(stat="identity") +
        ggtitle("CHEW Checklist")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        geom_text(aes(label = paste(chewchecklist,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none",
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  
  ###TT CHECKLIST
  obs_TTchecklist_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year,Wave,TT_Checklist) %>%
      dplyr:: summarise(TTchecklist = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( !is.na(TT_Checklist))
  })
  
  obs_TTchecklist_cnt <- reactive({
    obs_TTchecklist%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county == input$years5 )%>%
      group_by(Year,Wave,TT_Checklist) %>%
      dplyr::summarise(TTchecklist =round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter( !is.na(TT_Checklist))
  })
  
  
  output$obs_TTchecklist <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_TTchecklist_all(), aes(stringr :: str_wrap(TT_Checklist), y = TTchecklist, fill= Year))+
              geom_bar(stat="identity") +
              ggtitle("TT Checklist")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(TTchecklist,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="none") +
              removeGrid())}
    (ggplot(data = obs_TTchecklist_cnt(), aes(stringr :: str_wrap(TT_Checklist), y = TTchecklist, fill= Year))+
        geom_bar(stat="identity") +
        ggtitle("TT Checklist")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        geom_text(aes(label = paste(TTchecklist,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none",
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  
  ###ALL MATERIALS
  obs_AllMaterials_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year,Wave,All_Materials) %>%
      dplyr:: summarise(AllMaterials = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( !is.na(All_Materials))
  })
  
  obs_AllMaterials_cnt <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county == input$years5 )%>%
      group_by(Year,Wave,All_Materials) %>%
      dplyr::summarise(AllMaterials =round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter( !is.na(All_Materials))
  })
  
  
  output$obs_AllMaterials <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_AllMaterials_all(), aes(stringr :: str_wrap(All_Materials), y = AllMaterials, fill= Year))+
              geom_bar(stat="identity") +
              ggtitle("Did they distribute all materials?")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(AllMaterials,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="none") +
              removeGrid())}
    (ggplot(data = obs_AllMaterials_cnt(), aes(stringr :: str_wrap(All_Materials), y = AllMaterials, fill= Year))+
        geom_bar(stat="identity") +
        ggtitle("Did they distribute all materials?")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        geom_text(aes(label = paste(AllMaterials,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none",
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  
  ######MEDICINE
  obs_Medicne_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year,Wave,Medicne) %>%
      dplyr:: summarise(Medicine = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( !is.na(Medicne))
  })
  
  obs_Medicne_cnt <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county == input$years5 )%>%
      group_by(Year,Wave,Medicne) %>%
      dplyr::summarise(Medicine =round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter( !is.na(Medicne))
  })
  
  
  output$obs_Medicne <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_Medicne_all(), aes(stringr :: str_wrap(Medicne), y = Medicine, fill= Year))+
              geom_bar(stat="identity") +
              ggtitle("Did they distribute Medicine?")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(Medicine,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="none") +
              removeGrid())}
    (ggplot(data = obs_Medicne_cnt(), aes(stringr :: str_wrap(Medicne), y = Medicine, fill= Year))+
        geom_bar(stat="identity") +
        ggtitle("Did they distribute Medicine")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        geom_text(aes(label = paste(Medicine,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank(),
              legend.position = "none") +
        removeGrid())
  })
  
  ######FUNDS
  obs_Funds_all <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year,Wave,Funds) %>%
      dplyr:: summarise(Funding = round(n()/sum(!is.na(obs_all()$Year))*100))%>%
      filter( !is.na(Funds))
  })
  
  obs_Funds_cnt <- reactive({
    TT_OBS%>%
      filter(Wave == input$Wave5 & Year == input$years5 & tto012_county == input$years5 )%>%
      group_by(Year,Wave,Funds) %>%
      dplyr::summarise(Funding =round(n()/sum(!is.na(obs_county()$Year))*100))%>%
      filter( !is.na(Funds))
  })
  
  
  output$obs_Funds <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_Funds_all(), aes(stringr :: str_wrap(Funds), y = Funding, fill= Year))+
              geom_bar(stat="identity") +
              ggtitle("Did they distribute Funds?")+
              scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
              geom_text(aes(label = paste(Funding,"%",sep=""), hjust = "left"),
                        colour = "white",
                        position = position_stack(vjust = 0.1),
                        fontface="bold",
                        size = 4,
                        show.legend = FALSE) +
              coord_flip()+
              theme_bw()+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.text.y = element_text(color="BlACK",size=14)) +
              theme(axis.title.x = element_blank(), 
                    axis.text.x = element_blank(),
                    axis.ticks.x =  element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    panel.border = element_blank(), 
                    legend.position="none") +
              removeGrid())}
    (ggplot(data = obs_Funds_cnt(), aes(stringr :: str_wrap(Funds), y = Funding, fill= Year))+
        geom_bar(stat="identity") +
        ggtitle("Did they distribute Funding")+
        scale_fill_manual(values =  c('#FE6F87', '#5E5F5F'))+
        geom_text(aes(label = paste(Funding,"%",sep=""), hjust = "left"),
                  colour = "white",
                  position = position_stack(vjust = 0.1),
                  fontface="bold",
                  size = 4,
                  show.legend = FALSE) +
        coord_flip()+
        theme_bw()+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x =  element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "none",
              plot.margin = unit(c(1,1,1,0), "mm"),
              panel.border = element_blank()) +
        removeGrid())
  })
  
  ####CHEW CONTACTS
  obs_Chews_all <- reactive({
    TT_OBS %>%
      filter(!is.na(Chew_Contact)& Wave == input$Wave5 & Year == input$years5)%>%
      group_by(Year, Wave, Chew_Contact) %>%
      summarise(contacts = round(n()/sum(!is.na(obs_all()$Chew_Contact))*100))%>%
      filter( !is.na(Year))
  })
  
  obs_Chews_county <- reactive({
    TT_OBS %>%
      filter(!is.na(Chew_Contact) & Wave == input$Wave5 & Year == input$years5 & tto012_county == input$years5)%>%
      group_by(Year, Wave, Chew_Contact) %>%
      summarise(contacts = round(n()/sum(!is.na(obs_county()$Chew_Contact))*100))%>%
      filter( !is.na(Year))
  })
  
  output$obs_chew_contacts <- renderPlot ({
    
    if (input$County5 == 'Kenya') 
    {return(ggplot(data = obs_Chews_all(), 
                   aes(x = 2, y = contacts, fill= Chew_Contact ))+
              geom_bar(stat = "identity")+
              ggtitle("Do you have contacts of the CHEW?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(contacts,"%", sep = ""),hjust = "centre"), col = "white",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('#5E5F5F','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    ggplot(data = obs_Chews_county(), 
           aes(x = 2, y = contacts, fill= Chew_Contact ))+
      geom_bar(stat = "identity")+
      ggtitle("Do you have contacts of the CHEW?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(contacts,"%", sep = ""),hjust = "centre"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c('#5E5F5F', '#FE6F87'))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  output$ddcomm_title <- renderText({
    paste(input$years5, " Wave ",input$Wave5, "DD COMM DASHBOARDS")
  })
  
  
  #####DD COMM
  dd_county_rows <- reactive({
    DD_Comm %>%
      dplyr::filter(DD_Comm$pdc012_county == input$County6 & Year == input$years6 & Wave == input$Wave6 & !is.na(pdc011_dd_main))
  })
  
  dd_rows <- reactive({
    DD_Comm %>%
      filter(Year == input$years6 & Wave == input$Wave6 & !is.na(pdc011_dd_main))
    
  })
  
  ####TREATMENTTYPES
  trt_all_xy <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc011_dd_main))%>%
      group_by(Year, Wave,pdc011_dd_main)%>%
      dplyr:: summarise(trt = round(n()/nrow(dd_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(pdc011_dd_main, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      spread(Labels, trt)%>%
      tibble::deframe()
  })
  
  sc_trt_c <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc011_dd_main)))%>%
      group_by(Year,Wave, pdc011_dd_main)%>%
      dplyr::  summarise(trt = round(n()/nrow(dd_county_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste(pdc011_dd_main, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      spread(Labels, trt)%>%
      tibble::deframe()
  })
  
  #### CHILDREN DEWORMED
  deworm_all_xy <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc128_send_children_dd))%>%
      group_by(Year, Wave,pdc128_send_children_dd)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc128_send_children_dd))*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(pdc128_send_children_dd, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  sc_deworm<- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc128_send_children_dd)))%>%
      group_by(Year,Wave, pdc128_send_children_dd)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc128_send_children_dd))*100))%>%
      ungroup()%>%
      mutate(Labels = paste(pdc128_send_children_dd, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  output$dd_dewormed <- renderPlot({
    if (input$County6 == 'Kenya')
    {return(waffle(deworm_all_xy(),rows = 10,
                   colors = c('#FE6F87',"grey", "maroon"),
                   title = "Children Dewomed",
                   legend_pos = "bottom")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    
    waffle(sc_deworm(),rows = 10,
           colors = c('#FE6F87','grey', 'maroon'),
           title = "Treatment Types",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  ####INTERVIEW TYPES
  int_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc100_interview_type))%>%
      group_by(Year, Wave,pdc100_interview_type)%>%
      dplyr:: summarise(trt = round(n()/nrow(dd_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(pdc100_interview_type, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  sc_int_all <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc100_interview_type)))%>%
      group_by(Year,Wave, pdc100_interview_type)%>%
      dplyr::  summarise(trt = round(n()/nrow(dd_county_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste(pdc100_interview_type, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  output$dd_int_type <- renderPlot({
    if (input$County6 == 'Kenya')
    {return(waffle(int_all(),rows = 10,
                   colors = c('#FE6F87', 'grey','maroon'),
                   title = "Interview Types",
                   legend_pos = "bottom")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    
    waffle(sc_int_all(),rows = 10,
           colors = c('#FE6F87','grey'),
           title = "Interview Types",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  ####AWARE DEWRMING ACTIVITIES
  deworm_act_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc100_interview_type))%>%
      group_by(Year, Wave,pdc100_interview_type)%>%
      dplyr:: summarise(trt = round(n()/nrow(dd_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(pdc100_interview_type, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  sc_deworm_act <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc100_interview_type)))%>%
      group_by(Year,Wave, pdc100_interview_type)%>%
      dplyr::  summarise(trt = round(n()/nrow(dd_county_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste(pdc100_interview_type, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  output$dd_activities <- renderPlot({
    if (input$County6 == 'Kenya')
    {return(waffle(int_all(),rows = 10,
                   colors = c('#FE6F87','dark grey','maroon'),
                   title = "Deworming Activities Happening",
                   legend_pos = "bottom")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    
    waffle(sc_int_all(),rows = 10,
           colors = c('#FE6F87','grey'),
           title = "Deworming Activities Happening",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  ####AWARE WORMS TREATED
  worms_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc121_treat_which_worm))%>%
      group_by(Year, Wave,pdc121_treat_which_worm)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc121_treat_which_worm))*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(pdc121_treat_which_worm, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  sc_worms_all <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc121_treat_which_worm)))%>%
      group_by(Year,Wave, pdc121_treat_which_worm)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc121_treat_which_worm))*100)) %>%
      ungroup()%>%
      mutate(Labels = paste(pdc121_treat_which_worm, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  output$dd_worms <- renderPlot({
    if (input$County6 == 'Kenya')
    {return(waffle(worms_all(),rows = 10,
                   colors = c('#FE6F87','dark grey',"maroon", "white"),
                   title = "Worms Treated",
                   legend_pos = "right")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    
    waffle(sc_worms_all(),rows = 10,
           colors = c('#FE6F87',"dark grey",'maroon', "white"),
           title = "Worms Treated",
           legend_pos = "right")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  ####Age Group
  age_group_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc122_treat_agegrp_sth))%>%
      group_by(Year, Wave,pdc122_treat_agegrp_sth)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc122_treat_agegrp_sth))*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(pdc122_treat_agegrp_sth, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  sc_age_group <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc122_treat_agegrp_sth)))%>%
      group_by(Year,Wave, pdc122_treat_agegrp_sth)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc122_treat_agegrp_sth))*100))%>%
      ungroup()%>%
      mutate(Labels = paste(pdc122_treat_agegrp_sth, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  output$dd_age_group <- renderPlot({
    if (input$County6 == 'Kenya')
    {return(waffle(age_group_all(),rows = 10,
                   colors = c('#FE6F87','grey',"dark grey", "white"),
                   title = "Age Groups Treated",
                   legend_pos = "right")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )}
    
    waffle(sc_age_group(),rows = 10,
           colors =c('#FE6F87','grey',"dark grey", "white"),
           title = "Age Groups Treated",
           legend_pos = "right")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  ####Aware deworming Activities going on
  shared_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc111_dd_comm_children))%>%
      group_by(Year, Wave,pdc111_dd_comm_children)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc111_dd_comm_children))*100))%>%
      ungroup()%>%
      filter(pdc111_dd_comm_children =="Yes; Deworming of children")%>%
      select(trt)
  })
  
  sc_shared_all <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc111_dd_comm_children)))%>%
      group_by(Year,Wave, pdc111_dd_comm_children)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc111_dd_comm_children))*100))%>%
      ungroup()%>%
      filter(pdc111_dd_comm_children=="Yes; Deworming of children")%>%
      select(trt)
  })
  
  output$awareness <- renderPlotly({
    if (input$County6 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = shared_all()[[1]],
        title = list(text = "Deworming Activities Awareness"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sc_shared_all()[[1]],
      title = list(text = "Aware Deworing Activities are happening"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  
  ###DEWORMED BEFORE
  deworm_bf_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc115_dd_child_bfore))%>%
      group_by(Year, Wave,pdc115_dd_child_bfore)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc111_dd_comm_children))*100))%>%
      ungroup()%>%
      filter(pdc115_dd_child_bfore =="Yes")%>%
      select(trt)
  })
  
  sc_deworm_bf <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc115_dd_child_bfore)))%>%
      group_by(Year,Wave, pdc115_dd_child_bfore)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc111_dd_comm_children))*100))%>%
      
      ungroup()%>%
      filter(pdc115_dd_child_bfore=="Yes")%>%
      select(trt)
  })
  
  output$deworm_before <- renderPlotly({
    if (input$County6 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = deworm_bf_all()[[1]],
        title = list(text = "Have you dewormed Before"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sc_deworm_bf()[[1]],
      title = list(text = "Have you dewormed Before"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  
  ###FEEDBACK
  dd_feedback_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc132_feel_abt_dd))%>%
      group_by(Year, Wave,pdc132_feel_abt_dd)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc132_feel_abt_dd))*100))%>%
      ungroup()%>%
      filter(pdc132_feel_abt_dd =="Positive")%>%
      select(trt)
  })
  
  sc_dd_feedback <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc132_feel_abt_dd)))%>%
      group_by(Year,Wave, pdc132_feel_abt_dd)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc132_feel_abt_dd))*100))%>%
      ungroup()%>%
      filter(pdc132_feel_abt_dd=="Positive")%>%
      select(trt)
  })
  
  output$deworm_feedback <- renderPlotly({
    if (input$County6 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = dd_feedback_all()[[1]],
        title = list(text = "Positive Feedback"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sc_dd_feedback()[[1]],
      title = list(text = "Positive feeling \n About Deworming"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  ###REASONS FOR DEWORMING
  
  dd_deworm_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc116_dd_child_why))%>%
      group_by(Year, Wave,pdc116_dd_child_why)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc116_dd_child_why))*100))
  })
  
  sc_deworm_feedback <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc116_dd_child_why)))%>%
      group_by(Year,Wave, pdc116_dd_child_why)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc116_dd_child_why))*100))
  })
  
  
  output$cloud <- renderPlot({
    if (input$County6 == 'Kenya') 
    {return( ggplot(data = dd_deworm_all(), aes(x = pdc116_dd_child_why, y = trt, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(pdc116_dd_child_why, 30), trt) + xlab(NULL)+
               ggtitle("Reasons for Deworming")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(trt),"%",sep=""),  hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     legend.text = element_text(color="BlACK",size=14),
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = sc_deworm_feedback(), aes(x = pdc116_dd_child_why, y = trt, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(pdc116_dd_child_why, 30), trt) + xlab(NULL)+
        ggtitle("Reasons for Dewormin")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(trt),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        removeGrid()}
  })
  
  
  ###REASONS FOR NOT DEWORMING
  
  dd_not_deworm_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc117_dd_child_whynot))%>%
      group_by(Year, Wave,pdc117_dd_child_whynot)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc117_dd_child_whynot))*100))
  })
  
  sc_not_deworm_feedback <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc117_dd_child_whynot)))%>%
      group_by(Year,Wave, pdc117_dd_child_whynot)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc117_dd_child_whynot))*100))
  })
  
  
  output$cloud2 <- renderPlot({
    if (input$County6 == 'Kenya') 
    {return( ggplot(data = dd_not_deworm_all(), aes(x = pdc117_dd_child_whynot, y = trt, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(pdc117_dd_child_whynot, 30), trt) + xlab(NULL)+
               ggtitle("Reasons for NOT Deworming")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(trt),"%",sep=""),  hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     legend.text = element_text(color="BlACK",size=14),
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = sc_not_deworm_feedback(), aes(x = pdc117_dd_child_whynot, y = trt, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(pdc117_dd_child_whynot, 30), trt) + xlab(NULL)+
        ggtitle("Reasons for NOT Dewormin")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(trt),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        removeGrid()}
  })
  
  #####COMMUNICATION MEANS TO PARENTS
  Comm_parents_all <- reactive({
    Comm_parents2%>%
      filter(Wave == input$Wave6 & Year == input$years6)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(chew_roles = round(n()/sum(!is.na(dd_rows()$pdc125_preffered_method_comm))*100))
  })
  
  Comm_parents_county <- reactive({
    Comm_parents2%>%
      filter(Wave == input$Wave6 & Year == input$years6 & scto012_county == input$County6)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(chew_roles = round(n()/sum(!is.na(dd_county_rows()$pdc125_preffered_method_comm))*100))
  })
  
  output$Comm_means1 <- renderPlot({
    if (input$County6 == 'Kenya') 
    {return( ggplot(data = Comm_parents_all(), aes(x = Values, y = chew_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
               aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
               ggtitle("Top 3 Preferred Communication Means\n by Parents")+
               scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                         colour = "white",
                         position = position_dodge(width = .0),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     legend.text = element_text(color="BlACK",size=14),
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = Comm_parents_county(), aes(x = Values, y = chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
        ggtitle("Top 3 Preferred Communication Means \n by Parents")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  #####COMMUNICATION MEANS TO CHV
  ##(Comm_CHV, Comm_parents)
  Comm_CHV_all <- reactive({
    Comm_CHV%>%
      filter(Wave == input$Wave6 & Year == input$years6)%>%
      group_by(Year,Role,Values) %>%
      dplyr:: summarise(chew_roles = round(n()/sum(!is.na(dd_rows()$pdc709_dd_how_learn))*100))
  })
  
  Comm_CHV_county <- reactive({
    Comm_CHV%>%
      filter(Wave == input$Wave6 & Year == input$years6 & scto012_county == input$County6)%>%
      group_by(Year,Role, Values) %>%
      dplyr:: summarise(chew_roles = round(n()/sum(!is.na(dd_county_rows()$pdc709_dd_how_learn))*100))
  })
  
  output$Comm_means2 <- renderPlot({
    if (input$County6 == 'Kenya') 
    {return( ggplot(data = Comm_CHV_all(), aes(x = Values, y = chew_roles, fill=Role))+
               geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
               aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
               ggtitle("Communication Means to CHV")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "centre", size = 4),
                         colour = "black",
                         position = position_dodge(width = 0.5),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     legend.text = element_text(color="BlACK",size=14),
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = Comm_CHV_county(), aes(x = Values, y = chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.8) +
        aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
        ggtitle("Communication Means to CHV")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "right",
              legend.text = element_text(color="BlACK",size=14),
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  #####COMMUNICATION MEANS TO Parents
  ##(Comm_CHV, Comm_parents)
  Comm_parents_all <- reactive({
    Comm_parents%>%
      filter(Wave == input$Wave6 & Year == input$years6)%>%
      group_by(Year,Role,Values) %>%
      dplyr:: summarise(chew_roles = round(n()/sum(!is.na(dd_rows()$pdc124_dd_how_learn))*100))
  })
  
  Comm_parents_county <- reactive({
    Comm_parents%>%
      filter(Wave == input$Wave6 & Year == input$years6 & scto012_county == input$County6)%>%
      group_by(Year,Role, Values) %>%
      dplyr:: summarise(chew_roles = round(n()/sum(!is.na(dd_county_rows()$pdc124_dd_how_learn))*100))
  })
  
  output$Comm_means3 <- renderPlot({
    if (input$County6 == 'Kenya') 
    {return( ggplot(data = Comm_parents_all(), aes(x = Values, y = chew_roles, fill=Role))+
               geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
               aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
               ggtitle("Communication Means to Parents")+
               scale_fill_manual(values = c('grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "centre", size = 4),
                         colour = "black",
                         position = position_dodge(width = 0.5),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     legend.text = element_text(color="BlACK",size=14),
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = Comm_parents_county(), aes(x = Values, y = chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.8) +
        aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
        ggtitle("Communication Means to Parents")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              legend.text = element_text(color="BlACK",size=14),
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  ###CHV EXISTS
  dd_chv_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc701_chw_exist))%>%
      group_by(Year, Wave,pdc701_chw_exist)%>%
      dplyr:: summarise(trt = round(n()/nrow(dd_rows())*100))%>%
      ungroup()%>%
      filter(pdc701_chw_exist =="Yes")%>%
      select(trt)
  })
  
  sc_dd_chv <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc701_chw_exist)))%>%
      group_by(Year,Wave, pdc701_chw_exist)%>%
      dplyr::  summarise(trt = round(n()/nrow(dd_county_rows())*100))%>%
      ungroup()%>%
      filter(pdc701_chw_exist=="Yes")%>%
      select(trt)
  })
  
  output$chv_exist <- renderPlotly({
    if (input$County6 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = dd_chv_all()[[1]],
        title = list(text = "CHV Exists?"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sc_dd_chv()[[1]],
      title = list(text = "CHV Exists?"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
  })
  
  ###Children Deworm
  chv_deworm_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc704_children_get_dd))%>%
      group_by(Year, Wave,pdc704_children_get_dd)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc704_children_get_dd))*100))%>%
      ungroup()%>%
      filter(pdc704_children_get_dd =="Yes")%>%
      select(trt)
  })
  
  sc_chv_deworm_ <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc704_children_get_dd)))%>%
      group_by(Year,Wave, pdc704_children_get_dd)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_county_rows()$pdc704_children_get_dd))*100))%>%
      ungroup()%>%
      filter(pdc704_children_get_dd=="Yes")%>%
      select(trt)
  })
  
  output$chv_dewrm <- renderPlotly({
    if (input$County6 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = chv_deworm_all()[[1]],
        title = list(text = "Are Children getting dewormed?"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sc_chv_deworm_()[[1]],
      title = list(text = "Are Children getting dewormed?"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  ###Who to deworm Deworm
  chv_who_deworm_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc706_treat_which_child))%>%
      group_by(Year, Wave,pdc706_treat_which_child)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc706_treat_which_child))*100))%>%
      ungroup()%>%
      filter(pdc706_treat_which_child =="All Children (2-14 Yrs)")%>%
      select(trt)
  })
  
  sc_chv_who_deworm <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc706_treat_which_child)))%>%
      group_by(Year,Wave, pdc706_treat_which_child)%>%
      dplyr::  summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc706_treat_which_child))*100))%>%
      ungroup()%>%
      filter(pdc706_treat_which_child=="All Children (2-14 Yrs)")%>%
      select(trt)
  })
  
  output$chv_who_deworm<- renderPlotly({
    if (input$County6 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = chv_who_deworm_all()[[1]],
        title = list(text = "Who will be getting dewormed?"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sc_chv_who_deworm()[[1]],
      title = list(text = "Who will be getting dewormed?"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
  })
  
  ###AGE GROUP
  chv_agegroup_all <-  reactive({
    DD_Comm %>%
      filter( Year == input$years6 & Wave == input$Wave6 & !is.na(pdc707_treat_agegrp_sth))%>%
      group_by(Year, Wave,pdc707_treat_agegrp_sth)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(dd_rows()$pdc707_treat_agegrp_sth))*100))%>%
      ungroup()%>%
      filter(pdc707_treat_agegrp_sth =="2-14 Years")%>%
      select(trt)
  })
  
  
  sc_chv_agegroup <- reactive ({
    DD_Comm %>%
      filter((DD_Comm$sctp012_county == input$County6 & DD_Comm$Year == input$years6 & Wave == input$Wave6 & !is.na(pdc707_treat_agegrp_sth)))%>%
      group_by(Year,Wave, pdc707_treat_agegrp_sth)%>%
      dplyr::  summarise(trt = round(n()/sum/(!is.na(dd_rows()$pdc707_treat_agegrp_sth))*100))%>%
      ungroup()%>%
      filter(pdc707_treat_agegrp_sth=="2-14 Years")%>%
      select(trt)
  })
  
  output$chv_agegroup<- renderPlotly({
    if (input$County6 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = chv_who_deworm_all()[[1]],
        title = list(text = "Correct age group getting dewormed"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sc_chv_who_deworm()[[1]],
      title = list(text = "Correct age group getting dewormed"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
  })
  
  
  
  #####DD MAin
  dd_main_county_rows <- reactive({
    DD_Main %>%
      dplyr::filter(DD_Main$ddm012_county == input$County7 & Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children))
  })
  
  dd_main_rows <- reactive({
    DD_Main %>%
      filter(Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children))
    
  })
  
  #### STH DRUGS
  
  dd_main_sthdrugs_all <- reactive ({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm113_sth_drug_treat))%>%
      group_by(Year, Wave,ddm113_sth_drug_treat)%>%
      dplyr:: summarise(drug = paste(round(n()/nrow(dd_main_rows()))*100,"%", sep=""))%>%
      ungroup()%>%
      filter(ddm113_sth_drug_treat == "Albendazole")%>%
      mutate(Drugs = HTML(paste(ddm113_sth_drug_treat, drug, sep ="                 ")))%>%
      select(Drugs)
  })
  
  dd_main_sc_sthdrug <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children)))%>%
      group_by(Year, ddm113_sth_drug_treat)%>%
      dplyr::  summarise(drug = (n()/nrow(dd_main_county_rows()))*100)%>%
      ungroup()%>%
      filter(DD_Main$ddm113_sth_drug_treat == "Albendazole")%>%
      mutate(Drugs = paste(ddm113_sth_drug_treat," ", drug,"%"))%>%
      select(Drugs)
  })
  
  
  output$dd_main_sth_drugs <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("What drug will be used to treat STH?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_sthdrugs_all()$Drugs),
      icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4("What drug will be used to treat STH?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_sc_sthdrug()$Drugs),
      icon = icon("prescription-bottle-alt")
    )
    
  })
  
  
  ####STH DOSAGE
  
  dd_main_sthdosage_all <- reactive ({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children))%>%
      group_by(Year, Wave,ddm114_sth_dose)%>%
      dplyr:: summarise(dosage = round(n()/nrow(dd_main_rows())*100))%>%
      ungroup()%>%
      filter(ddm114_sth_dose == "One Tablet Per Child")%>%
      mutate(Drugs = paste(ddm114_sth_dose," ", dosage,"%"))%>%
      select(Drugs)
  })
  
  
  
  dd_main_sc_sthdsage <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children)))%>%
      group_by(Year, ddm114_sth_dose)%>%
      dplyr::  summarise(dosage = round(n()/nrow(dd_main_dd_main_county_rows())*100))%>%
      ungroup()%>%
      filter(ddm114_stdd_main_sc_sthdsageh_dose == "One Tablet Per Child")%>%
      mutate(Drugs = paste(ddm114_sth_dose," ", dosage,"%"))%>%
      select(Drugs)
  })
  
  
  
  output$dd_main_sth_dosage <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("What will be the STH dosage?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_sthdosage_all()$Drugs),
      icon = icon("capsules")
    ))}
    
    infoBox(
      h4("What will be the STH dosage?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_sc_sthdsage()$Drugs),
      icon = icon("capsules")
    )
    
  })
  
  
  
  #STH AGE GROUP
  dd_main_sthage_all <- reactive ({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children))%>%
      group_by(Year, Wave,ddm115_sth_agegroup)%>%
      dplyr:: summarise(age = round(n()/nrow(dd_main_rows())*100))%>%
      ungroup()%>%
      dplyr:: filter(ddm115_sth_agegroup == "2-14 Years")%>%
      dplyr::mutate(Drugs = paste(ddm115_sth_agegroup," ", age,"%"))%>%
      select(Drugs)
  })
  
  dd_main_sc_sthage <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children)))%>%
      group_by(Year, ddm115_sth_agegroup)%>%
      dplyr::  summarise(age = round(n()/nrow(dd_main_county_rows())*100))%>%
      ungroup()%>%
      dplyr:: filter(ddm115_sth_agegroup == "2-14 Years")%>%
      mutate(Drugs = paste(ddm115_sth_agegroup," ", age,"%"))%>%
      select(Drugs)
  })
  
  
  output$dd_main_sth_age <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("What is the age-group for STH Age?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red",  dd_main_sthage_all()$Drugs),
      icon = icon("calendar-week")
    ))}
    
    infoBox(
      h4("What is the age-group for STH Age?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_sc_sthage()$Drugs),
      icon = icon("calendar-week")
    )
    
  })
  
  
  #STH CORRECT STEPS KNOWLDEGE
  dd_main_sthstep_all <- reactive ({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children))%>%
      group_by(Year, Wave)%>%
      dplyr:: summarise(age =  round(mean(Correct_STH_Steps, na.rm = T)*100))%>%
      ungroup()%>%
      #dplyr:: filter(Correct_STH_Steps == "2-14 years")%>%
      dplyr::mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
  })
  
  dd_main_sc_sthstep <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children)))%>%
      group_by(Year)%>%
      dplyr::  summarise(age = round(mean(Correct_STH_Steps, na.rm = T)*100))%>%
      ungroup()%>%
      #dplyr:: filter(Correct_STH_Steps == "2-14 years")%>%
      mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
  })
  
  output$dd_main_sth_step <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("Correct Treatment Steps"),
      tags$p(style = "font-size: 30px;",  dd_main_sthstep_all()$Drugs),
      icon = icon("calendar-week")
    ))}
    
    infoBox(
      h4("What is the age-group for STH ddm009a_num_children?"),
      tags$p(style = "font-size: 30px;", dd_main_sc_sthstep()$Drugs),
      icon = icon("calendar-week")
    )
    
  })
  
  
  #### sch DRUGS
  
  dd_main_schdrugs_all <- reactive ({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm117_sch_drugs))%>%
      group_by(Year, Wave,ddm117_sch_drugs)%>%
      dplyr:: summarise(drug = round(n()/sum(!is.na(dd_main_rows()$ddm117_sch_drugs))*100))%>%
      ungroup()%>%
      filter(ddm117_sch_drugs == "Praziquantel")%>%
      mutate(Drugs = HTML(paste(ddm117_sch_drugs, drug, sep ="")))%>%
      select(Drugs)
  })
  
  dd_main_sc_schdrug <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm117_sch_drugs)))%>%
      group_by(Year, ddm117_sch_drugs)%>%
      dplyr::  summarise(drug = round(n()/sum(!is.na(dd_main_rows()$ddm117_sch_drugs))*100))%>%
      ungroup()%>%
      filter(DD_Main$ddm117_sch_drugs == "Praziquantel")%>%
      mutate(Drugs = paste(ddm117_sch_drugs," ", drug,"%"))%>%
      select(Drugs)
  })
  
  
  output$dd_main_sch_drugs <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("What drug will be used to treat SCH?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_schdrugs_all()$Drugs),
      icon = icon("prescription-bottle-alt")
    ))}
    
    infoBox(
      h4("What drug will be used to treat SCH?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_sc_schdrug()$Drugs),
      icon = icon("prescription-bottle-alt")
    )
    
  })
  
  
  ####sch DOSAGE
  
  dd_main_schdosage_all <- reactive ({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm118_sch_dose))%>%
      group_by(Year, Wave,ddm118_sch_dose)%>%
      dplyr:: summarise(dosage =paste(round(n()/sum(!is.na(ddm118_sch_dose)))*100,"%", sep=""))%>%
      ungroup()%>%
      filter(ddm118_sch_dose == "According to the tablet pole")%>%
      mutate(Drugs = paste(ddm118_sch_dose," ", dosage,"%"))%>%
      select(Drugs)
  })
  
  
  
  dd_main_sc_schdsage <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm118_sch_dose)))%>%
      group_by(Year, ddm118_sch_dose)%>%
      dplyr::  summarise(dosage = paste(round(n()/sum(!is.na(ddm117_sch_drugs)))*100,"%", sep=""))%>%
      ungroup()%>%
      filter(ddm114_stdd_main_sc_schdsageh_dose == "According to the tablet pole")%>%
      mutate(Drugs = paste(ddm118_sch_dose," ", dosage,"%"))%>%
      select(Drugs)
  })
  
  
  
  output$dd_main_sch_dosage <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("What will be the sch dosage?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_schdosage_all()$Drugs),
      icon = icon("capsules")
    ))}
    
    infoBox(
      h4("What will be the sch dosage?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_sc_schdsage()$Drugs),
      icon = icon("capsules")
    )
    
  })
  
  
  
  #sch AGE GROUP
  dd_main_schage_all <- reactive ({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm119_sch_agegroup))%>%
      group_by(Year, Wave,ddm119_sch_agegroup)%>%
      dplyr:: summarise(age = round(n()/nrow(dd_main_rows())*100))%>%
      ungroup()%>%
      dplyr:: filter(ddm119_sch_agegroup == "6-14 Years")%>%
      dplyr::mutate(Drugs = paste(ddm119_sch_agegroup," ", age,"%"))%>%
      select(Drugs)
  })
  
  dd_main_sc_schage <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm119_sch_agegroup)))%>%
      group_by(Year, ddm119_sch_agegroup)%>%
      dplyr::  summarise(age = round(n()/nrow(dd_main_county_rows())*100))%>%
      ungroup()%>%
      dplyr:: filter(ddm119_sch_agegroup == "6-14 Years")%>%
      mutate(Drugs = paste(ddm119_sch_agegroup," ", age,"%"))%>%
      select(Drugs)
  })
  
  
  output$dd_main_sch_age <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("What is the age-group for SCH?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red",  dd_main_schage_all()$Drugs),
      icon = icon("calendar-week")
    ))}
    
    infoBox(
      h4("What is the age-group for SCH?"),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_main_sc_schage()$Drugs),
      icon = icon("calendar-week")
    )
    
  })
  
  
  #sch CORRECT STEPS KNOWLDEGE
  dd_main_schstep_all <- reactive ({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children))%>%
      group_by(Year, Wave)%>%
      dplyr:: summarise(age =  round(mean(Correct_SCH_Steps, na.rm = T)*100))%>%
      ungroup()%>%
      #dplyr:: filter(Correct_sch_Steps == "2-14 years")%>%
      dplyr::mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
  })
  
  dd_main_sc_schstep <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm009a_num_children)))%>%
      group_by(Year)%>%
      dplyr::  summarise(age = round(mean(Correct_SCH_Steps, na.rm = T)*100))%>%
      ungroup()%>%
      #dplyr:: filter(Correct_sch_Steps == "2-14 years")%>%
      mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
  })
  
  output$dd_main_sch_step <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("Correct SCH Treatment Steps"),
      tags$p(style = "font-size: 30px;",  dd_main_schstep_all()$Drugs),
      icon = icon("calendar-week")
    ))}
    
    infoBox(
      h4("Correct SCH Treatment Steps"),
      tags$p(style = "font-size: 30px;", dd_main_sc_schstep()$Drugs),
      icon = icon("calendar-week")
    )
    
  })
  
  
  
  #####TREATMENT PLAN
  ###aTTACHED ECD
  attached_ecd_all <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & !is.na(ddm107a_dd_ecd_treatplan) & ddm107a_dd_ecd_treatplan != "Other (Specify)")%>%
      group_by(Year,ddm107a_dd_ecd_treatplan) %>%
      dplyr:: summarise(ecd = round(n()/sum(!is.na(dd_main_rows()$ddm107a_dd_ecd_treatplan))*100))
  })
  
  attached_ecd_county <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7 & !is.na(ddm107a_dd_ecd_treatplan) )%>%
      group_by(Year,ddm107a_dd_ecd_treatplan) %>%
      dplyr:: summarise(ecd = (round(n()/sum(!is.na(dd_main_rows()$ddm107a_dd_ecd_treatplan))*100)))
  })
  
  output$attached_ecd <- renderPlot({
    if (input$County7 == 'Kenya') 
    {return( ggplot(data = attached_ecd_all(), aes(x = ddm107a_dd_ecd_treatplan, y = ecd, fill=Year))+
               geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
               aes(stringr::str_wrap(ddm107a_dd_ecd_treatplan, 30), ecd) + xlab(NULL)+
               ggtitle("Roles of CHEW")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(ecd),"%",sep=""),  hjust = "centre", size = 4),
                         colour = "black",
                         position = position_dodge(width = 0.5),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "none",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = attached_ecd_county(), aes(x = ddm107a_dd_ecd_treatplan, y = ecd, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.8) +
        aes(stringr::str_wrap(ddm107a_dd_ecd_treatplan, 30), ecd) + xlab(NULL)+
        ggtitle("Roles of CHEW")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(ecd),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  ###STAND ALONE ECDs
  stand_alone_ecd_all <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & !is.na(ddm108_dd_ecd_treatplan) )%>%
      group_by(Year,ddm108_dd_ecd_treatplan) %>%
      dplyr:: summarise(ecd = round(n()/sum(!is.na(dd_main_rows()$ddm108_dd_ecd_treatplan))*100))
  })
  
  stand_alone_ecd_county <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7 & !is.na(ddm108_dd_ecd_treatplan))%>%
      group_by(Year,ddm108_dd_ecd_treatplan) %>%
      dplyr:: summarise(ecd = (round(n()/nrow(dd_main_county_rows()$ddm108_dd_ecd_treatplan))*100))
  })
  
  output$stand_alone_ecd <- renderPlot({
    if (input$County7 == 'Kenya') 
    {return( ggplot(data = stand_alone_ecd_all(), aes(x = ddm108_dd_ecd_treatplan, y = ecd, fill=Year))+
               geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
               aes(stringr::str_wrap(ddm108_dd_ecd_treatplan, 30), ecd) + xlab(NULL)+
               ggtitle("Stand Alone ECDs")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(ecd),"%",sep=""),  hjust = "centre", size = 4),
                         colour = "black",
                         position = position_dodge(width = 0.5),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "right",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = stand_alone_ecd_county(), aes(x = ddm108_dd_ecd_treatplan, y = ecd, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.8) +
        aes(stringr::str_wrap(ddm108_dd_ecd_treatplan, 30), ecd) + xlab(NULL)+
        ggtitle("Stand Alone ECDs")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(ecd),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  ###NON ENROLLED
  non_enrolled_ecd_all <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & !is.na(ddm109_dd_plan_nonenroll))%>%
      group_by(Year,ddm109_dd_plan_nonenroll) %>%
      dplyr:: summarise(ecd = round(n()/sum(!is.na(dd_main_rows()$ddm109_dd_plan_nonenroll))*100))
  })
  
  non_enrolled_ecd_county <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7 & !is.na(ddm109_dd_plan_nonenroll))%>%
      group_by(Year,ddm109_dd_plan_nonenroll) %>%
      dplyr:: summarise(ecd = (round(n()/nrow(dd_main_county_rows()$ddm109_dd_plan_nonenroll))*100))
  })
  
  output$non_enrolled_ecd <- renderPlot({
    if (input$County7 == 'Kenya') 
    {return( ggplot(data = non_enrolled_ecd_all(), aes(x = ddm109_dd_plan_nonenroll, y = ecd, fill=Year))+
               geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
               aes(stringr::str_wrap(ddm109_dd_plan_nonenroll, 30), ecd) + xlab(NULL)+
               ggtitle("Non Enrolled")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(ecd),"%",sep=""),  hjust = "centre", size = 4),
                         colour = "black",
                         position = position_dodge(width = 0.5),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "right",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = non_enrolled_ecd_county(), aes(x = ddm109_dd_plan_nonenroll, y = ecd, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.8) +
        aes(stringr::str_wrap(ddm109_dd_plan_nonenroll, 30), ecd) + xlab(NULL)+
        ggtitle("Non Enrolled")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(ecd),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  ####Materials to Stand Alone
  ##FORMS
  standalone_forms_all <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & !is.na(ddm108a_sa_tt_data_forms))%>%
      group_by(Year, Wave, ddm108a_sa_tt_data_forms)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(dd_main_rows()$ddm108a_sa_tt_data_forms))*100))
  })
  
  standalone_forms_county <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7 & !is.na(ddm108a_sa_tt_data_forms))%>%
      group_by(Year, Wave, ddm108a_sa_tt_data_forms,ddm012_county)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(dd_main_county_rows()$ddm108a_sa_tt_data_forms))*100))
  })
  
  output$standalone_forms <- renderPlot ({
    
    if (input$County7 == 'Kenya') 
    {return(ggplot(data = standalone_forms_all(), 
                   aes(x = 2, y = sanitizer, fill= ddm108a_sa_tt_data_forms ))+
              geom_bar(stat = "identity")+
              ggtitle("Did the Stand Alone receive Treatment Forms?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = standalone_forms_county(), 
            aes(x = 2, y = sanitizer, fill= ddm108a_sa_tt_data_forms ))+
        geom_bar(stat = "identity")+
        ggtitle("Did the Stand Alone receive Treatment Forms?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  ###DRUGS
  standalone_drugs_all <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & !is.na(ddm108b_sa_dmedicine))%>%
      group_by(Year, Wave, ddm108b_sa_dmedicine)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(dd_main_rows()$ddm108b_sa_dmedicine))*100))
  })
  
  standalone_drugs_county <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7 & !is.na(ddm108b_sa_dmedicine))%>%
      group_by(Year, Wave, ddm108b_sa_dmedicine,ddm012_county)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(dd_main_county_rows()$ddm108b_sa_dmedicine))*100))
  })
  
  output$standalone_drugs <- renderPlot ({
    
    if (input$County7 == 'Kenya') 
    {return(ggplot(data = standalone_drugs_all(), 
                   aes(x = 2, y = sanitizer, fill= ddm108b_sa_dmedicine ))+
              geom_bar(stat = "identity")+
              ggtitle("Did the Stand Alone receive Treatment Drugs?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = standalone_drugs_county(), 
            aes(x = 2, y = sanitizer, fill= ddm108b_sa_dmedicine ))+
        geom_bar(stat = "identity")+
        ggtitle("Did the Stand Alone receive Treatment Drugs?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  
  ###DRUGS
  standalone_trtdata_all <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & !is.na(ddm108c_tt_data_cap))%>%
      group_by(Year, Wave, ddm108c_tt_data_cap)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(dd_main_rows()$ddm108c_tt_data_cap))*100))
  })
  
  standalone_trtdata_county <- reactive({
    DD_Main%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7 & !is.na(ddm108c_tt_data_cap))%>%
      group_by(Year, Wave, ddm108c_tt_data_cap,ddm012_county)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(dd_main_county_rows()$ddm108c_tt_data_cap))*100))
  })
  
  output$standalone_trtdata <- renderPlot ({
    
    if (input$County7 == 'Kenya') 
    {return(ggplot(data = standalone_trtdata_all(), 
                   aes(x = 2, y = sanitizer, fill= ddm108c_tt_data_cap ))+
              geom_bar(stat = "identity")+
              ggtitle("Did the Stand Alone capture treatment data?")+
              coord_polar("y", start = 200)+
              geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                        position = position_stack(vjust = 0.5), 
                        size = 5,
                        show.legend = T,
                        fontface="bold") +
              theme_void() +
              scale_fill_manual(values=c('dark grey','#FE6F87', 'grey'))+
              theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
              theme(axis.title.x = element_blank(), 
                    axis.ticks.x =  element_blank(),
                    axis.title.y = element_blank(), 
                    axis.text.y = element_blank(), 
                    axis.ticks.y = element_blank(),
                    plot.margin = unit(c(1,1,1,0), "mm"),
                    legend.position = "bottom",
                    legend.title= element_blank(),
                    panel.border = element_blank()) +
              removeGrid() +
              xlim(.5, 2.5))}
    (ggplot(data = standalone_trtdata_county(), 
            aes(x = 2, y = sanitizer, fill= ddm108c_tt_data_cap ))+
        geom_bar(stat = "identity")+
        ggtitle("Did the Stand Alone capture treatment data?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c('dark grey','#FE6F87'))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  
  #####COMMUNICATION
  ###key messages
  dd_keysms_all <- reactive({
    key_messages%>%
      filter(Wave == input$Wave7 & Year == input$years7)%>%
      group_by(Year,Variables,Values) %>%
      dplyr:: summarise(chew_roles = round(n()/nrow(dd_main_rows())*100))
  })
  
  dd_keysms_county <- reactive({
    key_messages%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7)%>%
      group_by(Year,Variables, Values) %>%
      dplyr:: summarise(chew_roles = (round(n()/nrow(dd_main_county_rows())*100)))
  })
  
  output$dd_keysms <- renderPlot({
    if (input$County7 == 'Kenya') 
    {return( ggplot(data = dd_keysms_all(), aes(x = Values, y = chew_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
               aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
               ggtitle("Key Messages")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "centre", size = 4),
                         colour = "black",
                         position = position_dodge(width = 0.5),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "right",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = dd_keysms_county(), aes(x = Values, y = chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.8) +
        aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
        ggtitle("Roles of CHEW")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  ###Sensitization Acts
  dd_sensitization_all <- reactive({
    sensitization_acts%>%
      filter(Wave == input$Wave7 & Year == input$years7)%>%
      group_by(Year,Variables,Values) %>%
      dplyr:: summarise(chew_roles = round(n()/nrow(dd_main_rows())*100))
  })
  
  dd_sensitization_county <- reactive({
    sensitization_acts%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7)%>%
      group_by(Year,Variables, Values) %>%
      dplyr:: summarise(chew_roles = (round(n()/nrow(dd_main_county_rows())*100)))
  })
  
  output$dd_sensitization <- renderPlot({
    if (input$County7 == 'Kenya') 
    {return( ggplot(data = dd_sensitization_all(), aes(x = Values, y = chew_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
               aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
               ggtitle("Senitization Activities")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "centre", size = 4),
                         colour = "black",
                         position = position_dodge(width = 0.5),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "right",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = dd_sensitization_county(), aes(x = Values, y = chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.8) +
        aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
        ggtitle("Roles of CHEW")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  
  ###Non_Enrolled
  non_enrolled_reach_all <- reactive({
    non_enrolled_reach%>%
      filter(Wave == input$Wave7 & Year == input$years7)%>%
      group_by(Year,Variables,Values) %>%
      dplyr:: summarise(chew_roles = round(n()/nrow(dd_main_rows())*100))
  })
  
  non_enrolled_reach_county <- reactive({
    non_enrolled_reach%>%
      filter(Wave == input$Wave7 & Year == input$years7 & ddm012_county == input$County7)%>%
      group_by(Year,Variables, Values) %>%
      dplyr:: summarise(chew_roles = (round(n()/nrow(dd_main_county_rows())*100)))
  })
  
  output$non_enrolled_reach <- renderPlot({
    if (input$County7 == 'Kenya') 
    {return( ggplot(data = non_enrolled_reach_all(), aes(x = Values, y = chew_roles, fill=Year))+
               geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
               aes(stringr::str_wrap(Values, 10), chew_roles) + xlab(NULL)+
               ggtitle("Ways to reach out to Non Enrolled")+
               scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
               #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
               geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "centre", size = 4),
                         colour = "black",
                         position = position_dodge(width = 0.5),
                         size = 5,
                         show.legend = FALSE,
                         fontface="bold") +
               theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
               theme(axis.text.y = element_text(color="BlACK",size=14)) +
               theme(axis.title.x = element_blank(), 
                     axis.ticks.x =  element_blank(),
                     axis.title.y = element_blank(),
                     axis.text.x = element_blank(),
                     #axis.text.y = element_blank(), 
                     # axis.ticks.y = element_blank(),
                     plot.margin = unit(c(1,1,1,0), "mm"),
                     legend.position = "NONE",
                     panel.border = element_blank()) +
               coord_flip()+
               removeGrid())}
    
    {ggplot(data = non_enrolled_reach_county(), aes(x = Values, y = chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.8) +
        aes(stringr::str_wrap(Values, 0), chew_roles) + xlab(NULL)+
        ggtitle("Ways to reach out to Non Enrolled")+
        scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid()}
  })
  
  #### treatment numbers
  
  
  dd_present_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Totals)
  })
  
  dd_present_sc <- reactive ({
    trt_num_all %>%
      filter((trt_num_all$ddm012_county == input$County7 & trt_num_all$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Totals)
  })
  
  
  
  output$dd_present <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      #h4("Total Present for Deoworming")
      h4(paste("Totals: ", dd_present_all()$Totals)),
      tags$p(style = "font-size: 30px;", dd_present_all()$Totals),
      icon = icon("male")
    ))}
    
    infoBox(
      h4(paste("Totals: ", dd_present_all()$Totals)),
      tags$p(style = "font-size: 30px;", dd_present_sc()$Totals),
      icon = icon("male")
    )
    
  })
  
  
  dd_male_present_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Males , perc_male )
  })
  
  dd_male_present_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Males , perc_male )
  })
  
  
  
  output$dd_male_present <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("Males : ", dd_male_present_all()$Males ),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_present_all()$perc_male[[1]]),
      icon = icon("male")
    ))}
    
    infoBox(
      h4("Males : ", dd_male_present_all()$Males ),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_present_sc()$perc_male),
      icon = icon("male")
    )
    
  })
  
  
  dd_Female_present_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Females , perc_female )
  })
  
  dd_Female_present_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Females , perc_female )
  })
  
  
  
  output$dd_Female_present <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4("Females : ", dd_Female_present_all()$Females ),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_Female_present_all()$perc_female[[1]]),
      icon = icon("female")
    ))}
    
    infoBox(
      h4("Females : ", dd_Female_present_all()$Females ),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_female_present_sc()$perc_female),
      icon = icon("female")
    )
    
  })
  
  ###Took Medicine
  
  dd_tookalb_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Total_took, perc_took)
  })
  
  dd_tookalb_sc <- reactive ({
    trt_num_all %>%
      filter((trt_num_all$ddm012_county == input$County7 & trt_num_all$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Total_took, perc_took)
  })
  
  
  
  output$dd_tookalb <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      #h4("Total tookalb for Deoworming")
      h4(paste("Total Number that took ALB: (n =", dd_tookalb_all()$Total_took, ")")),
      tags$p(style = "font-size: 30px;", dd_tookalb_all()$perc_took),
      icon = icon("male")
    ))}
    
    infoBox(
      h4(paste("Total Number that took ALB: (n =", Total_took, ")")),
      tags$p(style = "font-size: 30px;", dd_tookalb_sc()$perc_took),
      icon = icon("male")
    )
    
  })
  dd_male_tookalb_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Male_took , perc_male_took )
  })
  
  dd_male_tookalb_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Male_took , perc_male_took )
  })
  
  
  
  
  output$dd_male_tookalb <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4(paste("% of Male that took ALB : (n =", dd_male_tookalb_all()$Male_took, ")")),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_tookalb_all()$perc_male_took[[1]]),
      icon = icon("male")
    ))}
    
    infoBox(
      h4(paste("% of Male that took ALB : (n =", dd_male_tookalb_all()$Male_took, ")")),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_tookalb_sc()$perc_male_took),
      icon = icon("male")
    )
    
  })
  
  
  dd_Female_tookalb_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Female_took , perc_female_took )
  })
  
  dd_Female_tookalb_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Female_took , perc_female_took )
  })
  
  
  
  output$dd_Female_tookalb <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4(paste("% of Female that took ALB : (n =", dd_Female_tookalb_all()$Female_took, ")")),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_Female_tookalb_all()$perc_female_took[[1]]),
      icon = icon("female")
    ))}
    
    infoBox(
      h4(paste("% of Female that took ALB : (n =", dd_Female_tookalb_all()$Female_took, ")")),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_female_tookalb_sc()$perc_female_took),
      icon = icon("female")
    )
    
  })
  
  ###Teacher Observes
  
  dd_trpresent_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Total_observe, perc_observe)
  })
  
  dd_trpresent_sc <- reactive ({
    trt_num_all %>%
      filter((trt_num_all$ddm012_county == input$County7 & trt_num_all$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Total_observe, perc_observe)
  })
  
  
  
  output$dd_trpresent <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      #h4("Total trpresent for Deoworming")
      h4(paste("Total Number that : (n =",  dd_trpresent_all()$Total_observe, ")")),
      tags$p(style = "font-size: 30px;", dd_trpresent_all()$perc_observe),
      icon = icon("male")
    ))}
    
    infoBox(
      h4(paste("Total Number that : (n =",  dd_trpresent_all()$Total_observe, ")")),
      tags$p(style = "font-size: 30px;", dd_trpresent_sc()$perc_observe),
      icon = icon("male")
    )
    
  })
  dd_male_trpresent_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Male_observe , perc_male_observe )
  })
  
  dd_male_trpresent_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Male_observe , perc_male_observe )
  })
  
  
  
  
  output$dd_male_trpresent <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4(paste("% of Male that  : (n =", dd_male_trpresent_all()$Male_observe, ")")),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_trpresent_all()$perc_male_observe[[1]]),
      icon = icon("male")
    ))}
    
    infoBox(
      h4(paste("% of Male that  : (n =", dd_male_trpresent_all()$Male_observe, ")")),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_trpresent_sc()$perc_male_observe),
      icon = icon("male")
    )
    
  })
  
  
  dd_Female_trpresent_all <- reactive ({
    trt_num_all %>%
      filter( Year == input$years7 & Wave == input$Wave7 )%>%
      select(Female_observe , perc_female_observe )
  })
  
  dd_Female_trpresent_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 ))%>%
      select(Female_observe , perc_female_observe )
  })
  
  
  
  output$dd_Female_trpresent <- renderInfoBox({
    if (input$County7 == 'Kenya')  
    {return (infoBox(
      h4(paste("% of Female that  : (n =", dd_Female_trpresent_all()$Female_observe, ")")),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_Female_trpresent_all()$perc_female_observe[[1]]),
      icon = icon("female")
    ))}
    
    infoBox(
      h4(paste("% of Female that  : (n =", dd_Female_trpresent_all()$Female_observe, ")")),
        tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_female_trpresent_sc()$perc_female_observe),
      icon = icon("female")
    )
    
  })
  
  
  ####SUFFICIENCY
  ###DRUGS
  sufficient_drugs_all <-  reactive({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm109_dd_plan_nonenroll))%>%
      group_by(Year, Wave,ddm702_drugs_enough_fordd)%>%
      dplyr:: summarise(trt = round(n()/nrow(dd_main_rows())*100))%>%
      ungroup()%>%
      filter(ddm702_drugs_enough_fordd =="Yes")%>%
      select(trt)
  })
  
  sufficient_drugs_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm109_dd_plan_nonenroll)))%>%
      group_by(Year,Wave, ddm702_drugs_enough_fordd)%>%
      dplyr::  summarise(trt = round(n()/nrow(dd_main_county_rows())*100))%>%
      ungroup()%>%
      filter(ddm702_drugs_enough_fordd=="Yes")%>%
      select(trt)
  })
  
  output$sufficient_drugs <- renderPlotly({
    if (input$County7 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = sufficient_drugs_all()[[1]],
        title = list(text = "Drugs for DD"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sufficient_drugs_sc()[[1]],
      title = list(text = "Drugs for DD"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  
  ###forms
  sufficient_forms_all <-  reactive({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm109_dd_plan_nonenroll))%>%
      group_by(Year, Wave,ddm707_monitoring_form_enough)%>%
      dplyr:: summarise(trt = round(n()/nrow(dd_main_rows())*100))%>%
      ungroup()%>%
      filter(ddm707_monitoring_form_enough =="Yes")%>%
      select(trt)
  })
  
  sufficient_forms_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm109_dd_plan_nonenroll)))%>%
      group_by(Year,Wave, ddm707_monitoring_form_enough)%>%
      dplyr::  summarise(trt = round(n()/nrow(dd_main_county_rows())*100))%>%
      ungroup()%>%
      filter(ddm707_monitoring_form_enough=="Yes")%>%
      select(trt)
  })
  
  output$sufficient_forms <- renderPlotly({
    if (input$County7 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = sufficient_forms_all()[[1]],
        title = list(text = "Monitoring Forms for DD"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sufficient_forms_sc()[[1]],
      title = list(text = "Monitoring forms for DD"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  
  ###mop_up_drugs
  sufficient_mop_up_drugs_all <-  reactive({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm109_dd_plan_nonenroll))%>%
      group_by(Year, Wave,ddm706b_suff_med_mopup)%>%
      dplyr:: summarise(trt = round(n()/nrow(dd_main_rows())*100))%>%
      ungroup()%>%
      filter(ddm706b_suff_med_mopup =="Yes")%>%
      select(trt)
  })
  
  sufficient_mop_up_drugs_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm109_dd_plan_nonenroll)))%>%
      group_by(Year,Wave, ddm706b_suff_med_mopup)%>%
      dplyr::  summarise(trt = round(n()/nrow(dd_main_county_rows())*100))%>%
      ungroup()%>%
      filter(ddm706b_suff_med_mopup=="Yes")%>%
      select(trt)
  })
  
  output$sufficient_mop_up_drugs <- renderPlotly({
    if (input$County7 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = sufficient_mop_up_drugs_all()[[1]],
        title = list(text = "Drugs For Mop Up"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sufficient_mop_up_drugs_sc()[[1]],
      title = list(text = "Drugs For Mop Up"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  
  ###mop_up_forms
  sufficient_mop_up_forms_all <-  reactive({
    DD_Main %>%
      filter( Year == input$years7 & Wave == input$Wave7 & !is.na(ddm109_dd_plan_nonenroll))%>%
      group_by(Year, Wave,ddm706d_suff_tdata_form_mopup)%>%
      dplyr:: summarise(trt = round(n()/nrow(dd_main_rows())*100))%>%
      ungroup()%>%
      filter(ddm706d_suff_tdata_form_mopup =="Yes")%>%
      select(trt)
  })
  
  sufficient_mop_up_forms_sc <- reactive ({
    DD_Main %>%
      filter((DD_Main$ddm012_county == input$County7 & DD_Main$Year == input$years7 & Wave == input$Wave7 & !is.na(ddm109_dd_plan_nonenroll)))%>%
      group_by(Year,Wave, ddm706d_suff_tdata_form_mopup)%>%
      dplyr::  summarise(trt = round(n()/nrow(dd_main_county_rows())*100))%>%
      ungroup()%>%
      filter(ddm706d_suff_tdata_form_mopup=="Yes")%>%
      select(trt)
  })
  
  output$sufficient_mop_up_forms <- renderPlotly({
    if (input$County7 == 'Kenya')
    {return(
      fig <- plot_ly(
        domain = list(x = c(0, 100), y = c(0, 100)),
        value = sufficient_mop_up_forms_all()[[1]],
        title = list(text = "Monitoring For Mop Up"),
        type = "indicator",
        mode = "gauge+number")%>%
        layout(margin = list(l=20,r=30))
      
    )}
    
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = sufficient_mop_up_forms_sc()[[1]],
      title = list(text = "Monitoring  For Mop Up"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
#####NIGERIA DATA
  output$SCTPost_title2 <- renderText({
    paste(input$years8, " Round ",input$Round, "LGAT POST DASHBOARDS")
  })
  
  output$SCTOBS_title2 <- renderText({
    paste(input$years9, " Round ",input$Round2, "LGA OBS DASHBOARDS")
  })
  
  output$SCTCHEW_title2 <- renderText({
    paste(input$years10, " Round ",input$Round3, "LGAT FLHF DASHBOARDS")
  }) 
  
  
  
  ng_lga_rows <- reactive({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round )
    
  })
  
  
  ng_sth_rows <- reactive({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round & !is.na(STH))
    
  })
  
  ng_sch_rows <- reactive({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round & !is.na(SCH))
    
  })
  ####TREATMENTTYPES
  
  ng_trt_all2 <- reactive ({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round & !is.na(Treatment))%>%
      group_by(State, Year, Round,Treatment)%>%
      dplyr:: summarise(trt = round( sum(!is.na(Treatment))/(nrow(ng_lga_rows()))*100))%>%
      ungroup()%>%
      mutate(Labels = paste(Treatment, "(",trt,"%)"))%>%
      select(Labels, trt)%>%
      tibble::deframe()
  })
  
  output$trt_type2 <- renderPlot({
    waffle(ng_trt_all2(),rows = 10,
           colors = c('#FE6F87',"#60BC68", "grey"),
           title = "Treatment Types",
           legend_pos = "right")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  
  #### STH DRUGS
  
  ng_sthdrugs_all <- reactive ({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round & !is.na(STH))%>%
      group_by(State, Year, Round,`STH Drug`)%>%
      dplyr:: summarise(drug = (n()/sum(!is.na(ng_sth_rows()$Year)))*100)%>%
      ungroup()%>%
      filter(`STH Drug` == "Albendazole")%>%
      mutate(Drugs = (paste(drug,"%")))%>%
      select(Drugs)
    
  })
  
  output$sth_drugs2 <- renderInfoBox({
    (infoBox(
      h4("Correct STH Drug"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",  ng_sthdrugs_all()$Drugs),
      icon = icon("prescription-bottle-alt"),
      color = "aqua"
    ))})
  
  
  ####STH DOSAGE
  
  ng_sthdosage_all <- reactive ({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round  & !is.na(STH))%>%
      group_by(State, Year, Round,`STH Dosage`)%>%
      dplyr:: summarise(dosage = (n()/sum(!is.na(ng_sth_rows()$Year)))*100)%>%
      ungroup()%>%
      filter(`STH Dosage` == "One Tablet Per Child")%>%
      mutate(Drugs = paste( dosage,"%"))%>%
      select(Drugs)
    
  })
  
  output$sth_dosage2 <- renderInfoBox({
    infoBox(
      h4("Correct STH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",  ng_sthdosage_all()$Drugs),
      icon = icon("capsules")
    )})
  
  
  
  #STH AGE GROUP
  
  
  ng_sthage_all2 <- reactive ({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round  & !is.na(STH))%>%
      group_by(State, Year, Round,`STH Age`)%>%
      dplyr:: summarise(age = (n()/sum(!is.na(ng_sth_rows()$Year)))*100)%>%
      ungroup()%>%
      dplyr:: filter(`STH Age` == "2-14 years")%>%
      dplyr::mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
  })
  
  output$sth_age2 <- renderInfoBox({
    infoBox(
      h4("Correct STH Age Group"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",  ng_sthage_all2()$Drugs),
      icon = icon("calendar-week")
    )})
  
  
  ###SICK CHILD RECEIVE DRUGS 
  ng_sick_all <- reactive ({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round  & !is.na(`Sick children`)   )%>%
      group_by(State, Year, Round,`Sick children`)%>%
      dplyr:: summarise(sick = round(sum(!is.na(`Sick children`))/(nrow(ng_lga_rows()))*100))%>%
      mutate(Labels = paste(`Sick children`, "(",sick,"%)"))%>%
      ungroup()%>%
      select(Labels, sick)%>%
      tibble::deframe()
  })
  
  
  output$sick2 <- renderPlot({
    (waffle(ng_sick_all(),rows= 10,
            colors = c('#FE6F87',"grey", "grey"),
            title = "Should Sick Children deworm",
            legend_pos = "bottom")+
       theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
             legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    )})
  
  #### SCH DRUGS
  
  ng_schdrugs_all <- reactive ({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round & !is.na(SCH))%>%
      group_by(State, Year, Round,`SCH Drug`)%>%
      dplyr:: summarise(drug = (n()/sum(!is.na(ng_sch_rows()$Year)))*100)%>%
      ungroup()%>%
      dplyr::  filter(`SCH Drug` == "Praziquantel")%>%
      mutate(Drugs = paste(`SCH Drug`," ", drug,"%"))%>%
      select(Drugs)
  })
  
  
  output$sch_drugs2 <- renderInfoBox({
    validate(
      need(ng_schdrugs_all()$drug, 'No SCHISTO Data at the Moment'))
    infoBox(
      h4("Correct SCH Drug"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", ng_sthdrugs_all()$Drugs),
      icon = icon("prescription-bottle-alt")
    )})
  
  
  ####SCH DOSAGE
  
  ng_schdosage_all <- reactive ({
    Nigeria_SCT_Post %>%
      filter( Year == input$years8 & Round == input$Round    & !is.na(SCH) )%>%
      group_by(Year, Round,`SCH Dosage`)%>%
      dplyr:: summarise(dosage = (n()/sum(!is.na(ng_sch_rows()$Year)))*100)%>%
      ungroup()%>%
      dplyr::  filter(`SCH Dosage` == "According to tablet pole")%>%
      mutate(Drugs = paste(`SCH Dosage`," ", dosage,"%"))%>%
      select(Drugs)
    
    
  })
  
  output$sch_dosage2 <- renderInfoBox({
    validate(
      need(ng_schdrugs_all()$drug, 'No SCHISTO Data at the Moment')) 
    infoBox(
      h4("Correct SCH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", ng_schdosage_all()$Drugs),
      icon = icon("capsules")
    )})
  
  
  
  #SCH AGE GROUP
  ng_schage_all <- reactive ({
    validate(
      need(ng_schdrugs_all()$drug, 'No SCHISTO Data at the Moment'))
    
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round & !is.na(SCH) )%>%
      group_by(Year, Round,`SCH Age`)%>%
      dplyr:: summarise(age =(n()/sum(!is.na(ng_sch_rows()$Year)))*100)%>%
      ungroup()%>%
      dplyr:: filter(`SCH Age` == "6-14 years")%>%
      mutate(Drugs = paste(`SCH Age`," ", age,"%"))%>%
      select(Drugs)
    
    
  })
  output$sch_age2 <- renderInfoBox({
    infoBox(
      h4("Correct SCH Age Group?"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", ng_schage_all()$Drugs),
      icon = icon("calendar-week")
    )})
  
  ####MATERIALS
  
  ng_formp_all <- reactive ({
    ng_post_materials %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round )%>%
      group_by(State, Year, Round,Variables, Values)%>%
      dplyr:: summarise(formP = round(sum(!is.na(Values))/sum(!is.na(ng_lga_rows()$Year))*100))%>%
      ungroup()
  })
  
  
  output$FormP2 <- renderPlot({
    
    ggplot(data = ng_formp_all(), aes(x = Values, y = formP, fill=Year))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      aes(stringr::str_wrap(Values, 30), formP) + xlab(NULL)+
      ggtitle("Materials Received")+
      scale_fill_manual(values = c("#60BC68"), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round(formP),"%",sep=""), hjust = "right", size = 4),
                colour = "white",
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            # axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()
    
  })
  
  ####TRAINING DATA
  
  ng_noforms_all <- reactive ({
    Nigeria_SCT_Post %>%
      filter(State == input$State & Year == input$years8 & Round == input$Round & !is.na(Treatment))%>%
      group_by(State, Year, Round,`Number of forms`)%>%
      dplyr:: summarise(no_forms = round(n()/(sum(!is.na(ng_lga_rows()$Year)))*100))%>%
      mutate(Labels = paste(`Number of forms`, "(",no_forms,"%)"))%>%
      ungroup()%>%
      select(Labels, no_forms)%>%
      tibble::deframe()
  })
  
  
  output$No_Forms2 <- renderPlot({
    waffle(ng_noforms_all(),rows= 10,
           colors = c("#60BC68",'#FE6F87', "grey"),
           title = "Number of forms to fill",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  ####SCT OBS
  ng_sc_obs <- reactive ({
    Nigeria_LGAT_OBS %>%
      # mutate(County = scto012_county)%>%
      group_by(State)%>%
      filter(Round == input$Round2 & Year == input$years9)%>%
      dplyr:: summarise(Number_of_LGAs = n())
  })
  
  output$Subcs2 <- DT ::  renderDataTable({
    ng_sc_obs()
  })
  ####ROWS
  ng_rows <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)
  }) 
  
  
  
  #####COVID
  ng_trainers_mask_all <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year, Round, lgato811_train_wear)%>%
      dplyr:: summarise(  trainers_mask = round(n()/sum(!is.na(ng_rows()$Year))*100))
  })
  
  
  
  output$trainers_masks2 <- renderPlot ({
    validate(
      need(ng_trainers_mask_all()$lgato811_train_wear, 'No  Data at the Moment'))
    
    ggplot(data = ng_trainers_mask_all(), 
           aes(x = 2, y = trainers_mask, fill= lgato811_train_wear ))+
      geom_bar(stat = "identity")+
      ggtitle("Were all the trainers wearing protective face \n masks during the training?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(trainers_mask,"%", sep = ""),hjust = "centre"), col = "black",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("red", "#60BC68"))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  
  
  ng_participants_mask_all <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year, Round, lgato812_wear_protctv)%>%
      dplyr:: summarise(  participants_mask = round(n()/sum(!is.na(ng_rows()$Year))*100))
  })
  
  
  
  output$participants_masks2 <- renderPlot ({
    validate(
      need(ng_trainers_mask_all()$lgato811_train_wear, 'No  Data at the Moment'))
    
    (ggplot(data = ng_participants_mask_all(), 
            aes(x = 2, y = participants_mask, fill= lgato812_wear_protctv ))+
        geom_bar(stat = "identity")+
        ggtitle("Were all the participants wearing protective face \n masks during the training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(participants_mask,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c("red", "#60BC68"))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  ####HAND SANITIZER AVAILABEL
  ng_sanitizer_all <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year, Round, lgato814_hndwashn_santzr)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(ng_rows()$Year))*100))
  })
  
  
  
  output$sanitizer2 <- renderPlot ({
    validate(
      need(ng_trainers_mask_all()$lgato811_train_wear, 'No  Data at the Moment'))
    
    (ggplot(data = ng_sanitizer_all(), 
            aes(x = 2, y = sanitizer, fill= lgato814_hndwashn_santzr ))+
        geom_bar(stat = "identity")+
        ggtitle("Was hand sanitizer or a hand washing facility \n available at the training?")+
        coord_polar("y", start = 200)+
        geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                  position = position_stack(vjust = 0.5), 
                  size = 5,
                  show.legend = T,
                  fontface="bold") +
        theme_void() +
        scale_fill_manual(values=c( "red", "#60BC68"))+
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(), 
              axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "bottom",
              legend.title= element_blank(),
              panel.border = element_blank()) +
        removeGrid() +
        xlim(.5, 2.5))
  })
  
  
  ####SANITIZE VANUE
  ng_sanitize_venue_all <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year, Round, lgato815_venue_clean)%>%
      dplyr:: summarise(  sanitize_venue = round(n()/sum(!is.na(ng_rows()$Year))*100))
  })
  
  
  
  output$sanitize_venue2 <- renderPlot ({
    validate(
      need(ng_trainers_mask_all()$lgato811_train_wear, 'No  Data at the Moment'))
    
    ggplot(data = ng_sanitize_venue_all(), 
           aes(x = 2, y = sanitize_venue, fill= lgato815_venue_clean ))+
      geom_bar(stat = "identity")+
      ggtitle("Was the training venue cleaned/sanitized \n before this training?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(sanitize_venue,"%", sep = ""),hjust = "centre"), col = "black",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("red", "#60BC68"))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  
  ng_parts_mask_all <- reactive({
    Nigeria_LGAT_OBS%>%
      group_by(lgato813_protctv_hmany)%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2 &!is.na(lgato813_protctv_hmany))%>%
      dplyr:: summarise( parts_mask = n()/sum(!is.na(ng_rows()$lgato813_protctv_hmany))*100)
  })
  
  
  output$num_parts_mask2 <- renderPlot({
    validate(
      need(ng_trainers_mask_all()$lgato811_train_wear, 'No  Data at the Moment'))
    
    ( ggplot(data = ng_parts_mask_all(), aes(x = lgato813_protctv_hmany, y =  parts_mask, fill=lgato813_protctv_hmany))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("How many participants were wearing protective \n face masks during the training?")+
        scale_fill_manual(values = c("#60BC68", '#5E5F5F', '#F05854'), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round( parts_mask),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid())
  })
  
  ###ATTENDANCE SHEET 
  ng_att_sheet_all <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year, Round, attendance_sheet)%>%
      dplyr:: summarise(Attendance = n()/sum(!is.na(ng_rows()$Year))*100)
  })
  
  
  
  output$attendance2 <- renderPlot({
    ( ggplot(data = ng_att_sheet_all(), aes(x = attendance_sheet, y = ng_att_sheet_all()$Attendance, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("Is there an attendance sheet?")+
        scale_fill_manual(values = c("#60BC68"), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(ng_att_sheet_all()$Attendance),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid())
  })
  
  #####BOOKLETS GIVEN
  
  
  ng_booklets_given_all <- reactive({
    Nigeria_LGAT_OBS%>%
      group_by(lgato107_lga_train_items_gvn)%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      dplyr:: summarise(Given = n()/sum(!is.na(ng_rows()$Year))*100)
  })
  
  output$booklets2 <- renderPlot({
    ( ggplot(data = ng_booklets_given_all(), aes(x = lgato107_lga_train_items_gvn, y = Given, fill=lgato107_lga_train_items_gvn))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        ggtitle("Was the Sub county Training Booklet distributed to \n all the participants?")+
        scale_fill_manual(values = c("#60BC68", '#5E5F5F', "#60BC68"), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(Given),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid())
  })
  ####ATTENDANCE RATES
  
  ng_attrate_1 <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      dplyr:: summarise(Day1 = round(mean(as.numeric(lgato702_total_partcipants_day1))))
  })
  
  
  ng_attrate_2 <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      dplyr:: summarise(Day2 = round(mean(as.numeric(lgato1202_ttl_partcipants_day2))))
  })
  
  output$box1102 <- renderInfoBox({
    infoBox(
      tags$p(style = "font-size: 40px; color: red; fonface: bold", ng_attrate_1()$Day1),
      h4("Average # of participants  on day 1"))
    
    #icon = icon("prescription-bottle-alt")
    
    
  })
  
  
  output$box212 <- renderInfoBox({
    infoBox(
      tags$p(style = "font-size: 40px; color: red; fonface: bold", ng_attrate_2()$Day2),
      h4("Average # of participants on day 2")
      
      #icon = icon("prescription-bottle-alt")
      
    )
  })
  
  
  
  ###attendance rate at start and end
  
  ng_attrate_start <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      dplyr:: summarise(Day1 =paste(round(mean(attendance_rate_start)*100),"%", sep=""))
  })
  
  
  
  ng_attrate_end <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      dplyr:: summarise(Day2 = paste(round(mean(attendance_rate_1)*100),"%", sep=""))
  })
  
  output$box312 <- renderInfoBox({
    infoBox(
      
      tags$p(style = "font-size: 40px; color: red; fonface: bold", ng_attrate_start()$Day1),
      h4("Attendance Rate on day 1 at the start"))
    #icon = icon("prescription-bottle-alt")
    
  })
  
  output$box412 <- renderInfoBox({
    infoBox(
      tags$p(style = "font-size: 40px; color: red; fonface: bold",  ng_attrate_end()$Day2),
      h4("Attendance Rate on day 1 at the end"))
    #icon = icon("prescription-bottle-alt")
    
  })
  
  
  
  ####TOPIC COVERAGE
  ng_tpoiccoverage_all <- reactive({
    ng_topics2%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Variables,Values) %>%
      dplyr:: summarise(coverage = round(n()/nrow(ng_rows())*100))
  })
  
  
  
  output$lga_coverage2 <- renderPlot ({
    (ggplot(data = ng_tpoiccoverage_all(), aes(stringr :: str_wrap(Variables), y = coverage, fill= Values))+
       geom_bar(stat="identity") +
       ggtitle("Topic Coverage")+
       scale_fill_manual(values =  c("#60BC68", '#5E5F5F','#F05854','dark grey'))+
       aes(stringr::str_wrap(Variables, 25), coverage) + xlab(NULL)+
       geom_text(aes(label = paste(ng_tpoiccoverage_all()$coverage,"%",sep=""), hjust = "right"),
                 colour = "white",
                 #position = position_dodge(width = -.3),
                 position = position_stack(vjust = 0.5),
                 size = 5,
                 fontface="bold",
                 show.legend = FALSE) +
       coord_flip()+
       theme_bw()+
       theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
       theme(axis.text.y = element_text(color="BlACK",size=14)) +
       theme(axis.title.x = element_blank(), 
             axis.text.x = element_blank(),
             axis.ticks.x =  element_blank(),
             axis.ticks.y = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = unit(c(1,1,1,0), "mm"),
             panel.border = element_blank(), 
             legend.position="bottom") +
       removeGrid())
  })
  
  #####NOT TREATED
  ng_not_trt_all <- reactive({
    ng_not_trt%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(ng_not_trt = round(n()/nrow(ng_rows())*100))
  })
  
  
  
  output$not_treated2 <- renderPlot({
    ( ggplot(data = ng_not_trt_all(), aes(stringr :: str_wrap( Values), y = ng_not_trt, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(Values, 30), ng_not_trt) + xlab(NULL)+
        ggtitle("Which children should not be treated under the program?")+
        scale_fill_manual(values = c("#60BC68"), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(ng_not_trt),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid())
  })
  
  #####FLHF ROLES
  ng_chew_roles_all <- reactive({
    ng_chew_roles%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(ng_chew_roles = round(n()/nrow(ng_rows())*100))
  })
  
  
  output$Roles_chew2 <- renderPlot({
    ( ggplot(data = ng_chew_roles_all(), aes(x = Values, y = ng_chew_roles, fill=Year))+
        geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
        aes(stringr::str_wrap(Values, 30), ng_chew_roles) + xlab(NULL)+
        ggtitle("Which children should not be treated under the program?")+
        scale_fill_manual(values = c("#60BC68"), aesthetics = "fill") +
        #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
        geom_text(aes(label = paste(round(ng_chew_roles),"%",sep=""),  hjust = "right", size = 4),
                  colour = "white",
                  position = position_dodge(width = .0),
                  size = 5,
                  show.legend = FALSE,
                  fontface="bold") +
        theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
        theme(axis.text.y = element_text(color="BlACK",size=14)) +
        theme(axis.title.x = element_blank(), 
              axis.ticks.x =  element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              #axis.text.y = element_blank(), 
              # axis.ticks.y = element_blank(),
              plot.margin = unit(c(1,1,1,0), "mm"),
              legend.position = "none",
              panel.border = element_blank()) +
        coord_flip()+
        removeGrid())
  })
  
  #####HT ROLES
  ng_HT_roles_all <- reactive({
    ng_HT_roles%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(ng_HT_roles = round(n()/nrow(ng_rows())*100))
  })
  
  
  
  output$Roles_HT2 <- renderPlot({
    ggplot(data = ng_HT_roles_all(), aes(x = Values, y = ng_HT_roles, fill=Year))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      aes(stringr::str_wrap(Values, 30), ng_HT_roles) + xlab(NULL)+
      ggtitle("What is the responsibility of the Head Teacher/ Health Teacher? ")+
      scale_fill_manual(values = c("#60BC68"), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round(ng_HT_roles),"%",sep=""),  hjust = "right", size = 4),
                colour = "white",
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            # axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()
  })
  
  #####DRUG STATUS
  ng_drug_availability_all <- reactive({
    Nigeria_LGAT_OBS%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year,lgato402_drug_availability) %>%
      dplyr:: summarise(drug_availability = round(n()/nrow(ng_rows())*100))
  })
  
  
  
  output$Drug_Availability2 <- renderPlot({
    ggplot(data = ng_drug_availability_all(), aes(x = lgato402_drug_availability, y = drug_availability, fill=Year))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      aes(stringr::str_wrap(lgato402_drug_availability, 30), drug_availability) + xlab(NULL)+
      ggtitle("What was the deworming medicine availability status discussed \n by the SCMOH? ")+
      scale_fill_manual(values = c("#60BC68"), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round(drug_availability),"%",sep=""),  hjust = "right", size = 4),
                colour = "white",
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            # axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()
  })
  
  
  
  #####Questions_asked
  ng_Questions_asked_all <- reactive({
    ng_Qst%>%
      filter(State == input$State2 & Year == input$years9 & Round == input$Round2)%>%
      group_by(Year,Values) %>%
      dplyr:: summarise(Questions_asked = round(n()/nrow(ng_rows())*100))
  })
  
  
  
  output$Questions_Asked2 <- renderPlot({
    ggplot(data = ng_Questions_asked_all(), aes(x = Values, y = Questions_asked, fill=Year))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      aes(stringr::str_wrap(Values, 30), Questions_asked) + xlab(NULL)+
      ggtitle("Areas that participants asked the most questions")+
      scale_fill_manual(values = c("#60BC68"), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round(Questions_asked),"%",sep=""), hjust = "right", size = 4),
                colour = "white",
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            # axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()
  })
  
  ####CHEW POST
  
  ng_chew_rows <- reactive({
    Nigeria_FLHF_POST %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3)
    
  })
  ####TREATMENTTYPES
  
  ng_chew_trt_all <- reactive ({
    x<- Nigeria_FLHF_POST %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 & !is.na(lgatp011_treat_type))%>%
      group_by(State, Year, Round,lgatp011_treat_type)%>%
      dplyr:: summarise(trt = (n()/nrow(ng_chew_rows()))*100)%>%
      ungroup()%>%
      mutate(Labels = paste(lgatp011_treat_type, "(",trt,"%)"))%>%
      select(Labels, trt)%>%
      tibble::deframe()
  })
  
  output$chew_trt_type2 <- renderPlot({
    waffle(ng_chew_trt_all(),rows= 10,
           colors = c('#FE6F87',"#60BC68", "grey"),
           title = "Treatment Types",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  
  #### STH DRUGS
  
  ng_chew_sthdrugs_all <- reactive ({
    Nigeria_FLHF_POST %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 &  !is.na(lgatp011_treat_type) & !is.na(lgatp204_sth_drugs))%>%
      group_by(Year, Round,`STH Drug`)%>%
      dplyr:: summarise(drug = (n()/sum(!is.na(ng_chew_rows()$lgatp204_sth_drugs))*100))%>%
      ungroup()%>%
      filter(`STH Drug` == "Albendazole")%>%
      mutate(Drugs = HTML(paste(drug,"%")))%>%
      select(Drugs)
    
    
  })
  
  output$chew_sth_drugs2 <- renderInfoBox({
    infoBox( h4("Correct STH Drug"),
             tags$p(style = "font-size: 30px; font-weight: bold; color: red", ng_chew_sthdrugs_all()$Drugs),
             icon = icon("prescription-bottle-alt"))
    
  })
  
  
  
  ####STH DOSAGE
  
  ng_chew_sthdosage_all <- reactive ({
    Nigeria_FLHF_POST %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 & !is.na(lgatp011_treat_type))%>%
      group_by(State, Year, Round,`STH Dosage`)%>%
      dplyr:: summarise(dosage = (n()/sum(!is.na(ng_chew_rows()$lgatp204_sth_drugs))*100))%>%
      filter(`STH Dosage` == "One Tablet Per Child")%>%
      mutate(Drugs = paste(dosage,"%"))%>%
      select(Drugs)
  })
  
  
  output$chew_sth_dosage2 <- renderInfoBox({
    infoBox(
      h4("Correct STH Dosage"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", ng_chew_sthdosage_all()$Drugs),
      icon = icon("capsules"))
    
  })
  
  
  #STH AGE GROUP
  ng_chew_sthage_all <- reactive ({
    Nigeria_FLHF_POST %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 )%>%
      group_by(Year, Round,`STH Age`)%>%
      dplyr:: summarise(age = (n()/sum(!is.na(ng_chew_rows()$lgatp204_sth_drugs))*100))%>%
      ungroup()%>%
      dplyr:: filter(`STH Age` == "2-14 Years")%>%
      dplyr::mutate(Drugs = paste(age,"%"))%>%
      select(Drugs)
  })
  output$chew_sth_age2 <- renderInfoBox({
    infoBox(
      h4("Correct STH Age Group"),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",  ng_chew_sthage_all()$Drugs),
      icon = icon("calendar-week"))
  })
  
  ###STH SIDE EFFECTS
  ng_sth_side_effect_all <- reactive ({
    ng_sth_side_effect %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3  )%>%
      group_by(Year, Round,Values)%>%
      dplyr:: summarise(Side_Effects = (n()/sum(!is.na(ng_chew_rows()$lgatp204_sth_drugs))*100))
    
  })
  
  
  output$sth_side_effects2 <- renderPlot({
    ggplot(data = ng_sth_side_effect_all(), aes(x = Values, y = Side_Effects, fill= Year))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      ggtitle("What are the likely expected side effects of STH treatment?")+
      scale_fill_manual(values = c("#60BC68",'#F05854'), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round(Side_Effects),"%",sep=""),  hjust = "right", size = 4),
                #colour = fct_rev(Grade)),
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            # axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()
  }) 
  
  ###sch SIDE EFFECTS
  ng_sch_side_effect_all <- reactive ({
    ng_sch_side_effect %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 )%>%
      group_by(Year, Round,Values)%>%
      dplyr:: summarise(Side_Effects = (n()/sum(is.na(ng_chew_rows()$lgatp204_sth_drugs))*100))
    
  })
  
  
  output$sch_side_effects2 <- renderPlot({
    validate(
      need(ng_sch_side_effect_all()$Side_Effects, 'No SCHISTO Data at the Moment'))
    ggplot(data = ng_sch_side_effect_all(), aes(x = Values, y = Side_Effects, fill= Year))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      ggtitle("What are the likely expected side effects of sch treatment?")+
      scale_fill_manual(values = c("#60BC68",'#F05854'), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round(Side_Effects),"%",sep=""),  hjust = "right", size = 4),
                #colour = fct_rev(Grade)),
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            # axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()
  }) 
  
  #### SCH DRUGS
  
  ng_chew_schdrugs_all <- reactive ({
    Nigeria_FLHF_POST %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 & !is.na(`grp_knowledge-grp_schisto-lgatp208_sch_drugs`))%>%
      group_by(Year, Round,`SCH Drug`)%>%
      dplyr:: summarise(drug = (n()/sum(!is.na(ng_chew_rows()$lgatp204_sth_drugs))*100))%>%
      ungroup()%>%
      dplyr::  filter(`SCH Drug` == "Praziquantel")%>%
      mutate(Drugs = paste(`SCH Drug`," ", drug,"%"))%>%
      select(Drugs)
  })
  
  
  output$chew_sch_drugs2 <- renderInfoBox({
    validate(
      need(ng_sch_side_effect_all()$Side_Effects, 'No SCHISTO Data at the Moment'))
    infoBox(
      h4("What drug will be used to treat STH?"),
      tags$p(style = "font-size: 30px;", ng_chew_schdrugs_all()$Drugs),
      icon = icon("prescription-bottle-alt"))
  })
  ####SCH DOSAGE
  
  ng_chew_schdosage_all <- reactive ({
    Nigeria_FLHF_POST %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 & !is.na(`SCH Age`))%>%
      group_by(Year, Round,`SCH Dosage`)%>%
      dplyr:: summarise(dosage = (n()/sum(is.na(ng_chew_rows()$lgatp204_sth_drugs))*100))%>%
      ungroup()%>%
      dplyr::  filter(`SCH Dosage` == "According to tablet pole")%>%
      mutate(Drugs = paste(`SCH Dosage`," ", dosage,"%"))%>%
      select(Drugs)
    
    
  })
  
  output$chew_sch_dosage2 <- renderInfoBox({
    validate(
      need(ng_sch_side_effect_all()$Side_Effects, 'No SCHISTO Data at the Moment')) 
    infoBox(
      h4("What will be the STH dosage?"),
      tags$p(style = "font-size: 30px;", ng_chew_schdosage_all()$Drugs),
      icon = icon("capsules")
    )
  })
  
  
  #SCH AGE GROUP
  ng_chew_schage_all <- reactive ({
    Nigeria_FLHF_POST %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 & !is.na(`SCH Age`))%>%
      group_by(Year, Round,`SCH Age`)%>%
      dplyr:: summarise(age =(n()/sum(is.na(ng_chew_rows()$lgatp204_sth_drugs))*100))%>%
      ungroup()%>%
      dplyr:: filter(`SCH Age` == "6-14 years")%>%
      mutate(Drugs = paste(`SCH Age`," ", age,"%"))%>%
      select(Drugs)
    
    
    
  })
  
  
  output$chew_sch_age2 <- renderInfoBox({
    validate(
      need(ng_sch_side_effect_all()$Side_Effects, 'No SCHISTO Data at the Moment')) 
    infoBox(
      h4("What is the age-group for STH treatment?"),
      tags$p(style = "font-size: 30px;", ng_chew_schage_all()$Drugs),
      icon = icon("calendar-week")
    )
  })
  
  ###sch SIDE EFFECTS
  ng_role_chew_all <- reactive ({
    ng_role_chew %>%
      filter(State == input$State3 & Year == input$years102 & Round == input$Round3 )%>%
      group_by(Year, Round,Values)%>%
      dplyr:: summarise(Role_Chew = (n()/sum(!is.na(ng_chew_rows()$Year))*100))
    
  })
  
  
  output$Role_Chew2 <- renderPlot({
    ggplot(data = ng_role_chew_all(), aes(x = Values, y = Role_Chew, fill= Year))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      ggtitle("CHEW responsibilities during NSBDP")+
      scale_fill_manual(values = c("#60BC68",'#F05854'), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round(Role_Chew),"%",sep=""),  hjust = "right", size = 4),
                #colour = fct_rev(Grade)),
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",size=18, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            # axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()  }) 
  
  
  
  #########TT DASHBOARDS  
  
  #############Number of wards  
  
  output$ng_title <-  renderText({
    paste0(input$State," TT Dashboard -", input$years2, " Round ", input$Round )
  })
  
  output$ng_title2 <-  renderText({
    paste0("Observations made at the teacher training for Round ", input$Round," in ", input$State , " State")
  })
  
  
  output$ng_title3 <- renderText({
    paste0("Teachers who were knowledgeable as regards the correct worms")
  })
  
  output$ng_title4 <- renderText({
    paste0("Teachers who were knowledgeable as regards the correct drugs")
  })
  
  output$ng_title5 <- renderText({
    paste0("Teachers who were knowledgeable as regards the correct age_group")
  })
  
  output$ng_title6 <- renderText({
    paste0("Teachers who were knowledgeable as regards the correct dosage")
  })
  
  ng_wards_visited <- reactive({
    (ng_no_wards %>%
       filter(State == input$State & Year == input$years2 & Round == input$Round)%>%
       select(LGA_Name,`No of Wards` ) )
  })
  
  output$ng_box1 <- renderTable({
    ng_wards_visited()
    
  })
  
  ng_rows2 <- reactive({
    ng_tp %>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4)
  })
  
  ng_rows_2 <- reactive({
    Nigeria_TT_Obs %>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4)
  })
  
  
  
  #####COVID
  ng_tt_trainers_mask_all <- reactive({
    Nigeria_TT_Obs%>%
      filter((State == input$State4 & Year == input$years11 & Round == input$Round4))%>%
      group_by(State,Round, Year, tto1114_trainer_wear_mask)%>%
      dplyr:: summarise(  trainers_mask = round(n()/sum(!is.na(ng_rows_2()$tto1114_trainer_wear_mask))*100))%>%
      filter(!is.na(trainers_mask))
  })
  
  
  
  output$ng_tt_trainers_masks <- renderPlot ({
    
    ggplot(data = ng_tt_trainers_mask_all(), 
           aes(x = 2, y = trainers_mask, fill= tto1114_trainer_wear_mask ))+
      geom_bar(stat = "identity")+
      ggtitle("Were all the trainers wearing protective face \n masks during the training?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(trainers_mask,"%", sep = ""),hjust = "centre"), col = "black",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c('dark grey','#FE6F87'))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  
  
  ng_tt_participants_mask_all <- reactive({
    Nigeria_TT_Obs%>%
      filter((State == input$State4 & Year == input$years11 & Round == input$Round4))%>%
      group_by(State,Round, Year, tto1115_prtcipants_wear_mask)%>%
      dplyr:: summarise(  participants_mask = round(n()/sum(!is.na(ng_rows_2()$tto1115_prtcipants_wear_mask))*100))%>%
      filter(!is.na(participants_masks))
  })
  
  
  output$ng_tt_participants_masks <- renderPlot ({
    ggplot(data = ng_tt_participants_mask_all(), 
           aes(x = 2, y = participants_mask, fill= tto1115_prtcipants_wear_mask ))+
      geom_bar(stat = "identity")+
      ggtitle("Were all the participants wearing protective face \n masks during the training?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(participants_mask,"%", sep = ""),hjust = "centre"), col = "black",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c('dark grey','#FE6F87'))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  
  ####HAND SANITIZER AVAILABEL
  ng_tt_sanitizer_all <- reactive({
    Nigeria_TT_Obs%>%
      filter((State == input$State4 & Year == input$years11 & Round == input$Round4))%>%
      group_by(State,Round, Year, tto1117_sanitizer_available)%>%
      dplyr:: summarise(  sanitizer = round(n()/sum(!is.na(ng_rows_2()$tto1117_sanitizer_available))*100))%>%
      filter(!is.na(sanitizer))
  })
  
  
  output$ng_tt_sanitizer <- renderPlot ({
    ggplot(data = ng_tt_sanitizer_all(), 
           aes(x = 2, y = sanitizer, fill= tto1117_sanitizer_available ))+
      geom_bar(stat = "identity")+
      ggtitle("Was hand sanitizer or a hand washing facility \n available at the training?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(sanitizer,"%", sep = ""),hjust = "centre"), col = "black",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c('dark grey','#FE6F87'))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  
  
  ####SANITIZE VANUE
  ng_tt_sanitize_venue_all <- reactive({
    Nigeria_TT_Obs%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & !is.na(tto1118_venue_sanitized))%>%
      group_by(State,Round, Year, tto1118_venue_sanitized)%>%
      dplyr:: summarise(  sanitize_venue = round(n()/sum(!is.na(ng_rows_2()$tto1118_venue_sanitized))*100))%>%
      filter(!is.na(sanitize_venue))
  })
  
  
  
  output$ng_tt_sanitize_venue <- renderPlot ({
    ggplot(data = ng_tt_sanitize_venue_all(), 
           aes(x = 2, y = sanitize_venue, fill= tto1118_venue_sanitized ))+
      geom_bar(stat = "identity")+
      ggtitle("Was the training venue cleaned/sanitized \n before this training?")+
      coord_polar("y", start = 200)+
      geom_text(aes(label = paste(sanitize_venue,"%", sep = ""),hjust = "centre"), col = "black",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c('dark grey','maroon', '#FE6F87'))+
      theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)
  })
  
  ng_tt_parts_mask_all <- reactive({
    Nigeria_TT_Obs%>%
      group_by(State,Round, Year, tto1116_num_prticipants_mask)%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & !is.na(tto1116_num_prticipants_mask))%>%
      dplyr:: summarise( parts_mask = round(n()/sum(!is.na(ng_rows_2()$tto1116_num_prticipants_mask))*100))%>%
      filter(!is.na(parts))
  })
  
  
  output$ng_tt_num_parts_mask <- renderPlot({
    ggplot(data = ng_tt_parts_mask_all(), aes(x = tto1116_num_prticipants_mask, y =  parts_mask, fill=tto1116_num_prticipants_mask))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      ggtitle("How many participants were wearing protective \n face masks during the training?")+
      scale_fill_manual(values = c('#FE6F87', "dark grey",'#5E5F5F', 'grey'), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round( parts_mask),"%",sep=""),  hjust = "right", size = 4),
                colour = "white",
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=18, hjust=0.5)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            # axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()
  })
  
  
  ####### Treatment Types
  ng_trt_types <- reactive({
    (ng_trt_type %>%
       filter(State == input$State4 & Year == input$years11 & Round == input$Round4)%>%
       select(ttp011_sth_schisto,`TREATMENT TYPES`) )%>%
      ungroup()%>%
      mutate(Labels = paste(ttp011_sth_schisto, `TREATMENT TYPES`,sep = "/n"))%>%
      ungroup()%>%
      select(Labels, `TREATMENT TYPES`)%>%
      tibble::deframe()
  })
  
  output$ng_box2 <- renderPlot({
    waffle(ng_trt_types(),rows = 10,
                   colors = c('#FE6F87',"grey", "grey"),
                   title = "Treatment Types",
                   legend_pos = "bottom")+
              theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
                    legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
    })
  
  

  ########## Number of ng_respondents      
  ng_Respondents <- reactive({
    (ng_respondents %>%
       filter(State == input$State4 & Year == input$years11 & Round == input$Round4)%>%
       select(source_file,RESPONDENTS) )
  })
  output$ng_box3 <- renderTable({
    ng_Respondents()
    
  })
  
  #######Attendance 
  ng_teacher_attendance <- reactive({
    ng_training_attendance %>%
       filter(State == input$State4 & Year == input$years11 & Round == input$Round4)%>%
       ungroup()%>%
       select(Teacher_Attendance)
  })
  

 
    
  output$ng_box4 <- renderPlotly({
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = ng_teacher_attendance()[[1]],
      title = list(text = "Teacher Attendance"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  
  ng_school_attendance <- reactive({
    ng_training_attendance %>%
       filter(State == input$State4 & Year == input$years11 & Round == input$Round4)%>%
       select(School_Attendance)
       
  })
  
  output$ng_box5 <- renderPlotly({
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = ng_school_attendance()[[1]],
      title = list(text = "School Attendance"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  
  
 
  ng_teacher_ontime <- reactive({
    ng_training_attendance %>%
       filter(State == input$State4 & Year == input$years11 & Round == input$Round4)%>%
       select( Teacher_on_Time)
  })
  
  output$ng_box6 <- renderPlotly({
    fig <- plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = ng_teacher_ontime()[[1]],
      title = list(text = "Teacher on-Time"),
      type = "indicator",
      mode = "gauge+number")%>%
      layout(margin = list(l=20,r=30))
    
  })
  

  

 
  ############ Topic Coverage
  ng_topic_cover <- reactive ({
    ng_data_long4%>%
    filter(State == input$State4 & Year == input$years11 & Round == input$Round4)%>%
      group_by(State, Year, Round, Variables, values)%>%
      summarise('Topic Coverage' = paste(round(n()/nrow(ng_rows2())*100),"%",sep=""),
                vals = round(n()/nrow(ng_rows2())*100))
   
    
  })
  
  
  
  output$ng_coverage2 <- renderPlot ({
    (ggplot(data = ng_topic_cover(), aes(stringr :: str_wrap(Variables), y = vals, fill= values))+
       geom_bar(stat="identity") +
       ggtitle("Topic Coverage")+
       scale_fill_manual(values =  c('#60BC68','#F05854',  "grey"))+
       aes(stringr::str_wrap(Variables, 20), vals) + xlab(NULL)+
       geom_text(aes(label = paste(vals,"%",sep=""), hjust = "centre"),
                 colour = "white",
                 vjust = -0.3,
                 #position = position_dodge(width = -.3),
                 position = position_stack(vjust = 0.3),
                 size = 5,
                 fontface="bold",
                 show.legend = FALSE) +
       coord_flip()+
       theme_bw()+
       theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
       theme(axis.text.y = element_text(color="BlACK",size=14)) +
       theme(axis.title.x = element_blank(), 
             axis.text.x = element_blank(),
             axis.ticks.x =  element_blank(),
             axis.ticks.y = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = unit(c(1,1,1,0), "mm"),
             panel.border = element_blank(), 
             legend.position="bottom") +
       removeGrid())})
  
  
  
  
  ###############Teacher Roles  
  ng_Teacher_roles <- reactive ({
    Nigeria_Extraanalysis %>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4) %>%
      summarise('Organize Drug Administration' = paste(round(sum(`Organize Drug Administration`, na.rm = T)/ nrow(ng_rows2())*100),"%",sep=""),
                `Disseminate health education messages` = paste(round(sum(`Disseminate health education messages`, na.rm = T)/nrow(ng_rows2())*100),"%",sep=""),
                `Mobilize non-enrolled children` = paste(round(sum(`Mobilize non-enrolled children`, na.rm = T)/nrow(ng_rows2())*100),"%",sep=""),
                `Complete forms` = paste(round(sum(`Complete forms`, na.rm = T)/nrow(ng_rows2())*100),"%",sep=""), 
                `Not covered` = paste(round(sum(`Not covered`, na.rm = T)/nrow(ng_rows2())*100),"%",sep=""))
  })
  
  ng_tr_roles <- reactive ({
    gather(ng_Teacher_roles(), Variables, values, 'Organize Drug Administration':`Not covered`, factor_key=TRUE)
    
  })
  
  output$ng_tt_roles <- renderPlot({
    ggplot(data = ng_tr_roles(), aes(x = Variables, y = values,fill = Variables))+
      geom_bar(stat = 'identity', position = "stack") +
      ggtitle("KEY TEACHER ROLES")+
      scale_fill_manual(values = ng_colot_progress, aesthetics = "fill") +
      aes(stringr::str_wrap(Variables, 15), values) + xlab(NULL)+
      geom_text(aes(label = values, hjust = "centre"),
                colour = "white",
                position = position_stack(vjust = 0.9), 
                size = 5,
                fontface="bold",
                show.legend = FALSE) +
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid() 
  }) 
  
  ###############Form Filling
  
  ng_form_filling <- reactive ({
    Nigeria_TT_Obs %>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & !is.na(tto805a_practical_sess)) %>%
      group_by(tto805a_practical_sess) %>%
      summarise(Form_Filling = (round(n()/nrow(ng_rows2())*100) ))%>%
      filter(!is.na(Form_Filling))
  })
  
  output$ng_FormsFIlled <- renderPlot({
    ggplot(data = ng_form_filling(), 
           aes(x = 2, y = Form_Filling, fill= tto805a_practical_sess ))+
      geom_bar(stat = "identity")+
      ggtitle("Form FIlling")+
      coord_polar("y", start = 200) +
      geom_text(aes(label = paste(Form_Filling,"%", sep = ""),hjust = "centre"), 
                vjust = -0.3,
                col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("#60BC68","#F05854"))+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5) 
  })
  
  ###### ng_pole
  ng_schnum <- reactive ({
    Nigeria_Extraanalysis %>%
      filter((State == input$State4 & Year == input$years11 & Round == input$Round4)) %>% 
      summarise(sch_num = sum(`SCH numbers`))
  })
  
  ng_pole <- reactive ({
    ng_pole1 %>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4) %>%  
      mutate(Tablet_pole_demonstration = round((`Tablet pole demonstration`/ng_schnum()$sch_num)*100))
    
  })
  
  output$ng_Poles <- renderPlot ({
    validate(
      need(ng_pole()$Tablet_pole_demonstration, 'No SCHISTO Data at the Moment'))
    ggplot(data = ng_pole(), 
           aes(x = 2, y = Tablet_pole_demonstration, fill= tto504_tabletpole_explained ))+
      geom_bar(stat = "identity")+
      ggtitle("Tablet pole demonstration")+
      coord_polar("y", start = 200) +
      geom_text(aes(label = paste(Tablet_pole_demonstration,"%", sep = ""),hjust = "right"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("#F05854","#60BC68"))+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)  
    
  })
  
  ng_Teaching_methods <- reactive ({
    mens <- ng_methods1%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4) %>%  
      group_by( State, Variables, values)%>%
      summarise(#ng_Teaching_methods = paste(round(n()/nrow(ng_rows())*100),"%",sep=""), 
        tm = round(n()/nrow(ng_rows2())*100))
    mens <-  mens[!is.na(mens$values),]  
  })
  
  output$ng_TT_Methods <- renderPlot({
    (ggplot(data = ng_Teaching_methods(), aes(stringr :: str_wrap(Variables), y = tm, fill = values))+
       geom_bar(stat="identity",  width = 0.4) +
       ggtitle("Training Methods Used")+
       scale_fill_manual(values =  c( '#60BC68'))+
       aes(stringr::str_wrap(Variables, 20), tm) + xlab(NULL)+
       geom_text(aes(label = paste(tm,"%",sep=""), hjust = "centre"),
                 colour = "white",
                 vjust = -0.3,
                 #position = position_dodge(width = -.3),
                 position = position_stack(vjust = 0.3),
                 size = 5,
                 fontface="bold",
                 show.legend = FALSE) +
       coord_flip()+
       theme_bw()+
       theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
       theme(axis.text.y = element_text(color="BlACK",size=14)) +
       theme(axis.title.x = element_blank(), 
             axis.text.x = element_blank(),
             axis.ticks.x =  element_blank(),
             axis.ticks.y = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = unit(c(1,1,1,0), "mm"),
             panel.border = element_blank())+ 
       #legend.position="bottom") +
       removeGrid())})
  
  
  
  
  #####CORRECT WORMS 
  gg2 <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "STH" & source_file == "Pre") 
  })
  
  gg2_post <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "STH" & source_file == "Post") 
  }) 
  

  output$ng_sthworms <-renderPlotly({
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg2_post()$`Worms treated`[[1]],
      title = list(text = "Worms Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg2()$`Worms treated`[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg2()$`Worms treated`[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
        layout(margin = list(l=20,r=30))
    })
    
    
 
  
  gg3 <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "SCH"  & source_file == "Pre")
  })
  gg3_post <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "SCH" & source_file == "Post" )
  })
  
  
  output$ng_schworms <-renderPlotly({
    validate(
      need(ng_pole()$Tablet_pole_demonstration, 'No SCHISTO Data at the Moment'))
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg3_post()$`Worms treated`[[1]],
      title = list(text = "Worms Treated"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg3()$`Worms treated`[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg3()$`Worms treated`[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  
  #####CORRECT DRUGS
  ###STH
   gg4 <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "STH" & source_file == "Pre") 
  })
  
  gg4_post <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "STH" & source_file == "Post") 
  }) 
    

  output$ng_sthdrugs <-renderPlotly({
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg4_post()$`Correct Drugs`[[1]],
      title = list(text = "Correct Drugs"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg4()$`Correct Drugs`[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg4()$`Correct Drugs`[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  ###SCH
 
  gg5 <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "SCH" & source_file == "Pre" )
  })
  gg5_post <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "SCH" & source_file == "Post" )
  })
  
  
  output$ng_schdrugs <-renderPlotly({
    validate(
      need(ng_pole()$Tablet_pole_demonstration, 'No SCHISTO Data at the Moment'))
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg5_post()$`Correct Drugs`[[1]],
      title = list(text = "Correct Drugs"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg5()$`Correct Drugs`[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg5()$`Correct Drugs`[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  
  
  ####### CORRECT AGE GROUP
  ###STH
 
  gg6 <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "STH" & source_file == "Pre") 
  })
  
  gg6_post <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "STH" & source_file == "Post") 
  }) 
  
  
  output$ng_sthage <-renderPlotly({
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg6_post()$`Correct age group`[[1]],
      title = list(text = "Correct Age Group"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg6()$`Correct age group`[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg6()$`Correct age group`[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  ###SCH
  
  gg7 <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "SCH" & source_file == "Pre" )
  })
  gg7_post <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "SCH" & source_file == "Post" )
  })
  
  
  output$ng_schage <-renderPlotly({
    validate(
      need(ng_pole()$Tablet_pole_demonstration, 'No SCHISTO Data at the Moment'))
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg7_post()$`Correct age group`[[1]],
      title = list(text = "Correct age group"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg7()$`Correct age group`[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg7()$`Correct age group`[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  
 
  #####CORRECT DOSAGE
  
  
  ###STH
 

  gg8 <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "STH" & source_file == "Pre" )
  })
  gg8_post <- reactive ({ng_knowledge%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "STH" & source_file == "Post" )
  })
  
  
  output$ng_sthdose <-renderPlotly({
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gg8_post()$`Correct dosage`[[1]],
      title = list(text = "Correct dosage"),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = gg8()$`Correct dosage`[[1]]),
      gauge = list(
        axis =list(range = list(NULL, 100)),
        steps = list(
          list(range = c(0, gg8()$`Correct dosage`[[1]]), color = "pink")),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90))) %>%
      layout(margin = list(l=20,r=30))
  })
  
 
  ###SCH
 
      gg9 <- reactive ({ng_knowledge%>%
          filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "SCH" & source_file == "Pre" )
      })
      gg9_post <- reactive ({ng_knowledge%>%
          filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & Type == "SCH" & source_file == "Post" )
      })
      
      
      output$ng_schdose <-renderPlotly({
        validate(
          need(ng_pole()$Tablet_pole_demonstration, 'No SCHISTO Data at the Moment'))
        plot_ly(
          domain = list(x = c(0, 1), y = c(0, 1)),
          value = gg9_post()$`Correct dosage`[[1]],
          title = list(text = "Correct dosage"),
          type = "indicator",
          mode = "gauge+number+delta",
          delta = list(reference = gg9()$`Correct dosage`[[1]]),
          gauge = list(
            axis =list(range = list(NULL, 100)),
            steps = list(
              list(range = c(0, gg9()$`Correct dosage`[[1]]), color = "pink")),
            threshold = list(
              line = list(color = "red", width = 4),
              thickness = 0.75,
              value = 90))) %>%
          layout(margin = list(l=20,r=30))
      })
      
  #########MATERIAL DISTRIBUTION
  ##DRUGS
  ng_drug_distribution <- reactive ({
    ng_materials1%>%
      filter(State == input$State4 & Variables == "tto1106_drugs_dist_sch" & Year == input$years11 & Round == input$Round4)%>%
      group_by( Variables, values)%>%
      dplyr::summarise(`Materials distributed` = (round(n()/nrow(ng_rows2())*100)))%>%
      filter(!is.na(`Materials distributed` ))
    #drug_dist <-  drug_dist[!is.na(drug_dist$values),]   
    
  })
  
  
  output$ng_drugsdistribution <- renderPlot({ 
    ggplot(data = ng_drug_distribution(), aes(x = values, y = `Materials distributed`,fill = values))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      ggtitle("Drugs Distributed")+
      scale_fill_manual(values = ng_colors, aesthetics = "fill") +
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(`Materials distributed`,"%",sep="")),  vjust = -0.3,
                fontface="bold")+
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid()
  })
  
  ##FORMS
  ng_form_distribution <- reactive ({
    form_dist <- ng_materials1%>%
      filter(State == input$State4 & Variables == "tto1107_forms_dist_sch" & Year == input$years11 & Round == input$Round4)%>%
      group_by( Variables, values)%>%
      dplyr::summarise(`Materials distributed` = (round(n()/nrow(ng_rows2())*100)))
    form_dist <-  form_dist[!is.na(form_dist$values),] 
  })
  
  
  
  output$ng_formsdistribution <- renderPlot({ 
    ggplot(data = ng_form_distribution(), aes(x = values, y = `Materials distributed`,fill = values))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      scale_fill_manual(values = ng_colors, aesthetics = "fill") +
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      ggtitle("Forms Distributed")+
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(`Materials distributed`,"%",sep="")),  vjust = -0.3,
                fontface="bold")+
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid()
  })
  ##POLES 
  pole_distribution <- reactive ({
    pp <- ng_materials1%>%
      filter(State == input$State4 & Variables == "tto1108_tapes_dist_sch" & Year == input$years11 & Round == input$Round4)%>%
      group_by( Variables, values)%>%
      dplyr::summarise(`Materials distributed` = (round(n()/(ng_schnum()$sch_num)*100)))
    
    pp <-  pp[!is.na(pp$values),]
    
  })
  
  
  
  output$ng_pole_dist <- renderPlot({ 
    validate(
      need(ng_pole()$Tablet_pole_demonstration, 'No SCHISTO Data at the Moment'))
    
    ggplot(data = pole_distribution(), aes(x = values, y = `Materials distributed`,fill = values))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      scale_fill_manual(values = ng_colors, aesthetics = "fill") +
      ggtitle("Poles Distributed")+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(`Materials distributed`,"%",sep="")),  vjust = -0.3,
                fontface="bold")+
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid()
  })
  
  ####POSTERS
  posts <- reactive ({
    xxyn <-ng_materials1%>%
      filter(State == input$State4 & Variables == "tto1110_posters_dist_sch"  & Year == input$years11 & Round == input$Round4)%>%
      group_by( Variables, values)%>%
      dplyr::summarise(`Materials distributed` = (round(n()/nrow(ng_rows2())*100)))
    xxyn <-  xxyn[!is.na(xxyn$values),]  
  })
  
  
  output$ng_Posters <- renderPlot({ 
    ggplot(data = posts(), aes(x = values, y = `Materials distributed`,fill = values))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      scale_fill_manual(values = ng_colors, aesthetics = "fill") +
      ggtitle("Posters Distributed")+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(`Materials distributed`,"%",sep="")),  vjust = -0.3,
                fontface="bold")+
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid()
  })
  
  
  ###TREATMENT REGISTER
  trt_register <- reactive ({
    aa <- ng_materials1%>%
      filter(State == input$State4 & Variables == "tto1109_register_dist_sch" & Year == input$years11 & Round == input$Round4)%>%
      group_by(State, Year, Round, Variables, values)%>%
      dplyr::summarise(`Materials distributed` = (round(n()/nrow(ng_rows2())*100)))
    aa <-  aa[!is.na(aa$values),]  
  })
  
  
  output$ng_treatment_register <- renderPlot({ 
    ggplot(data = trt_register(), aes(x = values, y = `Materials distributed`,fill = values))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      scale_fill_manual(values = ng_colors, aesthetics = "fill") +
      ggtitle("Treatment Register Distributed")+
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(`Materials distributed`,"%",sep=""))
                ,  vjust = -0.3,
                fontface="bold")+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid()
  })
  
  
  ####REASONS LATE 
  reasonslate <- reactive({
    lt <- ng_late1%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4) %>%
      group_by(State, Year, Round, Variables, values)%>%
      summarise(Reasons_late = (round(n()/nrow(ng_rows2())*100)))
    lt <-  lt[!is.na(lt$values),] 
  })
  
  output$ng_Lateness <- renderPlot({
    (ggplot(data = reasonslate(), aes(stringr :: str_wrap(Variables), y = Reasons_late, fill = State))+
       geom_bar(stat="identity",  width = 0.4) +
       ggtitle("How did you hear about the \n deworming exercise conducted \n in your community?")+
       scale_fill_manual(values =  c( '#A5A5A5', '#A5A5A5', '#A5A5A5', '#A5A5A5'))+
       aes(stringr::str_wrap(Variables, 20), Reasons_late) + xlab(NULL)+
       geom_text(aes(label = paste(Reasons_late,"%",sep=""), hjust = "centre"),
                 colour = "black",
                 vjust = -0.3,
                 #position = position_dodge(width = -.3),
                 position = position_stack(vjust = 0.3),
                 size = 5,
                 fontface="bold",
                 show.legend = FALSE) +
       coord_flip()+
       theme_bw()+
       theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
       theme(axis.text.y = element_text(color="BlACK",size=14)) +
       theme(axis.title.x = element_blank(), 
             axis.text.x = element_blank(),
             axis.ticks.x =  element_blank(),
             axis.ticks.y = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = unit(c(1,1,1,0), "mm"),
             panel.border = element_blank())+ 
       #legend.position="bottom") +
       removeGrid())})
  
  
  
  ####ENOUGH FORMS 
  pp_rows <- reactive({
    Nigeria_TTPrePost_Extra%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 )
  })
  
  ng_summaryforms <- reactive({
    Nigeria_TTPrePost_Extra%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & source_file == "Post") %>%
      group_by(State, Year, Round, `Summary form`)%>%
      summarise(Summary_Forms = (round(n()/sum(!is.na(pp_rows()$ttp231_form_submit))*100)))
    
  })
  
  
  
  
  output$ng_Enough_Forms <- renderPlot({
    ggplot(data=ng_summaryforms(), aes(x=`Summary form`, y=Summary_Forms, fill = `Summary form` )) +
      geom_bar(stat="identity", fill="#A5A5A5", width=0.4, position = position_dodge(width=0.5)) +
      ggtitle("Are the Forms Given Enough?")+
      geom_text(aes(label=paste(Summary_Forms,"%", sep ="")), vjust=-0.3,hjust = "left",  size = 3.5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "none",
            panel.border = element_blank())+
      removeGrid() +
      coord_flip()
  })
  
  
  
  chew_contacts <- reactive({
    Nigeria_TT_PrePost%>%
      filter(State == input$State4 & Year == input$years11 & Round == input$Round4 & source_file == "Post") %>%
      group_by(State,Year, Round, Chew_Contact) %>%
      summarise(ng_contacts = round(n()/sum(!is.na(pp_rows()$ttp231_form_submit))*100))
    
  })
  
  output$ng_Contacts <-renderPlot({
    ggplot(data = chew_contacts(), aes(x =Chew_Contact , y = ng_contacts, fill = Chew_Contact))+
      geom_bar(stat = 'identity', position = "stack") +
      scale_fill_manual(values = c('#A5A5A5', '#F05854'), aesthetics = "fill") +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(ng_contacts,"%",sep="")),  vjust = -0.3,
                fontface="bold")+
      ggtitle("Do you have AEO contacts?")+
      coord_flip()+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.x = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid()
  })
  
  
  ####NIGERIA CMS
  Nigeria_DD_Com_rows <-  reactive ({
    Nigeria_DD_Com%>%
      filter(State == input$State20 & Year == input$years20 & Round == input$Round20) 
  })
  
  #### CHILDREN DEWORMED
  ng_deworm_all_xy <-  reactive({
    Nigeria_DD_Com %>%
      filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20 & !is.na(cms131_child_deworm))%>%
      group_by(State, Year, Round, cms131_child_deworm)%>%
      dplyr:: summarise(trt = round(n()/nrow(Nigeria_DD_Com_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(cms131_child_deworm, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  output$ng_dd_dewormed <- renderPlot({
    waffle(ng_deworm_all_xy(),rows = 10,
           colors = c('dark green',"red","grey", "maroon"),
           title = "Children Dewomed",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  ####INTERVIEW TYPES
  ng_int_all <-  reactive({
    Nigeria_DD_Com %>%
      filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms100_interview_type_1))%>%
      group_by(State, Year, Round, cms100_interview_type_1)%>%
      dplyr:: summarise(trt = round(n()/nrow(Nigeria_DD_Com_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(cms100_interview_type_1, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  
  output$ng_dd_int_type <- renderPlot({
    waffle(ng_int_all(),rows = 10,
           colors = c('dark green',"red","grey", "maroon"),
           title = "Interview Types",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  
  ####TREATMENT TYPES
  ng_cms_trt_all <-  reactive({
    Nigeria_DD_Com %>%
      filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20 & !is.na(cms000_treat_type))%>%
      group_by(State, Year, Round, cms000_treat_type)%>%
      dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms000_treat_type))*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(cms000_treat_type, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      
      tibble::deframe()
  })
  
  output$ng_dd_trt_type <- renderPlot({
    waffle(ng_cms_trt_all(),rows = 10,
           colors = c('dark green','red','grey'),
           title = "Treatment Types",
           legend_pos = "bottom")+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
            legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })
  
  ####AWARE WORMS TREATED
  ng_cms_worms_all <-  reactive({
    Nigeria_DD_Com %>%
      filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20 & !is.na(cms125f_worms_treated))%>%
      group_by(State, Year, Round, cms125f_worms_treated)%>%
      dplyr:: summarise(trt = round(n()/nrow(Nigeria_DD_Com_rows())*100))%>%
      ungroup()%>%
      mutate(Labels = paste0(cms125f_worms_treated, "(",trt,"%)")) %>%
      select(Labels, trt)%>%
      tibble::deframe()
  })
  
  
  
  output$ng_dd_worms <- renderPlot({
    waffle(ng_cms_worms_all(),rows = 10,
    colors = c('#FE6F87','dark grey',"maroon", "white"),
    title = "Worms Treated",
    legend_pos = "right")+
    theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold", colour = "#034988"),
          legend.text = element_text(size = 15, color = "#F8670D",  face = "bold"))
  })


####Age Group
##SCH
ng_cms_sch_age_group_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms126b_child_age_sch))%>%
    group_by(State, Year, Round, cms126b_child_age_sch)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms126b_child_age_sch))*100))%>%
    ungroup()%>%
    filter(cms126b_child_age_sch == "5-14 Years")%>%
    select(trt)
})


output$ng_cms_sch_age <- renderPlotly({
  validate(
    need(ng_cms_sch_age_group_all()$trt, 'No Data at the Moment'))
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_cms_sch_age_group_all()[[1]],
    title = list(text = "Correct SCH Age Group"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
})
##STH
ng_cms_sth_age_group_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms126a_child_age_sth))%>%
    group_by(State, Year, Round, cms126a_child_age_sth)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms126a_child_age_sth))*100))%>%
    ungroup()%>%
    filter(cms126a_child_age_sth == "5-14 Years")%>%
    select(trt)
})


output$ng_cms_sth_age <- renderPlotly({
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_cms_sth_age_group_all()[[1]],
    title = list(text = "Correct STH Age Group"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
})

###SAFE TO DEWORM
ng_cms_child_eat <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms132b_child_eat))%>%
    group_by(State, Year, Round, cms132b_child_eat)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms132b_child_eat))*100))%>%
    ungroup()%>%
    filter(cms132b_child_eat == "Yes")%>%
    select(trt)
})


output$cms_child_eat <- renderPlotly({
  validate(
    need(ng_cms_child_eat()$trt, 'No Data at the Moment'))
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_cms_child_eat()[[1]],
    title = list(text = "Child Ate Berfore Deworming"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
})





####Aware deworming Activities going on
ng_shared_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms120_activities_aware))%>%
    group_by(State, Year, Round, cms120_activities_aware)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms120_activities_aware))*100))%>%
    ungroup()%>%
    filter(cms120_activities_aware =="Yes;Deworming of children")%>%
    select(trt)
})



output$ng_awareness <- renderPlotly({
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_shared_all()[[1]],
    title = list(text = "Deworming Activities Awareness"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
  
})


###DEWORMED BEFORE
ng_deworm_bf_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms125_taken_child))%>%
    group_by(State, Year, Round, cms125_taken_child)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms125_taken_child))*100))%>%
    ungroup()%>%
    filter(cms125_taken_child =="Yes")%>%
    select(trt)
})


output$ng_deworm_before <- renderPlotly({
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = deworm_bf_all()[[1]],
    title = list(text = "Have you dewormed Before?"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
})


###FEEDBACK
ng_dd_feedback_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms133_feel_program))%>%
    group_by(State, Year, Round, cms133_feel_program)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms133_feel_program))*100))%>%
    ungroup()%>%
    filter(cms133_feel_program =="Positive")%>%
    select(trt)
})



output$deworm_feedback <- renderPlotly({
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_dd_feedback_all()[[1]],
    title = list(text = "Positive Feedback on Deworming"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
  
})

###REASONS FOR DEWORMING

ng_dd_deworm_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms125a_take_reason))%>%
    group_by(State, Year, Round, cms125a_take_reason)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms125a_take_reason))*100))
})




output$ng_cloud <- renderPlot({
  ggplot(data = ng_dd_deworm_all(), aes(x = cms125a_take_reason, y = trt, fill=Year))+
    geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
    aes(stringr::str_wrap(cms125a_take_reason, 30), trt) + xlab(NULL)+
    ggtitle("Reasons for Deworming")+
    scale_fill_manual(values = c('dark green'), aesthetics = "fill") +
    #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
    geom_text(aes(label = paste(round(trt),"%",sep=""),  hjust = "right", size = 4),
              colour = "white",
              position = position_dodge(width = .0),
              size = 5,
              show.legend = FALSE,
              fontface="bold") +
    theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
    theme(axis.text.y = element_text(color="BlACK",size=14)) +
    theme(axis.title.x = element_blank(), 
          axis.ticks.x =  element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(), 
          # axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,1,1,0), "mm"),
          legend.position = "none",
          legend.text = element_text(color="BlACK",size=14),
          panel.border = element_blank()) +
    coord_flip()+
    removeGrid()
})


###REASONS FOR NOT DEWORMING

ng_dd_not_deworm_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms125b_nottake_reason))%>%
    group_by(State, Year, Round, cms125b_nottake_reason)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms125b_nottake_reason))*100))
})



output$ng_cloud2 <- renderPlot({
  ggplot(data = ng_dd_not_deworm_all(), aes(x = cms125b_nottake_reason, y = trt, fill=Year))+
    geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
    aes(stringr::str_wrap(cms125b_nottake_reason, 30), trt) + xlab(NULL)+
    ggtitle("Reasons for NOT Deworming")+
    scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
    #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
    geom_text(aes(label = paste(round(trt),"%",sep=""),  hjust = "right", size = 4),
              colour = "white",
              position = position_dodge(width = .0),
              size = 5,
              show.legend = FALSE,
              fontface="bold") +
    theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
    theme(axis.text.y = element_text(color="BlACK",size=14)) +
    theme(axis.title.x = element_blank(), 
          axis.ticks.x =  element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(), 
          # axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,1,1,0), "mm"),
          legend.position = "none",
          legend.text = element_text(color="BlACK",size=14),
          panel.border = element_blank()) +
    coord_flip()+
    removeGrid()
})

#####COMMUNICATION MEANS TO PARENTS
ng_Comm_parents_all <- reactive({
  ng_Comm_parents2%>%
    filter(State == input$State20 &  Round == input$Round20& Year ==  input$years20)%>%
    group_by(Year,Values) %>%
    dplyr:: summarise(chew_roles = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms128_most_prefered))*100))
})






output$ng_Comm_means1 <- renderPlot({
  ggplot(data = ng_Comm_parents_all(), aes(x = Values, y = chew_roles, fill=Year))+
    geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
    aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
    ggtitle("Top 3 Preferred Communication Means\n by Parents")+
    scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
    #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
    geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "right", size = 4),
              colour = "white",
              position = position_dodge(width = .0),
              size = 5,
              show.legend = FALSE,
              fontface="bold") +
    theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
    theme(axis.text.y = element_text(color="BlACK",size=14)) +
    theme(axis.title.x = element_blank(), 
          axis.ticks.x =  element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(), 
          # axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,1,1,0), "mm"),
          legend.position = "none",
          legend.text = element_text(color="BlACK",size=14),
          panel.border = element_blank()) +
    coord_flip()+
    removeGrid()
})

#####COMMUNICATION MEANS TO CHV
##(Comm_CHV, Comm_parents)
ng_Comm_CHV_all <- reactive({
  ng_Comm_CHV%>%
    filter(State == input$State20 &  Round == input$Round20& Year ==  input$years20)%>%
    group_by(Year,Role,Values) %>%
    dplyr:: summarise(chew_roles = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms208_deworm_hear))*100))
})


output$ng_Comm_means2 <- renderPlot({
  ggplot(data = ng_Comm_CHV_all(), aes(x = Values, y = chew_roles, fill=Role))+
    geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
    aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
    ggtitle("Communication Means to FLHF")+
    scale_fill_manual(values = c('#FE6F87','grey'), aesthetics = "fill") +
    #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
    geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "centre", size = 4),
              colour = "black",
              position = position_dodge(width = 0.5),
              size = 5,
              show.legend = FALSE,
              fontface="bold") +
    theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
    theme(axis.text.y = element_text(color="BlACK",size=14)) +
    theme(axis.title.x = element_blank(), 
          axis.ticks.x =  element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(), 
          # axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,1,1,0), "mm"),
          legend.position = "none",
          legend.text = element_text(color="BlACK",size=14),
          panel.border = element_blank()) +
    coord_flip()+
    removeGrid()
})


#####COMMUNICATION MEANS TO Parents
##(Comm_CHV, Comm_parents)
ng_Comm_parents_all <- reactive({
  ng_Comm_parents%>%
    filter(State == input$State20 &  Round == input$Round20& Year ==  input$years20)%>%
    group_by(Year,Role,Values) %>%
    dplyr:: summarise(chew_roles = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms124_heard_drug_cmpaign))*100))
})


output$ng_Comm_means3 <- renderPlot({
  ggplot(data = ng_Comm_parents_all(), aes(x = Values, y = chew_roles, fill=Role))+
    geom_bar(stat = 'identity', position = "dodge",  width = 0.6) +
    aes(stringr::str_wrap(Values, 30), chew_roles) + xlab(NULL)+
    ggtitle("Communication Means to Parents")+
    scale_fill_manual(values = c('grey'), aesthetics = "fill") +
    #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
    geom_text(aes(label = paste(round(chew_roles),"%",sep=""),  hjust = "centre", size = 4),
              colour = "black",
              position = position_dodge(width = 0.5),
              size = 5,
              show.legend = FALSE,
              fontface="bold") +
    theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
    theme(axis.text.y = element_text(color="BlACK",size=14)) +
    theme(axis.title.x = element_blank(), 
          axis.ticks.x =  element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          #axis.text.y = element_blank(), 
          # axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,1,1,0), "mm"),
          legend.position = "none",
          legend.text = element_text(color="BlACK",size=14),
          panel.border = element_blank()) +
    coord_flip()+
    removeGrid()
})


###CHV EXISTS
ng_dd_chv_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms200_chv_exist))%>%
    group_by(State, Year, Round, cms200_chv_exist)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms200_chv_exist))*100))%>%
    ungroup()%>%
    filter(cms200_chv_exist =="Yes")%>%
    select(trt)
})

output$ng_chv_exist <- renderPlotly({
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_dd_chv_all()[[1]],
    title = list(text = "CHV Exists?"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
})

###Children Deworm
ng_chv_deworm_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms203_child_deworm_nxtwk))%>%
    group_by(State, Year, Round, cms203_child_deworm_nxtwk)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms203_child_deworm_nxtwk))*100))%>%
    ungroup()%>%
    filter(cms203_child_deworm_nxtwk =="Yes")%>%
    select(trt)
})

output$ng_chv_dewrm <- renderPlotly({
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_chv_deworm_all()[[1]],
    title = list(text = "Are Children getting dewormed?"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
  
})

###Who to deworm Deworm
ng_chv_who_deworm_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms205_child_elligible))%>%
    group_by(State, Year, Round, cms205_child_elligible)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms205_child_elligible))*100))%>%
    ungroup()%>%
    filter(cms205_child_elligible =="All children (aged 5-14 years)")%>%
    select(trt)
})

output$ng_chv_who_deworm<- renderPlotly({
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_chv_who_deworm_all()[[1]],
    title = list(text = "Who will be getting dewormed?"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
})

###AGE GROUP
ng_chv_agegroup_all <-  reactive({
  Nigeria_DD_Com %>%
    filter( Year ==  input$years20 & State == input$State20 &  Round == input$Round20& !is.na(cms206_childage_sth))%>%
    group_by(State, Year, Round, cms206_childage_sth)%>%
    dplyr:: summarise(trt = round(n()/sum(!is.na(Nigeria_DD_Com_rows()$cms206_childage_sth))*100))%>%
    ungroup()%>%
    filter(cms206_childage_sth ==1)%>%
    select(trt)
})

output$ng_chv_agegroup<- renderPlotly({
  fig <- plot_ly(
    domain = list(x = c(0, 100), y = c(0, 100)),
    value = ng_chv_agegroup_all()[[1]],
    title = list(text = "Age Group getting dewormed?"),
    type = "indicator",
    mode = "gauge+number")%>%
    layout(margin = list(l=20,r=30))
})

  
  ######DD MAIN 
  Nigeria_DD_Main_rows <-  reactive ({
    Nigeria_DD_Main%>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21) 
  })
  
  
  ng_deworm_happen <-  reactive({Nigeria_DD_Main%>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21 & dwo015_interview_at_schl != 0) %>%
      group_by(Year, Round, dwo015_interview_at_schl)%>%
      summarise(Interviews = (round(n()/nrow(Nigeria_DD_Main_rows())*100)))
  })
  
  output$deworming_day<- renderPlot({ 
    ggplot(data = ng_deworm_happen(), aes(x = dwo015_interview_at_schl, y = Interviews,fill = dwo015_interview_at_schl))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      scale_fill_manual(values = c("red", "dark green"), aesthetics = "fill") +
      ggtitle("Is deworming happening at this \n school today?")+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(Interviews,"%",sep="")),  vjust = -0.3,
                fontface="bold")+
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid()
  })
  
  deworm_non_enrolled <-  reactive({Nigeria_DD_Main%>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21) %>%
      group_by(Year, Round, deworm_non_enrolled_plan7)%>%
      summarise(Treatment_Plan = (round(n()/nrow(Nigeria_DD_Main_rows())*100)))
  })
  
  output$deworming_plan<- renderPlot({ 
    ggplot(data = deworm_non_enrolled(), aes(x = deworm_non_enrolled_plan7, y = Treatment_Plan,fill = deworm_non_enrolled_plan7))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      scale_fill_manual(values = c("red", "dark green"), aesthetics = "fill") +
      ggtitle("Is there a plan for the treatment of \n non-enrolled children?")+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.text.x = element_text(color="BlACK",size=14)) +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(Treatment_Plan,"%",sep="")),  vjust = -0.3,
                fontface="bold")+
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      removeGrid()
  })
  
  
  Trained2 <- reactive ({
    Nigeria_DD_Main %>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21 & !is.na(train_when)) %>%  
      group_by(Year, Round, train_when)%>%
      summarise(Trained = (round(n()/sum(!is.na(Nigeria_DD_Main_rows()$train_when), na.rm = T)*100)))
    
  })
  
  output$Did_Train <- renderPlot({
    ggplot(data = Trained2(), 
           aes(x = 2, y = Trained, fill= train_when ))+
      geom_bar(stat = "identity")+
      ggtitle("Did anyone from the school \n get trained?")+
      coord_polar("y", start = 200) +
      geom_text(aes(label = paste(Trained,"%", sep = ""),hjust = "centre"), 
                vjust = -0.3,
                col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("dark green","#F05854"))+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5) 
      #annotate(geom = 'text', x = 0.5, y = 0, label = paste0(form_filling()$Form_Filling[form_filling()$tto805a_practical_sess == 'Yes-Both']))
  })
  
  
  

  
  Sensitization <- reactive ({
    hens <- ng_hear_deworming1%>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21) %>%  
      group_by( State, Variables, values)%>%
      summarise(#Teaching_methods = paste(round(n()/nrow(ng_rows())*100),"%",sep=""), 
        tm = round(n()/nrow(Nigeria_DD_Com_rows())*100))
    hens <-  hens[!is.na(hens$values),]  
  })
  
  output$Sensitization_Methods <- renderPlot({
    (ggplot(data = Sensitization(), aes(stringr :: str_wrap(Variables), y = tm, fill = State))+
       geom_bar(stat="identity",  width = 0.4) +
       ggtitle("How did you hear about the  deworming exercise conducted in your community?")+
       scale_fill_manual(values =  c( 'dark green'))+
       aes(stringr::str_wrap(Variables, 20), tm) + xlab(NULL)+
       geom_text(aes(label = paste(tm,"%",sep=""), hjust = "centre"),
                 colour = "black",
                 vjust = -0.3,
                 #position = position_dodge(width = -.3),
                 position = position_stack(vjust = 0.3),
                 size = 5,
                 fontface="bold",
                 show.legend = FALSE) +
      
       theme_bw()+
       theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
       theme(axis.text.x = element_text(color="BlACK",size=14)) +
       theme(axis.title.x = element_blank(), 
             axis.text.y = element_blank(),
             axis.ticks.x =  element_blank(),
             axis.ticks.y = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = unit(c(1,1,1,0), "mm"),
             panel.border = element_blank())+ 
       #legend.position="bottom") +
       removeGrid())})
  
  
  
  dd_drugs <- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Tablets) )
  })
  
  output$box10 <- renderInfoBox({
    infoBox(
      tags$p(style = "font-size: 30px; font-weight: bold; color: red",(dd_drugs())),
       "One tablet of mebendazole given to each child")
  })
  
  dd_Poles <- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Poles) )
  })
  
  output$box11 <- renderInfoBox({
    infoBox(
        tags$p(dd_Poles() , style = "font-size: 30px; font-weight: bold; color: red"),
      "The tablet pole was used correctly to give tablets of Praziquantel ")
  })
  
  dd_Tablet_Disposal <- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Tablet_Disposal) )
  })
  
  output$box12 <- renderInfoBox({
    infoBox(
        tags$p(dd_Tablet_Disposal() ,style = "font-size: 30px; font-weight: bold; color: red"),
      "Spoilt tablets were properly disposed, and not given to children or left on the floor")
  })
  
  dd_Sick_Deworm <- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Sick_Deworm) )
  })
  
  output$box13 <- renderInfoBox({
    infoBox(
        tags$p(dd_Sick_Deworm() ,style = "font-size: 30px; font-weight: bold; color: red"),
      "Teachers asked the children if they are sick or under medication before giving them the deworming medicine ")
  })
  
  dd_Registers<- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Registers) )
  })
  
  output$box14 <- renderInfoBox({
    infoBox(
        tags$p(dd_Registers() ,style = "font-size: 30px; font-weight: bold; color: red"),
      "The Treatment register was used to record treatment ")
  })
  
  dd_Transfers<- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Transfers) )
  })
  
  output$box15 <- renderInfoBox({
    infoBox(
        tags$p(dd_Transfers() ,style = "font-size: 30px; font-weight: bold; color: red"),
      "The teacher transferred the names from the class register to treatment register prior to the deworming exercise ")
  })
  
  dd_Eat<- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Eat) )
  })
  
  output$box16 <- renderInfoBox({
    infoBox(
        tags$p(dd_Eat() ,style = "font-size: 30px; font-weight: bold; color: red"),
      "The teacher asked the all children if they had eaten prior to deworming")
  })
  
  dd_Hand_wash<- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Hand_wash) )
  })
  
  output$box17 <- renderInfoBox({
    infoBox(
        tags$p(dd_Hand_wash() ,style = "font-size: 30px; font-weight: bold; color: red"),
      "Handwashing done by the children prior to treatment and facilitated by the teacher")
  })
  
  dd_Health<- reactive({
    (ng_dd_summary %>%
       filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
       select(Health) )
  })
  
  output$box18 <- renderInfoBox({
    infoBox(
        tags$p(dd_Health() ,style = "font-size: 30px; font-weight: bold; color: red"),
      "Health education messages were given prior to treatment")
  })
  
  
  
  obs_effects <- reactive ({
    Nigeria_DD_Main %>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21 & !is.na(dwo230_obsrv_side_effect)) %>%  
      group_by(Year, Round, dwo230_obsrv_side_effect)%>%
      summarise(Deworm = (round(n()/sum(!is.na(Nigeria_DD_Main_rows()$dwo230_obsrv_side_effect))*100)))
    
  })
  
  output$Obs_side_effects <- renderPlot ({
    ggplot(data = obs_effects(), 
           aes(x = 2, y = Deworm, fill= dwo230_obsrv_side_effect ))+
      geom_bar(stat = "identity")+
      ggtitle("Did you see any children with side \n effects (abdominal pain, nausea,  \n vomiting, etc.) after taking the \n medicine?")+
      coord_polar("y", start = 200) +
      geom_text(aes(label = paste(Deworm,"%", sep = ""),hjust = "right"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("Yes" ="dark green","No" ="red"))+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)  
    #annotate(geom = 'text', x = 0.5, y = 0, label = paste0(pole()$Tablet_pole_demonstration[pole()$tto504_tabletpole_explained == 'Yes-Both']))
  })
  
  obs_effects_handle <- reactive ({
    Nigeria_DD_Main %>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21 & !is.na(dwo235_team_well_trnd)) %>%  
      group_by(Year, Round, dwo235_team_well_trnd)%>%
      summarise(Deworm = (round(n()/sum(!is.na(Nigeria_DD_Main_rows()$dwo235_team_well_trnd))*100)))
    
  })
  
  output$Obs_skils <- renderPlot ({
    ggplot(data = obs_effects_handle(), 
           aes(x = 2, y = Deworm, fill= dwo235_team_well_trnd ))+
      geom_bar(stat = "identity")+
      ggtitle("Overall, did the team show ability \n to handle and knowledgeable on \n side effects by treating children \n with side effects properly?")+
      coord_polar("y", start = 200) +
      geom_text(aes(label = paste(Deworm,"%", sep = ""),hjust = "right"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("Yes" ="dark green","No" ="red"))+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)  
    #annotate(geom = 'text', x = 0.5, y = 0, label = paste0(pole()$Tablet_pole_demonstration[pole()$tto504_tabletpole_explained == 'Yes-Both']))
  })
  
  
  Sd_Effects <- reactive ({
    sd <- ng_dd_side_effects1%>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21 & !is.na(Values)) %>%  
      group_by( State, Variables, values)%>%
      summarise(#Teaching_methods = paste(round(n()/nrow(ng_rows())*100),"%",sep=""), 
        tm = round(n()/nrow(Nigeria_DD_Main_rows())*100))
    #sd <-  sd[!is.na(sd$values),]  
  })
  
  output$DD_Side_Effects <- renderPlot({
    validate(
      need(Sd_Effects()$tm, 'No Data at the Moment'))
    (ggplot(data = Sd_Effects(), aes(stringr :: str_wrap(Variables), y = tm, fill = State))+
       geom_bar(stat="identity",  width = 0.4) +
       ggtitle("What side effects did you observe?")+
       scale_fill_manual(values =  c( '#A5A5A5'))+
       aes(stringr::str_wrap(Variables, 20), tm) + xlab(NULL)+
       geom_text(aes(label = paste(tm,"%",sep=""), hjust = "centre"),
                 colour = "black",
                 vjust = -0.3,
                 #position = position_dodge(width = -.3),
                 position = position_stack(vjust = 0.3),
                 size = 5,
                 fontface="bold",
                 show.legend = FALSE) +
       coord_flip()+
       theme_bw()+
       theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
       theme(axis.text.y = element_text(color="BlACK",size=14)) +
       theme(axis.title.x = element_blank(), 
             axis.text.x = element_blank(),
             axis.ticks.x =  element_blank(),
             axis.ticks.y = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = unit(c(1,1,1,0), "mm"),
             panel.border = element_blank(),
             legend.position="none") +
       removeGrid())})
  
  Sufficient_tools <- reactive ({
    sf<- Sufficiency1%>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21) %>%  
      group_by( State, Variables, values)%>%
      summarise(#Teaching_methods = paste(round(n()/nrow(ng_rows())*100),"%",sep=""), 
        tm = round(n()/sum(!is.na(Nigeria_DD_Main_rows()$Drugs))*100))
    sf <-  sf[!is.na(sf$values),]  
  })
  
  output$DD_Suffficiency <- renderPlot({
    (ggplot(data = Sufficient_tools(), aes(stringr :: str_wrap(Variables), y = tm, fill = values))+
       geom_bar(stat="identity",  width = 0.4) +
       ggtitle("Were drugs and reporting forms sufficient?")+
       scale_fill_manual(values = c("Yes" ="dark green","No" ="red"))+
       aes(stringr::str_wrap(Variables, 20), tm) + xlab(NULL)+
       geom_text(aes(label = paste(tm,"%",sep=""), hjust = "centre"),
                 colour = "black",
                 vjust = -0.3,
                 #position = position_dodge(width = -.3),
                 position = position_stack(vjust = 0.3),
                 size = 5,
                 fontface="bold",
                 show.legend = FALSE) +
       theme_bw()+
       theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
       theme(axis.text.x = element_text(color="BlACK",size=14)) +
       theme(axis.title.x = element_blank(), 
             axis.text.y = element_blank(),
             axis.ticks.x =  element_blank(),
             axis.ticks.y = element_blank(),
             axis.title.y = element_blank(),
             plot.margin = unit(c(1,1,1,0), "mm"),
             panel.border = element_blank(),
             legend.position="bottom") +
       removeGrid())})
  
  
  dd_tabs <- reactive ({
    Nigeria_DD_Main %>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21 & !is.na(dwo705_extra_tabs)) %>%  
      group_by(Year, Round, dwo705_extra_tabs)%>%
      summarise(Deworm = (round(n()/sum(!is.na(Nigeria_DD_Main_rows()$dwo705_extra_tabs))*100)))
    
  })
  
  output$extra_tabs <- renderPlot ({
    ggplot(data = dd_tabs(), 
           aes(x = 2, y = Deworm, fill= dwo705_extra_tabs ))+
      geom_bar(stat = "identity")+
      ggtitle("Did the school have extra tablets \n after Deworming Day? ")+
      coord_polar("y", start = 200) +
      geom_text(aes(label = paste(Deworm,"%", sep = ""),hjust = "right"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("#F05854","#60BC68"))+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)  
    #annotate(geom = 'text', x = 0.5, y = 0, label = paste0(pole()$Tablet_pole_demonstration[pole()$tto504_tabletpole_explained == 'Yes-Both']))
  })
  
  dd_extra_tabs_actn <- reactive ({
    ng_actions %>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21 & !is.na(dwo706_extr_tabs_actn)) 
    
  })
  
  
  
  output$dd_actn_extras <- renderPlot ({
    treemap( dd_extra_tabs_actn(),
             index = c("label"),
             vSize = "Deworm",
             vColor = "type_1",
             title = "Fate of excess drugs, after Deworming Day")
  })
  
  
  
  submit <- reactive ({
    Nigeria_DD_Main %>%
      filter(State == input$State21 & Year == input$years21 & Round == input$Round21 & !is.na(dwo707a_submit_sum_form)) %>%  
      group_by(Year, Round, dwo707a_submit_sum_form)%>%
      summarise(Deworm = (round(n()/sum(!is.na(Nigeria_DD_Main_rows()$dwo707a_submit_sum_form))*100)))
    
  })
  
  output$Submit_Forms <- renderPlot ({
    ggplot(data = submit(), 
           aes(x = 2, y = Deworm, fill= dwo707a_submit_sum_form ))+
      geom_bar(stat = "identity")+
      ggtitle("When will you submit Form the \n School treatment Summary \n Form Level 1 to the FHLF Staff?")+
      coord_polar("y", start = 200) +
      geom_text(aes(label = paste(Deworm,"%", sep = ""),hjust = "right"), col = "white",
                position = position_stack(vjust = 0.5), 
                size = 5,
                show.legend = T,
                fontface="bold") +
      theme_void() +
      scale_fill_manual(values=c("#F05854","#60BC68", "#F9A33A"))+
      theme(plot.title = element_text(color="Black",  size=20, hjust=0)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "bottom",
            legend.title= element_blank(),
            panel.border = element_blank()) +
      removeGrid() +
      xlim(.5, 2.5)  
    #annotate(geom = 'text', x = 0.5, y = 0, label = paste0(pole()$Tablet_pole_demonstration[pole()$tto504_tabletpole_explained == 'Yes-Both']))
  })
  
  #### treatment numbers
  
  
  ng_dd_present_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Totals)
  })
  
  
  output$ng_dd_present <- renderInfoBox({
    infoBox(
      #h4("Total Present for Deoworming")
      h4(paste("Totals: ", ng_dd_present_all()$Totals)),
      tags$p(style = "font-size: 30px;", ng_dd_present_all()$Totals),
      icon = icon("male")
    )
  })
  
  
  ng_dd_male_present_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Males , perc_male )
  })
  
  
  output$ng_dd_male_present <- renderInfoBox({
    infoBox(
      h4("Males : ", ng_dd_male_present_all()$Males ),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_present_all()$perc_male[[1]]),
      icon = icon("male")
    ) 
  })
  
  
  ng_dd_Female_present_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Females , perc_female )
  })
  
  output$ng_dd_Female_present <- renderInfoBox({
    infoBox(
      h4("Females : ", ng_dd_Female_present_all()$Females ),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_Female_present_all()$perc_female[[1]]),
      icon = icon("female")
    )
  })
  
  ###Took Medicine
  
  ng_dd_tookalb_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Total_took, perc_took)
  })
  
 
  
  output$ng_dd_tookalb <- renderInfoBox({
   infoBox(
      #h4("Total tookalb for Deoworming")
      h4(paste("Total Number that took ALB: (n =", ng_dd_tookalb_all()$Total_took, ")")),
      tags$p(style = "font-size: 30px;", ng_dd_tookalb_all()$perc_took),
      icon = icon("male")
    )
  })
      
  ng_dd_male_tookalb_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Male_took , perc_male_took )
  })
  
  output$ng_dd_male_tookalb <- renderInfoBox({
    infoBox(
      h4(paste("% of Male that took ALB : (n =", ng_dd_male_tookalb_all()$Male_took, ")")),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_tookalb_all()$perc_male_took[[1]]),
      icon = icon("male")
    )
  })
  
  
  ng_dd_Female_tookalb_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Female_took , perc_female_took )
  })
  

  output$ng_dd_Female_tookalb <- renderInfoBox({
    infoBox(
      h4(paste("% of Female that took ALB : (n =", ng_dd_Female_tookalb_all()$Female_took, ")")),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_Female_tookalb_all()$perc_female_took[[1]]),
      icon = icon("female")
    )
  })
  
  ###Teacher Observes
  
  ng_dd_trpresent_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Total_observe, perc_observe)
  })

  
  output$ng_dd_trpresent <- renderInfoBox({
  infoBox(
      #h4("Total trpresent for Deoworming")
      h4(paste("Total Number that : (n =",  ng_dd_trpresent_all()$Total_observe, ")")),
      tags$p(style = "font-size: 30px;", ng_dd_trpresent_all()$perc_observe),
      icon = icon("male")
    )
    })
      
  ng_dd_male_trpresent_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Male_observe , perc_male_observe )
  })
  
  
  
  output$ng_dd_male_trpresent <- renderInfoBox({
    infoBox(
      h4(paste("% of Male that  : (n =", ng_dd_male_trpresent_all()$Male_observe, ")")),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_male_trpresent_all()$perc_male_observe[[1]]),
      icon = icon("male")
    )
  })
  
  
  ng_dd_Female_trpresent_all <- reactive ({
    ng_trt_num_all %>%
     filter(State == input$State21 & Year == input$years21 & Round == input$Round21)%>%
      select(Female_observe , perc_female_observe )
  })
  
  
  
  
  output$ng_dd_Female_trpresent <- renderInfoBox({
    infoBox(
      h4(paste("% of Female that  : (n =", ng_dd_Female_trpresent_all()$Female_observe, ")")),
      tags$p(style = "font-size: 30px; font-weight: bold; color: red", dd_Female_trpresent_all()$perc_female_observe[[1]]),
      icon = icon("female")
    )
    
  })
  
  
#######CES DASHBOARD KENYA
  output$coverage_chart=renderPlot({
    
    ##program reach
    df=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      group_by(cluster, surveyarea) %>% 
      dplyr::summarise(coverage_give = mean(ALB_give_drug)) %>% 
      ungroup() %>%
      as.data.frame(ces_data)
    p1=df %>% 
      group_by(surveyarea) %>%
      dplyr::summarise(mean = round(mean(coverage_give, na.rm = TRUE),2),
                       sd = sd(coverage_give, na.rm = TRUE),
                       n = n()) %>%
      mutate(se = sd / sqrt(n),
             cimin= mean - qt(1 - (0.05 / 2), n - 1) * se,
             cimax= mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
      ggplot(aes(y=mean, x=surveyarea, fill="indianred2")) + geom_col() +
      geom_pointrange(aes(ymin = cimin, ymax = cimax)) + 
      geom_hline(yintercept = 0.75, linetype = "dashed", show.legend = T) + 
      geom_text(aes(label = paste(round((mean*100), 0), "%", sep = "")), hjust =-0.5) + 
      labs(title = "Program Reach", x="", y="Percent") + 
      theme(legend.position = "none") +
      scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), label = scales::percent) ;p1
    
    ##surveyed coverage
    df=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      group_by(cluster, surveyarea) %>% 
      dplyr::summarise(coverage_swallow= mean(ALB_swallow_drug)) %>% 
      ungroup() %>%
      as.data.frame(ces_data)
    p2=df %>% 
      group_by(surveyarea) %>%
      dplyr::summarise(mean = round(mean(coverage_swallow, na.rm = TRUE),2),
                       sd = sd(coverage_swallow, na.rm = TRUE),
                       n = n()) %>%
      mutate(se = sd / sqrt(n),
             cimin= mean - qt(1 - (0.05 / 2), n - 1) * se,
             cimax= mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
      ggplot(aes(y=mean, x=surveyarea, fill="indianred2")) + geom_col() +
      geom_pointrange(aes(ymin = cimin, ymax = cimax)) + 
      geom_hline(yintercept = 0.75, linetype = "dashed") + 
      geom_text(aes(label = paste(round((mean*100), 0), "%", sep = "")), hjust =-0.5) + 
      labs(title = "Survey Coverage", x="", y="") + 
      #scale_y_continuous(labels = scales::percent) +
      ylim(0, 1) + 
      theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank())
    
    grid.arrange(p1, p2, ncol=2)
    
  })
  
  output$disagregation_chart=renderPlot({
    #gender 
    plot1=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      group_by(surveyarea, gender) %>%
      dplyr::summarise(mean = round(mean(ALB_swallow_drug, na.rm = TRUE),2)) %>% 
      ungroup() %>% 
      ggplot(aes(y=mean, x=surveyarea, fill=gender)) + geom_col(width=0.5,position = "dodge") +
      geom_text(aes(label = paste(round((mean*100), 0), "%", sep = "")), position = position_dodge(width = .5), vjust = -.2, size=4) + 
      labs(title = "By Gender", x="", y="", fill="Gender") + 
      scale_fill_manual(values=c("indianred2", "grey50")) +
      theme(axis.text.y =element_blank(), axis.ticks.y = element_blank(),  plot.title = element_text(hjust = 0.5, size=12, face="bold")) +
      ylim(0,1) ;plot1
    #enrollment status
    plot2=ces_data %>%
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      group_by(surveyarea, enrol_status) %>%
      dplyr::summarise(mean = round(mean(ALB_swallow_drug, na.rm = TRUE),2)) %>% 
      ungroup() %>% 
      ggplot(aes(y=mean, x=surveyarea, fill=enrol_status)) + geom_col(width=0.5,position = "dodge") +
      geom_text(aes(label = paste(round((mean*100), 0), "%", sep = "")), position = position_dodge(width = .5), vjust = -.2, size=4) + 
      labs(title = "By Enrollment Status", x="", y="", fill="Enrollment Status") + 
      scale_fill_manual(values=c("indianred2", "grey50")) +
      theme(axis.text.y =element_blank(), axis.ticks.y = element_blank(),  plot.title = element_text(hjust = 0.5, size=12, face="bold")) +
      ylim(0,1);plot2
    #school type
    plot3=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      filter(school_type %in% c("Private", "Public")) %>% 
      group_by(surveyarea, school_type) %>%
      drop_na(school_type) %>% 
      dplyr::summarise(mean = round(mean(ALB_swallow_drug, na.rm = TRUE),2)) %>% 
      ungroup() %>% 
      ggplot(aes(y=mean, x=surveyarea, fill=school_type)) + geom_col(width=0.5,position = "dodge") +
      geom_text(aes(label = paste(round((mean*100), 0), "%", sep = "")), position = position_dodge(width = .5), vjust = -.2, size=4) + 
      labs(title = "By School Type", x="", y="", fill="School Type") + 
      scale_fill_manual(values=c("indianred2", "grey50")) +
      theme(axis.text.y =element_blank(), axis.ticks.y = element_blank(),  plot.title = element_text(hjust = 0.5, size=12, face="bold")) +
      ylim(0,1) ;plot3
    
    grid.arrange(plot1, plot2, plot3, ncol=3, top = textGrob("Survey Coverage Disagregation",gp=gpar(fontsize=13,font=2)))
  })
  
  output$rsn_not_give=renderPlot({
    df=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      select(starts_with("p207s_given_whynot_"), surveyarea) 
    #renaming
    df$'School did not receive drugs'=df$p207s_given_whynot_1
    df$'Drugs finished'=df$p207s_given_whynot_2
    df$'Pregnant'=df$p207s_given_whynot_3
    df$'Breastfeeding'=df$p207s_given_whynot_4
    df$'Ill'=df$p207s_given_whynot_5
    df$'Not at risk for this disease'=df$p207s_given_whynot_6
    df$'Taking other medications'=df$p207s_given_whynot_7
    df$'Already took at home'=df$p207s_given_whynot_10
    df$'Less than 6 years old'=df$p207s_given_whynot_11
    df$'Lack of parent consent'=df$p207s_given_whynot_14
    df$'Child did not have food and water'=df$p207s_given_whynot_15
    df$'Not in the target age'=df$p207s_given_whynot_16
    df$'Child refused'=df$p207s_given_whynot_17
    df$'Not aware'=df$p207s_given_whynot_19
    df$'Child was not at school'=df$p207s_given_whynot_20
    df$'Dont know'=df$p207s_given_whynot_999
    
    df %>% 
      select(!contains("_")) %>% 
      gather(key="key", value="value", -surveyarea) %>% 
      group_by(key, surveyarea) %>% 
      dplyr::summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc)) %>% 
      ggplot(aes(x=perc, y=key, fill=surveyarea)) +
      geom_col(position = "dodge") +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Why was the drug NOT given to you?", size=0.1, y="", x="", fill='Survey Area') +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), position = position_dodge(width = .9), hjust =0, size=4)  +  
      scale_fill_manual(values=c("indianred2", "grey50"))
    
  })
  
  output$rsn_not_swallow=renderPlot({
    df=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      select(starts_with("p208as_swallow_whynot_"), surveyarea) 
    #renaming
    df$'Fear of side effects'=df$p208as_swallow_whynot_1
    df$'Bad taste'=df$p208as_swallow_whynot_2
    df$'Not sick'=df$p208as_swallow_whynot_3
    df$'Not enough information given'=df$p208as_swallow_whynot_4
    df$'My religion is against taking the medication'=df$p208as_swallow_whynot_5
    df$'My parents asked me not to'=df$p208as_swallow_whynot_6
    df$'I did not have food to eat before deworming'=df$p208as_swallow_whynot_7
    df$'Misplaced the drug'=df$p208as_swallow_whynot_8
    df$'Already dewormed at home'=df$p208as_swallow_whynot_10
    
    
    df %>% 
      select(!contains("_")) %>% 
      gather(key="key", value="value", -surveyarea) %>% 
      group_by(key, surveyarea) %>% 
      dplyr::summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc)) %>% 
      ggplot(aes(x=perc, y=key, fill=surveyarea)) +
      geom_col(position = "dodge") +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Why did you not swallow drugs?", size=0.1, y="", x="", fill='Survey Area') +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), position = position_dodge(width = .9), hjust =0, size=4)  +
      scale_fill_manual(values=c("indianred2", "grey50"))
    
  })
  
  output$unprogrammed_chart=renderPlot({
    dfp209=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      select(p209_other_tablet, surveyarea)
    dfp209$'Yes'=ifelse(dfp209$p209_other_tablet=="Yes", 1, 0)
    dfp209$'No'=ifelse(dfp209$p209_other_tablet=="No", 1, 0)
    dfp209$'Dont Know'=ifelse(dfp209$p209_other_tablet=="Don't Know", 1, 0)
    
    cols=c("Yes" = "indianred3", "No" = "grey50", "Dont Know" = "grey20")
    plot1=dfp209 %>% 
      select(!p209_other_tablet) %>% 
      gather(key="key", value="value", -surveyarea) %>% 
      group_by(key, surveyarea) %>% 
      dplyr::summarise(perc=mean(value, na.rm = T)) %>% 
      ggplot(aes(y=perc, x=key, fill=surveyarea)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels=scales::percent) +
      labs(title="Consumed a deworming tablet outside of school in the last six months" , y="", x="") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), position = position_dodge(width = .9), size=4, vjust=-0.1)  + theme(legend.title = element_blank(),  plot.title = element_text(hjust = 0.5, size=10, face="bold")) +
      scale_fill_manual(values=c("indianred2", "grey50"))
    
    ###Where dewormed
    df=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      select(starts_with("p210_other_drug_where_"), surveyarea) 
    #renaming
    df$'Home'=df$p210_other_drug_where_1
    df$'Health Facility'=df$p210_other_drug_where_2
    df$'Pharmacy'=df$p210_other_drug_where_3
    df$'Others'=df$p210_other_drug_where_996
    
    plot2=df %>% 
      select(!contains("_")) %>% 
      gather(key="key", value="value", -surveyarea) %>% 
      group_by(key, surveyarea) %>% 
      dplyr::summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc, .desc = T)) %>% 
      ggplot(aes(y=perc, x=key, fill=surveyarea)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels=scales::percent) +
      labs(title = "Where was the medicine taken?", size=0.1, y="", x="") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), position = position_dodge(width = .9), size=4, vjust=-0.1)  + theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5, size=12, face="bold")) +
      scale_fill_manual(values=c("indianred2", "grey50")) ;plot2
    
    grid.arrange(plot1, plot2, ncol=2, top = textGrob("Unprogrammed Deworming",gp=gpar(fontsize=13,font=2)))
    #grid_arrange_shared_legend(plot1, plot2, ncol=2, top = textGrob("Unprogrammed Deworming",gp=gpar(fontsize=13,font=2)))
    
  })
  
  output$coverage_table1=renderDataTable({
    df=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      group_by(cluster, surveyarea) %>% 
      dplyr::summarise(coverage_give = mean(ALB_give_drug)) %>% 
      ungroup() %>%
      as.data.frame(ces_data)
    df %>% 
      group_by(surveyarea) %>%
      dplyr::summarise(mean = round(mean(coverage_give, na.rm = TRUE),2),
                       sd = sd(coverage_give, na.rm = TRUE),
                       n = n()) %>%
      mutate(se = sd / sqrt(n),
             cimin= mean - qt(1 - (0.05 / 2), n - 1) * se,
             cimax= mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
      mutate(mean=paste(round(mean*100,0),"%",sep = ""), 
             cimin=paste(round(cimin*100,0),"%",sep = ""), 
             cimax=paste(round(cimax*100,0),"%",sep = "")) %>% 
      ungroup() %>% 
      as.data.frame(df) %>%
      select(surveyarea, mean, cimin, cimax, n) %>% 
      datatable(rownames=FALSE, colnames=c('Survey Area', "Program Reach", "95%CI_LB", "95%CI_UB", "#Clusters"), options=list(dom = 't', searching=F,columnDefs = list(list(className = 'dt-left',  targets = "_all"))), caption = 'Program reach and survey coverage by survey area')
  })
  
  output$coverage_table2=renderDataTable({
    df=ces_data %>% 
      filter(Wave == input$Wave12 & Year == input$years12)%>%
      #group_by(cluster, surveyarea, reported_trt) %>% 
      group_by(cluster, surveyarea)  %>% 
      dplyr::summarise(coverage_swallow = mean(ALB_swallow_drug)) %>% 
      ungroup() %>%
      as.data.frame(ces_data)
    df %>% 
      #group_by(surveyarea, reported_trt) %>%
      group_by(surveyarea) %>%
      dplyr::summarise(mean = round(mean(coverage_swallow, na.rm = TRUE),2),
                       sd = sd(coverage_swallow, na.rm = TRUE),
                       n = n()) %>%
      mutate(se = sd / sqrt(n),
             cimin= mean - qt(1 - (0.05 / 2), n - 1) * se,
             cimax= mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
      mutate(mean=paste(round(mean*100,0),"%",sep = ""), 
             cimin=paste(round(cimin*100,0),"%",sep = ""), 
             cimax=paste(round(cimax*100,0),"%",sep = "")) %>% 
      as.data.frame(df) %>%
      #select(surveyarea, mean, cimin, cimax, n, reported_trt) %>% 
      select(surveyarea, mean, cimin, cimax, n) %>% 
      datatable(rownames=FALSE, colnames=c('Survey Area', "Survey Coverage", "95%CI_LB", "95%CI_UB", "#Clusters"), options=list(dom = 't', searching=F,columnDefs = list(list(className = 'dt-left',  targets = "_all"))))
    
  })
  
  
  ####PAKISTAN DATA
  ###TT OBS
  output$pk_sample=renderInfoBox({
    sample=tt_obs %>%
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      count() 
    
    infoBox(
      h4("Sample size"),
      tags$p(style = "font-size: 30px;color:red", sample)
    )
  })
    
  output$pk_att_rate_tcher=renderInfoBox({
    mean=tt_obs %>%
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      summarize(mean=mean(as.numeric(obs9_5a), na.rm = T))
    infoBox(h4("Teacher attendance rate"),tags$p(style = "font-size: 30px;color:red",paste(round((mean/27)*100,0),"%", sep = "")), color = "maroon")
  })
  output$pk_att_tcher=renderInfoBox({
    mean=tt_obs %>%
      summarize(mean=round(mean(as.numeric(obs9_5a), na.rm = T),0))
    infoBox( h4("Avg # of participants at the training"), tags$p(style = "font-size: 30px;color:red",mean), color = "maroon")
  })
  output$pk_att_sch=renderInfoBox({
    mean=tt_obs %>%
      summarize(mean=round(mean(obs9_5b, na.rm = T),0))
    infoBox(  h4("Average # of schools represented"), tags$p(style = "font-size: 30px;color:red",mean), color = "maroon")
  })
  output$pk_att_rate_sch=renderInfoBox({
    mean=tt_obs %>%
      summarize(mean=mean(attendance_rate_sch, na.rm = T))
    infoBox(h4("School attendance rate"),tags$p(style = "font-size: 30px;color:red",paste(round(mean*100,0),"%", sep = "")), color = "maroon")
  })
  output$pk_reg_sheet=renderInfoBox({
    mean=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      filter(!is.na(obs1_6)) %>% 
      count(obs1_6) %>% 
      summarise(prop=round(n/nrow(tt_obs),2)) 
    infoBox(h4("TTC with attendance register"),tags$p(style = "font-size: 30px;color:red",paste(round(mean*100,0),"%", sep = "")),color = "maroon")
  })
  
  output$pk_coverage_graph = renderPlot({
    tt_obs$coverage_worms<-paste0(toupper(substr(tt_obs$coverage_worms, 1, 1)), tolower(substring(tt_obs$coverage_worms, 2)))
    tt_obs$coverage_medicine_doses<-paste0(toupper(substr(tt_obs$coverage_medicine_doses, 1, 1)), tolower(substring(tt_obs$coverage_medicine_doses, 2)))
    tt_obs$coverage_drug_admin<-paste0(toupper(substr(tt_obs$coverage_drug_admin, 1, 1)), tolower(substring(tt_obs$coverage_drug_admin, 2)))
    tt_obs$coverage_forms<-paste0(toupper(substr(tt_obs$coverage_forms, 1, 1)), tolower(substring(tt_obs$coverage_worms, 2)))
    tt_obs$coverage_practice_forms<-paste0(toupper(substr(tt_obs$coverage_practice_forms, 1, 1)), tolower(substring(tt_obs$coverage_practice_forms, 2)))
    #rename variable
    tt_obs$'Information on Worms'=tt_obs$coverage_worms
    tt_obs$'Information on Drugs & Doses'=tt_obs$coverage_medicine_doses
    tt_obs$'Information on Drugs Administration'=tt_obs$coverage_drug_admin
    tt_obs$'Information on Reverse Cascade'= tt_obs$coverage_forms
    tt_obs$'Practice form filling'=tt_obs$coverage_practice_forms
    ##change the order  indianred3
    cols=c("Covered in detail" = "indianred3", "Covered partially" = "grey50", "Not covered" = "grey20")
    tt_obs %>%
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(starts_with("Information"), starts_with("Practice")) %>%
      gather() %>%
      group_by(key) %>% 
      count(value) %>% 
      mutate(perc=n/nrow(tt_obs)) %>% 
      ggplot(aes(x=perc, y=key, fill=value)) +
      geom_col() + 
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Topic Coverage", color="Category") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), position = position_stack(vjust = 0.75), colour = "white", size=4) + 
      ylab("") + xlab("")  +
      theme(legend.position = "bottom", axis.text.x =element_blank(), axis.ticks.x =element_blank()) +
      scale_fill_manual(values=cols)
  })    
  
  output$pk_coverage_pie=renderPlot({
    #all required mats ---pie chart
    cols=c("Covered in detail" = "indianred3", "Covered partially" = "grey50", "Not covered" = "grey20")
    tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(coverage_all_topics) %>% 
      filter(!is.na(coverage_all_topics)) %>% 
      group_by(coverage_all_topics) %>% 
      count(coverage_all_topics) %>% 
      summarise(prop=n/nrow(tt_obs)) %>% 
      rename(Label=coverage_all_topics) %>% 
      ggplot(aes(x = "", y = prop, fill = Label)) +   
      geom_bar(width = 1, stat = "identity") +   
      coord_polar("y", start = 0) + 
      geom_text(aes(y = prop, label = paste(round((prop*100), 0), "%", sep = "")), color = "white", position = position_stack(vjust = 0.5)) + 
      labs(title="Coverage on all topics") +
      scale_fill_manual(values = cols) +   theme_update(plot.title = element_text(hjust =0.5, face="bold")) +
      theme_void() 
    
  })
  
  
  output$pk_mat_drug = renderPlot({
    cols=c("Training Handout" = "indianred3", "Banner" = "indianred3", "Form 1A" = "indianred3", "Form 1B" = "indianred3", "Form 2" = "indianred3", "Melbendazole" = "indianred3", "None" = "grey50")
    df=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(starts_with("obs8_1r_mat_dist_"),"obs8_2_1")
    #renaming
    df$'Training Handout'=df$obs8_1r_mat_dist_1
    df$'Banner'=df$obs8_1r_mat_dist_2
    df$'Form 1A'=df$obs8_1r_mat_dist_3
    df$'Form 1B'=df$obs8_1r_mat_dist_4
    df$'Form 2'=df$obs8_1r_mat_dist_5
    df$'None'=df$obs8_1r_mat_dist_6
    df$'Melbendazole'=df$obs8_2_1
    df %>% 
      select(!contains("_")) %>% 
      gather() %>% 
      group_by(key) %>% 
      summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc, .desc = T)) %>% 
      ggplot(aes(y=perc, x=key, fill=key)) +
      #geom_bar(stat = "identity", position = "dodge") +
      geom_col() +
      labs(title = "Materials and drug distributed to the participants after the training", size=0.1) +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = ""),  size=4), vjust = 1, colour = "grey20", size=4) + 
      ylab("") + xlab("") + theme(legend.position="none",axis.text.y =element_blank(), axis.ticks.y =element_blank()) + scale_fill_manual(values=cols)
    
  })  
  
  output$pk_all_mat_drug = renderInfoBox({
    
    mean=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(obs8_1_all_mats_drugs) %>% 
      filter(!is.na(obs8_1_all_mats_drugs)) %>% 
      summarise(mean=mean(obs8_1_all_mats_drugs, na.rm=F))
    infoBox( h4("% of TTC where all required materials and drug were distributed"), tags$p(style = "font-size: 30px;color:red",paste(round(mean*100,0),"%", sep = "")),color = "maroon")
  })
  
  output$pk_all_mat_pie=renderPlot({
    #all required mats ---pie chart
    tt_obs$obs8_1_all_mats_drugs1=ifelse(tt_obs$obs8_1_all_mats_drugs==1, "Yes", ifelse(tt_obs$obs8_1_all_mats_drugs==0, "No", "NA"))
    cols=c("Yes" = "indianred3", "No" = "grey50", "Not covered" = "grey20")
    tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(obs8_1_all_mats_drugs1) %>% 
      filter(!is.na(obs8_1_all_mats_drugs1)) %>% 
      group_by(obs8_1_all_mats_drugs1) %>% 
      count(obs8_1_all_mats_drugs1) %>% 
      summarise(prop=n/nrow(tt_obs)) %>% 
      rename(Label=obs8_1_all_mats_drugs1) %>% 
      ggplot(aes(x = "", y = prop, fill = Label)) +   
      geom_bar(width = 1, stat = "identity") +   
      coord_polar("y", start = 0) + 
      geom_text(aes(y = prop, label = paste(round((prop*100), 0), "%", sep = "")), color = "white", position = position_stack(vjust = 0.5)) + 
      labs(title="All required materials (forms) and drug distributed") +
      scale_fill_manual(values = cols) +   theme_update(plot.title = element_text(hjust =0.5, face="bold")) +
      theme_void() 
    
  })
  
  output$pk_target_pie=renderPlot({
    cols=c("Yes" = "indianred3", "No" = "grey50", "No " = "grey50")
    df=tt_obs %>%
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(obs3_1) %>% 
      filter(!is.na(obs3_1)) %>% 
      group_by(obs3_1) %>% 
      count(obs3_1) %>% 
      summarise(prop=round(n/nrow(tt_obs),2)) %>% 
      rename(Label=obs3_1)
    ggplot(df,aes(x = "", y = prop, fill = Label)) +   
      geom_bar(width = 1, stat = "identity") +   
      coord_polar("y", start = 0) + 
      geom_text(aes(y = prop, label = paste(round((prop*100), 0), "%", sep = "")), color = "white", position = position_stack(vjust = 0.5)) + 
      labs(title="Trainer discussed target population") +
      scale_fill_manual(values = cols) +   
      theme_void()
  })
  
  output$pk_announced_dd_pie=renderPlot({
    cols=c("Yes" = "indianred3", "No" = "grey50", "No " = "grey50")
    df=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(obs2_4) %>% 
      filter(!is.na(obs2_4)) %>% 
      group_by(obs2_4) %>% 
      count(obs2_4) %>% 
      summarise(prop=round(n/nrow(tt_obs),2)) %>% 
      rename(Label=obs2_4)
    ggplot(df,aes(x = "", y = prop, fill = Label)) +   
      geom_bar(width = 1, stat = "identity") +   
      coord_polar("y", start = 0) + 
      geom_text(aes(y = prop, label = paste(round((prop*100), 0), "%", sep = "")), color = "white", position = position_stack(vjust = 0.5)) + 
      labs(title="Trainer announced the deworming day/week") +
      scale_fill_manual(values = cols) +   
      theme_void()
  })
  output$pk_target=renderInfoBox({
    tt_obs$obs3_1=ifelse(tt_obs$obs3_1=="Yes", 1, 0)
    mean=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(obs3_1) %>% 
      filter(!is.na(obs3_1)) %>% 
      summarise(mean=mean(as.numeric(obs3_1), na.rm = T)) ;mean
    
    infoBox(  h4("Target Popuplation discussed"),tags$p(style = "font-size: 30px;color:red",paste(round(mean*100,0),"%", sep = "")), color = "maroon")
  })
  
  output$pk_tcher_resp=renderPlot({
    
    df=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(starts_with("obs2_3r_roles_tcher_")) 
    #renaming
    df$'Giving deworming tablets to Enrolled Children'=df$obs2_3r_roles_tcher_1
    df$'Giving deworming tablets to Non-Enrolled Children'=df$obs2_3r_roles_tcher_2
    df$'Recording Treatment on Monitoring Forms'=df$obs2_3r_roles_tcher_3
    df$'Raising student and parent awareness and knowledge of DD'=df$obs2_3r_roles_tcher_4
    df$'Passing on Health Messages to Children'=df$obs2_3r_roles_tcher_5
    df$'Handling adverse events'=df$obs2_3r_roles_tcher_6
    #tt_obs_2_3$'Distribute drugs'=tt_obs_2_3$obs2_3r_roles_tcher_7
    df$'Hanging banner at school'=df$obs2_3r_roles_tcher_8
    df$'Not mentioned'=df$obs2_3r_roles_tcher_9
    df %>% 
      select(!starts_with("obs2_3r_roles_tcher_")) %>% 
      gather() %>% 
      group_by(key) %>% 
      summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc)) %>% 
      ggplot(aes(x=perc, y=key, fill=key)) +
      geom_col() +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Roles of the teacher discussed", size=0.1, y="", x="") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), hjust = 1, colour = "grey20", size=4) + theme(legend.position="none", axis.text.x =element_blank(), axis.ticks.x =element_blank()) + scale_fill_manual(values=c( "grey50","indianred3", "indianred3", "indianred3", "indianred3", "indianred3", "indianred3", "indianred3"))
  })
  
  output$pk_coverage_all=renderPlot ({
    cols=c("Covered in detail" = "indianred3", "Covered partially" = "grey50", "Not covered" = "grey20")
    df=tt_obs  %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(coverage_all_topics, region) %>% 
      filter(!is.na(coverage_all_topics)) %>% 
      group_by(region, coverage_all_topics) %>% 
      count(coverage_all_topics) %>% 
      summarise(prop=round(n/nrow(tt_obs),2)) %>%  
      rename(Label=coverage_all_topics)
    
    ggplot(df,aes(x = "", y = prop, fill = Label)) +   
      geom_bar(width = 1, stat = "identity") +   
      coord_polar("y", start = 0) + 
      geom_text(aes(y = prop, label = paste(round((prop*100), 0), "%", sep = "")), color = "white", position = position_stack(vjust = 0.5)) +
      labs(title="Trainer announced the deworming day/week") +
      scale_fill_manual(values = cols) +   
      theme_void()
  })
  
  
  output$pk_resp_health=renderPlot ({
    
    cols=c("Discuss Deworming Day at Health Days" = "indianred3", "Support Teachers in managing SAE" = "indianred3", "None" = "grey50")
    tt_obs %>%
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(starts_with("obs2_3a_roles_health_")) %>% 
      rename('Discuss Deworming Day at Health Days' =obs2_3a_roles_health_1, 'Support Teachers in managing SAE' = obs2_3a_roles_health_2, 'None'=obs2_3a_roles_health_8) %>% 
      gather() %>% 
      group_by(key) %>% 
      summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc)) %>% 
      ggplot(aes(x=perc, y=key, fill=key)) +
      geom_col() +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Responsibility of the health officers discussed", size=0.1, x="", y="") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), vjust = 0.5, colour = "grey20", size=4) + theme(legend.position="none",axis.text.x =element_blank(), axis.ticks.x =element_blank()) + scale_fill_manual(values=cols)
  })
  
  
  output$pk_not_treated=renderPlot ({
    df=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(starts_with("obs5_1a_not_treated_")) 
    #renaming
    df$'Any child sick on deworming day'=df$obs5_1a_not_treated_1
    df$'Any child currently on medication'=df$obs5_1a_not_treated_2
    df$'Any child with a history of epilepsy,fit,sseizures'=df$obs5_1a_not_treated_3
    df$'Any pregnant female student'=df$obs5_1a_not_treated_4
    df$'Any child with allergy'=df$obs5_1a_not_treated_5
    df$'Not Covered'=df$obs5_1a_not_treated_8
    
    df %>% 
      select(!starts_with("obs5_1a_not_treated_")) %>% 
      gather() %>% 
      group_by(key) %>% 
      summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc)) %>% 
      ggplot(aes(x=perc, y=key, fill=key)) +
      geom_col() +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Children not to be treated discussed", size=0.1, y="", x="") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), hjust = 1, colour = "grey20", size=4) + theme(legend.position="none", axis.text.x =element_blank(), axis.ticks.x =element_blank()) + scale_fill_manual(values=c( "grey50", "indianred3", "indianred3", "indianred3", "indianred3",  "indianred3"))
    
  })
  
  output$pk_mobilize=renderPlot ({
    
    df=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(starts_with("obs7_1r_mobilize_")) 
    #renaming
    df$'Conduct Health Education in Class'=df$obs7_1r_mobilize_1
    df$'Display banners in the School'=df$obs7_1r_mobilize_2
    df$'Discuss Deworming Day at School Management Meetings'=df$obs7_1r_mobilize_3
    df$'Discuss deworming day at school assemblies'=df$obs7_1r_mobilize_4
    df$'Encourage Children to share Deworming Day Information with Parents'=df$obs7_1r_mobilize_5
    df$'Encourage children to tell non-enrolled friends to come to the school on MDA'=df$obs7_1r_mobilize_6
    df$'Not Covered'=df$obs7_1r_mobilize_8
    
    df %>% 
      select(!starts_with("obs7_1r_mobilize_")) %>% 
      gather() %>% 
      group_by(key) %>% 
      summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc)) %>% 
      ggplot(aes(x=perc, y=key, fill=key)) +
      geom_col() +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Areas of community mobilization discussed", size=0.1, x="", y="") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), hjust = 1, colour = "grey20", size=4)  + 
      theme(legend.position="none", axis.text.x =element_blank(), axis.ticks.x =element_blank()) + scale_fill_manual(values=c("grey50", "indianred3", "indianred3", "indianred3", "indianred3", "indianred3", "indianred3"))
    
  }) 
  output$pk_mostaskedquix=renderPlot({
    
    df=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(starts_with("obs9_4r1_mostquiz_")) 
    #renaming
    df$'The Problem Of Worms'=df$obs9_4r1_mostquiz_1
    df$'Why School-Based Deworming'=df$obs9_4r1_mostquiz_2
    df$'Target Population'=df$obs9_4r1_mostquiz_3
    df$'How To Administer Tablets'=df$obs9_4r1_mostquiz_4
    df$'Cautions & Severe Adverse Effects'=df$obs9_4r1_mostquiz_5
    df$'Community Sensitization'=df$obs9_4r1_mostquiz_6
    df$'How To Fill Out Reporting Forms'=df$obs9_4r1_mostquiz_7
    df$'Submitting Reporting Forms'=df$obs9_4r1_mostquiz_8
    df$'Side Effects'=df$obs9_4r1_mostquiz_9
    df$'Funds'=df$obs9_4r1_mostquiz_10
    
    df %>%
      select(!contains("_")) %>% 
      gather() %>% 
      group_by(key) %>% 
      summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc)) %>% 
      ggplot(aes(x=perc, y=key, fill="indianred3")) +
      geom_col() +
      scale_x_continuous(labels=percent) +
      labs(title = "Areas that participants asked the most questions", size=0.1, y="",x="") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), hjust = 1, colour = "grey20", size=4) +
      theme(legend.position = "none", axis.text.x =element_blank(), axis.ticks.x =element_blank())
    
  })
  
  output$pk_covid19=renderPlot({
    
    tt_obs$obs9_7_yes=ifelse(tt_obs$obs9_7=="Yes",1, 0)
    tt_obs$obs9_8_yes=ifelse(tt_obs$obs9_8=="Yes",1, 0)
    tt_obs$obs9_10_yes=ifelse(tt_obs$obs9_10=="Yes",1, 0)
    tt_obs$obs9_11_yes=ifelse(tt_obs$obs9_11=="Yes",1, 0)
    
    df=tt_obs %>% 
      filter(Province == input$Province2 & Year == input$years14 & Round == input$Round14 )%>%
      select(obs9_7_yes,obs9_8_yes,obs9_10_yes,obs9_11_yes) 
    #renaming
    df$'All the trainers wearing protective mask'=df$obs9_7_yes
    df$'All participants wearing protective mask'=df$obs9_8_yes
    df$'Handwashing or sanitizor available at TTC'=df$obs9_10_yes
    df$'TTC cleaned before the trainig'=df$obs9_11_yes 
    
    df %>%
      select(!contains("_")) %>% 
      gather() %>% 
      group_by(key) %>% 
      summarise(perc=mean(value, na.rm = T)) %>% 
      mutate(key=fct_reorder(key, perc)) %>% 
      ggplot(aes(y=key, x=perc, fill="indianred3")) +
      geom_col() +
      scale_x_continuous(labels=percent) +
      labs(title = "COVID-19 Adherence", size=0.1, x="", y="") +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), vjust = 0.5, colour = "grey20", size=4) + theme(legend.position="", axis.text.x =element_blank(), axis.ticks.x =element_blank())
    
  })
  
  ####TT PRE POST APP######################################
  ##Training Knowledge
  knowledge_pre <- reactive({
  Pakistan_tt_data_pre %>% 
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    summarise(mean(Kowledge_drg_dose_age))*100
  })
  
  knowledge_post <- reactive({
    Pakistan_tt_data_post %>% 
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    summarise(mean(Kowledge_drg_dose_age))*100 })
  
  output$pk_box1 <- renderInfoBox({
    infoBox(
              h4(paste("STH medicine, dosage & target age knowledge - pre training")),
              tags$p(style = "font-size: 30px;color:red",paste(round(knowledge_pre(), 0),"%", sep="")), color = "maroon")  
  })
  output$pk_box2 <- renderInfoBox({
    infoBox(h4(paste("STH medicine, dosage & target age knowledge - post training")), 
            tags$p(style = "font-size: 30px;color:red",paste(round(knowledge_post(), 0),"%", sep="")),color = "maroon")
  })
  
  ##Kowledge_forms
  Kowledge_forms_pre <- reactive({
    Pakistan_tt_data_pre %>%
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    summarise(mean(Kowledge_forms))*100
  })
  
  Kowledge_forms_post <- reactive({
    Pakistan_tt_data_post %>%
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    summarise(mean(Kowledge_forms))*100 
  })
  
  output$pk_box3 <- renderInfoBox({
    infoBox(            h4(paste("Knowledge on treatment forms - pre training")),
                        tags$p(style = "font-size: 30px;color:red",paste(round(Kowledge_forms_pre(), 0),"%", sep="")),color = "maroon")  
  })
  output$pk_box4 <- renderInfoBox({
    infoBox(h4(paste("Knowledge on treatment forms - post training")),
            tags$p(style = "font-size: 30px;color:red",paste(round(Kowledge_forms_post(), 0),"%", sep="")),color = "maroon")
  })
  
  ##Knwldg_mld_effct_hndlng_stps_all
  sae_pre <- reactive({
    Pakistan_tt_data_pre %>% 
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    summarise(mean(Knwldg_mld_effct_hndlng_stps_all))*100 })
  
  sae_post <- reactive({
    Pakistan_tt_data_post %>% 
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    summarise(mean(Knwldg_mld_effct_hndlng_stps_all))*100 })
  
  output$pk_box5 <- renderInfoBox({
    infoBox(h4(paste("Mention of all steps handling mild-effects - Pre training")),
            tags$p(style = "font-size: 30px;color:red",paste(round(sae_pre(), 0),"%", sep="")), color = "maroon")  
  })
  output$pk_box6 <- renderInfoBox({
    infoBox(h4(paste("Mention of all steps handling mild-effects - Post training")),
            tags$p(style = "font-size: 30px;color:red",paste(round(sae_post(), 0),"%", sep="")),color = "maroon")
  })
  
  ##Arrival on time
  Timely_arrival_all <- reactive({
    Pakistan_tt_data_pre %>%
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    summarise(mean(as.numeric(Timely_arrival))*100) })
  
  output$pk_box7 <- renderInfoBox({
    infoBox(h4(paste("% Respondents who arrived on time")),
            tags$p(style = "font-size: 30px;color:red",paste(round(Timely_arrival_all()),"%", sep="")),color = "maroon")  
  })
  ##STH target age group  
  knowledge_agegrp <- reactive({
    Pakistan_tt_data %>%
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
      select(po2_8, survey) %>% #gather()%>% 
    group_by(po2_8, survey) %>% count(po2_8) %>% 
    mutate(perc_post=n/nrow(Pakistan_tt_data_post))
  
  })
  
  output$pk_STH_age <- renderPlot({  
    ggplot(knowledge_agegrp(), aes( x=perc_post, y=po2_8, fill=survey)) +
      geom_col(position = "dodge") +
      labs(title = "Agegroup treated for STH", size=0.1) + ylab(NULL) + xlab(NULL) +
      scale_x_continuous(labels=scales::percent) +
      geom_text(aes(label = paste(round((perc_post*100), 0), "%", sep = "")), hjust = 1, 
                position = position_dodge(width = .9), hjust = -.1, colour = "grey20", size=4) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.ticks.x = element_blank(), panel.border = element_blank()) +
      scale_fill_manual(values=cols)        
  })
  
  ##STH Drug dosage in treating STH  
  knowledge_dose <- reactive({
    Pakistan_tt_data %>%
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
      select(po2_7, survey) %>%
       group_by(po2_7, survey) %>% count(po2_7) %>% 
    mutate(perc=n/nrow(Pakistan_tt_data_post))
 
  })
  
  output$pk_STH_dosage <- renderPlot({  
    ggplot(knowledge_dose(), aes( x=perc, y=po2_7, fill=survey)) +
      geom_col(position = "dodge") +
      labs(title = "STH Dosage", size=0.1) + ylab(NULL) + xlab(NULL) +
      scale_x_continuous(labels=scales::percent) +
      geom_text(aes(label = paste(round((perc*100), 0), "%", sep = "")), hjust = 1, 
                position = position_dodge(width = .9), hjust = -.1, colour = "grey20", size=4) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.ticks.x = element_blank(), panel.border = element_blank()) +
      scale_fill_manual(values=cols)
  })
  
  ##Drug used in treating STH  
  knowledge_drug <- reactive({
    knowledge_drug_post <- Pakistan_tt_data %>% 
    filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
      select(po2_6, survey) %>%   group_by(po2_6, survey) %>% count(po2_6) %>% 
    mutate(perc_post=n/nrow(Pakistan_tt_data_post))
  
  })
  
  output$pk_STH_treatment_drug <- renderPlot({  
    ggplot(knowledge_drug(), aes( x=perc_post, y=po2_6, fill=survey)) +
      geom_col(position = "dodge") +
      labs(title = "Drug used for STH treatment", size=0.1) +
      ylab(NULL) + xlab(NULL) +
      scale_x_continuous(labels=scales::percent) +
      geom_text(aes(label = paste(round((perc_post*100), 0), "%", sep = "")), hjust = 1, 
                position = position_dodge(width = .9), hjust = -.1, colour = "grey20", size=4) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.ticks.x = element_blank(), panel.border = element_blank()) +
      scale_fill_manual(values=cols)
  })
  
  ##How children are infected with worms
  output$pk_How_infected <- renderPlot({
    data_how_infected=Pakistan_tt_data %>% 
      filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
      select(starts_with("po2_5r_"), survey) %>% 
      group_by(survey) %>% 
      summarise_all(mean) %>% 
      gather(percentage, key=Vars, po2_5r_1:po2_5r_999)
    
    data_how_infected$Vars[data_how_infected$Vars == "po2_5r_1"] <- 'Walking in bare feet'
    data_how_infected$Vars[data_how_infected$Vars == "po2_5r_2"] <- "Not washing hands after using toilets"
    data_how_infected$Vars[data_how_infected$Vars == "po2_5r_3"] <- 'Eating foods without washing hands'
    data_how_infected$Vars[data_how_infected$Vars == "po2_5r_4"] <- 'Absence of latrine'
    data_how_infected$Vars[data_how_infected$Vars == "po2_5r_5"] <- 'Eating unwashed fruits and vegetables'
    data_how_infected$Vars[data_how_infected$Vars == "po2_5r_6"] <- 'Eating improperly cooked food'
    data_how_infected$Vars[data_how_infected$Vars == "po2_5r_7"] <- 'Other: Poor hygeine practices'
    data_how_infected$Vars[data_how_infected$Vars == "po2_5r_999"] <- 'Dont Know'
    
    ggplot(data_how_infected, aes(x = percentage, y = Vars, group=survey, fill = survey)) +
      geom_col(position = "dodge") +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "How children get infected", size=0.1) +
      geom_text(aes(label = paste(round((percentage*100), 0), "%", sep = "")), 
                position = position_dodge(width = .9), hjust = -.1, colour = "grey20") + 
      ylab("") + xlab("")  + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.border = element_blank()) +
      scale_fill_manual(values=cols)
  })
  
  ##Side effects
  data_side_effects <- 
    Pakistan_tt_data %>% 
   # filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    select(starts_with("po2_9r_"), survey) %>%
    group_by(survey) %>% 
    summarise_all(mean) %>% 
    gather(percentage, key=Vars, po2_9r_1:po2_9r_999 )
  
  data_side_effects$Vars[data_side_effects$Vars == "po2_9r_1"] <- 'Headache'
  data_side_effects$Vars[data_side_effects$Vars == "po2_9r_2"] <- "Nausea"
  data_side_effects$Vars[data_side_effects$Vars == "po2_9r_3"] <- 'Mild abdominal discomfort'
  data_side_effects$Vars[data_side_effects$Vars == "po2_9r_4"] <- 'Vomiting'
  data_side_effects$Vars[data_side_effects$Vars == "po2_9r_5"] <- 'Fainting'
  data_side_effects$Vars[data_side_effects$Vars == "po2_9r_6"] <- 'Diarrhea'
  data_side_effects$Vars[data_side_effects$Vars == "po2_9r_7"] <- 'Fatigue'
  data_side_effects$Vars[data_side_effects$Vars == "po2_9r_999"] <- 'Dont Know'
  
  
  output$pk_Side_effects <- renderPlot({
    ggplot(data_side_effects, aes(x = percentage, y = Vars, group=survey, fill = survey)) +
      geom_col(position = "dodge") +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Mild side effects", size=0.1) +
      geom_text(aes(label = paste(round((percentage*100), 0), "%", sep = "")), 
                position = position_dodge(width = .9), hjust = -.1, colour = "grey20") + 
      ylab("") + xlab("")  + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.border = element_blank()) +
      scale_fill_manual(values=cols)
  })
  
  ##Sensitization activities
  output$pk_sensitization_activities <- renderPlot({
    data_how_infected <- Pakistan_tt_data %>% 
      filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
      select(starts_with("po3_9r_"), survey) %>%
      group_by(survey) %>% 
      summarise_all(mean) %>% 
      gather(percentage, key=Vars, po3_9r_1:po3_9r_999 )
    
    data_how_infected$Vars[data_how_infected$Vars == "po3_9r_1"] <- 'Display Banners'
    data_how_infected$Vars[data_how_infected$Vars == "po3_9r_2"] <- 'Mention DD during school assemblies'
    data_how_infected$Vars[data_how_infected$Vars == "po3_9r_3"] <- 'Conduct health education in classrooms'
    data_how_infected$Vars[data_how_infected$Vars == "po3_9r_4"] <- 'Cover in School Management Meetings-PTA Meetings'
    data_how_infected$Vars[data_how_infected$Vars == "po3_9r_6"] <- 'Other: Contact and run a campaign alongside area administrator'
    data_how_infected$Vars[data_how_infected$Vars == "po3_9r_7"] <- 'Other: Sensitization via WhatsApp'
    data_how_infected$Vars[data_how_infected$Vars == "po3_9r_999"] <- 'Dont Know'
    
    ggplot(data_how_infected, aes(x = percentage, y = Vars, group=survey, fill = survey)) +
      geom_col(position = "dodge") +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Sensitization activities", size=0.1) +
      geom_text(aes(label = paste(round((percentage*100), 0), "%", sep = "")), 
                position = position_dodge(width = .9), hjust = -.1, colour = "grey20") + 
      ylab("") + xlab("")  + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.border = element_blank()) +
      scale_fill_manual(values=cols)
  })       
  
  ##Children/Persons not treated
  output$pk_child_not_treated <- renderPlot({
    data_chld_not_treated <- Pakistan_tt_data %>%
      filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
      select(starts_with("po2_10r_"), survey) %>%
      group_by(survey) %>% 
      summarise_all(mean) %>% 
      gather(percentage, key=Vars, po2_10r_1:po2_10r_999 )
    
    data_chld_not_treated$Vars[data_chld_not_treated$Vars == "po2_10r_1"] <- 'Sick children'
    data_chld_not_treated$Vars[data_chld_not_treated$Vars == "po2_10r_2"] <- 'Children under any medication'
    data_chld_not_treated$Vars[data_chld_not_treated$Vars == "po2_10r_3"] <- 'Children with known allergy to deworming medicine'
    data_chld_not_treated$Vars[data_chld_not_treated$Vars == "po2_10r_4"] <- 'Children with a history of seizures'
    data_chld_not_treated$Vars[data_chld_not_treated$Vars == "po2_10r_5"] <- 'Other: Children who are perfcetly alright'
    data_chld_not_treated$Vars[data_chld_not_treated$Vars == "po2_10r_6"] <- 'Other: Children who dewormed within the last month'
    data_chld_not_treated$Vars[data_chld_not_treated$Vars == "po2_10r_7"] <- 'Other: Elderly'
    data_chld_not_treated$Vars[data_chld_not_treated$Vars == "po2_10r_999"] <- 'Dont Know'
    
    ggplot(data_chld_not_treated, aes(x = percentage, y = Vars, group=survey, fill = survey)) +
      geom_col(position = "dodge") +
      scale_x_continuous(labels=scales::percent) +
      labs(title = "Children not treated", size=0.1) +
      geom_text(aes(label = paste(round((percentage*100), 0), "%", sep = "")), 
                position = position_dodge(width = .9), hjust = -.1, colour = "grey20") + 
      ylab("") + xlab("")  + 
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.border = element_blank()) +
      scale_fill_manual(values=cols)    
  })
  
  ##Training improvement areas
  improvement_area <- 
    Pakistan_tt_data %>% 
    #filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    select(starts_with("po4_5r_"))%>% 
    summarise_all(mean, na.rm=T) %>% 
    mutate_all(funs(paste(round(.,4)*100, "%", sep = " "))) %>% 
    gather('Proportion', key='Areas of improvement', po4_5r_1:po4_5r_998 )
  
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_1"] <- "Seating arrangement should be improved"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_2"] <- "Such programs should be conducted frequently"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_3"] <- "Give literate persons opportunities to guide parents in a better way"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_4"] <- "Proper guideline and clarity on the program should be given"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_5"] <- "Punctuality"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_6"] <- "Trainers should have a medical background"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_7"] <- "Use technology in training and sensitization"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_8"] <- "Good food should be given"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_9"] <- "Teaching method should involve use of examples, projectors, charts and videos"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_11"] <- "Ensure the cleanliness of the venue"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_13"] <- "Training should be conducted after 20th March each year"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_14"] <- "Trained teachers should not participate"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_15"] <- "Have participation of two persons from each school as mandatory"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_16"] <- "People were less involved"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_17"] <- "Alternate power sources should be arranged for"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_18"] <- "Have training organized at school level for proper responsibility and results"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_19"] <- "Have the information dispensed not just to teachers"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_20"] <- "Have detailed demonstrations during the training"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_22"] <- "Govt should organize a workshop on it"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_23"] <- "Material should be availed"
  improvement_area$'Areas of improvement'[improvement_area$'Areas of improvement' =="po4_5r_998"] <- "None"
 
  
  output$pk_improvement_area <- DT::renderDataTable({
    DT::datatable( improvement_area , caption = 'Table 1: Areas mentioned for improvement by respondents',
                   options = list(dom = 'Bfrtip',lengthChange = FALSE,
                                  searching = FALSE,dom = 't',bSort=FALSE,paging = FALSE,scrollX = TRUE,pageLength = 105,pagingType ='numbers',
                                  info = FALSE,columnDefs = list(list(className = 'dt-left',  targets = "_all"))))
    
  })
  
  ##Feedback from Training
  feedback_provided <-  
    Pakistan_tt_data %>% 
    #filter(Province == input$Province & Year == input$years13 & Round == input$Round13 )%>%
    select(starts_with("po4_6r_"))%>% 
    summarise_all(mean, na.rm=T) %>% 
    mutate_all(funs(paste(round(.,4)*100, "%", sep = " "))) %>% 
    gather('Proportion', key='Feedback', po4_6r_1:po4_6r_998 )
  
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_1"] <- "Proper seating arrangement should be made"      
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_2"] <- "More such programs should be organized"      
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_3"] <- "Good for children"      
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_4"] <- "Material (banners and training booklets) should be provided"      
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_5"] <- "Create more awareness through media outlets and social media"      
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_6"] <- "Parents should be invited alongside teachers"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_7"] <- "A detailed practice session should be arranged concerning filling of forms"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_8"] <- "Conduct sensitization to the whole community"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_9"] <- "Use projectors and include videos in the trainings"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_10"] <- "Training was good and satisfactory"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_11"] <- "Training should not be in the school"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_12"] <- "Pancuality should be observed"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_13"] <- "Visit to be made in the schools"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_14"] <- "Arrange for a centralized training for all schools"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_15"] <- "Religious leaders should also be included to enhance reach"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_16"] <- "At least two teachers should be nominated from each school"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_17"] <- "Trainers from medical background should be called for training"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_18"] <- "Feedbacks from previous training sessions should be discussed"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_19"] <- "Provide consent forms to students"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_20"] <- "Have tablets administered under supervision of a doctor"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_21"] <- "Certificate should be provided to teachers attending trainings"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_22"] <- "Medical centre should be in close proximity"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_23"] <- "Teachers training should be provided according to the number of students"
  feedback_provided$'Feedback'[feedback_provided$'Feedback' == "po4_6r_998"] <- "None"

    
  output$pk_feedback <- DT::renderDataTable({
    DT::datatable( feedback_provided , caption = 'Table 2: Feedback provided by respondents',
                   options = list(dom = 'Bfrtip',lengthChange = FALSE,
                                  searching = FALSE,dom = 't',bSort=FALSE,paging = FALSE,scrollX = TRUE,pageLength = 105,pagingType ='numbers',
                                  info = FALSE,columnDefs = list(list(className = 'dt-left',  targets = "_all"))))
  })
  
}
