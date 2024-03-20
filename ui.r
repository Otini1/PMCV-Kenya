# Load the required packages
library(haven)
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
library(readxl)
library(treemap)
library(flexdashboard)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(waffle)
library(plotly)
library(shinyWidgets)


shinyUI(navbarPage(
  
  id="navbar", title = "DTW PROGRAM DASHBOARDS ",inverse = T, position = "fixed-top",
  
  tabPanel("Home  Page",
           title = "Home", 
           value = "home",
           hr(),
           br(), br(),
           
           HTML("<h1><center>WELCOME TO <b>MLE DTW PROGRAM DASHBOARDS </b> WEBPAGE...</center></h1>"),
           
           br(), br(), br(), br(),
           
           br(), br(), br(), br(),
           
           column(4,align = "center",
                  actionButton('dtw_Kenya', label = HTML("<span style='font-size:2em;'>DTW KENYA PMCV <br /> DASHBOARDS</span>"),
                               style="color: #fff; background-color: #DC143C; border-color: #2e6da4; font-size:200%'",
                               size = "large")),
           column(4, align = "center",
                  actionButton('dtw_Nigeria',  label = HTML("<span style='font-size:2em;'>DTW NIGERIA PMCV <br /> DASHBOARDS</span>"),
                               style="color: #fff; background-color: darkgreen; border-color: #2e6da4; font-size:200%'",
                               size = "large")),
           column(4,align = "center",
                  actionButton('dtw_Pakistan',  label = HTML("<span style='font-size:2em;'>DTW PAKISTAN PMCV <br /> DASHBOARDS</span>"),
                               style="color: #fff; background-color: #FF4500; border-color: #2e6da4; font-size:200%'",
                               size = "large")),
           br(),br(),br(), br(),br(),br(),
           HTML("<h1><b>DTW PROGRAM DASHBOARDS</b></h1>"),
           HTML("<h4>This dashboard(s) provides statistics for select KPIs for a selected implementation period. In instances where the activity is still on-going, the dashboards provide some quick insights on program implementation. Please note that statistics at this stage are only indicative and may differ from the final figures once
                               the data has been cleaned and analyzed. Upon activity completion, the dashboards are based upon cleaned data.")),
  
  tabPanel("DTW Kenya",value = "dtw1" ,
           tabPanel("DTW Kenya", br(), br(), 
                    fluidPage(
                      tags$head(
                        tags$style("label{font-family: Arial;}")
                      ),
                      tags$style('.container-fluid {
                             background-color: teal;
              }'),
                      tabsetPanel(
                        tabPanel("SCT Dashboards", br(),
                                 tabsetPanel(
                                   tabPanel("SCT Post Dashboard",br(),
                                            fluidPage(
                                              tags$head(
                                                tags$style("label{font-family: Arial;}")),
                                              h1(textOutput("SCTPost_title")), class = "active",
                                              
                                              fluidRow(column(4, selectInput("County", "County", choices = c("Kenya","Bomet","Bungoma", "Busia", "Kisumu","Kericho", "Kirinyaga","Homabay", "Kisii","Nandi", "Narok","Nyamira", "Siaya", "Trans Nzoia","Vihiga"), multiple = F, selected = "Kenya")),
                                                       column(4,selectInput("years", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 9")),
                                                       column(4,selectInput("Wave","Wave", choices = c(1,2),multiple = F, selected = 1))),
                                              
                                              
                                              
                                              fluidRow(column(3, plotOutput("trt_type", height = "300px" )),
                                                       column(3, plotOutput("sick", height = "300px")),
                                                       column(3, plotOutput("FormP", height = "300px")),
                                                       column(3, plotOutput("No_Forms", height = "300px"))),
                                              
                                              
                                              
                                              fluidRow(column(6,style = "background-color:#CCFFCC;", height = "20px",
                                                              h3(id= "dd7","STH Knowledge")),
                                                       column(6,style = "background-color:#CCFFCC;", height = "20px",
                                                              h3(id= "dd7","SCH Knowledge"))),
                                              
                                              tags$style(".small-box.bg-aqua { background-color: pink !important; color: #000000 !important; }"),
                                              
                                              fluidRow(column(2, valueBoxOutput("sth_drugs", width = 15)),
                                                       column(2, infoBoxOutput("sth_dosage", width = 15)),
                                                       column(2, infoBoxOutput("sth_age", width = 15)),
                                                       column(2, infoBoxOutput("sch_drugs", width = 15)),
                                                       column(2, infoBoxOutput("sch_dosage", width = 15)),
                                                       column(2, infoBoxOutput("sch_age", width = 15))),
                                              
                                              
                                              br(),
                                              br(),
                                              fluidRow(column(6,DT ::dataTableOutput(outputId = "subcounties")),
                                                       column(6,DT ::dataTableOutput("fb")))
                                            )
                                   ),
                                   tabPanel("SCT OBS Dashboards", br(),
                                            fluidPage(
                                              tags$head(
                                                tags$style("label{font-family: Arial;}")),
                                              fluidRow(column(4, selectInput("County2", "County", choices = c("Kenya","Bomet","Bungoma", "Busia", "Kisumu","Kericho", "Kirinyaga","Homabay", "Kisii","Nandi", "Narok","Nyamira", "Siaya", "Trans Nzoia","Vihiga"), multiple = F, selected = "Kenya")),
                                                       column(4,selectInput("years2", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 9")),
                                                       column(4,selectInput("Wave2","Wave", choices = c(1,2),multiple = F, selected = 1))),
                                              
                                              h1(textOutput("SCTOBS_title")), class = "active",
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","COVID Preparations"))),
                                              fluidRow(column(4, 
                                                              (plotOutput("trainers_masks", height = "300px" ))),
                                                       column(4, 
                                                              (plotOutput("participants_masks", height = "300px"))),
                                                       column(4, 
                                                              (plotOutput("sanitizer", height = "300px")))),
                                              
                                              fluidRow(column(4, 
                                                              (plotOutput("sanitize_venue", height = "300px" ))),
                                                       column(4, 
                                                              (plotOutput("num_parts_mask", height = "300px")))),
                                              br(),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","Attendance Rate"))),
                                              br(),
                                              fluidRow(column(4, 
                                                              (plotOutput("attendance", height = "300px" ))),
                                                       column(4, 
                                                              (plotOutput("booklets", height = "300px"))),
                                                       (column(2, 
                                                               (infoBoxOutput("box1", width = 15)),
                                                               (infoBoxOutput("box2", width = 15)))),
                                                       (column(2, infoBoxOutput("box3", width = 15
                                                       ),
                                                       infoBoxOutput("box4", width = 15)))),
                                              
                                              br(),
                                              br(),
                                              fluidRow(column(6, 
                                                              (plotOutput("coverage", height = "400px" ))),
                                                       column(6, plotOutput("not_treated", height = "400px" ))),
                                              
                                              br(),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","Roles and Responsibilities"))),
                                              br(),
                                              fluidRow(column(6, 
                                                              (plotOutput("Roles_chew", height = "400px" ))),
                                                       column(6, plotOutput("Roles_HT", height = "400px" ))), 
                                              br(),
                                              br(),
                                              fluidRow(column(6, 
                                                              (plotOutput("Drug_Availability", height = "400px" ))),
                                                       column(6, plotOutput("Questions_Asked", height = "400px" ))),
                                              br(),
                                              br(),
                                              fluidRow(column(6,DT ::dataTableOutput(outputId = "Subcs")))
                                            )),
                                   tabPanel("SCT CHEW Dashboards", br(),
                                            fluidPage(
                                              h1(textOutput("SCTCHEW_title")), class = "active",
                                              fluidRow(column(4, selectInput("County3", "County", choices = c("Kenya","Bomet","Bungoma", "Busia", "Kisumu","Kericho", "Kirinyaga","Homabay", "Kisii","Nandi", "Narok","Nyamira", "Siaya", "Trans Nzoia","Vihiga"), multiple = F, selected = "Kenya")),
                                                       column(4,selectInput("years3", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 9")),
                                                       column(4,selectInput("Wave3","Wave", choices = c(1,2),multiple = F, selected = 1))),
                                              
                                              fluidRow(column(6, plotOutput("chew_trt_type", height = "120px" )),
                                                       column(6, plotOutput("Role_Chew", height = "120px"))),
                                              br(),
                                              fluidRow(column(6,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","CHEW STH Knowledge")),
                                                       column(6,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","CHEW SCH Knowledge"))),
                                              fluidRow(column(2, infoBoxOutput("chew_sth_drugs", width = 15)),
                                                       column(2,infoBoxOutput("chew_sth_dosage", width = 15)),
                                                       column(2, infoBoxOutput("chew_sth_age",width = 15)),
                                                       column(2, infoBoxOutput("chew_sch_drugs", width = 15)),
                                                       column(2,infoBoxOutput("chew_sch_dosage", width = 15)),
                                                       column(2, infoBoxOutput("chew_sch_age",width = 15))),
                                              
                                              br(),
                                              
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","CHEW Side Effects"))),
                                              fluidRow(column(6, plotOutput("sth_side_effects", height = "300px")),
                                                       column(6, plotOutput("sch_side_effects", height = "300px"))),
                                              br(),
                                              
                                              fluidRow(column(6,DT ::dataTableOutput(outputId = "chew_subcounties"))))) 
                                 )), 
                        tabPanel("TT Dashboards", 
                                 br(),
                                 tabsetPanel(
                                   tabPanel("TT PRE-POST DASHBOARDS",br(),
                                            fluidPage(
                                              fluidRow(column(4, selectInput("County4", "County", choices = c("Kenya","Bomet","Bungoma", "Busia", "Kisumu","Kericho", "Kirinyaga","Homabay", "Kisii","Nandi", "Narok","Nyamira", "Siaya", "Trans Nzoia","Vihiga"), multiple = F, selected = "Kenya")),
                                                       column(4,selectInput("years4", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 9")),
                                                       column(4,selectInput("Wave4","Wave", choices = c(1,2),multiple = F, selected = 1))),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","STH Knowledge Pre-Post Comparison"))),
                                              fluidRow(column(12,style = "background-color:light grey;", height = "50px",
                                                              h2(id= "dd7",HTML("<em>This gauge charts below compares knowldege of correct drugs, dosage and age group for STH for the PRE and Post. The
                                                                 region shaded in pink represent knowldege Pre training while the green shows knowldge post. The figure below the chart is the knowledge Post where as the figure besides the delta sign is the percentage 
                                                                 change from PRE to Post</em>")))),
                                              fluidRow(column(3, plotlyOutput("sthworms_tt",height = "300px")),
                                                       column(3, plotlyOutput("sthdrugs_tt",height = "300px")),
                                                       column(3, plotlyOutput("sthage_tt",height = "300px")),
                                                       column(3, plotlyOutput("sthdose_tt",height = "300px")
                                                       )),
                                              br(),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","SCH Knowledge Pre-Post Comparison"))),
                                              fluidRow(column(12,style = "background-color:light grey;", height = "50px",
                                                              h2(id= "dd7",HTML("<em>This gauge charts below compares knowldege of correct drugs, dosage and age group for STH for the PRE and Post. The
                                                                 region shaded in pink represent knowldege Pre training while the green shows knowldge post. The figure below the chart is the knowledge Post where as the figure besides the delta sign is the percentage 
                                                                 change from PRE to Post</em>")))),
                                              br(),
                                              fluidRow(column(4, plotlyOutput("schdrugs_tt",height = "300px")),
                                                       column(4, plotlyOutput("schage_tt",height = "300px")),
                                                       column(4, plotlyOutput("schdose_tt",height = "300px"))
                                                       #column(3, plotOutput("",height = "300px"))
                                              ),
                                              br(),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","Side Effects"))),
                                              br(),
                                              fluidRow(column(6, plotOutput("sth_side_effects1",height = "300px")),
                                                       column(6, plotOutput("sch_side_effects1",height = "300px"))
                                                       
                                              ),
                                              br(),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","Materials Distribution"))),
                                              br(),
                                              fluidRow(column(4, plotOutput("chew_contacts",height = "300px")),
                                                       column(4, plotOutput("AEO_contacts",height = "300px")),
                                                       column(4, plotOutput("Deworm_sick",height = "300px"))
                                                       
                                              ),
                                              br(),
                                              br(),
                                              fluidRow(column(4, plotOutput("tt_materials_given",height = "300px")),
                                                       column(4, plotOutput("tt_posters",height = "300px")),
                                                       column(4, plotOutput("enough_posters",height = "300px"))
                                                       
                                              ),
                                              br(),
                                              br(),
                                              fluidRow(column(4,DT ::dataTableOutput(outputId = "tt_sucbcounties")),
                                                       column(4,DT ::dataTableOutput(outputId = "tt_type")),
                                                       column(4,DT ::dataTableOutput(outputId = "tt_Respondents"))))),
                                   tabPanel("TT OBS Dashboards",br(),
                                            fluidRow(column(4, selectInput("County5", "County", choices = c("Kenya","Bomet","Bungoma", "Busia", "Kisumu","Kericho", "Kirinyaga","Homabay", "Kisii","Nandi", "Narok","Nyamira", "Siaya", "Trans Nzoia","Vihiga"), multiple = F, selected = "Kenya")),
                                                     column(4,selectInput("years5", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 9")),
                                                     column(4,selectInput("Wave5","Wave", choices = c(1,2),multiple = F, selected = 1))),
                                            fluidPage(  fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                        h2(id= "dd7","COVID Prevention"))),
                                                        fluidRow(column(4, 
                                                                        (plotOutput("tt_trainers_masks", height = "300px" ))),
                                                                 column(4, 
                                                                        (plotOutput("tt_participants_masks", height = "300px"))),
                                                                 column(4, 
                                                                        (plotOutput("tt_sanitizer", height = "300px")))),
                                                        br(),
                                                        br(),
                                                        fluidRow(column(4, 
                                                                        (plotOutput("tt_sanitize_venue", height = "300px" ))),
                                                                 column(4, 
                                                                        (plotOutput("tt_num_parts_mask", height = "300px")))),
                                                        br(),
                                                        br(),
                                                        fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                        h2(id= "dd7","Attendance and Topic Coverage"))),
                                                        fluidRow(column(3,DT ::dataTableOutput(outputId = "tt_obs_type")),
                                                                 column(3,infoBoxOutput("obs_Attendance",  width = 15)),
                                                                 column(6,plotOutput("obs_attendance_sheets",height = "120px"))),
                                                        br(),
                                                        br(),
                                                        fluidRow(column(4,plotOutput(outputId = "coverage_general1")),
                                                                 column(4,plotOutput("coverage_sth1")),
                                                                 column(4,plotOutput("coverage_sch1"))),
                                                        br(),
                                                        br(),
                                                        fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                        h2(id= "dd7","Form Filling and Side Effects"))),
                                                        fluidRow(column(4,plotOutput(outputId = "obs_Form_Filling")),
                                                                 column(4,plotOutput("obs_Practice_sections")),
                                                                 column(4,plotOutput("obs_side_effects1"))), 
                                                        br(),
                                                        fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                        h2(id= "dd7","Roles and Responsibilities"))),
                                                        br(),
                                                        fluidRow(column(4,plotOutput(outputId = "obs_tt_roles")),
                                                                 column(4,plotOutput("obs_chew_roles")),
                                                                 column(4,plotOutput("obs_HT_roles"))), 
                                                        br(),
                                                        fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                        h2(id= "dd7","Materials Distributions"))),
                                                        br(),
                                                        fluidRow(column(4,plotOutput(outputId = "obs_not_treat")),
                                                                 column(4,plotOutput("obs_materials")),
                                                                 column(4,plotOutput("obs_AllMaterials"))),
                                                        br(),
                                                        br(),
                                                        fluidRow(column(4,plotOutput(outputId = "obs_chewchecklist")),
                                                                 column(4,plotOutput("obs_TTchecklist")),
                                                                 column(4,plotOutput("obs_chew_contacts"))),
                                                        br(),
                                                        br(),
                                                        fluidRow(column(4,plotOutput(outputId = "obs_Medicne")),
                                                                 column(4,plotOutput("obs_Funds")),
                                                                 column(4,plotOutput("")))
                                            )))),
                        tabPanel("DD Dashboards",
                                 br(),
                                 tabsetPanel(
                                   tabPanel("DD COMM DASHBOARDS", br(),
                                            fluidPage(
                                              h1(textOutput("ddcomm_title")), class = "active",
                                              fluidRow(column(4, selectInput("County6", "County", choices = c("Kenya","Bomet","Bungoma", "Busia", "Kisumu","Kericho", "Kirinyaga","Homabay", "Kisii","Nandi", "Narok","Nyamira", "Siaya", "Trans Nzoia","Vihiga"), multiple = F, selected = "Kenya")),
                                                       column(4,selectInput("years6", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 9")),
                                                       column(4,selectInput("Wave6","Wave", choices = c(1,2),multiple = F, selected = 1))),
                                              
                                              
                                              br(),
                                              br(),
                                              
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","Deworming Details"))),
                                              tags$style(HTML("#dd1{color: #034988;}")),
                                              fluidRow(column(3, plotOutput("dd_dewormed", height = "300px" )),
                                                       column(3, plotOutput("dd_int_type", height = "300px")),
                                                       column(3, plotOutput("dd_worms", height = "300px")),
                                                       column(3, plotOutput("dd_age_group", height = "300px"))),
                                              br(),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","Awareness"))),
                                              tags$style(HTML("#dd7{color: #034988;}")),
                                              br(),
                                              fluidRow(column(4, plotlyOutput("awareness", height = "400px" )),
                                                       column(4, plotlyOutput("deworm_before", height = "400px")),
                                                       column(4, plotlyOutput("deworm_feedback", height = "400px"))),
                                              br(),
                                              br(),
                                              fluidRow(column(6, h2(id= "dd7", "Reasons for Deworming"),
                                                              tags$style(HTML("#dd7{color: #034988;}")),
                                                              plotOutput("cloud", height = "400px" )),
                                                       h2(id= "dd7", "  Reasons for NOT Deworming"),
                                                       tags$style(HTML("#dd7{color: #034988;}")),
                                                       column(6, plotOutput("cloud2", height = "400px" ))),
                                              br(),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","Communication Means To Parents and CHV"))),
                                              tags$style(HTML("#dd7{color: #034988;}")),
                                              br(),
                                              fluidRow(column(4, plotOutput("Comm_means2", height = "400px" )),
                                                       column(4, plotOutput("Comm_means3", height = "400px" )),
                                                       column(4, plotOutput("Comm_means1", height = "400px" ))
                                              ),
                                              br(),
                                              fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                              h2(id= "dd7","CHV Knowledge"))),
                                              tags$style(HTML("#dd4{color: #034988;}")),
                                              br(),
                                              fluidRow(column(3, plotlyOutput("chv_exist", height = "300px" )),
                                                       column(3, plotlyOutput("chv_dewrm", height = "300px")),
                                                       column(3, plotlyOutput("chv_who_deworm", height = "300px")),
                                                       column(3, plotlyOutput("chv_agegroup", height = "300px")))
                                            )),
                                   
                                   
                                   tabPanel("DD MAIN DASHBOARDs", br(),
                                            fluidPage(h1(textOutput("ddmain_title")), class = "active",
                                                      fluidRow(column(4, selectInput("County7", "County", choices = c("Kenya","Bomet","Bungoma", "Busia", "Kisumu","Kericho", "Kirinyaga","Homabay", "Kisii","Nandi", "Narok","Nyamira", "Siaya", "Trans Nzoia","Vihiga"), multiple = F, selected = "Kenya")),
                                                               column(4,selectInput("years7", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 9")),
                                                               column(4,selectInput("Wave7","Wave", choices = c(1,2),multiple = F, selected = 1))),
                                                      fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                      h2(id= "dd7","STH Knowledge"))),  
                                                      tags$style(HTML("#dd7{color: #034988;}")),
                                                      fluidRow(column(3, infoBoxOutput("dd_main_sth_drugs", width = 15)),
                                                               column(3, infoBoxOutput("dd_main_sth_dosage", width = 15)),
                                                               column(3, infoBoxOutput("dd_main_sth_age", width = 15)),
                                                               column(3, infoBoxOutput("dd_main_sth_step", width = 15))),
                                                      fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                      h2(id= "dd7","SCH Knowledge"))),
                                                      tags$style(HTML("#dd7{color: #034988;}")),
                                                      fluidRow(column(3, infoBoxOutput("dd_main_sch_drugs", width = 15)),
                                                               column(3, infoBoxOutput("dd_main_sch_dosage", width = 15)),
                                                               column(3, infoBoxOutput("dd_main_sch_age", width = 15)),
                                                               column(3, infoBoxOutput("dd_main_sch_step", width = 15))),
                                                      fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                      h2(id= "dd7","Treatment Plan"))),
                                                      tags$style(HTML("#dd7{color: #034988;}")),
                                                      fluidRow(column(4, plotOutput("attached_ecd", height = "400px" )),
                                                               column(4, plotOutput("stand_alone_ecd", height = "400px" )),
                                                               column(4, plotOutput("non_enrolled_ecd", height = "400px" ))),
                                                      br(),
                                                      fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                      h2(id= "dd7","Stand Alone ECD Treatment Details"))),
                                                      fluidRow(column(4, plotOutput("standalone_forms", height = "300px" )),
                                                               column(4, plotOutput("standalone_drugs", height = "300px" )),
                                                               column(4, plotOutput("standalone_trtdata", height = "300px" ))),
                                                      br(),
                                                      fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                      h2(id= "dd7","Communication"))),
                                                      fluidRow(column(4, plotOutput("dd_keysms", height = "400px" )),
                                                               column(4, plotOutput("dd_sensitization", height = "400px" )),
                                                               column(4, plotOutput("non_enrolled_reach", height = "400px" ))),
                                                      br(),
                                                      fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                      h2(id= "dd7","Treatment Numbers"))),
                                                      h3("Number of Students Present for Deworming"),
                                                      fluidRow(column(4, infoBoxOutput("dd_present", width = 15)),
                                                               column(4, infoBoxOutput("dd_male_present", width = 15)),
                                                               column(4, infoBoxOutput("dd_Female_present", width = 15))),
                                                      h3("Number of Students Who Took Deworming Medicine"),
                                                      fluidRow(column(4, infoBoxOutput("dd_tookalb", width = 15)),
                                                               column(4, infoBoxOutput("dd_male_tookalb", width = 15)),
                                                               column(4, infoBoxOutput("dd_Female_tookalb", width = 15))),
                                                      h3("Number of Students Who Took Deworming Medicine as Teacher Observed"),
                                                      fluidRow(column(4, infoBoxOutput("dd_trpresent", width = 15)),
                                                               column(4, infoBoxOutput("dd_male_trpresent", width = 15)),
                                                               column(4, infoBoxOutput("dd_Female_trpresent", width = 15))),
                                                      fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                      h2(id= "dd7","Sufficiency of Materials"))),
                                                      h4("Are the materials(drugs and forms) sufficient for mop up and Deworming?"),
                                                      fluidRow(column(3, plotlyOutput("sufficient_drugs", height = "300px" )),
                                                               column(3, plotlyOutput("sufficient_forms", height = "300px" )),
                                                               column(3, plotlyOutput("sufficient_mop_up_drugs", height = "300px" )),
                                                               column(3, plotlyOutput("sufficient_mop_up_forms", height = "300px" )))
                                                      
                                            ))
                                 )),
                        tabPanel("Coverage Validation Dashboard",br(),
                              fluidPage(
                              tags$head(
                             tags$style("label{font-family: Arial;}")),
                                #h1(textOutput("SCTPost_title2")), class = "active",
                             fluidRow(#column(4, selectInput("County7", "County", choices = c("Kenya","Bomet","Bungoma", "Busia", "Kisumu","Kericho", "Kirinyaga","Homabay", "Kisii","Nandi", "Narok","Nyamira", "Siaya", "Trans Nzoia","Vihiga"), multiple = F, selected = "Kenya")),
                                      column(4,selectInput("years12", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 9")),
                                      column(4,selectInput("Wave12","Wave", choices = c(1,2),multiple = F, selected = 1))),
                             fluidRow(
                               #h4("2021 wave 1 CES dashboard for STH treatment"),
                               column(6, plotOutput("coverage_chart", height = "250px")),
                               column(6, 
                                      (dataTableOutput("coverage_table1")),
                                      fluidRow(dataTableOutput("coverage_table2")))
                             ),
                             br(),
                             #h5("Survey coverage disagregation by gender, enrolment status and school type"),
                             fluidRow(
                               column(6, plotOutput("disagregation_chart", height = "300px")),
                               column(6, plotOutput("unprogrammed_chart", height = "300px"))),
                             br(),
                             fluidRow(
                               column(6, plotOutput("rsn_not_give", height = "300px")),
                               column(6, plotOutput("rsn_not_swallow", height = "300px")))
                              )
                              )
                        
                      )))),

           tabPanel("DTW Nigeria",value = "dtw2" ,
                    tabPanel("DTW Nigeria", br(), br(), 
                             hr(),
                             fluidPage(
                               tags$head(
                                 tags$style("label{font-family: Arial;}")
                               ),
                               tags$style('.container-fluid {
                             background-color: ghostwhite;
              }'),
                               tabsetPanel(
                                 tabPanel("LGA Dashboards", br(),
                                        tabsetPanel(
                                            tabPanel("LGA Post Dashboard",br(),
                                                     fluidPage(
                                                       tags$head(
                                                         tags$style("label{font-family: Arial;}")),
                                                       h1(textOutput("SCTPost_title2")), class = "active",
                                                       
                                                       fluidRow(column(4, selectInput("State", "State", choices = c("Lagos", "Ogun","Oyo", "Rivers", "Cross Rivers"), multiple = F, selected = "Lagos")),
                                                                column(4,selectInput("years8", "Year", choices = c("Year 1","Year 2" , "Year 3" ,"Year 4" ,"Year 5","Year 6","Year 7"), multiple = F,selected = "Year 1")),
                                                                column(4,selectInput("Round","Round", choices = c(1,2),multiple = F, selected = 1))),
                                                       br(),
                                                       fluidRow(column(4, plotOutput("trt_type2", height = "300px" )),
                                                                column(4, plotOutput("sick2", height = "300px")),
                                                                column(4, plotOutput("No_Forms2", height = "300px"))),
                                                       fluidRow(column(6,style = "background-color:#CCFFCC;", height = "20px",
                                                      h3(id= "dd7","STH Knowledge")),
                                               column(6,style = "background-color:#CCFFCC;", height = "20px",
                                                      h3(id= "dd7","SCH Knowledge"))),
                                      tags$style(".small-box.bg-aqua { background-color: pink !important; color: #000000 !important; }"),
                                      fluidRow(column(2, valueBoxOutput("sth_drugs2", width = 15)),
                                               column(2, infoBoxOutput("sth_dosage2", width = 15)),
                                               column(2, infoBoxOutput("sth_age2", width = 15)),
                                               column(2, infoBoxOutput("sch_drugs2", width = 15)),
                                               column(2, infoBoxOutput("sch_dosage2", width = 15)),
                                               column(2, infoBoxOutput("sch_age2", width = 15))),
                                      br(),
                                      fluidRow(column(6, plotOutput("FormP2", height = "300px")))
                                      
                                      
                                      
                                    )),
                           tabPanel("LGAT OBS Dashboard", br(),
                                    fluidPage(
                                      tags$head(
                                        tags$style("label{font-family: Arial;}")),
                                      h1(textOutput("SCTOBS_title2")), class = "active",
                                      fluidRow(column(4, selectInput("State2", "State", choices = c("Ogun","Oyo","Lagos", "Rivers", "Cross Rivers")),selected = "Lagos"),
                                               column(4,selectInput("years9", "Years", choices = c("Year 1","Year 2" , "Year 3" ,"Year 4" ,"Year 5","Year 6","Year 7"), multiple = F,selected = "Year 1")),
                                               column(4,selectInput("Round2","Round", choices = c(1,2),multiple = F, selected = 1))),
                                                
                                      br(),
                                      fluidRow(column(4, 
                                                      (plotOutput("trainers_masks2", height = "300px" ))),
                                               column(4, 
                                                      (plotOutput("participants_masks2", height = "300px"))),
                                               column(4, 
                                                      (plotOutput("sanitizer2", height = "300px")))),
                                      br(),
                                      br(),
                                      fluidRow(column(4,
                                                      (plotOutput("sanitize_venue2", height = "300px"))),
                                               column(4,
                                                      plotOutput("num_parts_mask2", height = "300px"))),
                                      br(),
                                      br(),
                                      fluidRow(column(4, 
                                                      (plotOutput("attendance2", height = "300px"))),
                                               column(4,
                                                      (plotOutput("booklets2", height = "300px"))),
                                               column(2,
                                                      (valueBoxOutput("box1102",width = 15)),
                                                      (valueBoxOutput("box212",width = 15))),
                                               column(2,
                                                      (valueBoxOutput("box312",width = 15)),
                                                      (valueBoxOutput("box412",width = 15)))),
                                      br(),
                                      br(),
                                      fluidRow(column(6, 
                                                      (plotOutput("lga_coverage2", height = "400px" ))),
                                               column(6, plotOutput("not_treated2", height = "400px" ))),
                                      
                                      br(),
                                      br(),
                                      fluidRow(column(6, 
                                                      (plotOutput("Roles_chew2", height = "400px" ))),
                                               column(6, plotOutput("Roles_HT2", height = "400px" ))), 
                                      br(),
                                      br(),
                                      fluidRow(column(6, 
                                                      (plotOutput("Drug_Availability2", height = "400px" ))),
                                               column(6, plotOutput("Questions_Asked2", height = "400px" ))),
                                      br(),
                                      br()
                                    )),
                           tabPanel("LGAT FLHF Dashboard", br(),
                                    fluidPage(
                                      tags$head(
                                        tags$style("label{font-family: Arial;}")),
                                      h1(textOutput("SCTCHEW_title2")), class = "active",
                                      fluidRow(column(4, selectInput("State3", "State", choices = c("Ogun","Oyo", "Rivers", "Cross Rivers", "Lagos")),selected = "Lagos"),
                                               column(4,selectInput("years10", "years", choices = c("Year 3","Year 4" , "Year 5" ,"Year 6" ,"Year 7","Year 8","Year 9"), multiple = F,selected = "Year 1")),
                                               column(4,selectInput("Round3","Round", choices = c(1,2),multiple = F, selected = 1))),
                                      
                                      br(),
                                      fluidRow(column(6, plotOutput("chew_trt_type2", height = "300px" )),
                                               column(6, plotOutput("Role_Chew2", height = "300px"))),
                                      br(),
                                      fluidRow(column(6,style = "background-color:#FDE5D7;", height = "50px",
                                                      h2(id= "dd7","CHEW STH Knowledge")),
                                               column(6,style = "background-color:#FDE5D7;", height = "50px",
                                                      h2(id= "dd7","CHEW SCH Knowledge"))),
                                      fluidRow(column(2, infoBoxOutput("chew_sth_drugs2", width = 15)),
                                               column(2,infoBoxOutput("chew_sth_dosage2", width = 15)),
                                               column(2, infoBoxOutput("chew_sth_age2",width = 15)),
                                               column(2, infoBoxOutput("chew_sch_drugs2", width = 15)),
                                               column(2,infoBoxOutput("chew_sch_dosage2", width = 15)),
                                               column(2, infoBoxOutput("chew_sch_age2",width = 15))),
                                      
                                      br(),
                                      
                                      fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                      h2(id= "dd7","CHEW Side Effects"))),
                                      fluidRow(column(6, plotOutput("sth_side_effects2", height = "300px")),
                                               column(6, plotOutput("sch_side_effects2", height = "300px"))),
                                      br()
                                    )))),
                           tabPanel("TT Dashboards",br(),
                             fluidPage(
                             tags$head(
                               tags$style("label{font-family: Arial;}")),
                             h1(textOutput("title")), class = "active",
                             fluidRow(column(4, selectInput("State4", "State", choices = c("Ogun","Oyo", "Rivers", "Cross Rivers", "Lagos"), multiple = F, selected = "Lagos")),
                                      column(4,selectInput("years11", "years", choices = c("Year 1","Year 2" , "Year 3" ,"Year 4" ,"Year 5","Year 6","Year 7"), multiple = F,selected = "Year 1")),
                                      column(4,selectInput("Round4","Round", choices = c(1,2),multiple = F, selected = 1))),
                             
                             br(),
                             h2("TRAINING ATTENDANCE"),
                             fluidRow(column(4,plotlyOutput("ng_box4", height = "300px")),
                                      column(4,plotlyOutput("ng_box5", height = "300px")),
                                      column(4,plotlyOutput("ng_box6", height = "300px"))),
                             br(),
                             fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                         h2(id= "dd7","COVID Prevention"))),
                                         fluidRow(column(4, 
                                                         (plotOutput("ng_tt_trainers_masks", height = "300px" ))),
                                                  column(4, 
                                                         (plotOutput("ng_tt_participants_masks", height = "300px"))),
                                                  column(4, 
                                                         (plotOutput("ng_tt_sanitizer", height = "300px")))),
                                         br(),
                                         br(),
                                         fluidRow(column(4, 
                                                         (plotOutput("ng_tt_sanitize_venue", height = "300px" ))),
                                                  column(4, 
                                                         (plotOutput("ng_tt_num_parts_mask", height = "300px")))),
                             br(),
                             h2("TOPIC COVERAGE"),
                             fluidRow(column(6,plotOutput("ng_coverage2")),
                                      column(6, plotOutput("ng_tt_roles"))),
                             br(),
                             br(),
                             fluidRow(column(4, plotOutput("ng_FormsFIlled")),
                                      column(4, plotOutput("ng_Poles")),
                                      column(4, plotOutput("ng_TT_Methods"))),
                             br(),
                             br(),
                             h2("KNOWLEDGE LEVELS:    TEACHERS KNOWLEDGEABLE POST-TRAINING vs PRE-TRAINING"),
                             h2("STH KNOWLEDGE"),
                             fluidRow(
                               column(3,plotlyOutput("ng_sthworms",  height = "400px")),
                               column(3,plotlyOutput("ng_sthdrugs",  height = "400px")),
                               column(3,plotlyOutput("ng_sthage",  height = "400px")),
                               column(3,plotlyOutput("ng_sthdose",  height = "400px"))),
                             fluidRow(
                               column(3,plotlyOutput("ng_schworms",  height = "400px")),
                               column(3,plotlyOutput("ng_schdrugs",  height = "400px")),
                               column(3,plotlyOutput("ng_schage",  height = "400px")),
                               column(3,plotlyOutput("ng_schdose",  height = "400px"))),
                             
                               
                             h2("MATERIAL DISTRIBUTION"),
                             fluidRow(column(4,plotOutput("ng_drugsdistribution")),
                                      column(4,plotOutput("ng_formsdistribution")),
                                      column(4,plotOutput("ng_pole_dist"))),
                             br(),
                             br(),
                             fluidRow(column(4,plotOutput("ng_treatment_register")),
                                      column(4,plotOutput("ng_Posters"))),
                             h2("PRE POST ANSWERS"),
                             fluidRow(column(4,plotOutput("ng_Lateness")),
                                      column(4,plotOutput("ng_Enough_Forms")),
                                      column(3, plotOutput("ng_Contacts"))))),
                           tabPanel("DD Dashboards",
                                    br(),
                                    hr(),
                                    tabsetPanel(
                                      tabPanel("CMS DASHBOARDS", br(),
                                               fluidPage(
                                                 #h1(textOutput("#ddcomm_title")), class = "active",
                                                 fluidRow(column(4, selectInput("State20", "State", choices = c("Ogun","Oyo", "Rivers", "Cross Rivers", "Lagos"), multiple = F, selected = "Lagos")),
                                                          column(4,selectInput("years20", "years", choices = c("Year 1","Year 2" , "Year 3" ,"Year 4" ,"Year 5","Year 6","Year 7"), multiple = F,selected = "Year 1")),
                                                          column(4,selectInput("Round20","Round", choices = c(1,2),multiple = F, selected = 1))),
                                                 
                                                 br(),
                                                 br(),
                                                 
                                                 fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                 h2(id= "dd7","Deworming `Details"))),
                                                 tags$style(HTML("#dd1{color: #034988;}")),
                                                 fluidRow(column(3, plotOutput("ng_dd_trt_type", height = "300px" )),
                                                          column(3, plotOutput("ng_dd_int_type", height = "300px")),
                                                          column(3, plotOutput("ng_dd_worms", height = "300px")),
                                                          column(3, plotOutput("ng_dd_dewormed", height = "300px"))),
                                                 br(),
                                                 fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                 h2(id= "dd7","Awareness"))),
                                                 tags$style(HTML("#dd7{color: #034988;}")),
                                                 br(),
                                                 fluidRow(column(4, plotlyOutput("ng_cms_sth_age", height = "400px" )),
                                                          column(4, plotlyOutput("ng_cms_sch_age", height = "400px")),
                                                          column(4, plotlyOutput("cms_child_eat", height = "400px"))),
                                                 br(),
                                                 
                                                 fluidRow(column(4, plotlyOutput("ng_awareness", height = "400px" )),
                                                          column(4, plotlyOutput("ng_deworm_before", height = "400px")),
                                                          column(4, plotlyOutput("ng_deworm_feedback", height = "400px"))),
                                                 br(),
                                                 
                                                 fluidRow(column(6, h2(id= "dd7", "Reasons for Deworming"),
                                                                 tags$style(HTML("#dd7{color: #034988;}")),
                                                                 plotOutput("ng_cloud", height = "400px" )),
                                                          h2(id= "dd7", "  Reasons for NOT Deworming"),
                                                          tags$style(HTML("#dd7{color: #034988;}")),
                                                          column(6, plotOutput("ng_cloud2", height = "400px" ))),
                                                 br(),
                                                 fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                 h2(id= "dd7","Communication Means To Parents and CHV"))),
                                                 tags$style(HTML("#dd7{color: #034988;}")),
                                                 br(),
                                                 fluidRow(column(4, plotOutput("ng_Comm_means2", height = "400px" )),
                                                          column(4, plotOutput("ng_Comm_means3", height = "400px" )),
                                                          column(4, plotOutput("ng_Comm_means1", height = "400px" ))
                                                 ),
                                                 br(),
                                                 fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                 h2(id= "dd7","FLHF Knowledge"))),
                                                 tags$style(HTML("#dd4{color: #034988;}")),
                                                 br(),
                                                 fluidRow(column(3, plotlyOutput("ng_chv_exist", height = "300px" )),
                                                          column(3, plotlyOutput("ng_chv_dewrm", height = "300px")),
                                                          column(3, plotlyOutput("ng_chv_who_deworm", height = "300px")),
                                                          column(3, plotlyOutput("ng_chv_agegroup", height = "300px")))
                                               )),
                                      
                                      
                                      tabPanel("DD MAIN DASHBOARDs", br(),
                                               fluidPage(h1(textOutput("ng_ddmain_title")), class = "active",
                                                         fluidRow(column(4, selectInput("State21", "State", choices = c("Ogun","Oyo", "Rivers", "Cross Rivers", "Lagos"), multiple = F, selected = "Lagos")),
                                                                  column(4,selectInput("years21", "years", choices = c("Year 1","Year 2" , "Year 3" ,"Year 4" ,"Year 5","Year 6","Year 7"), multiple = F,selected = "Year 1")),
                                                                  column(4,selectInput("Round21","Round", choices = c(1,2),multiple = F, selected = 1))),
                                                         
                                                         br(),
                                                         fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                         h2(id= "dd7","D-Day Preparedness"))),  
                                                         tags$style(HTML("#dd7{color: #034988;}")),
                                                         fluidRow(column(4, plotOutput("deworming_day", height = '300px')),
                                                                  column(4, plotOutput("deworming_plan", height = '300px')),
                                                                  column(4, plotOutput("Did_Train", height = '300px'))),
                                                                  #column(3, infoBoxOutput("ng_dd_main_sth_step", width = 15))),
                                                         fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                         h2(id= "dd7","Sensitization Means"))),
                                                         tags$style(HTML("#dd7{color: #034988;}")),
                                                         fluidRow(column(10, plotOutput("Sensitization_Methods", height = '400px'))
                                                                  ),
                                                         fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                         h2(id= "dd7","D-Day Observations"))),
                                                         tags$style(HTML("#dd7{color: #034988;}")),
                                                         fluidRow(column(4, infoBoxOutput("box10", width = 15  )),
                                                                  column(4, infoBoxOutput("box11",width = 15 )),
                                                                  column(4, infoBoxOutput("box12",width = 15 ))),
                                                         br(),
                                                         fluidRow(column(4, infoBoxOutput("box13",width = 15 )),
                                                                  column(4, infoBoxOutput("box14",width = 15 )),
                                                                  column(4, infoBoxOutput("box15",width = 15 ))),
                                                         br(),
                                                         fluidRow(column(4, infoBoxOutput("box16",width = 15 )),
                                                                  column(4, infoBoxOutput("box17",width = 15 )),
                                                                  column(4, infoBoxOutput("box18",width = 15 ))),
                                                         br(),
                                                         fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                         h2(id= "dd7","C: Side Effects"))),
                                                         fluidRow(column(4, plotOutput("Obs_side_effects", height = "300px" )),
                                                                  column(4, plotOutput("Obs_skils", height = "300px" )),
                                                                  column(4, plotOutput("DD_Side_Effects", height = "300px" ))),
                                                         br(),
                                                         fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                         h2(id= "dd7","Head Teacher Post Training Interviews"))),
                                                         fluidRow(column(4, plotOutput("DD_Suffficiency", height = "400px" )),
                                                                  column(4, plotOutput("extra_tabs", height = "400px" )),
                                                                  column(4, plotOutput("dd_actn_extras", height = "400px" ))),
                                                         br(),
                                                         fluidRow(column(4, plotOutput("Submit_Forms", height = "400px" ))),
                                                         br(),
                                                         fluidRow(column(12,style = "background-color:#FDE5D7;", height = "50px",
                                                                         h2(id= "dd7","Treatment Numbers"))),
                                                         h3("Number of Students Present for Deworming"),
                                                         fluidRow(column(4, infoBoxOutput("ng_dd_present", width = 15)),
                                                                  column(4, infoBoxOutput("ng_dd_male_present", width = 15)),
                                                                  column(4, infoBoxOutput("ng_dd_Female_present", width = 15))),
                                                         h3("Number of Students Who Took Deworming Medicine"),
                                                         fluidRow(column(4, infoBoxOutput("ng_dd_tookalb", width = 15)),
                                                                  column(4, infoBoxOutput("ng_dd_male_tookalb", width = 15)),
                                                                  column(4, infoBoxOutput("ng_dd_Female_tookalb", width = 15))),
                                                         h3("Number of Students Who Took Deworming Medicine as Teacher Observed"),
                                                         fluidRow(column(4, infoBoxOutput("ng_dd_trpresent", width = 15)),
                                                                  column(4, infoBoxOutput("ng_dd_male_trpresent", width = 15)),
                                                                  column(4, infoBoxOutput("ng_dd_Female_trpresent", width = 15)))
                                                        
                                                         
                                               ))
                                    )))))),
  tabPanel("DTW Pakistan", value = "dtw3",
           tabPanel("DTW Pakistan", br(), br(), 
                    fluidPage(
                      tags$head(
                        tags$style("label{font-family: Arial;}")
                      ),
                      tags$style('.container-fluid {
                             background-color: ghostwhite;
              }'),
                      tabsetPanel(
                        tabPanel("TT Dashboards", br(),
                                 tabsetPanel(
                                   tabPanel("TT OBS Dashboard",br(),
                                            fluidPage(
                                              tags$head(
                                                tags$style("label{font-family: Arial;}")),
                                             #h1(textOutput("SCTPost_title2")), class = "active",
                                              
                                             fluidRow(column(4, selectInput("Province", 'Region:', choices = c("GB", "ICT", "KP", "Punjab", "Sindh"), selected = "ICT", multiple = F)),
                                                      column(4,selectInput("years13", "years", choices = c(2020,2021), multiple = F,selected = 2021)),
                                                      column(4,selectInput("Round13","Round", choices = c(1,2),multiple = F, selected = 1))),
                                             fluidRow(infoBoxOutput("pk_sample", width = 2), 
                                                               infoBoxOutput("pk_att_tcher", width = 2),
                                                               infoBoxOutput("pk_att_rate_tcher", width= 2),
                                                               infoBoxOutput("pk_att_sch", width = 2),
                                                               infoBoxOutput("pk_att_rate_sch", width = 2),
                                                               infoBoxOutput("pk_reg_sheet", width = 2)),
                                                      
                                                      br(),
                                             fluidRow(column(width = 6, plotOutput("pk_coverage_graph", height = "300px")), 
                                                               column(width = 6, plotOutput("pk_mat_drug", height = "300px"))),
                                                      br(),
                                                      fluidRow(column(width = 3, plotOutput("pk_coverage_pie", height = "200px")),
                                                               column(width = 3, plotOutput("pk_all_mat_pie", height = "200px")),
                                                               column(width = 3, plotOutput("pk_target_pie", height = "200px")),
                                                               column(width = 3, plotOutput("pk_announced_dd_pie", height = "200px"))),
                                                      br(),
                                                      fluidRow(column(width = 6, plotOutput("pk_not_treated", height = "250px")),
                                                               column(width = 6, plotOutput("pk_tcher_resp", height = "250px"))), 
                                                      br(),
                                                      fluidRow(column(width = 6, plotOutput("pk_resp_health", height = "250px")),
                                                               column(width = 6, plotOutput("pk_mobilize", height = "250px"))),
                                                      br(),
                                                      fluidRow(column(width = 6, plotOutput("pk_mostaskedquix", height = "250px")), 
                                                               column(width = 6, plotOutput("pk_covid19", height = "250px"))),
                                                      
                                             
                                             br()
                                            )),
                                   tabPanel("TT Pre-Post Dashboard",br(),
                                            fluidPage(
                                              tags$head(
                                                tags$style("label{font-family: Arial;}")),
                                              #h1(textOutput("SCTPost_title2")), class = "active",
                                              fluidRow(column(4, selectInput("Province2", 'Region:', choices = c("GB", "ICT", "KP", "Punjab", "Sindh"), selected = "ICT", multiple = F)),
                                                             column(4,selectInput("years14", "years", choices = c(2020,2021), multiple = F,selected = 2021)),
                                                             column(4,selectInput("Round14","Round", choices = c(1,2),multiple = F, selected = 1))),
                                                      
                                                      br(),
                                                      fluidRow(
                                                        infoBoxOutput("pk_box1", width=3),
                                                        infoBoxOutput("pk_box2", width=3),
                                                        infoBoxOutput("pk_box3", width=3),
                                                        infoBoxOutput("pk_box4", width=3)
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        infoBoxOutput("pk_box5", width=4),
                                                        infoBoxOutput("pk_box6", width=4),
                                                        infoBoxOutput("pk_box7", width=4)
                                                      ),
                                                      fluidRow(
                                                        column(4, plotOutput("pk_STH_age", height = "200px")),
                                                        column(4, plotOutput("pk_STH_dosage", height = "200px")),
                                                        column(4, plotOutput("pk_STH_treatment_drug", height = "200px"))),
                                                      br(),
                                                      # fluidRow(
                                                      #  column(6, plotOutput("STH_treatment_drug", height = "200px")),
                                                      # column(6, plotOutput("", height = "200px"))),
                                                      # br(),
                                                      fluidRow(
                                                        column(6, plotOutput("pk_Side_effects", height = "270px")),
                                                        column(6, plotOutput("pk_sensitization_activities", height = "270px"))),
                                                      br(),
                                                      fluidRow(
                                                        column(6, plotOutput("pk_child_not_treated", height = "270px")),
                                                        column(6, plotOutput("pk_How_infected", height = "270px"))),
                                                      br(),
                                                      fluidRow(
                                                        column(6, div(DT::dataTableOutput('pk_improvement_area'), style = "font-size:80%",align="center")),
                                                        column(6, div(DT::dataTableOutput('pk_feedback'), style = "font-size:80%",align="center")))
                                                      
                                              
                                              ))))))
                                   
           
           
           )))
)



