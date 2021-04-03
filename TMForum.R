setwd()
library(dplyr)
library(ggplot2)
library(lubridate)
library(magick)

tmf <- read.csv("data/tmforum.csv")

library(stringr)
tmf %>% mutate(Enroll_Date=str_trim(Enroll_Date, side=c("both")),
               End_Date=str_trim(End_Date, side=c("both")),
               Score=str_trim(Score,side=c("both")),
               Course=str_trim(Course,side=c("both"))) -> tmf
tmf %>% mutate(Enroll_Date= mdy(Enroll_Date), End_Date=mdy(End_Date)) -> tmf


tmf %>% mutate(exam=case_when(str_detect(Course,"Exam") ~ "Y",
                              TRUE ~ "N")) -> tmf
library(forcats)
#tmf %>% filter(exam=="Y") %>% mutate(Course=fct_reorder(Course,End_Date))

library(png)
library(cowplot)
library(fmsb)
library(tidyr)
library(magick)

tmf %>% filter(exam=="Y") %>%
  mutate(Course=str_remove(Course," Foundation Level Exam"),
         Course=str_remove(Course,"TM Forum "),
         Course=fct_reorder(Course,End_Date)) %>% 
  ggplot(aes(Course,End_Date,color=Course)) +
  geom_point(size=4)+
  geom_segment(aes(x=.8, xend=.8, y=ymd("20201228"), yend=ymd("20210126")), 
               color="blue",linetype=1,size=1.5)+
  scale_y_date(limits=c(ymd("20201228"),NA),
               date_breaks = "1 week",
               date_labels = "%d %B")+
  geom_hline(yintercept = ymd("20201227"), color="magenta",linetype=5,size=.8)+
  geom_hline(yintercept = ymd("20210321"), color="blue",linetype=5,size=.8)+
  geom_segment(aes(x=0, xend=7, y=ymd("20210126"), yend=ymd("20210126")), 
               color="magenta",linetype=4,size=1.2)+
  geom_segment(aes(x=0, xend=10, y=ymd("20210207"), yend=ymd("20210207")), 
               color="black",linetype=3,size=.9)+
  geom_segment(aes(x=0, xend=7, y=ymd("20210221"), yend=ymd("20210221")), 
               color="black",linetype=3,size=.9)+
  geom_segment(aes(x=0, xend=12, y=ymd("20210223"), yend=ymd("20210223")), 
               color="black",linetype=3,size=.9)+
  geom_segment(aes(x=0, xend=9, y=ymd("20210228"), yend=ymd("20210228")), 
               color="black",linetype=3,size=.9)+
  annotate("text", x = 4, y = ymd("20210124"), label = "Enrolled in  1st course",
           size=3.5,angle=90)+
  annotate("text", x = 6, y = ymd("20201228"), label = "Enrollment announced",
           size=3.5,angle=90)+
  annotate("text", x = 6, y = ymd("20210323"), label = "4 Badges/ 12 Certifications/ 13 Courses\n COMPLETED",
           size=3.5,angle=90)+
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle=45),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black",linetype = 1,size=2))+
  coord_flip()+
  annotate("text", x = 2, y = ymd("20210110"), 
           label="Personal setback delayed enrollment\n but better late than never",
           size=3.5, color="purple")+
  annotate("text", x = 1, y = ymd("20210310"), 
           label="(Source:TMForum Training History)",size=2.5) -> myplot


img_bdm <- image_read("data/BDM.png")
img_pdm <- image_read("data/PDM.png")
img_aidm <- image_read("data/AIDM.png")
img_ftm <- image_read("data/FTM.png")

myplot_2 <- 
  ggdraw() +
  draw_plot(myplot)+
  draw_image(img_bdm,  x = .15, y = .42, scale = .15) +
  draw_image(img_aidm,  x = .005, y = 0.35, scale = .15) +
  draw_image(img_pdm,  x = 0.1, y = 0.2, scale = .15) +
  draw_image(img_ftm,  x = 0.21, y = 0.25, scale = .15) 

tmf_new<- tmf %>% filter(exam=="Y") %>%
  mutate(Course=str_remove(Course," Foundation Level Exam"),
         Course=str_remove(Course,"TM Forum "),
         maxval=c(100),
         passval=c(60),
         Score=as.numeric(str_remove(Score,"%")))

#Radar chart

tmf_new %>%
  select(Course,Score) %>% 
  pivot_wider(names_from = Course, values_from = Score) ->tmf_score
  
tmf_new %>%
  select(Course,maxval) %>% 
  pivot_wider(names_from = Course, values_from = maxval) ->tmf_max

tmf_new %>%
  select(Course,passval) %>% 
  pivot_wider(names_from = Course, values_from = passval) ->tmf_min

tmf_comb <- bind_rows(tmf_max,tmf_min,tmf_score) %>% as.data.frame()
  
row.names(tmf_comb) <- c("Maxval","Passval","Score")

tmf_comb  %>% 
radarchart(axistype = 1,seg=1,
           pcol = "#00AFBB", 
           pfcol = scales::alpha("#00AFBB", 0.5), plwd = 2, plty = 1,
           cglcol = "grey", cglty = 1, cglwd = 0.8,caxislabels =c("60%","100%"),
           axislabcol = "red", vlcex = 0.7, vlabels = colnames(tmf_comb))

#Lollipop chart

tmf_new %>% 
  mutate(Course=fct_reorder(Course,End_Date)) %>% 
  ggplot(aes(Course,Score))+
  geom_segment(aes(x=Course ,xend=Course, y=0, yend=Score), color="grey",
               linetype=1,size=1.5)+
  geom_point(size=5.5,color="blue")+
  coord_flip()+
  ylim(0,100)+ 
  geom_hline(yintercept = 60, color="red",linetype=4,size=.8)+
  geom_hline(yintercept = 100, color="purple",linetype=3,size=.8)+
  geom_text(aes(label=Score),hjust=-1)+
  theme_gray()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size=10))+
    annotate("text", x = 10.5, y = 58, label = "Pass Marks: 60",size=3.5,angle=90)+
    annotate("text", x = 4, y = 98, label = "Total Marks: 100",size=3.5,angle=90)+
  #geom_curve(aes(x = 0.85, y = 38, xend = .85, yend = 60),curvature = .2,color="red")+
    annotate("text", x = 0.75, y = 75, 
             label="(Source:TMForum Training History)",size=2.5) -> p

ggdraw(p) + 
  draw_label("Marks  obtained  in\n  each  exam", 
             color = "#C0A0A0", size = 20, angle = 45) -> final_p

myplot_2  
final_p
