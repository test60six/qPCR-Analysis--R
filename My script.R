 #install packages
 # install.packages(c( "plyr","tidyverse","readxl",))
 
 
library(plyr)
library(readxl)

pcr <- read_excel("Data/qPCR_08_10.xlsx")

view(pcr)

#imported data set from qpcr xlsx
  qpcr <- read_excel("Data/SampGF_SampSPF_AKR_Gingiva_GAPDH_ACTIN_AVERAGED_8-8-2022.xlsx",sheet="R file")
 
#visualize dataset 
  View(qpcr)


#summarize housekeeping data by average ct
  hk_qpcr <- ddply(qpcr, c("Sample_Name", "Target_Name"), summarise,
                           HK_AvgCt = mean(Ct) ) 
      il1b_il6 <- read_excel("Data/SampGF_SampSPF_AKR_Gingiva__IL1b_IL-6_8-9-2022_data.xls",sheet="R_file")
 
        View (il1b_il6)
 
#summarize target IL1b and IL-6 data by average ct
    target_qpcr <- ddply(il1b_il6, c ("Sample_Name","Target_Name"),summarise,
                      Gene_AvgCt = mean(Ct))

 
#merge hk_qpcr ct's and target_qpcr ct's
    total <- merge(hk_qpcr, target_qpcr, by = "Sample_Name")
          View(total)
 
 
# subset data (timming actin from the group)
    total <- subset(total, Target_Name.x != "Actin")
 
#Calculate delta delta ct
    total$DDCT <- ((2^total$HK_AvgCt)/(2^total$Gene_AvgCt)) 
 
#Change decimal place for the delta delta ct value
    total$DDCT <- (round( total$DDCT, 10))
    
#Grabbed information for Animal,Strain,sex,notes(col 1-4)    
strain_grouped <- ( pcr[1:4])

#renamed ANIMAL to Sample_Name for merging data 
names(strain_grouped)[2]<- ("Sample_Name")  

#Mergedstrain_grouped to total data set
total <- merge(strain_grouped, total, by= "Sample_Name") 



library("ggplot2")
ggplot(data=total)+
geom_boxplot(aes(x=NOTES,y=DDCT))+
  facet_wrap(~Target_Name.y)+
  labs(x="",y="mRNAexpression")





library("ggpubr")
my_comparisons <- list( c("SPF", "AKR"), c("AKR", "GF"), c("GF", "SPF") )

 ggboxplot(total, x = "NOTES", y = "DDCT",
            color = "NOTES",
            facet.by = "Target_Name.y",
            add = "jitter", shape = "NOTES")+
   stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
   stat_compare_means(label.y = 0.003)                
 
 

 
 
 

  
  
    
                 
 
 
 
