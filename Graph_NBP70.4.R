#install required packages ggplot2 and ggrepel to draw graphs.
# we need to use or refer them using library function in R
install.packages("ggplot2")
library("ggplot2")

install.packages("ggrepel")
library("ggrepel")

#read csv with headers and store it in dataset_1 variable.
dataset_1 <- read.csv("try.csv", header=TRUE)

#read initial few records/rows of dataset_1
head(dataset_1)

#filter data/rows from dataset_1 where M_NBP greather than 70.4. store it in Dataset_NBP70.4
Dataset_NBP70.4 <- subset(dataset_1, dataset_1$M_NBP > 70.4)

#check few rows from Dataset_NBP70.4
head(Dataset_NBP70.4)

#cross check maximum value of M_NBP field from filtered dataset Dataset_NBP70.4 
max(Dataset_NBP70.4$M_NBP)

#code to set color for each genotype based on first two letters of genotype name 
#implemented this logic based on graph and still need to get correct logic for CONTROL categorization
#this for loop will iterate or execute from 1st row of dataset till last row of dataset
for (i in 1:nrow(Dataset_NBP70.4)) {
  # execute this loop only when genotypename is not null
  if (!is.na(Dataset_NBP70.4$GENOTYPE_NAME[i])) {
        if(substr(Dataset_NBP70.4$GENOTYPE_NAME[i],1,2)=="TX") { 
          #check if genotype name's 1st two letters are TX. for this use substring function to get 1st two letters. 
          #if condition satisfies then assign color lightgreen to respective genotype name
      Dataset_NBP70.4$Col[i] <-  "lightgreen"
    }else if(substr(Dataset_NBP70.4$GENOTYPE_NAME[i],1,2)=="P0") {
      #check if genotype name's 1st two letters are PO. for this use substring function to get 1st two letters
      #if condition satisfies then assign color orange(sienna1 is color code for it in R) to respective genotype name
      Dataset_NBP70.4$Col[i] <- "sienna1"
    }else if(substr(Dataset_NBP70.4$GENOTYPE_NAME[i],1,2)=="CA") {
      #check if genotype name's 1st two letters are CA. for this use substring function to get 1st two letters
      #if condition satisfies then assign color purple(purple1 is color code for it in R) to respective genotype name
      Dataset_NBP70.4$Col[i] <- "purple1"
    } else
      # for all other genotype names which are not satisfying above conditions, set color code to gray.
      Dataset_NBP70.4$Col[i] <- "slategray"
    
  }
}

# check dataset again if color code is assigned for each genotype name. 
#new column "Col" should be added or created with above color code values
Dataset_NBP70.4 

# draw graphics using ggplot with dataset Dataset_NBP70.4 and x axis with M_HUM_ field and y axis with M_RDT field
#to ignore the null values for these fields use argument na.rm=TRUE
ggplot(Dataset_NBP70.4, aes(x=M_HUM_, y=M_RDT),na.rm=TRUE) +
  # on graph to draw the point size based on M_HTP field, we are using size argument 
  # to set the color of point based on genotype name from above for loop logic we are using color argument and its value assigned to newly created column
  geom_point(aes(size=Dataset_NBP70.4$M_HTP,color =Dataset_NBP70.4$Col),na.rm=TRUE) +
  # to overrride the legend based to show color by CONTROL and color code corresponding to its control
  scale_colour_identity(guide="legend",name="color by CONTROL", breaks=c("lightgreen","purple1","sienna1","slategray"),labels=c("TX","R2n","competitor","check")) +
  # to overrride the legend for size of points 
  scale_size(range = c(3, 18) ,guide="legend",name="size by M_HTP" ,breaks = c(300,240),labels=c(">=306.25","<=221.25"),trans = "identity") +
  # points on graph drawn based on genotype name, so to put lable as genotype name need to add package of ggrepel in r and use below funtion. 
  #other arguments used in this function to set position of box on x & y axis and type of box for that label
  geom_label_repel(aes(label=Dataset_NBP70.4$GENOTYPE_NAME),size=3,na.rm = TRUE, nudge_x = 0,nudge_y =7, box.padding = unit(0.25, "lines")) +
  # to reverse the x axis scale from range as shown in original graphs
   scale_x_reverse(breaks = seq(32, 20, by = -1), limits=c(32,20)) +
  # to show the y axis continuous scale range as shown in original graphs
  scale_y_continuous(breaks = seq(80, 165, by = 5),limits=c(80,165)) +
  # to clear the background of graph plus set border of graph to blank, grid to blank
  # all these arguments are to clear background and set it to white with only axis line to grey color
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey")) +
  # to set the order of legends and adjust size size and distance between labels
  theme(legend.text=element_text(size=12), legend.key.size = unit(0.8, 'cm')) +
  guides(color = guide_legend(order=1,override.aes = list(size=5,linetype=4)))
