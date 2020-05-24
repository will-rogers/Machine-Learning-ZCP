########################################################
#Script to split data into training and testing datasets 
########################################################
require(stringr)
require(dplyr)
require(readr)
require(tidyr)
require(jpeg)
require(furrr)
require(magick)
require(ggplot2)

ZCP.256.Files <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/ZCP.256.Files.csv") # from Creel Machine Learning Script.R
head(ZCP.256.Files) # lets simplify this for clarity
columns <- c(
  "time.captured","files","Year_Park","Season","Area","Section","Site","Camera",
  "Image","Year","Park","ELEMENTNAME","LOCATIONNAME","VISITNAME","TIMEOFDAY","WEATHER",
  "NUMBEROFELEMENTS","MOONPHASE","DURATIONMINUTES","DURATIONSECONDS","LATDECDEG",
  "LONDECDEG","Park.y","Section.y","Site.y","Camera.y","Unique","new.file.name"
)
files <- ZCP.256.Files[,columns]
files$Species <- files$ELEMENTNAME

# this chunk can be highlighted and run to verify that everything works as it should
i <- sample(1:nrow(files),1) 
files$ELEMENTNAME[i]
img <- readJPEG(paste0("/Volumes/Seagate_Backup_Plus_Drive/WildLog/256x256_Photos/",files$new.file.name[i],".JPG"))
plot(1:2, type='n')
rasterImage(img, 2, 1, 1, 2)

# Macs and Microsoft store files differently, lets universalize this

# for mac
files$mac.file <- paste0("/Volumes/Seagate_Backup_Plus_Drive/WildLog/256x256_Photos/", 
                         files$new.file.name, 
                         ".JPG")
files$windows.file <- paste0("E:/WildLog/256x256_Photos/", 
                         files$new.file.name, 
                         ".JPG")

# We need to build data sets that are title less csvs of image source and image id
# We are using two different models, called NA and SS, both have different classes
# so we will need to create testing and traing files unique to each for transfer 
# learning. The only differences will come from species converisons.

NA.conversion <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/speciesID.csv") 
SS.conversion <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/ssspeciesID.csv") 

# seems like the only overlap with NA is "Human"
NA.conversion$Group.name[which(NA.conversion$Group.name %in% unique(files$ELEMENTNAME))]
length(NA.conversion$Group.name)

files$NA.code <- NA 
id <- 28:(28+length(levels(files$ELEMENTNAME)))
id[28] <- 11
id[29:length(levels(files$ELEMENTNAME))] <- id[29:length(levels(files$ELEMENTNAME))]-1

for (i in 1:length(id)){
  files$NA.code[which(files$ELEMENTNAME == levels(files$ELEMENTNAME)[i])] <- id[i]
}

# seems like ~60 percent overlap in species 
levels(SS.conversion$Species)[levels(SS.conversion$Species) %in% levels(files$ELEMENTNAME)]
length(SS.conversion$Species)
22/length(SS.conversion$Species)

files$SS.code <- NA 
id <- c(0,8,48,11,27,28,49,50,51,52,5,53,54,4,6,55,56,45,33,57,58,46,59,60,16,19,20,21,
        24,14,18,61,62,63,64,65,66,67,17,39,68,25,69,70,71,22,72,15,73,74,75,76,41,42,43,77,78,79,2)

for (i in 1:length(id)){
  files$SS.code[which(files$ELEMENTNAME == levels(files$ELEMENTNAME)[i])] <- id[i]
}

# Lets get an idea of what we are working with in terms of detections 
# Here, we only have images with animals in them - e.g. no blanks
files %>% 
  group_by(ELEMENTNAME) %>% 
  summarize(n=n()) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(ELEMENTNAME, -n), y = n, fill = reorder(ELEMENTNAME, -n)), stat = "identity") +
  theme_classic()

files %>% 
  group_by(Year_Park, Camera) %>% 
  summarize(n=n()) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(Year_Park, -n), y = n, fill = Camera), stat = "identity") +
  theme_classic()

a <- files %>% 
  group_by(Year_Park) %>% 
  summarize(n=n())

b <- files %>% 
  group_by(Year_Park, ELEMENTNAME) %>% 
  summarize(n=n())

c <- merge(a, b, by = "Year_Park")
c$prop <- c$n.y/c$n.x
c$prop <- ifelse(c$prop < .01, NA, c$prop)
c %>% 
  ggplot() +
  geom_bar(aes(x = Year_Park, y = prop, fill = reorder(ELEMENTNAME, -prop)), stat = "identity") +
  scale_fill_discrete() +
  theme_classic()

#establishing a testing dataset 
a <- files %>% 
  group_by(Species) %>% 
  summarize(n = n()) %>% 
  filter(n>10)

set.seed(12345)

test <- files %>% 
  filter(Species %in% a$Species) %>% 
  group_by(Species) %>% 
  sample_frac(.25, replace = F)


train_2.5 <- files %>% 
  filter(Species %in% a$Species) %>% 
  filter(!new.file.name %in% test$new.file.name) %>% 
  group_by(Species) %>% 
  sample_frac(.025, replace = F)

train_5 <- files %>% 
  filter(Species %in% a$Species) %>% 
  filter(!new.file.name %in% test$new.file.name) %>% 
  group_by(Species) %>% 
  sample_frac(.05, replace = F)

train_10 <- files %>% 
  filter(Species %in% a$Species) %>% 
  filter(!new.file.name %in% test$new.file.name) %>% 
  group_by(Species) %>% 
  sample_frac(.10, replace = F)

train_25 <- files %>% 
  filter(Species %in% a$Species) %>% 
  filter(!new.file.name %in% test$new.file.name) %>% 
  group_by(Species) %>% 
  sample_frac(.25, replace = F)

train_50 <- files %>% 
  filter(Species %in% a$Species) %>% 
  filter(!new.file.name %in% test$new.file.name) %>% 
  group_by(Species) %>% 
  sample_frac(.5, replace = F)

b <- files %>% 
  group_by(Species) %>% 
  summarize(n = n()) %>% 
  filter(n>200)

train_flat200 <- files %>% 
  filter(Species %in% b$Species) %>% 
  group_by(Species) %>% 
  sample_n(200, replace = F)

test.200 <- files %>% 
  filter(Species %in% b$Species) %>% 
  filter(!new.file.name %in% train_flat200$new.file.name)

c <- files %>% 
  group_by(Species) %>% 
  summarize(n = n()) %>% 
  filter(n>500)

train_flat500 <- files %>% 
  filter(Species %in% c$Species) %>% 
  group_by(Species) %>% 
  sample_n(500, replace = F)

test.500 <- files %>% 
  filter(Species %in% c$Species) %>% 
  filter(!new.file.name %in% train_flat500$new.file.name)

table(test$new.file.name %in% train_2.5$new.file.name)
table(test$new.file.name %in% train_5$new.file.name)
table(test$new.file.name %in% train_10$new.file.name)
table(test$new.file.name %in% train_25$new.file.name)
table(test$new.file.name %in% train_50$new.file.name)

table(test.200$new.file.name %in% train_flat200$new.file.name)
table(test.500$new.file.name %in% train_flat500$new.file.name)

write.csv(test, "E:/Machine_Learning/MLCreel/train_test/testSS.csv")

write.csv(test, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/test.csv")
write.csv(test.200, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/test.200.csv")
write.csv(test.500, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/test.500.csv")

write.csv(train_2.5, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/tr2.5.csv")
write.csv(train_5, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/tr5.csv")
write.csv(train_10, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/tr10.csv")
write.csv(train_25, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/tr25.csv")
write.csv(train_50, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/tr50.csv")
write.csv(train_flat200, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/tr200.csv")
write.csv(train_flat500, "/Volumes/Seagate_Backup_Plus_Drive/Machine_Learning/MLCreel/train_test/tr500.csv")

test.per <- read.csv("E:/Machine_Learning/MLCreel/train_test/test.csv")
test.f200 <- read.csv("E:/Machine_Learning/MLCreel/train_test/test.200.csv")
test.f500 <- read.csv("E:/Machine_Learning/MLCreel/train_test/test.500.csv")

tr.2.5 <- read.csv("E:/Machine_Learning/MLCreel/train_test/tr2.5.csv") 
tr.5 <- read.csv("E:/Machine_Learning/MLCreel/train_test/tr5.csv") 
tr.10 <- read.csv("E:/Machine_Learning/MLCreel/train_test/tr10.csv") 
tr.25 <- read.csv("E:/Machine_Learning/MLCreel/train_test/tr25.csv") 
tr.50 <- read.csv("E:/Machine_Learning/MLCreel/train_test/tr50.csv") 
tr.200 <- read.csv("E:/Machine_Learning/MLCreel/train_test/tr200.csv") 
tr.500 <- read.csv("E:/Machine_Learning/MLCreel/train_test/tr500.csv")

