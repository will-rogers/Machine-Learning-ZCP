#Script to handle the merging of metadata from wildlog with the images themselves
###Includes resizing and drawing out images for analysis
########
library(stringr)
require(dplyr)
require(readr)

#This will be unique based on where images are stored
folder <- "/Volumes/Seagate_Backup_Plus_Drive/WildLog/Camera Trap Photos"
# path is the parent folder location on my external hard drive
files <- list.files(path = folder, full.names = F, recursive = TRUE)
ZCP.cam <- data.frame(files)
# write.csv(ZCP.cam, "FilesinWildlog.csv")

# ZCP.cam <- ZCP.cam %>%
#   filter(!str_detect(files, "Thumbs.db"))
# 
# ####       ZCP.cam$files <- paste0(folder,"/",ZCP.cam$files)
# 
# 
# #list.dirs(path = folder,full.names = TRUE)
# #stripping the capture time - should match that of the observations.csv
# ZCP.cam$time.captured <-NA
# library(exif)
# for (i in 684987:length(ZCP.cam$files)){
#   ZCP.cam$time.captured[i] <- read_exif(paste0(folder, "/", ZCP.cam$files[i]))$origin_timestamp #will take forever to run - need solution for 100K images
# }
# 
# write.csv(ZCP.cam, "ZCP.cam.csv")
ZCP.cam <- read.csv("ZCP.cam.csv")
View(ZCP.cam)
###################
ZCP.cam <- ZCP.cam %>%
  filter(!is.na(ZCP.cam$time.captured))

Observations <- read_csv("~/Documents/Hire Work 2019/Observations.csv") #observations from Eli

summary(ZCP.cam$time.captured %in% Observations$SIGHTINGDATE)

ZCP.cam$time.captured <- as.POSIXct(ZCP.cam$time.captured, format = "%Y:%m:%d %H:%M:%S")
Observations$time.captured <- as.POSIXct(Observations$SIGHTINGDATE, format = "%m/%d/%y %H:%M:%S")

summary(ZCP.cam$time.captured %in% Observations$time.captured)

ZCP.cam.sub <- ZCP.cam %>%
  filter(ZCP.cam$time.captured %in% Observations$time.captured)

######################
ZCP.cam$Year_Park <- rep(NA, length(ZCP.cam$files))
ZCP.cam$Season <- rep(NA, length(ZCP.cam$files))
ZCP.cam$Area <- rep(NA, length(ZCP.cam$files))
ZCP.cam$Section <- rep(NA, length(ZCP.cam$files))
ZCP.cam$Site <- rep(NA, length(ZCP.cam$files))
ZCP.cam$Camera <- rep(NA, length(ZCP.cam$files))
  
#k <- list()
# write.csv(ZCP.cam, "ZCP.cam.1.csv")
# ZCP.cam <- read.csv("ZCP.cam.1.csv")
for (i in 644158:length(ZCP.cam$files)){
  k <- str_split(ZCP.cam$files[i], "/")[[1]]
   ZCP.cam$Year_Park[i] <- k[str_detect(k, "Camera Traps")==T]
  # ZCP.cam$Season[i] <- k[7]
  # ZCP.cam$Area[i] <- k[8]
  # ZCP.cam$Section[i] <- k[str_detect(k[[1]], "Section")==T]
  # ZCP.cam$Site[i] <- k[str_detect(k[[1]], "Site")==T]
  # ZCP.cam$Camera[i] <- k[str_detect(k[[1]], "Camera ")==T]
}

ZCP.cam$Year_Park <- factor(ZCP.cam$Year_Park)
levels(ZCP.cam$Year_Park)

SLNP_2012 <- ZCP.cam %>% 
  filter(Year_Park == "2012 SLNP Camera Traps")
SLNP_2013 <- ZCP.cam %>% 
  filter(Year_Park == "2013 SLNP Camera Traps")
SLNP_2014 <- ZCP.cam %>% 
  filter(Year_Park == "2014 SLNP Camera Traps")
SLNP_2015 <- ZCP.cam %>% 
  filter(Year_Park == "2015 SLNP Camera Traps")
KNP_2013 <- ZCP.cam %>% 
  filter(Year_Park == "2013 KNP Camera Traps")
KNP_2014 <- ZCP.cam %>% 
  filter(Year_Park == "2014 KNP Camera Traps")
KNP_2015 <- ZCP.cam %>% 
  filter(Year_Park == "2015 KNP Camera Traps")

##################
# SLNP 2012
##################
for (i in 14177:length(SLNP_2012$files)){
  k <- str_split(SLNP_2012$files[i], "/")[[1]]
  SLNP_2012$Section[i] <- (k[2])
  SLNP_2012$Site[i] <- (k[3])
  SLNP_2012$Camera[i] <- (k[4])
  SLNP_2012$Picture[i] <- (k[5])
}
SLNP_2012

##################
# KNP 2013
##################
for (i in 1:length(KNP_2013$files)){
  k <- str_split(KNP_2013$files[i], "/")[[1]]
  KNP_2013$Season[i] <- (k[2])
  KNP_2013$Section[i] <- (k[2])
  KNP_2013$Site[i] <- (k[3])
  KNP_2013$Camera[i] <- (k[4])
  KNP_2013$Picture[i] <- (k[5])
}
KNP_2013
KNP_2013$Season[str_detect(KNP_2013$Season, "Cold Dry")==T] <- "Cold Dry"
KNP_2013$Season[str_detect(KNP_2013$Season, "Hot Dry")==T] <- "Hot Dry"
KNP_2013$Section[str_detect(KNP_2013$Section, "Section 1")==T] <- "Section 1"
KNP_2013$Section[str_detect(KNP_2013$Section, "Section 2")==T] <- "Section 2"
KNP_2013$Section[str_detect(KNP_2013$Section, "Section 3")==T] <- "Section 3"
unique(KNP_2013$Camera)

################## 
#SLNP 2013
##################
for (i in 1:length(SLNP_2013$files)){
  k <- str_split(SLNP_2013$files[i], "/")[[1]]
  SLNP_2013$Season[i] <- (k[2])
  SLNP_2013$Section[i] <- (k[2])
  SLNP_2013$Site[i] <- (k[3])
  SLNP_2013$Camera[i] <- (k[4])
  SLNP_2013$Picture[i] <- (k[6])
}
# SLNP_2013 <- SLNP_2013 %>% 
#   filter(Camera != "Nsefu Section 1")
# SLNP_2013 <- SLNP_2013 %>% 
#   filter(Camera != "Site 25")
unique(SLNP_2013$Site)

SLNP_2013$Season[str_detect(SLNP_2013$Season, "Cold Dry")==T] <- "Cold Dry"
SLNP_2013$Season[str_detect(SLNP_2013$Season, "Hot Dry")==T] <- "Hot Dry"

SLNP_2013$Area[str_detect(SLNP_2013$Section, "Main Game")==T] <- "Main Game"
SLNP_2013$Area[str_detect(SLNP_2013$Section, "Nsefu")==T] <- "Nsefu"

SLNP_2013$Section[str_detect(SLNP_2013$Section, "Section 1")==T] <- "Section 1"
SLNP_2013$Section[str_detect(SLNP_2013$Section, "Section 2")==T] <- "Section 2"
SLNP_2013$Section[str_detect(SLNP_2013$Section, "Section 3")==T] <- "Section 3"
SLNP_2013$Section[str_detect(SLNP_2013$Section, "Section 4")==T] <- "Section 4"

SLNP_2013$Camera[str_detect(SLNP_2013$Site, "MSU")==T] <- SLNP_2013$Site[str_detect(SLNP_2013$Site, "MSU")==T]
SLNP_2013$Camera[str_detect(SLNP_2013$Site, "ZCP")==T] <- SLNP_2013$Site[str_detect(SLNP_2013$Site, "ZCP")==T]
SLNP_2013$Site[str_detect(SLNP_2013$Site, "MSU")==T] <- NA
SLNP_2013$Site[str_detect(SLNP_2013$Site, "ZCP")==T] <- NA

##################
# KNP 2014
##################
for (i in 1:length(KNP_2014$files)){
  k <- str_split(KNP_2014$files[i], "/")[[1]]
  KNP_2014$Season[i] <- (k[2])
  KNP_2014$Section[i] <- (k[2])
  KNP_2014$Site[i] <- (k[3])
  KNP_2014$Camera[i] <- (k[4])
}
unique(KNP_2014$Camera)

KNP_2014$Season[str_detect(KNP_2014$Season, "Hot Dry")==F] <- "Cold Dry"
KNP_2014$Season[str_detect(KNP_2014$Season, "Hot Dry")==T] <- "Hot Dry"
KNP_2014$Section[str_detect(KNP_2014$Section, "Section 1")==T] <- "Section 1"
KNP_2014$Section[str_detect(KNP_2014$Section, "Section 2")==T] <- "Section 2"
KNP_2014$Section[str_detect(KNP_2014$Section, "Section 3")==T] <- "Section 3"

################## 
#SLNP 2014
##################
for (i in 1:length(SLNP_2014$files)){
  k <- str_split(SLNP_2014$files[i], "/")[[1]]
  SLNP_2014$Season[i] <- (k[2])
}
SLNP_2014.CD <- SLNP_2014 %>% 
  filter(Season == "Cold-dry")
SLNP_2014.HD <- SLNP_2014 %>% 
  filter(Season == "Hot-dry")

for (i in 1:length(SLNP_2014.CD$files)){
  k <- str_split(SLNP_2014.CD$files[i], "/")[[1]]
  SLNP_2014.CD$Season[i] <- (k[2])
  SLNP_2014.CD$Area[i] <- (k[3])
  SLNP_2014.CD$Section[i] <- (k[3])
  SLNP_2014.CD$Site[i] <- (k[4])
  SLNP_2014.CD$Camera[i] <- (k[5])
}

for (i in 1:length(SLNP_2014.HD$files)){
  k <- str_split(SLNP_2014.HD$files[i], "/")[[1]]
  SLNP_2014.HD$Season[i] <- (k[2])
  SLNP_2014.HD$Area[i] <- (k[3])
  SLNP_2014.HD$Section[i] <- (k[4])
  SLNP_2014.HD$Site[i] <- (k[5])
  SLNP_2014.HD$Camera[i] <- (k[6])
}

SLNP_2014 <- rbind(SLNP_2014.CD,SLNP_2014.HD)
unique(SLNP_2014$Section)
SLNP_2014$Season[str_detect(SLNP_2014$Season, "Cold-dry")==T] <- "Cold Dry"
SLNP_2014$Season[str_detect(SLNP_2014$Season, "Hot-dry")==T] <- "Hot Dry"

SLNP_2014$Area[str_detect(SLNP_2014$Section, "MG")==T] <- "Main Game"
SLNP_2014$Area[str_detect(SLNP_2014$Section, "Nsefu")==T] <- "Nsefu"
SLNP_2014$Area[str_detect(SLNP_2014$Section, "CNS")==T] <- "Nsefu"

SLNP_2014$Section[str_detect(SLNP_2014$Section, "Section 1")==T] <- "Section 1"
SLNP_2014$Section[str_detect(SLNP_2014$Section, "Section 2")==T] <- "Section 2"
SLNP_2014$Section[str_detect(SLNP_2014$Section, "Section 3")==T] <- "Section 3"
SLNP_2014$Section[str_detect(SLNP_2014$Section, "Section 4")==T] <- "Section 4"



##################
# KNP 2015
##################

for (i in 1:length(KNP_2015$files)){
  k <- str_split(KNP_2015$files[i], "/")[[1]]
  KNP_2015$Season[i] <- k[2]
  KNP_2015$Section[i] <- (k[3])
  KNP_2015$Camera[i] <- (k[5])
  KNP_2015$Site[i] <- NA
}
KNP_2015
levels(factor(KNP_2015$Season))
levels(factor(KNP_2015$Section))
levels(factor(KNP_2015$Site))
levels(factor(KNP_2015$Camera))
KNP_2015$Season[str_detect(KNP_2015$Season, "Cold Dry")==T] <- "Cold Dry"
KNP_2015$Season[str_detect(KNP_2015$Season, "Hot Dry")==T] <- "Hot Dry"
KNP_2015$Section[str_detect(KNP_2015$Section, "S1")==T] <- "Section 1"
KNP_2015$Section[str_detect(KNP_2015$Section, "S2")==T] <- "Section 2"
KNP_2015$Section[str_detect(KNP_2015$Section, "S3")==T] <- "Section 3"

KNP_2015$Camera[str_detect(KNP_2015$Camera, "KCT 9")==T] <- "KCT 9"
KNP_2015$Camera[str_detect(KNP_2015$Site, "KCT 4")==T] <- "KCT 4"
KNP_2015$Camera[str_detect(KNP_2015$Site, "KCT 6")==T] <- "KCT 6"
KNP_2015$Camera[str_detect(KNP_2015$Site, "KCT 7")==T] <- "KCT 7"
unique(KNP_2015$Section)
################## 
#SLNP 2015
##################
for (i in 1:length(SLNP_2015$files)){
  k <- str_split(SLNP_2015$files[i], "/")[[1]]
  SLNP_2015$Season[i] <- k[2]
  SLNP_2015$Area[i] <- k[3]
  SLNP_2015$Section[i] <- k[4]
  SLNP_2015$Site[i] <- k[5]
  SLNP_2015$Camera[i] <- k[6]
}

SLNP_2015_ns <- SLNP_2015 %>% 
  filter(Area == "Nsefu")
for (i in 1:length(SLNP_2015_ns$files)){
  k <- str_split(SLNP_2015_ns$files[i], "/")[[1]]
  SLNP_2015_ns$Season[i] <- (k[2])
  SLNP_2015_ns$Area[i] <- (k[3])
  SLNP_2015_ns$Section[i] <- (k[5])
  SLNP_2015_ns$Site[i] <- (k[6])
  SLNP_2015_ns$Camera[i] <- (k[7])
}

SLNP_2015 <- bind_rows(SLNP_2015_mg,SLNP_2015_ns)

SLNP_2015$Camera[str_detect(SLNP_2015$Camera, ".JPG")==T] <- NA
unique(SLNP_2015$Site)
SLNP_2015$Camera[str_detect(SLNP_2015$Site, "MSU")==T] <- SLNP_2015$Site[str_detect(SLNP_2015$Site, "MSU")==T]


SLNP_2015$Season[str_detect(SLNP_2015$Season, "Cold Dry")==T] <- "Cold Dry"
SLNP_2015$Season[str_detect(SLNP_2015$Season, "Cool Dry")==T] <- "Cold Dry"

SLNP_2015$Section[str_detect(SLNP_2015$Section, "Section 1")==T] <- "Section 1"
SLNP_2015$Section[str_detect(SLNP_2015$Section, "Section 2")==T] <- "Section 2"
SLNP_2015$Section[str_detect(SLNP_2015$Section, "Section 3")==T] <- "Section 3"
SLNP_2015$Section[str_detect(SLNP_2015$Section, "Section 4")==T] <- "Section 4"

SLNP_2013$Camera[str_detect(SLNP_2013$Site, "MSU")==T] <- SLNP_2013$Site[str_detect(SLNP_2013$Site, "MSU")==T]
SLNP_2013$Camera[str_detect(SLNP_2013$Site, "ZCP")==T] <- SLNP_2013$Site[str_detect(SLNP_2013$Site, "ZCP")==T]
SLNP_2013$Site[str_detect(SLNP_2013$Site, "MSU")==T] <- NA
SLNP_2013$Site[str_detect(SLNP_2013$Site, "ZCP")==T] <- NA

ZCP.cam.sub <- bind_rows(SLNP_2012,SLNP_2013,SLNP_2014,SLNP_2015,KNP_2013,KNP_2014,KNP_2015)

ZCP.cam.sub$Year_Park <- factor(ZCP.cam.sub$Year_Park )
unique(ZCP.cam.sub$Year_Park)

ZCP.cam.sub$Season <- factor(ZCP.cam.sub$Season)
unique(ZCP.cam.sub$Season)
levels(ZCP.cam.sub$Season)[levels(ZCP.cam.sub$Season) == "Hot-Dry"] <- "Hot Dry"

ZCP.cam.sub$Area <- factor(ZCP.cam.sub$Area)
unique(ZCP.cam.sub$Area)

ZCP.cam.sub$Section <- factor(ZCP.cam.sub$Section)
unique(ZCP.cam.sub$Section)

ZCP.cam.sub$Site <- factor(ZCP.cam.sub$Site)
unique(ZCP.cam.sub$Site)

ZCP.cam.sub$Camera <- factor(ZCP.cam.sub$Camera)
unique(ZCP.cam.sub$Camera)

write.csv(ZCP.cam.sub, "DATABASEFORZCP")
head(ZCP.cam.sub)

ZCP.cam.sub[which(ZCP.cam.sub$Section == 'Site 1'),]


head(ZCP.cam.sub)

ZCP.cam.sub$Season <- factor(ZCP.cam.sub$Season)
ZCP.cam.sub$Area <- factor(ZCP.cam.sub$Area)
ZCP.cam.sub$Section <- factor(ZCP.cam.sub$Section)
ZCP.cam.sub$Site <- factor(ZCP.cam.sub$Site)
ZCP.cam.sub$Camera <- factor(ZCP.cam.sub$Camera)
summary(ZCP.cam.sub)


#main headings - should be fairly similar so little need to build multiple levels
levels(ZCP.cam.sub$Year_Park)[levels(ZCP.cam.sub$Year_Park ) == "2015 SLNP Camera Traps"] <- "2015_SLNP"
levels(ZCP.cam.sub$Season)[levels(ZCP.cam.sub$Season) == "Cold Dry"] <- "Cold_Dry"
levels(ZCP.cam.sub$Area)[levels(ZCP.cam.sub$Area) == "Main Game"] <- "Main_Game"

#sub headings - will be very variable and likely wont follow a standard procedure
ZCP.cam.sub$Section <- str_replace_all((ZCP.cam.sub$Section), fixed(" "), "")
ZCP.cam.sub$Site <- str_replace_all((ZCP.cam.sub$Site), fixed(" "), "")
ZCP.cam.sub$Camera <- str_replace_all((ZCP.cam.sub$Camera), fixed(" "), "")

ZCP.cam.sub$unique.id <- 1:length(ZCP.cam.sub$files)

ZCP.cam.sub$new.photo.id <- paste0(paste(ZCP.cam.sub$Year_Park,ZCP.cam.sub$Season,ZCP.cam.sub$Area,ZCP.cam.sub$Section,ZCP.cam.sub$Site,ZCP.cam.sub$Camera,ZCP.cam.sub$unique.id, sep = "_"),".JPG")

ZCP.cam.sub$base.dir <- rep(NA, length(ZCP.cam.sub$files))
for (i in 1:length(ZCP.cam.sub$files)){
  k <- str_split(ZCP.cam.sub$files[i], "/")[[1]]
  final.string <- length(k)
  dir <- k[1:(final.string-1)]
  ZCP.cam.sub$base.dir[i] <- paste(dir, collapse='/')
}

ZCP.cam.sub$new.file.name <- paste(ZCP.cam.sub$base.dir,ZCP.cam.sub$new.photo.id,sep = '/')

file.rename(as.character(ZCP.cam.sub$files), ZCP.cam.sub$new.photo.id)

length(unique(Observations$SIGHTINGDATE)) - length((Observations$SIGHTINGDATE)) #may be problematic in the future

table(Observations$ELEMENTNAME)

sub.merger <- merge(ZCP.cam.sub, Observations, by = "time.captured")
sub.merger$new.file.name[which(sub.merger$ELEMENTNAME == "Serval Cat")] 

require(jpeg)
img <- readJPEG(sub.merger$new.file.name[which(sub.merger$ELEMENTNAME == "Serval Cat")])
plot(1:2, type='n')
rasterImage(img, 1, 2, 2, 1) #WOOOHOOOOOOOOOOO



resize <- data.frame(stored.as   = rep(NA, length(sub.merger$files)), #labeling the current storage
                     file.name   = rep(NA, length(sub.merger$files)), #keeping any easy form of photo ID around
                     dest.resize = rep(NA, length(sub.merger$files)),
                     result      = rep(NA, length(sub.merger$files))) #specifying the destination folder

# Function tries to copy and resize images to a new location; 
# however, script does not fail if file is corrupted and records
# whether it worked to a new vector.

process_image <- function(image.path, new.image.path){
  tryCatch(
    {
      image_write(image_scale(image_read(image.path),"256x256!"), path = new.image.path)
      return(substr(image.path, nchar(image.path) - 2, nchar(image.path)))
    },
    error = function(e)  
    {
      return(NA)
    }
  )
}


for (i in 1:length(sub.merger$files)){
  resize$file.name[i] <- sub.merger$new.photo.id[i]
  resize$stored.as[i] <- sub.merger$new.file.name[i] #labeling storage
  resize$dest.resize[i] <- paste0("/Volumes/Seagate Backup Plus Drive/Wildlog/256x256_Photos/", resize$file.name[i]) #labeling destination
  resize$result[i] <- future_map2_chr(resize$stored.as[i], resize$dest.resize[i], process_image) 
}

length(list.files(path = "/Volumes/Seagate Backup Plus Drive/Wildlog/256x256_Photos"))


img <- readJPEG("/Volumes/Seagate_Backup_Plus_Drive/SS/256x256_Photos/S1_D07_R3_PICT0324.JPG")
plot(1:2, type='n')
rasterImage(img, 1, 2, 2, 1)
 l
