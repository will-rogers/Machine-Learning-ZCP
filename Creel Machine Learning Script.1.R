#################################################################################
#Script to handle the merging of metadata from wildlog with the images themselves
#################################################################################
require(stringr)
require(dplyr)
require(readr)
require(tidyr)
require(jpeg)
require(furrr)
require(magick)

#This will be unique based on where images are stored
folder <- "/Volumes/Seagate_Backup_Plus_Drive/WildLog/Camera Trap Photos"
# path is the parent folder location on my external hard drive
# files <- list.files(path = folder, full.names = F, recursive = TRUE)
# ZCP.cam <- data.frame(files)
# write.csv(ZCP.cam, "FilesinWildlog.csv")
ZCP.cam <- read.csv("ZCP.cam.csv") 

ZCP.cam <- ZCP.cam %>%
  filter(!is.na(ZCP.cam$time.captured)) #removing excel, Wildlog files, ect.

Split <- separate(ZCP.cam, files, into = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), 
                  sep = "/", remove = FALSE, convert = FALSE, extra = "warn", 
                  fill = "warn") #splitting file name into a variable number of columns a:j

Split$Year_Park <- NA #creating variables
Split$Season <- NA 
Split$Area <- NA
Split$Section <- NA
Split$Site <- NA
Split$Camera <- NA
Split$Image <- NA

##### This section attributes the broken string elements (as best as possible) to a variable name
unique(Split$a) 
Split$Year_Park <- Split$a

unique(Split$b) 
Split$Season[which(str_detect(Split$b, "Cold")==T)] <- "Cold Dry"
Split$Season[which(str_detect(Split$b, "Hot")==T)] <- "Hot Dry"
Split$Area[which(str_detect(Split$b, "Nsefu")==T)] <- "Nsefu"
Split$Area[which(str_detect(Split$b, "Main Game")==T)] <- "Main Game"
Split$Section[which(str_detect(Split$b, "Section")==T)] <- Split$b[which(str_detect(Split$b, "Section")==T)]

unique(Split$c) 
Split$Area[which(str_detect(Split$c, "Nsefu")==T)] <- "Nsefu"
Split$Area[which(str_detect(Split$c, "Main Game")==T)] <- "Main Game"
Split$Area[which(str_detect(Split$c, "MG")==T)] <- "Main Game"
Split$Section[which(str_detect(Split$c, "Section")==T)] <- Split$c[which(str_detect(Split$c, "Section")==T)]
Split$Section[which(str_detect(Split$c, "KCT")==T)] <- Split$c[which(str_detect(Split$c, "KCT")==T)]
Split$Site[which(str_detect(Split$c, "Site")==T)] <- Split$c[which(str_detect(Split$c, "Site")==T)]
Split$Camera[which(str_detect(Split$c, "MSU")==T)] <- Split$c[which(str_detect(Split$c, "MSU")==T)]
Split$Camera[which(str_detect(Split$c, "ZCP")==T)] <- Split$c[which(str_detect(Split$c, "ZCP")==T)]

unique(Split$d)
Split$Section[which(str_detect(Split$d, "Section")==T)] <- Split$d[which(str_detect(Split$d, "Section")==T)]
Split$Site[which(str_detect(Split$d, "Site")==T)] <- Split$d[which(str_detect(Split$d, "Site")==T)]
Split$Camera[which(str_detect(Split$d, "MSU")==T)] <- Split$d[which(str_detect(Split$d, "MSU")==T)]
Split$Camera[which(str_detect(Split$d, "ZCP")==T)] <- Split$d[which(str_detect(Split$d, "ZCP")==T)]
Split$Camera[which(str_detect(Split$d, "Cam")==T)] <- Split$d[which(str_detect(Split$d, "Cam")==T)]
Split$Camera[which(str_detect(Split$d, "Camera")==T)] <- Split$d[which(str_detect(Split$d, "Camera")==T)]
Split$Camera[which(str_detect(Split$d, "CMG")==T)] <- Split$d[which(str_detect(Split$d, "CMG")==T)]
Split$Camera[which(str_detect(Split$d, "CNS")==T)] <- Split$d[which(str_detect(Split$d, "CNS")==T)]

unique(Split$e)
Split$Image[which(str_detect(Split$e, ".JPG")==T)] <- Split$e[which(str_detect(Split$e, ".JPG")==T)]
Split$e[which(str_detect(Split$e, ".JPG")==T)] <- NA
unique(Split$e)
Split$Section[which(str_detect(Split$e, "Section")==T)] <- Split$e[which(str_detect(Split$e, "Section")==T)]
Split$Section[which(str_detect(Split$e, "S")==T)] <- Split$e[which(str_detect(Split$e, "Section")==T)]
Split$Site[which(str_detect(Split$e, "Site")==T)] <- Split$e[which(str_detect(Split$e, "Site")==T)]
Split$Camera[which(str_detect(Split$e, "MSU")==T)] <- Split$e[which(str_detect(Split$e, "MSU")==T)]
Split$Camera[which(str_detect(Split$e, "ZCP")==T)] <- Split$e[which(str_detect(Split$e, "ZCP")==T)]
Split$Camera[which(str_detect(Split$e, "Cam")==T)] <- Split$e[which(str_detect(Split$e, "Cam")==T)]
Split$Camera[which(str_detect(Split$e, "Camera")==T)] <- Split$e[which(str_detect(Split$e, "Camera")==T)]
Split$Camera[which(str_detect(Split$e, "CMG")==T)] <- Split$e[which(str_detect(Split$e, "CMG")==T)]
Split$Camera[which(str_detect(Split$e, "CNS")==T)] <- Split$e[which(str_detect(Split$e, "CNS")==T)]
Split$Camera[which(str_detect(Split$e, "KCT")==T)] <- Split$e[which(str_detect(Split$e, "KCT")==T)]

unique(Split$f)
Split$Image[which(str_detect(Split$f, ".JPG")==T)] <- Split$e[which(str_detect(Split$f, ".JPG")==T)]
Split$Image[which(str_detect(Split$f, ".jpg")==T)] <- Split$e[which(str_detect(Split$f, ".jpg")==T)]
Split$f[which(str_detect(Split$f, ".JPG")==T)] <- NA
Split$f[which(str_detect(Split$f, ".jpg")==T)] <- NA
unique(Split$f)
Split$Section[which(str_detect(Split$f, "Section")==T)] <- Split$f[which(str_detect(Split$f, "Section")==T)]
Split$Site[which(str_detect(Split$f, "Site")==T)] <- Split$f[which(str_detect(Split$f, "Site")==T)]
Split$Camera[which(str_detect(Split$f, "MSU")==T)] <- Split$f[which(str_detect(Split$f, "MSU")==T)]
Split$Camera[which(str_detect(Split$f, "ZCP")==T)] <- Split$f[which(str_detect(Split$f, "ZCP")==T)]
Split$Camera[which(str_detect(Split$f, "Cam")==T)] <- Split$f[which(str_detect(Split$f, "Cam")==T)]
Split$Camera[which(str_detect(Split$f, "Camera")==T)] <- Split$f[which(str_detect(Split$f, "Camera")==T)]
Split$Camera[which(str_detect(Split$f, "CMG")==T)] <- Split$f[which(str_detect(Split$f, "CMG")==T)]
Split$Camera[which(str_detect(Split$f, "CNS")==T)] <- Split$f[which(str_detect(Split$f, "CNS")==T)]
Split$Camera[which(str_detect(Split$f, "KCT")==T)] <- Split$f[which(str_detect(Split$f, "KCT")==T)]

unique(Split$g)
Split$Image[which(str_detect(Split$g, ".JPG")==T)] <- Split$e[which(str_detect(Split$g, ".JPG")==T)]
Split$Image[which(str_detect(Split$g, ".jpg")==T)] <- Split$e[which(str_detect(Split$g, ".jpg")==T)]
Split$g[which(str_detect(Split$g, ".JPG")==T)] <- NA
Split$g[which(str_detect(Split$g, ".jpg")==T)] <- NA
unique(Split$g)
Split$Camera[which(str_detect(Split$g, "MSU")==T)] <- Split$g[which(str_detect(Split$g, "MSU")==T)]
Split$Camera[which(str_detect(Split$g, "ZCP")==T)] <- Split$g[which(str_detect(Split$g, "ZCP")==T)]
Split$Camera[which(str_detect(Split$g, "Cam")==T)] <- Split$g[which(str_detect(Split$g, "Cam")==T)]
Split$Camera[which(str_detect(Split$g, "Camera")==T)] <- Split$g[which(str_detect(Split$g, "Camera")==T)]
Split$Camera[which(str_detect(Split$g, "CMG")==T)] <- Split$g[which(str_detect(Split$g, "CMG")==T)]
Split$Camera[which(str_detect(Split$g, "CNS")==T)] <- Split$g[which(str_detect(Split$g, "CNS")==T)]
Split$Camera[which(str_detect(Split$g, "KCT")==T)] <- Split$g[which(str_detect(Split$g, "KCT")==T)]

unique(Split$h)
Split$Image[which(str_detect(Split$h, ".JPG")==T)] <- Split$e[which(str_detect(Split$h, ".JPG")==T)]
Split$Image[which(str_detect(Split$h, ".jpg")==T)] <- Split$e[which(str_detect(Split$h, ".jpg")==T)]
Split$h[which(str_detect(Split$h, ".JPG")==T)] <- NA
Split$h[which(str_detect(Split$h, ".jpg")==T)] <- NA
unique(Split$h)
Split$Camera[which(str_detect(Split$h, "ZCP")==T)] <- Split$h[which(str_detect(Split$h, "ZCP")==T)]

unique(Split$i)
Split$Image[which(str_detect(Split$i, ".JPG")==T)] <- Split$e[which(str_detect(Split$i, ".JPG")==T)]
Split$Image[which(str_detect(Split$i, ".jpg")==T)] <- Split$e[which(str_detect(Split$i, ".jpg")==T)]
Split$i[which(str_detect(Split$i, ".JPG")==T)] <- NA
Split$i[which(str_detect(Split$i, ".jpg")==T)] <- NA
unique(Split$i)

unique(Split$j)


##### Obviously there is variance in how people have named things, needs to be fixed
Leveling <- Split 

unique(Leveling$Year_Park) #standard
Leveling$Year <- NA #creating variables
Leveling$Park <- NA #creating variables
Leveling$Year[which(str_detect(Leveling$Year_Park, "2012")==T)] <- "2012"
Leveling$Year[which(str_detect(Leveling$Year_Park, "2013")==T)] <- "2013"
Leveling$Year[which(str_detect(Leveling$Year_Park, "2014")==T)] <- "2014"
Leveling$Year[which(str_detect(Leveling$Year_Park, "2015")==T)] <- "2015"
Leveling$Park[which(str_detect(Leveling$Year_Park, "SLNP")==T)] <- "SLNP"
Leveling$Park[which(str_detect(Leveling$Year_Park, "KNP")==T)] <- "KNP"
Leveling$Year_Park <- factor(Leveling$Year_Park)
Leveling$Year <- factor(Leveling$Year)
Leveling$Park <- factor(Leveling$Park)

unique(Leveling$Season) #standard
Leveling$Season <- factor(Leveling$Season)

unique(Leveling$Area) #standard
Leveling$Area <- factor(Leveling$Area)

unique(Leveling$Section) #Some oddities
Leveling$Section[which(str_detect(Leveling$Section, "Section 1")==T)] <- "Section 1"
Leveling$Section[which(str_detect(Leveling$Section, "S1")==T)] <- "Section 1"
Leveling$Section[which(str_detect(Leveling$Section, "Section 2")==T)] <- "Section 2"
Leveling$Section[which(str_detect(Leveling$Section, "S2")==T)] <- "Section 2"
Leveling$Section[which(str_detect(Leveling$Section, "Section 3")==T)] <- "Section 3"
Leveling$Section[which(str_detect(Leveling$Section, "S3")==T)] <- "Section 3"
Leveling$Section[which(str_detect(Leveling$Section, "Section 4")==T)] <- "Section 4"
Leveling$Section[which(str_detect(Leveling$Section, "S4")==T)] <- "Section 4"
Leveling$Section <- factor(Leveling$Section)
levels(Leveling$Section)

unique(Leveling$Site) #very bad
Leveling$Site[which(str_detect(Leveling$Site, "Site3")==T)] <- "Site 3"
Leveling$Site[which(str_detect(Leveling$Site, "Site4")==T)] <- "Site 4"
Leveling$Site[which(str_detect(Leveling$Site, "Site5")==T)] <- "Site 5"
Leveling$Site[which(str_detect(Leveling$Site, "Site6")==T)] <- "Site 6"
Leveling$Site[which(str_detect(Leveling$Site, "Site7")==T)] <- "Site 7"
Leveling$Site[which(str_detect(Leveling$Site, "Site  8")==T)] <- "Site 8"
Leveling$Site[which(str_detect(Leveling$Site, "Site9")==T)] <- "Site 9"
Leveling$Site[which(str_detect(Leveling$Site, "Site10")==T)] <- "Site 10"
Leveling$Site[which(str_detect(Leveling$Site, "Site11")==T)] <- "Site 11"
Leveling$Site[which(str_detect(Leveling$Site, "Site12")==T)] <- "Site 12"
Leveling$Site[which(str_detect(Leveling$Site, "Site13")==T)] <- "Site 13"
Leveling$Site[which(str_detect(Leveling$Site, "Site14")==T)] <- "Site 14"
Leveling$Site[which(str_detect(Leveling$Site, "Site15")==T)] <- "Site 15"
Leveling$Site[which(str_detect(Leveling$Site, "Site16")==T)] <- "Site 16"
Leveling$Site[which(str_detect(Leveling$Site, "Site17")==T)] <- "Site 17"
Leveling$Site[which(str_detect(Leveling$Site, "Site18")==T)] <- "Site 18"
Leveling$Site[which(str_detect(Leveling$Site, "Site19")==T)] <- "Site 19"
Leveling$Site[which(str_detect(Leveling$Site, "Site20")==T)] <- "Site 20"
Leveling$Site[which(str_detect(Leveling$Site, "Site21")==T)] <- "Site 21"
Leveling$Site[which(str_detect(Leveling$Site, "Site22")==T)] <- "Site 22"
Leveling$Site[which(str_detect(Leveling$Site, "Site23")==T)] <- "Site 23"
Leveling$Site[which(str_detect(Leveling$Site, "Site24")==T)] <- "Site 24"
Leveling$Site[which(str_detect(Leveling$Site, "Site25")==T)] <- "Site 25"
Leveling$Site[which(str_detect(Leveling$Site, "Site1")==T)] <- "Site 1"
Leveling$Site[which(str_detect(Leveling$Site, "Site2")==T)] <- "Site 2"
Leveling$Site[which(str_detect(Leveling$Site, "Site 3")==T)] <- "Site 3"
Leveling$Site[which(str_detect(Leveling$Site, "Site 4")==T)] <- "Site 4"
Leveling$Site[which(str_detect(Leveling$Site, "Site 5")==T)] <- "Site 5"
Leveling$Site[which(str_detect(Leveling$Site, "Site 6")==T)] <- "Site 6"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 1 Site 1")==T)] <- "Site 1"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 1 Site 2")==T)] <- "Site 2"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 1 Site 3")==T)] <- "Site 3"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 1 Site 4")==T)] <- "Site 4"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 1 Site 5")==T)] <- "Site 5"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 2 Site 10")==T)] <- "Site 10"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 2 Site 6")==T)] <- "Site 6"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 2 Site 7")==T)] <- "Site 7"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 2 Site 8")==T)] <- "Site 8"
Leveling$Site[which(str_detect(Leveling$Site, "KNP Hot 2015 Section 2 Site 9")==T)] <- "Site 9"
Leveling$Site[which(str_detect(Leveling$Site, "Site 1 - Needs Trimming")==T)] <- "Site 1"
Leveling$Site <- factor(Leveling$Site)
levels(Leveling$Site)

unique(Leveling$Camera) #Very bad
Leveling$Camera[which(Leveling$Camera == "Cam 1")] <- "Camera 1"
Leveling$Camera[which(Leveling$Camera == "Cam 2")] <- "Camera 2"
Leveling$Camera[which(Leveling$Camera == "Cam 3")] <- "Camera 3"
Leveling$Camera[which(Leveling$Camera == "Cam 4")] <- "Camera 4"
Leveling$Camera[which(Leveling$Camera == "Cam 5")] <- "Camera 5"
Leveling$Camera[which(Leveling$Camera == "Cam 6")] <- "Camera 6"
Leveling$Camera[which(Leveling$Camera == "Cam 7")] <- "Camera 7"
Leveling$Camera[which(Leveling$Camera == "Cam 8")] <- "Camera 8"
Leveling$Camera[which(Leveling$Camera == "Cam 9")] <- "Camera 9"
Leveling$Camera[which(Leveling$Camera == "Cam 10")] <- "Camera 10"
Leveling$Camera[which(Leveling$Camera == "Camera10")] <- "Camera 10"
Leveling$Camera[which(Leveling$Camera == "Camera9")] <- "Camera 9"
Leveling$Camera[which(Leveling$Camera == "Camera8")] <- "Camera 8"
Leveling$Camera[which(Leveling$Camera == "Camera7")] <- "Camera 7"
Leveling$Camera[which(Leveling$Camera == "Camera6")] <- "Camera 6"
Leveling$Camera[which(Leveling$Camera == "Camera5")] <- "Camera 5"
Leveling$Camera[which(Leveling$Camera == "Camera4")] <- "Camera 4"
Leveling$Camera[which(Leveling$Camera == "Camera3")] <- "Camera 3"
Leveling$Camera[which(Leveling$Camera == "Camera2")] <- "Camera 2"
Leveling$Camera[which(Leveling$Camera == "Camera1")] <- "Camera 1"
Leveling$Camera[which(Leveling$Camera == "MSU 10 NOT CURRENT")] <- "MSU 10"
Leveling$Camera[which(Leveling$Camera == "ZCP11")] <- "ZCP 11"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 1")] <- "KCT 1"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 2")] <- "KCT 2"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 3")] <- "KCT 3"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 4")] <- "KCT 4"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 5")] <- "KCT 5"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 6")] <- "KCT 6"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 7")] <- "KCT 7"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 8")] <- "KCT 8"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 9")] <- "KCT 9"
Leveling$Camera[which(Leveling$Camera == "S2 KCT 10")] <- "KCT 10"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 1")] <- "KCT 1"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 1 ")] <- "KCT 1"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 2")] <- "KCT 2"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 3")] <- "KCT 3"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 4")] <- "KCT 4"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 5")] <- "KCT 5"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 6")] <- "KCT 6"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 6 ")] <- "KCT 6"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 7")] <- "KCT 7"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 8")] <- "KCT 8"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 8 to change location")] <- "KCT 8"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 9")] <- "KCT 9"
Leveling$Camera[which(Leveling$Camera == "S3 KCT 10")] <- "KCT 10"
Leveling$Camera[which(Leveling$Camera == "MSU 01")] <- "MSU 1"
Leveling$Camera[which(Leveling$Camera == "MSU 02")] <- "MSU 2"
Leveling$Camera[which(Leveling$Camera == "MSU 03")] <- "MSU 3"
Leveling$Camera[which(Leveling$Camera == "MSU 04")] <- "MSU 4"
Leveling$Camera[which(Leveling$Camera == "MSU 05")] <- "MSU 5"
Leveling$Camera[which(Leveling$Camera == "MSU 06")] <- "MSU 6"
Leveling$Camera[which(Leveling$Camera == "MSU 07")] <- "MSU 7"
Leveling$Camera[which(Leveling$Camera == "MSU 08")] <- "MSU 8"
Leveling$Camera[which(Leveling$Camera == "MSU 09")] <- "MSU 9"
Leveling$Camera[which(Leveling$Camera == "MSU1")] <- "MSU 1"
Leveling$Camera[which(Leveling$Camera == "MSU2")] <- "MSU 2"
Leveling$Camera[which(Leveling$Camera == "MSU3")] <- "MSU 3"
Leveling$Camera[which(Leveling$Camera == "MSU4")] <- "MSU 4"
Leveling$Camera[which(Leveling$Camera == "MSU5")] <- "MSU 5"
Leveling$Camera[which(Leveling$Camera == "MSU6")] <- "MSU 6"
Leveling$Camera[which(Leveling$Camera == "MSU7")] <- "MSU 7"
Leveling$Camera[which(Leveling$Camera == "MSU8")] <- "MSU 8"
Leveling$Camera[which(Leveling$Camera == "MSU9")] <- "MSU 9"
Leveling$Camera[which(Leveling$Camera == "MSU10")] <- "MSU 10"
Leveling$Camera[which(Leveling$Camera == "MSU11")] <- "MSU 11"
Leveling$Camera[which(Leveling$Camera == "MSU12")] <- "MSU 12"
Leveling$Camera[which(Leveling$Camera == "MSU13")] <- "MSU 13"
Leveling$Camera[which(Leveling$Camera == "MSU14")] <- "MSU 14"
Leveling$Camera[which(Leveling$Camera == "ZCP1")] <- "ZCP 1"
Leveling$Camera[which(Leveling$Camera == "ZCP2")] <- "ZCP 2"
Leveling$Camera[which(Leveling$Camera == "ZCP3")] <- "ZCP 3"
Leveling$Camera[which(Leveling$Camera == "ZCP4")] <- "ZCP 4"
Leveling$Camera[which(Leveling$Camera == "ZCP5")] <- "ZCP 5"
Leveling$Camera[which(Leveling$Camera == "ZCP6")] <- "ZCP 6"
Leveling$Camera[which(Leveling$Camera == "ZCP7")] <- "ZCP 7"
Leveling$Camera[which(Leveling$Camera == "ZCP8")] <- "ZCP 8"
Leveling$Camera[which(Leveling$Camera == "ZCP9")] <- "ZCP 9"
Leveling$Camera[which(Leveling$Camera == "ZCP10")] <- "ZCP 10"
Leveling$Camera[which(Leveling$Camera == "ZCP11")] <- "ZCP 11"
Leveling$Camera[which(Leveling$Camera == "ZCP12")] <- "ZCP 12"
Leveling$Camera[which(Leveling$Camera == "ZCP13")] <- "ZCP 13"
Leveling$Camera[which(Leveling$Camera == "ZCP14")] <- "ZCP 14"
Leveling$Camera[which(Leveling$Camera == "ZCP15")] <- "ZCP 15"
Leveling$Camera[which(Leveling$Camera == "ZCP16")] <- "ZCP 16"
Leveling$Camera[which(Leveling$Camera == "ZCP17")] <- "ZCP 17"
Leveling$Camera[which(Leveling$Camera == "Camera 1 Ty_Keep Guard")] <- "Camera 1"
Leveling$Camera[which(Leveling$Camera == "Camera 2_Caz Bushnell HD")] <- "Camera 2"
Leveling$Camera <- factor(Leveling$Camera)


##### Just trying to get a better sense of what is missing and potentially why
table(is.na(Leveling$Year_Park)) #all have some study year and park - good
table(is.na(Leveling$Season)) #78k missing season - just from some weird camera traps, shouldnt be an issue
table(is.na(Leveling$Area)) # 274k missing area - area was only important for SLNP so KNP and some odd balls will not have it
table(Leveling$Park)
table(is.na(Leveling$Section)) # 13K missing section - just from some weird camera traps, shouldnt be an issue
table(is.na(Leveling$Site)) # 277k missing site - 
table(is.na(Leveling$Camera)) # all have some camera - good; we should be able to figure out with a bit of detective work where any one image comes from!

odd.cases <- Leveling[which(complete.cases(Leveling[,c("Season")])==F),]
table(odd.cases$Park)
View(odd.cases)

##### Lets Check our data before going on to attribute species
Leveling %>% 
  group_by(files) %>% 
  filter(n()>1) #good - files dont show up in duplicate
Leveling %>% 
  group_by(time.captured, Camera) %>% 
  filter(n()>1) #bad news - times in between photos is not resolute enough to pick up differences in the 5 shot sequence

##### Adding in species id's
Observations <- read_csv("~/Documents/Hire Work 2019/Observations.csv") #observations from Eli
summary(ZCP.cam$time.captured %in% Observations$SIGHTINGDATE)

Leveling$time.captured <- as.POSIXct(Leveling$time.captured, format = "%Y:%m:%d %H:%M:%S")
Observations$time.captured <- as.POSIXct(Observations$SIGHTINGDATE, format = "%m/%d/%y %H:%M:%S")

summary(Leveling$time.captured %in% Observations$time.captured)

Only.First <- Leveling %>%
  filter(Leveling$time.captured %in% Observations$time.captured)

nrow(Observations)
nrow(Only.First)

Merged <- merge(Only.First, Observations, by = "time.captured")
length(Merged[,1]) #37713
length(unique(Merged$files)) #36748 - 965 non-unique entries - duplicates in time - need to sort out 

#Lets add some identifying variables to Observations.csv and then remerge to see if that resolves the issues
table(Observations$LOCATIONNAME)

Observations$Park <- NA
Observations$Park[which(str_detect(Observations$LOCATIONNAME, "KNP")==T)] <- "KNP"
Observations$Park[which(str_detect(Observations$LOCATIONNAME, "CMG")==T)] <- "SLNP"
Observations$Park[which(str_detect(Observations$LOCATIONNAME, "CNS")==T)] <- "SLNP"
unique(Observations$Park)

Observations$Section <- NA
Observations$Section[which(str_detect(Observations$LOCATIONNAME, "Section 1")==T)] <- "Section 1"
Observations$Section[which(str_detect(Observations$LOCATIONNAME, "Section 2")==T)] <- "Section 2"
Observations$Section[which(str_detect(Observations$LOCATIONNAME, "Section 3")==T)] <- "Section 3"
Observations$Section[which(str_detect(Observations$LOCATIONNAME, "Section 4")==T)] <- "Section 4"
unique(Observations$Section)

Observations$Site <- NA
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 1\\b")==T)] <- "Site 1"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 2\\b")==T)] <- "Site 2"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 3\\b")==T)] <- "Site 3"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 4\\b")==T)] <- "Site 4"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 5\\b")==T)] <- "Site 5"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 6\\b")==T)] <- "Site 6"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 7\\b")==T)] <- "Site 7"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 8\\b")==T)] <- "Site 8"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 9\\b")==T)] <- "Site 9"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 10\\b")==T)] <- "Site 10"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 11\\b")==T)] <- "Site 11"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 12\\b")==T)] <- "Site 12"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 13\\b")==T)] <- "Site 13"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 14\\b")==T)] <- "Site 14"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 15\\b")==T)] <- "Site 15"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 16\\b")==T)] <- "Site 16"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 17\\b")==T)] <- "Site 17"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 18\\b")==T)] <- "Site 18"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 19\\b")==T)] <- "Site 19"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 20\\b")==T)] <- "Site 20"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 21\\b")==T)] <- "Site 21"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 22\\b")==T)] <- "Site 22"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 23\\b")==T)] <- "Site 23"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 24\\b")==T)] <- "Site 24"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 25\\b")==T)] <- "Site 25"
Observations$Site[which(str_detect(Observations$LOCATIONNAME, "\\bSite 26\\b")==T)] <- "Site 26"
unique(Observations$Site)

Observations$Camera <- NA
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bCam 1\\b")==T)] <-  "Camera 1"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bCam 2\\b")==T)] <-  "Camera 2"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Cam 3")==T)] <-  "Camera 3"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Cam 4")==T)] <-  "Camera 4"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Cam 5")==T)] <-  "Camera 5"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Cam 6")==T)] <-  "Camera 6"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Cam 7")==T)] <-  "Camera 7"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Cam 8")==T)] <-  "Camera 8"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Cam 9")==T)] <-  "Camera 9"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Cam 10")==T)] <-  "Camera 10"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Camera 10")==T)] <-  "Camera 10"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Camera 9")==T)] <-  "Camera 9"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Camera 8")==T)] <-  "Camera 8"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Camera 7")==T)] <-  "Camera 7"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Camera 6")==T)] <-  "Camera 6"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Camera 5")==T)] <-  "Camera 5"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Camera 4")==T)] <-  "Camera 4"
Observations$Camera[which(str_detect(Observations$VISITNAME, "Camera 3")==T)] <-  "Camera 3"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bCamera 2\\b")==T)] <-  "Camera 2"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bCamera 1\\b")==T)] <-  "Camera 1"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bMSU 10\\b")==T)] <-  "MSU 10"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bZCP 11\\b")==T)] <-  "ZCP 11"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bS2 KCT 1\\b")==T)] <-  "KCT 1"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bS2 KCT 2\\b")==T)] <-  "KCT 2"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S2 KCT 3")==T)] <-  "KCT 3"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S2 KCT 4")==T)] <-  "KCT 4"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S2 KCT 5")==T)] <-  "KCT 5"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S2 KCT 6")==T)] <-  "KCT 6"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S2 KCT 7")==T)] <-  "KCT 7"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S2 KCT 8")==T)] <-  "KCT 8"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S2 KCT 9")==T)] <-  "KCT 9"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S2 KCT 10")==T)] <-  "KCT 10"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bS3 KCT 1\\b")==T)] <-  "KCT 1"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bS3 KCT 1 \\b")==T)] <-  "KCT 1"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bS3 KCT 2\\b")==T)] <-  "KCT 2"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 3")==T)] <-  "KCT 3"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 4")==T)] <-  "KCT 4"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 5")==T)] <-  "KCT 5"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 6")==T)] <-  "KCT 6"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 6 ")==T)] <-  "KCT 6"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 7")==T)] <-  "KCT 7"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 8")==T)] <-  "KCT 8"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 8 to change VISIT")==T)] <-  "KCT 8"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 9")==T)] <-  "KCT 9"
Observations$Camera[which(str_detect(Observations$VISITNAME, "S3 KCT 10")==T)] <-  "KCT 10"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 01")==T)] <-  "MSU 1"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 02")==T)] <-  "MSU 2"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 03")==T)] <-  "MSU 3"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 04")==T)] <-  "MSU 4"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 05")==T)] <-  "MSU 5"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 06")==T)] <-  "MSU 6"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 07")==T)] <-  "MSU 7"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 08")==T)] <-  "MSU 8"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 09")==T)] <-  "MSU 9"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bMSU 1\\b")==T)] <-  "MSU 1"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bMSU 2\\b")==T)] <-  "MSU 2"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 3")==T)] <-  "MSU 3"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 4")==T)] <-  "MSU 4"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 5")==T)] <-  "MSU 5"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 6")==T)] <-  "MSU 6"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 7")==T)] <-  "MSU 7"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 8")==T)] <-  "MSU 8"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 9")==T)] <-  "MSU 9"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 10")==T)] <-  "MSU 10"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 11")==T)] <-  "MSU 11"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 12")==T)] <-  "MSU 12"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 13")==T)] <-  "MSU 13"
Observations$Camera[which(str_detect(Observations$VISITNAME, "MSU 14")==T)] <-  "MSU 14"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bZCP 1\\b")==T)] <-  "ZCP 1"
Observations$Camera[which(str_detect(Observations$VISITNAME, "\\bZCP 2\\b")==T)] <-  "ZCP 2"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 3")==T)] <-  "ZCP 3"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 4")==T)] <-  "ZCP 4"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 5")==T)] <-  "ZCP 5"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 6")==T)] <-  "ZCP 6"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 7")==T)] <-  "ZCP 7"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 8")==T)] <-  "ZCP 8"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 9")==T)] <-  "ZCP 9"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 10")==T)] <-  "ZCP 10"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 11")==T)] <-  "ZCP 11"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 12")==T)] <-  "ZCP 12"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 12A")==T)] <-  "ZCP 12"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 13")==T)] <-  "ZCP 13"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 14")==T)] <-  "ZCP 14"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 15")==T)] <-  "ZCP 15"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 16")==T)] <-  "ZCP 16"
Observations$Camera[which(str_detect(Observations$VISITNAME, "ZCP 17")==T)] <-  "ZCP 17"

table(is.na(Observations$VISITNAME))
table(is.na(Observations$Camera))

Merged <- merge(Only.First, Observations, by = c("time.captured"), suffixes = c("",".y"))
length(Merged[,1]) 

Merged %>%  
  group_by(time.captured, files) %>% 
  filter(n()>=2) #so - goodish news 1887 observations are from multiple species in an image

Merged %>%  
  group_by(time.captured, LOCATIONNAME) %>% 
  filter(n()>=2) #also goodish news a ton of these observations (~7600) are probably from subjects triggering the double camera set at the same time

a <- Merged %>%  
  group_by(time.captured) %>% 
  filter(n()>1) #just the observations that are replicated by time

b <- a  %>% 
  group_by(time.captured, files) %>% 
  filter(n()>1) 
b <- data.frame(b)
b.vect <- b$files
a <- a %>% 
  filter(!files %in% b.vect) #removing observations with multiple species in an image

c <- a  %>% 
  group_by(time.captured, LOCATIONNAME) %>% 
  filter(n()>1) 
c <- data.frame(c)
c.vect <- c$files
a <- a %>% 
  filter(!files %in% c.vect) #removing observations based on being triggered at the same camera site

a #okay, cool - the only duplicates in the database are from multiple species in an image -
 # so multiple WildLog files resulting in matches or from triggers at the same camera location 

Unique <- Merged %>% 
  filter(!files %in% b.vect) %>% 
  filter(!files %in% c.vect)
nrow(Unique) #So, there are 28177 independent capture events

Usable <- Merged %>% 
  filter(!files %in% b.vect) 
nrow(Usable) #So, there are 35816 capture events, inclusive of those which are double trigger events
View(Usable)
Usable.broken <- Usable %>% 
  filter(!duplicated(SIGHTINGDATE))
nrow(Usable.broken) #there are 31867 capture events, taking 1/2 of double trigger events
table(duplicated(Usable.broken$SIGHTINGDATE))

write.csv(Usable, "Usable.csv")

# You can use the below code if you want to play with images
# require(jpeg)
# 2012 SLNP Camera Traps/Section 2/Site 6/Camera 12/P800EE11141765-20120525-IMG_0005-4.JPG	
# img <- readJPEG("/Volumes/Seagate_Backup_Plus_Drive/WildLog/Camera Trap Photos/2012 SLNP Camera Traps/Section 2/Site 6/Camera 12/P800EE11141765-20120525-IMG_0005-4.JPG")
# plot(1:2, type='n')
# rasterImage(img, 1, 2, 2, 1)



# Merged.Dataset <- merge(Leveling, Usable, by = colnames(Usable)[colnames(Usable) %in% colnames(Leveling)], all = T)
Ranked <- Leveling %>% 
       arrange(time.captured) %>%
       group_by(Year_Park, Section, Site, Camera) %>% 
       mutate(capture.time.diff = cumsum(ifelse(difftime(time.captured,
                                            shift(time.captured, 
                                                  fill = time.captured[1]), 
                                                              units = "secs") >= 2, 1, 0))+1,
              row.rank = row_number())

View(Ranked) #way more complicated than I thought - forging on with just the first images in a capture for now

A <- Ranked %>%
  group_by(Year_Park, Section, Site, Camera) %>%
  mutate(n=n(),
         div = (n%%5 == 0)) %>%
  filter(div == T) %>% 
  mutate(rank = rep(1:5, n()/5),
         capture = capture.time.diff)
View(A)

B <- Ranked %>%
  group_by(Year_Park, Section, Site, Camera, capture.time.diff) %>% 
  filter(n() == 5) %>% 
  mutate(rank = 1:5,
         capture = capture.time.diff)

View(B)

Captures <- B#rbind(A,B)
summary(Captures$time.captured %in% Observations$time.captured)
Only.First <- Captures %>%
  filter(time.captured %in% Observations$time.captured)
Merged <- merge(Only.First, Observations, by = c("time.captured"), suffixes = c("",".y"))
Merged.C <- merge(Captures, Merged, by = c("Year_Park", "Section", "Site", "Camera", "capture"), suffixes = c("",".z"))
View(Merged.C)

Usable.R <- Merged.C %>% 
  group_by(files) %>% 
  filter(n()==1) 
nrow(Usable.R) #So, there are 94815 capture events, inclusive of those which are double trigger events
View(Usable.R)

write.csv(Usable.R, "Usable.R.csv")
colnames(Usable.R)
Usable.R <- Usable.R[,c(1,2,3,4,5,7,18,19,20,21,22,23,25,27,51:56,59,82,83,77,78,73,70,71)]
i <- i+4
Usable.R$rank[i]
Usable.R$ELEMENTNAME[i]
img <- readJPEG(paste0("/Volumes/Seagate_Backup_Plus_Drive/WildLog/Camera Trap Photos/",Usable.R$files[i]))
plot(1:2, type='n')
rasterImage(img, 1, 2, 2, 1)

# 
# Leveling$group <- cumsum(ifelse(difftime(Leveling$time.captured,
#                        shift(Leveling$time.captured, fill = Leveling$time.captured[1]), 
#                        units = "secs") >= 1, 1, 0))+1


#############
Usable <- Usable %>% 
  arrange(time.captured)
Usable$Unique <- 1:nrow(Usable)
Usable$new.file.name <- paste(Usable$Year, Usable$Park, Usable$Camera, Usable$Unique, sep = "_")
Usable$new.file.name <- str_replace(Usable$new.file.name, " ", "_")

resize <- data.frame(stored.as   = rep(NA, length(Usable$files)), #labeling the current storage
                     file.name   = rep(NA, length(Usable$files)), #keeping any easy form of photo ID around
                     dest.resize = rep(NA, length(Usable$files)),
                     result      = rep(NA, length(Usable$files))) #specifying the destination folder

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

for (i in 1:length(Usable$files)){
  resize$file.name[i] <- Usable$new.file.name[i]
  resize$stored.as[i] <- paste0("/Volumes/Seagate_Backup_Plus_Drive/WildLog/Camera Trap Photos/", Usable$files[i]) #labeling storage
  resize$dest.resize[i] <- paste0("/Volumes/Seagate_Backup_Plus_Drive/WildLog/256x256_photos/", resize$file.name[i],".JPG") #labeling destination
  resize$result[i] <- future_map2_chr(resize$stored.as[i], resize$dest.resize[i], process_image) 
}

ZCP.256.Files <- Usable
write.csv(ZCP.256.Files, "ZCP.256.Files.csv")
ZCP.256.Files <- read.csv("ZCP.256.Files.csv")


i <- sample(1:nrow(ZCP.256.Files),1)
ZCP.256.Files$ELEMENTNAME[i]
img <- readJPEG(paste0("/Volumes/Seagate_Backup_Plus_Drive/WildLog/256x256_Photos/",ZCP.256.Files$new.file.name[i],".JPG"))
plot(1:2, type='n')
rasterImage(img, 1, 2, 2, 1)
