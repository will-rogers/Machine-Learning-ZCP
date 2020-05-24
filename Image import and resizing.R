#Making training datasets from Serengetti Snapshot dataset
library(readr)
require(magick) #for image reduction
require(furrr)
require(tidyverse)
require(dplyr)

# all_images <- read_csv("~/Downloads/doi_10/all_images.csv") #capture event IDs and URL info
# consensus_data <- read_csv("~/Downloads/doi_10/consensus_data.csv") #events IDs etc
# gold_standard_data <- read_csv("~/Downloads/doi_10/gold_standard_data.csv") # sameish as consensus - reviewed by experts
# raw_data_for_dryad <- read_csv("~/Downloads/doi_10/raw_data_for_dryad.csv") #HUGE

# table(consensus_data$Species) #data that are probably classified properly
# table(gold_standard_data$Species) #probably not usable 
# table(raw_data_for_dryad$Species) #probably not best to trust by itself
# length(raw_data_for_dryad$Species) #matches pub
# length(unique(raw_data_for_dryad$CaptureEventID)) #matches pub

# annotated_images <- read_csv("C:/Users/q19r165/Desktop/Model_Repo/SnapshotSerengeti_v2_1_annotations.csv")
images <- read_csv("C:/Users/q19r165/Desktop/Model_Repo/SnapshotSerengeti_S1-11_v2_1_images.csv")
# 
# names(annotated_images)
# names(images)
# # Identifies which species are frequent
# sub_merge <- annotated_images %>% 
#   group_by(question__species) %>% 
#   summarize(n=n()) %>%
#   filter(n>5000) 
# 
# # Samples 5,000 images of common species
# set.seed(123456)
# frequent  <- annotated_images %>% 
#   filter(question__species %in% sub_merge$question__species) %>% 
#   group_by(question__species) %>% 
#   sample_n(5000, replace=F)
# 
# #shows that length is 5k * number of common species
# length(frequent$capture_id)
# 
# #subsets dataset for infrequent species
# infrequent_sub <- annotated_images %>% 
#   group_by(question__species) %>% 
#   summarize(n=n()) %>%
#   filter(n<5000) 
# 
# infrequent  <- annotated_images %>% 
#   filter(question__species %in% infrequent_sub$question__species) 

# #combines subset of frequent and full infrequent datasets
# access <- data.frame(bind_rows(frequent,infrequent))
# table(access$question__species) #only capture IDs
# #merges by capture ID
# data <- merge(access, images, by="capture_id") #merging to get photo id's associated with captures
# #write.csv(data, "data.for.download.csv")
data <- read_csv("E:/Machine_Learning/MLCreel/data.for.download.csv")

table(data$question__species)

# n_occur <- data.frame(table(data$image_path_rel))
# n_occur[n_occur$Freq > 1,]
# abcd <- data[data$image_path_rel %in% n_occur$Var1[n_occur$Freq > 1],]
# table(abcd$question__species)

#Basic Camera Trap photos will be around 550-650 kb 
600*length(data$capture_id)*10^-6 # about 184 gb 

#we can resize images for the model and storage to 256x256 pixels. about 50kb
50*length(data$capture_id)*10^-6 # about 15.34 gb

url <- "https://snapshotserengeti.s3.msi.umn.edu/" #base url for image download

# # for (i in 405:306667){
# #   file.name <- strsplit(data$image_path_rel[i], "/")[[1]][4] #stripping only the sighting info
# #   download.file(url = paste(url, data$image_path_rel[i], sep = ""), 
# #               destfile = paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", file.name),
# #               quiet = T) # have to provide destination for download
# #   if (file.exists(paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", file.name))) {}
# # }
# #42856
# #k <- length(list.files(path = "/Volumes/Seagate Backup Plus Drive/SS/for_loop"))
# #length(unique(list.files(path = "/Volumes/Seagate Backup Plus Drive/SS/for_loop")))
# 
# # k <- rep(NA, 306667)
# 
# # for (i in 1:306667){
# #   
# #   k[i] <- i
# #   
# #   file.name <- strsplit(data$image_path_rel[i], "/")[[1]][4] #stripping only the sighting info
# #   
# #   if (file.exists(paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", file.name))) {next}
# #   
# #   download.file(url = paste(url, data$image_path_rel[i], sep = ""), 
# #                 destfile = paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", file.name),
# #                 quiet = T) # have to provide destination for download
# # }
# 
# # k[1:240944] <- 1:240944
# # j <- length(k[is.na(k)==F])
# # 
# # for (i in j:306667){
# #   
# #   k[i] <- i #tractable way to record where I am in download
# #   
# #   file.name <- strsplit(data$image_path_rel[i], "/")[[1]][4] #stripping only the sighting info
# #   
# #   if (file.exists(paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", file.name))) {next} #moving on if there is a duplicate ID, seem to be ~1k
# #   
# #   download.file(url = paste(url, data$image_path_rel[i], sep = ""), 
# #                 destfile = paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", file.name),
# #                 quiet = T) # have to provide destination for download, quiet to keep the opperations managable
# # }
# 
# 
# for_loop <- list.files(path = "/Volumes/Seagate Backup Plus Drive/SS/for_loop")
# 
# files <- rep(NA, length(data$image_path_rel))
# for (i in 1:length(data$image_path_rel)){
#   files[i] <- strsplit(data$image_path_rel[i], "/")[[1]][4]
# }
# # 
# # data$files <- files
# # missing <- data[which(data$files %in% for_loop == F),]
# # 
# # for (i in 43:61){
# #   
# #   file.name <- strsplit(missing$image_path_rel[i], "/")[[1]][4] #stripping only the sighting info
# #   
# #   if (file.exists(paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", file.name))) {next} #moving on if there is a duplicate ID, seem to be ~1k
# #   
# #   download.file(url = paste(url, missing$image_path_rel[i], sep = ""), 
# #                 destfile = paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", file.name),
# #                 quiet = T) # have to provide destination for download, quiet to keep the opperations managable
# # }
# # 
# 
# data$files <- files
# 
# # for_loop <- list.files(path = "/Volumes/Seagate Backup Plus Drive/SS/for_loop")
# length(for_loop)
# length(unique(for_loop))
# 
# data <- data %>%
#   dplyr::filter(files %in% for_loop) %>% 
#   distinct(image_path_rel, .keep_all = TRUE)
# length(data$X1)
# 
# wtf <- for_loop[!for_loop %in% data$files]
# length(wtf)
# 
# files <- rep(NA, length(images$image_path_rel))
# for (i in 1:length(images$image_path_rel)){
#   files[i] <- strsplit(images$image_path_rel[i], "/")[[1]][4]
# }
# images$files <- files
# 
# not.sure <- images %>%
#   dplyr::filter(files %in% wtf) 
# length(not.sure$files)
# 
# resize <- data.frame(stored.as   = rep(NA, length(data$capture_id)), #labeling the current storage
#                      file.name   = rep(NA, length(data$capture_id)), #keeping any easy form of photo ID around
#                      dest.resize = rep(NA, length(data$capture_id)),
#                      result      = rep(NA, length(data$capture_id))) #specifying the destination folder
# 
# # Function tries to copy and resize images to a new location; 
# # however, script does not fail if file is corrupted and records
# # whether it worked to a new vector.
# 
# process_image <- function(image.path, new.image.path){
#   tryCatch(
#     {
#       image_write(image_scale(image_read(image.path),"256x256!"), path = new.image.path)
#       return(substr(image.path, nchar(image.path) - 2, nchar(image.path)))
#     },
#     error = function(e)  
#     {
#       return(NA)
#     }
#   )
# }
# 
# for (i in 249542:length(data$capture_id)){
#   resize$file.name[i] <- strsplit(data$image_path_rel[i], "/")[[1]][4]
#   resize$stored.as[i] <- paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", resize$file.name[i]) #labeling storage
#   resize$dest.resize[i] <- paste0("/Volumes/Seagate Backup Plus Drive/SS/256x256_Photos/", resize$file.name[i]) #labeling destination
#   resize$result[i] <- future_map2_chr(resize$stored.as[i], resize$dest.resize[i], process_image) 
# }
# i
# # length(resize$file.name[is.na(resize$result)==F])
# photos256 <- list.files(path = "/Volumes/Seagate_Backup_Plus_Drive/SS/256x256_Photos")
# length(photos256)
# length(unique(photos256))
# 
# missing <- data %>%
#   dplyr::filter(!files %in% photos256) 
# length(missing$X1)
# 
# # resize <- data.frame(stored.as   = rep(NA, length(missing$capture_id)), #labeling the current storage
# #                      file.name   = rep(NA, length(missing$capture_id)), #keeping any easy form of photo ID around
# #                      dest.resize = rep(NA, length(missing$capture_id)),
# #                      result      = rep(NA, length(missing$capture_id))) 
# # 
# # for (i in 1:length(missing$capture_id)){
# #   resize$file.name[i] <- strsplit(missing$image_path_rel[i], "/")[[1]][4]
# #   resize$stored.as[i] <- paste0("/Volumes/Seagate Backup Plus Drive/SS/for_loop/", resize$file.name[i]) #labeling storage
# #   resize$dest.resize[i] <- paste0("/Volumes/Seagate Backup Plus Drive/SS/256x256_Photos/", resize$file.name[i]) #labeling destination
# #   resize$result[i] <- future_map2_chr(resize$stored.as[i], resize$dest.resize[i], process_image) 
# # }
# 
# data <- data %>%
#   dplyr::filter(files %in% photos256) 
# length(data$X1)
# 
# data$directory <- paste0("/Volumes/Seagate Backup Plus Drive/SS/256x256_Photos/", data$files)
# 
# table(data$question__species)
# 
# require(jpeg)
# img <- readJPEG(data$directory[which(data$question__species == "bat")][3])
# plot(1:2, type='n')
# rasterImage(img, 1, 2, 2, 1)
# 
# images_labels <- data.frame(data$directory,data$question__species)
# images_labels$number <- as.numeric(images_labels$data.question__species)+27
# images_labels$number[which(images_labels$data.question__species=="human")] <- 11
# images_labels$number[which(images_labels$data.question__species=="blank")] <- 27

#write.csv(images_labels, "images_labels.csv")
images_labels <- read.csv("E:/Machine_Learning/MLCreel/images_labels.csv")

images_labels$name <- NA
for (i in 1: length(images_labels$data.directory)){
  v <- str_split(images_labels$data.directory[i], "/")
  images_labels$name[i] <- v[[1]][6]
}


test_run <- images_labels
test_run <- test_run %>% 
  filter(number== 28 |
           number==30 |
           number== 34 |
           number== 11 |
           number== 72)

train_run <- test_run %>% 
  group_by(factor(number)) %>% 
  sample_n(200, replace = F)

train_r <- train_run%>% 
  group_by(number) %>% 
  sample_n(150, replace = F)

table(train_r$data.question__species)

require(readxl)
species_conversions <- read_excel("C:/Users/q19r165/Desktop/Model_Repo/phase2_recognition_only/species_conversions.xlsx")
View(species_conversions)
train_r$number[which(train_r$data.question__species == "human")] <- 21
train_r$number[which(train_r$data.question__species == "serval")] <- 39
train_r$number[which(train_r$data.question__species == "aardvark")] <- 0
train_r$number[which(train_r$data.question__species == "buffalo")] <- 4


test_r <- train_run %>% 
  filter(!name %in% train_r$name)

#write.csv(train_r, "train_labels.csv")
#write.csv(test_r, "test_labels.csv")

testimages$data.directory

for (i in 1:50){
  testimages$file.name[i] <- strsplit(testimages$data.directory[100], "/")[[1]][6]
}

testimages %>% 
  filter(file.name %in% photos256)