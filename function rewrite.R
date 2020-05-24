train <- function(
  # set up some parameters for function
  path_prefix = paste0(getwd(), "/images"), # absolute path to location of the images on your computer
  data_info = paste0(getwd(), "/image_labels.csv"), # csv with file names for each photo. See details
  model_dir = getwd(),
  python_loc = "/anaconda2/bin/",
  os="Mac",
  num_gpus = 2,
  num_classes = 28, # number of classes in model
  delimiter = ",", # this will be , for a csv.
  architecture = "resnet",
  depth = "18",
  batch_size = "128",
  log_dir_train = "train_output",
  retrain = TRUE,
  retrain_from = "USDA182",
  num_epochs = 55,
  print_cmd = FALSE
) {
  
  wd1 <- getwd() # the starting working directory
  
  # set these parameters before changing directory
  path_prefix = path_prefix
  data_info = data_info
  model_dir = model_dir
  
  # navigate to directory with trained model
  if(endsWith(model_dir, "/")){
    setwd(paste0(model_dir, "L1"))
  } else {
    setwd(paste0(model_dir, "/L1"))
  }
  wd <- getwd()
  
  # load in data_info and store it in the model_dir
  # labels <- utils::read.csv(data_info, header=FALSE)
  # utils::write.csv(labels, "data_info_train.csv", row.names=FALSE)
  
  if(os=="Windows"){
    # deal with windows file format issues
    data_file <- read.table(data_info, header=FALSE, sep=",")
    output.file <- file("data_info_train.csv", "wb")
    write.table(data_file,
                file = output.file,
                append = TRUE,
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE,
                sep = ",")
    close(output.file)
    rm(output.file)
  } else {
    cpfile <- paste0("cp ", data_info, " ", wd, "/data_info_train.csv")
    system(cpfile)
  }
  
  
  # set depth
  if(architecture == "alexnet"){
    depth <- 8
  }
  if(architecture == "nin"){
    depth <- 16
  }
  if(architecture == "vgg"){
    depth <- 22
  }
  if(architecture == "googlenet"){
    depth <- 32
  }
  
  # run function
  if(retrain){
    train_py <- paste0(python_loc,
                       "python train.py --architecture ", architecture,
                       " --depth ", depth,
                       " --path_prefix ", path_prefix,
                       " --num_gpus ", num_gpus,
                       " --batch_size ", batch_size,
                       " --data_info data_info_train.csv",
                       " --delimiter ", delimiter,
                       " --num_epochs ", num_epochs,
                       " --retrain_from ", retrain_from,
                       " --num_classes ", num_classes,
                       " --log_dir ", log_dir_train, 
                       " --retrain_mode ", retrain_mode)
  }else {
    train_py <- paste0(python_loc,
                       "python train.py --architecture ", architecture,
                       " --depth ", depth,
                       " --path_prefix ", path_prefix,
                       " --num_gpus ", num_gpus,
                       " --batch_size ", batch_size,
                       " --data_info data_info_train.csv",
                       " --delimiter ", delimiter,
                       " --num_epochs ", num_epochs,
                       " --num_classes ", num_classes,
                       " --log_dir ", log_dir_train)
  }
  
  
  # printing only?
  if(print_cmd){
    print(train_py)
  } else {
    # run code
    toc <- Sys.time()
    system(train_py)
    tic <- Sys.time()
    runtime <- difftime(tic, toc, units="auto")
    
    # end function
    txt <- paste0("training of model took ", runtime, " ", units(runtime),  ". ",
                  "The trained model is in ", log_dir_train, ". ",
                  "Specify this directory as the log_dir when you use classify(). ")
    print(txt)
  }
  
}
