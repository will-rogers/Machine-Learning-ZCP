require(devtools)
# devtools::install_github("mikeyEcology/MLWIC")
library(MLWIC)

# setup(python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3")

#####
#This runs the example output from Mikey, validating the results
#####
classify(
  path_prefix = "C:/Users/q19r165/Desktop/MLWIC_examples-master/images",
  data_info = "C:/Users/q19r165/Desktop/MLWIC_examples-master/image_labels.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "model_predictions.txt"
)

make_output(
  output_location = "C:/Users/q19r165/Desktop/Model_Repo",
  output_name = "example_results.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo",
  saved_predictions = "model_predictions.txt"
)

train(path_prefix = "C:/Users/q19r165/Desktop/MLWIC_examples-master/images", # this is the absolute path to the images.
      data_info = "C:/Users/q19r165/Desktop/MLWIC_examples-master/image_labels.csv", # this is the location of the csv containing image information. It has Unix linebreaks and no headers.
      model_dir = "C:/Users/q19r165/Desktop/Model_Repo", # assuming this is where you stored the L1 folder in Step 3 of the instructions: github.com/mikeyEcology/MLWIC/blob/master/README
      python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # the location of Python on your computer.
      num_classes = 28, # this is the number of species from our model. When you train your own model, you will replace this with the number of species/groups of species in your dataset
      log_dir_train = "training_output" # this will be a folder that contains the trained model (call it whatever you want). You will specify this folder as the "log_dir" when you classify images using this trained model. For example, the log_dir for the model included in this package is called "USDA182"
)

#####
# This trains a transfer model with 150 images of each species
train <- function (path_prefix = paste0(getwd(), "/images"),
                   data_info = paste0(getwd(),"/image_labels.csv"),
                   model_dir = getwd(),
                   python_loc = "/anaconda2/bin/",
                   os = "Mac",
                   num_gpus = 2,
                   num_classes = 28,
                   delimiter = ",",
                   architecture = "resnet",
                   depth = "18",
                   batch_size = "128",
                   log_dir_train = "train_output",
                   retrain = TRUE,
                   retrain_from,
                   num_epochs = 55, print_cmd = FALSE)
{
  wd1 <- getwd()
  path_prefix = path_prefix
  data_info = data_info
  model_dir = model_dir
  wd <- getwd()
  if (os == "Windows") {
    data_file <- read.table(data_info, header = FALSE, sep = ",")
    output.file <- file("data_info_train.csv", "wb")
    write.table(data_file, file = output.file, append = TRUE,
                quote = FALSE, row.names = FALSE, col.names = FALSE,
                sep = ",")
    close(output.file)
    rm(output.file)
  }
  else {
    cpfile <- paste0("cp ", data_info, " ", wd,
                     "/data_info_train.csv")
    system(cpfile)
  }
  if (architecture == "alexnet") {
    depth <- 8
  }
  if (architecture == "nin") {
    depth <- 16
  }
  if (architecture == "vgg") {
    depth <- 22
  }
  if (architecture == "googlenet") {
    depth <- 32
  }
  if (retrain) {
    train_py <- paste0(python_loc, "python train.py --architecture ",
                       architecture, " --depth ", depth, " --path_prefix ",
                       path_prefix, " --num_gpus ", num_gpus, " --batch_size ",
                       batch_size, " --data_info data_info_train.csv",
                       " --delimiter ", delimiter, " --num_epochs ",
                       num_epochs, " --retrain_from ", retrain_from,
                       " --num_classes ", num_classes, " --log_dir ",
                       log_dir_train)
  }
  else {
    train_py <- paste0(python_loc, "python train.py --architecture ",
                       architecture, " --depth ", depth, " --path_prefix ",
                       path_prefix, " --num_gpus ", num_gpus, " --batch_size ",
                       batch_size, " --data_info data_info_train.csv",
                       " --delimiter ", delimiter, " --num_epochs ",
                       num_epochs, " --num_classes ", num_classes,
                       " --log_dir ", log_dir_train)
  }
  if (print_cmd) {
    print(train_py)
  }
  else {
    toc <- Sys.time()
    system(train_py)
    tic <- Sys.time()
    runtime <- difftime(tic, toc, units = "auto")
    txt <- paste0("training of model took ", runtime,
                  " ", units(runtime), ". ", "The trained model is in ",
                  log_dir_train, ". ", "Specify this directory as the log_dir when you use classify(). ")
    print(txt)
  }
}

train(path_prefix = "E:/WildLog/256x256_Photos/",
      data_info = "E:/Machine_Learning/MLCreel/train/train50.csv",
      model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
      python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/",
      os = "Windows",
      num_gpus = 2,
      num_classes = 86,
      delimiter = ",",
      architecture = "resnet",
      depth = "18",
      batch_size = "64",
      log_dir_train = "tr50",
      retrain = TRUE,
      num_epochs = 55,
      print_cmd = FALSE,
      retrain_from = "USDA182")



classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "2.5model_predictions.txt",
  os = "Windows",
  num_classes = 86,
  log_dir = "tr2.5"
)

classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "5model_predictions.txt",
  os = "Windows",
  num_classes = 86,
  log_dir = "tr5"
)

classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "10model_predictions.txt",
  os = "Windows",
  num_classes = 86,
  log_dir = "tr10"
)

classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "25model_predictions.txt",
  os = "Windows",
  num_classes = 86,
  log_dir = "tr25"
)

classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "50model_predictions.txt",
  os = "Windows",
  num_classes = 86,
  log_dir = "tr50"
)

classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "2.5SSmodel_predictions.txt",
  os = "Windows",
  num_classes = 80,
  log_dir = "tr2.5SS"
)

classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "5SSmodel_predictions.txt",
  os = "Windows",
  num_classes = 80,
  log_dir = "tr5SS"
)

classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "10SSmodel_predictions.txt",
  os = "Windows",
  num_classes = 80,
  log_dir = "tr10SS"
)

classify(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/test.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "25SSmodel_predictions.txt",
  os = "Windows",
  num_classes = 80,
  log_dir = "tr25SS"
)



classify.cust <- function (path_prefix = paste0(getwd(), "/images"), data_info = paste0(getwd(),
                                                                                   "/image_labels.csv"), model_dir = getwd(), save_predictions = "model_predictions.txt",
                      python_loc = "/anaconda3/bin/", os = "Mac", num_classes = 28,
                      delimiter = ",", architecture = "resnet", depth = "18",
                      top_n = "5", log_dir = "USDA182")
{
  wd1 <- getwd()
  path_prefix = path_prefix
  data_info = data_info
  model_dir = model_dir
  if (endsWith(model_dir, "/")) {
    setwd(paste0(model_dir, "L1"))
  }
  else {
    setwd(paste0(model_dir))
  }
  wd <- getwd()
  if (os == "Windows") {
    data_file <- read.table(data_info, header = FALSE, sep = ",")
    output.file <- file("data_info.csv", "wb")
    write.table(data_file, file = output.file, append = TRUE,
                quote = FALSE, row.names = FALSE, col.names = FALSE,
                sep = ",")
    close(output.file)
    rm(output.file)
  }
  else {
    cpfile <- paste0("cp ", data_info, " ", wd,
                     "/data_info.csv")
    system(cpfile)
  }
  if (architecture == "alexnet") {
    depth <- 8
  }
  if (architecture == "nin") {
    depth <- 16
  }
  if (architecture == "vgg") {
    depth <- 22
  }
  if (architecture == "googlenet") {
    depth <- 32
  }
  eval_py <- paste0(python_loc, "python eval.py --architecture ",
                    architecture, " --depth ", depth, " --log_dir ",
                    log_dir, " --path_prefix ", path_prefix, " --batch_size 128 --data_info data_info.csv",
                    " --delimiter ", delimiter, " --save_predictions ",
                    save_predictions, " --top_n ", top_n, " --num_classes=",
                    num_classes, "\n")
  toc <- Sys.time()
  system(eval_py)
  tic <- Sys.time()
  runtime <- difftime(tic, toc, units = "auto")
  txt <- paste0("evaluation of images took ", runtime,
                " ", units(runtime), ". ", "The results are stored in ",
                model_dir, "/L1/", save_predictions, ". ",
                "To view the results in a viewer-friendly format, please use the function make_output")
  print(txt)
  setwd(wd1)
}

classify.cust(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/testSS.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "2.5SSmodel_predictions.txt",
  os = "Windows",
  num_classes = 80,
  log_dir = "tr2.5SS",
  architecture = "resnet",
  depth = "152",
  top_n = "5"
  )

classify.cust(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/testSS.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "5SSmodel_predictions.txt",
  os = "Windows",
  num_classes = 80,
  log_dir = "tr5SS",
  architecture = "resnet",
  depth = "152",
  top_n = "5"
)

classify.cust(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/testSS.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "10SSmodel_predictions.txt",
  os = "Windows",
  num_classes = 80,
  log_dir = "tr10SS",
  architecture = "resnet",
  depth = "152",
  top_n = "5"
)

classify.cust(
  path_prefix = "E:/WildLog/256x256_Photos/",
  data_info = "E:/Machine_Learning/MLCreel/train_test/testSS.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo/",
  python_loc = "C:/Users/q19r165/AppData/Local/Continuum/anaconda3/", # remember to include the last slash
  save_predictions = "25SSmodel_predictions.txt",
  os = "Windows",
  num_classes = 80,
  log_dir = "tr25SS",
  architecture = "resnet",
  depth = "152",
  top_n = "5"
)

make_output(
  output_location = "C:/Users/q19r165/Desktop/Model_Repo",
  output_name = "25mod_pred.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo",
  saved_predictions = "50model_predictions.txt"
)

make_output(
  output_location = "C:/Users/q19r165/Desktop/Model_Repo",
  output_name = "10SSmod_pred.csv",
  model_dir = "C:/Users/q19r165/Desktop/Model_Repo",
  saved_predictions = "10SSmodel_predictions.txt"
)


