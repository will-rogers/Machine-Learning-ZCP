# Processing output
###################
require(ggplot2)
require(dplyr)

# results <- read.csv("C:/Users/q19r165/Desktop/Model_Repo/25SSmod_pred.csv")

plot_results <- function(results, conf.from, conf.to, length){
  conf <- seq(conf.from, conf.to, length.out = length)
  right <- rep(NA, length)
  proportion <- rep(NA, length)
  for (i in 1:length){
    high.conf <- results[results$confidence1 >= conf[i], ]
    right[i] <- length(high.conf$answer[high.conf$answer==high.conf$guess1]==T)/length(high.conf$answer)
    proportion[i] <- length(high.conf$answer)/length(results$answer)
    df <- data.frame(conf, right, proportion)
  }
  print(df %>% ggplot(aes(x = conf)) +
    geom_line(aes(y = right, color = "Model Assesment"), lwd = 2) +
    geom_line(aes(y = proportion, color = "Model Containment"), lwd = 2) +
    geom_hline(aes(yintercept = .95, color = "Acceptable Threshold"), lty = "dotted", lwd = 1.5) +
    ylab("Proportion Correct") +
    scale_y_continuous(sec.axis = sec_axis(~., name = "Proportion Contained")) +
    scale_colour_manual(values = c("green", "blue", "black")) +
    labs(y = "Proportion Correct",
                  x = "Confidence Level of First Guess",
                  colour = "Diagnostic") +
    theme_classic() +
    theme(legend.position = c(conf.from, 0.30)))
}

plot_results(read.csv("/Volumes/Seagate_Backup_Plus_Drive/25SSmod_pred.csv"), .25, .99, 99)
ggsave("ss_model_performance.tiff", dpi=500, width = 5, height = 5)

# results$right <- ifelse(results$answer-results$guess1 == 0, "Yes", "No")
# 
# length(results$right[results$right=="Yes"])/length(results$right)

most.seen <- c(
unique(files$SS.code[which(files$ELEMENTNAME == "African Elephant")]),
unique(files$SS.code[which(files$ELEMENTNAME == "Hippo")]),
unique(files$SS.code[which(files$ELEMENTNAME == "Impala")])
)

mod.25SS <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/25SSmod_pred.csv")

ms.25SS <- mod.25SS %>% 
  group_by(answer) %>% 
  filter(n()>500)

unique(mod.25SS$answer)

plot_results(ms.25SS, .25, .99, 99)


unique(mod.25SS$answer)
