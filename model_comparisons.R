mod.2.5 <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/2.5mod_pred.csv")
mod.5 <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/5mod_pred.csv")
mod.10 <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/10mod_pred.csv")
mod.25 <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/25mod_pred.csv")
mod.2.5SS <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/2.5SSmod_pred.csv")
mod.5SS <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/5SSmod_pred.csv")
mod.10SS <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/10SSmod_pred.csv")
mod.25SS <- read.csv("/Volumes/Seagate_Backup_Plus_Drive/25SSmod_pred.csv")





plot_results(mod.25SS, .25, .99, 99)
plot_results(mod.10SS, .25, .99, 99)
plot_results(mod.5SS, .25, .99, 99)
plot_results(mod.2.5SS, .25, .99, 99)


correct <- c(
  table(mod.2.5$answer == mod.2.5$guess1)[2]/8937,
  table(mod.5$answer == mod.5$guess1)[2]/8937,
  table(mod.10$answer == mod.10$guess1)[2]/8937,
  table(mod.25$answer == mod.25$guess1)[2]/8937,
  table(mod.2.5SS$answer == mod.2.5SS$guess1)[2]/8937,
  table(mod.5SS$answer == mod.5SS$guess1)[2]/8937,
  table(mod.10SS$answer == mod.10SS$guess1)[2]/8937,
  table(mod.25SS$answer == mod.25SS$guess1)[2]/8937
)

trained <- c( rep(c(2.5,5,10,25),2))

model <- c(rep("nt", 4), rep("ss", 4))

df <- data.frame(correct, trained, model)
ggplot(df, aes(y = correct, x = trained, color = model)) +
  geom_point(lwd = 2) +
  geom_line() +
  ylab("Proportion Correct") +
  labs(y = "Proportion Correct",
       x = "Training Data %",
       colour = "Model"
       ) +
  theme_classic()
ggsave("model_comparison.png", dpi=500, width = 6, height = 5)

x <- .90
c.2.5 <- mod.2.5[which(mod.2.5$confidence1 >= x),]
c.5 <- mod.5[which(mod.5$confidence1 >= x),]
c.10 <- mod.10[which(mod.10$confidence1 >= x),]
c.25 <- mod.25[which(mod.25$confidence1 >= x),]
c.2.5SS <- mod.2.5SS[which(mod.2.5SS$confidence1 >= x),]
c.5SS <- mod.5SS[which(mod.5SS$confidence1 >= x),]
c.10SS <- mod.10SS[which(mod.10SS$confidence1 >= x),]
c.25SS <- mod.25SS[which(mod.25SS$confidence1 >= x),]

correct <- c(
  table(c.2.5$answer == c.2.5$guess1)[2]/nrow(c.2.5),
  table(c.5$answer == c.5$guess1)[2]/nrow(c.5),
  table(c.10$answer == c.10$guess1)[2]/nrow(c.10),
  table(c.25$answer == c.25$guess1)[2]/nrow(c.25),
  table(c.2.5SS$answer == c.2.5SS$guess1)[2]/nrow(c.2.5SS),
  table(c.5SS$answer == c.5SS$guess1)[2]/nrow(c.5SS),
  table(c.10SS$answer == c.10SS$guess1)[2]/nrow(c.10SS),
  table(c.25SS$answer == c.25SS$guess1)[2]/nrow(c.25SS)
)

containment <- c(
  nrow(c.2.5)/8937,
  nrow(c.5)/8937,
  nrow(c.10)/8937,
  nrow(c.25)/8937,
  nrow(c.2.5SS)/8937,
  nrow(c.5SS)/8937,
  nrow(c.10SS)/8937,
  nrow(c.25SS)/8937
)


df <- data.frame(correct, containment, trained, model)
ggplot(df, aes(x = trained, color = model)) +
  geom_point(aes(y = correct, shape = "Correct"), lwd = 2) +
  geom_point(aes(y = containment, shape = "Containment"), lwd = 2) +
  geom_line(aes(y = correct, linetype = "Correct")) +
  geom_line(aes(y = containment, linetype = "Containment")) +
  ylab("Proportion Correct") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Proportion Contained")) +
  labs(y = "Proportion Correct",
       x = "Training Data %",
       colour = "Model",
       linetype = "Diagnostic",
       shape = "Diagnostic") +
  theme_classic()
ggsave("model_comparison_conf.png", dpi=500, width = 6, height = 5)


x <- 1500
ms.2.5SS <- mod.2.5SS %>% 
  group_by(answer) %>% 
  filter(n()>x*(.25/.025))
ms.2.5 <- mod.2.5 %>% 
  group_by(answer) %>% 
  filter(n()>x*(.25/.025))
ms.5SS <- mod.5SS %>% 
  group_by(answer) %>% 
  filter(n()>x*(.25/.05))
ms.5 <- mod.5 %>% 
  group_by(answer) %>% 
  filter(n()>x*(.25/.05))
ms.10 <- mod.10 %>% 
  group_by(answer) %>% 
  filter(n()>x*(.25/.1))
ms.10SS <- mod.10SS %>% 
  group_by(answer) %>% 
  filter(n()>x*(.25/.1))
ms.25 <- mod.25 %>% 
  group_by(answer) %>% 
  filter(n()>x*(.25/.25))
ms.25SS <- mod.25SS %>% 
  group_by(answer) %>% 
  filter(n()>x*(.25/.25))

correct <- c(
  table(ms.2.5$answer == ms.2.5$guess1)[2]/nrow(ms.2.5),
  table(ms.5$answer == ms.5$guess1)[2]/nrow(ms.5),
  table(ms.10$answer == ms.10$guess1)[2]/nrow(ms.10),
  table(ms.25$answer == ms.25$guess1)[2]/nrow(ms.25),
  table(ms.2.5SS$answer == ms.2.5SS$guess1)[2]/nrow(ms.2.5SS),
  table(ms.5SS$answer == ms.5SS$guess1)[2]/nrow(ms.5SS),
  table(ms.10SS$answer == ms.10SS$guess1)[2]/nrow(ms.10SS),
  table(ms.25SS$answer == ms.25SS$guess1)[2]/nrow(ms.25SS)
)
correct
