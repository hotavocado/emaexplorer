#make NIMHvars

clvars <- read.csv("EMAApp100/data/nimhfs_core_v10_2.csv")

clvars <- clvars[c(4, 1:3, 5:142)]

clvars$studyid <- paste0(substr(clvars$studyid, 1,4), substr(clvars$studyid, 6,7))

class(clvars$studyid)

write.csv(clvars, "EMAApp100/data/NIMHvars.csv", row.names = F)


EMA <- read.csv("EMAApp100/data/EMA_Master_3_1_19.csv")


EMAmax <- EMA[1,]

EMAmax <- EMAmax %>% mutate_all(as.numeric)

EMAmax[1,] <- 56

write.csv(EMAmax, "EMAApp100/data/maxmerged.csv", row.names = F)
