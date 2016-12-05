data <- read.csv("User1/ADA/Advanced-Data-Analysis-Project/Emails_cleaned.csv",header = TRUE)
names(data)
id <- data[data$MetadataFrom == "H",]$Id
id_H_81 <- data[data$MetadataFrom == "H" && data$MetadataTo == "abedinh@state.gov", ]$Id
id_H_81 <- grep("H", data[grep("abedinh@state.gov", data$MetadataTo),]$MetadataFrom)
length(id)
sum <- summary(data[id, ]$MetadataTo)[1: 10]
recv <- names(sum)
names(data)
for (i in 1 : 10){
  text <- ""
  for (j in 1 : length(grep(recv[1], data[id, ]$MetadataTo))){
    text <- paste(text, data[grep(recv[1], data[id, ]$MetadataTo), ]$RawText[j])
  }
  write(text, file = paste(recv[i], ".txt"))
}