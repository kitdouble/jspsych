d1 <- read.csv("A.csv")
d2 <- read.csv("C.csv")
d3 <- read.csv("D.csv")
d4 <- read.csv("G.csv")
d5 <- read.csv("L.csv")
d6 <- read.csv("P.csv")
d7 <- read.csv("S.csv")
d8 <- read.csv("T.csv")

mydata <- rbind(d1,d2, d3,d4,d5,d6,d7,d8)
rm(d1,d2,d3,d4,d5,d6,d7,d8)



# Related 

related <- subset(mydata, FSG > .45 & FSG < .7)
related <- related[sample(1:nrow(related), size = 30),]
mean(related$FSG)


# Unrelated

unrelated <- subset(mydata, FSG < .02)
unrelated <- unrelated[sample(1:nrow(unrelated), size = 30),]
mean(unrelated$FSG)


table(duplicated(c(related$CUE, related$TARGET, unrelated$CUE, unrelated$TARGET)))

related$code <- paste0('{cue: "', related$CUE, '", target: "', gsub(" ", "", related$TARGET), '"},', sep = "")
unrelated$code <- paste0('{cue: "', unrelated$CUE, '", target: "', gsub(" ", "", unrelated$TARGET), '"},', sep = "")


write.csv(related, "related.csv", row.names = F, na = "")
write.csv(unrelated, "unrelated.csv", row.names = F, na = "")



