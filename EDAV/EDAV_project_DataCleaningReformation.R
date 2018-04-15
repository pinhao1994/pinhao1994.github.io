plot(food$Rating, food$Review)
mean(food$Review, na.rm = TRUE)
foodout <- food[food$Review<1.5*mean(food$Review, na.rm = TRUE), ]
plot(foodout$Rating, foodout$Review)
require(ggplot2)
pairs(foodout[14:16], pch=21)

library(extracat)
visna(food_mi)

library(mi)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DAAG)
tidyfood <- food_mi %>% 
  mutate(missing2 = ifelse(missing == "yes", 1, 0))

ggplot(tidycars, aes(x = fct_reorder(key, -missing2, sum), y = fct_reorder(id, -missing2, sum), fill = Std)) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", mid = "white", high ="yellow", na.value = "black") + theme_bw()

tidyfood <- food_mi %>% 
  rownames_to_column("id") %>% 
  gather(key, value, -id) %>% 
  mutate(missing = ifelse(is.na(value), "yes", "no"))

ggplot(tidyfood, aes(x = key, y = fct_rev(id), fill = missing)) +
  geom_tile(color = "white") + 
  ggtitle("mtcars with NAs added") +
  scale_fill_viridis_d() + # discrete scale #error, function not found
  theme_bw()


hotel <- read.csv("~/Documents/Columbia University/Data Visualization/hotel.csv", stringsAsFactors=FALSE)

df_food <- joinYelp(df_food)
df_hotel <- joinYelp(df_hotel)

food$ID <- paste("food", rownames(food), sep="_")
hotel$ID <- paste("hotel", rownames(), sep="_")

hotel_top5_pivot <- read.csv("~/Documents/Columbia University/Data Visualization/hotel_top5_pivot.csv", stringsAsFactors=FALSE)

colnames(hotel_top5_pivot) <- unname(hotel_top5_pivot[1, ])
hotel_top5_pivot <- hotel_top5_pivot[-1, ]
hotel_top5_pivot <- hotel_top5_pivot[-c(43, 42),]
hotel_top5_pivot <- hotel_top5_pivot[, -c(38, 39, 40)]
hotel_top5_pivot <- hotel_top5_pivot[, colnames(hotel_top5_pivot) != "NA"]


catsort <- sort(hotel_top5_pivot[1,], na.omit=TRUE)
length(catsort)


cats <- data.frame()

for(i in 1:nrow(hotel_top5_pivot)){
  catsort <- sort(hotel_top5_pivot[i,], na.omit=TRUE)
  cat1 <- data.frame(hotelID = catsort[36], top5cat = paste(
    paste(names(catsort[35]), catsort[35], sep = ":"),
    paste(names(catsort[34]), catsort[34], sep = ":"),
    paste(names(catsort[33]), catsort[33], sep = ":"),
    paste(names(catsort[32]), catsort[32], sep = ":"),
    paste(names(catsort[31]), catsort[31], sep = ":"),
    sep = "|"))
  cats <- rbind(cats, cat1)
}

cats<- cats[-nrow(cats), ]
# hotel_18 having extra column??

cats <- rbind(cats, data.frame(hotelID = catsort[39], top5cat = paste(
  paste(names(catsort[38]), catsort[38], sep = ""),
  paste(names(catsort[37]), catsort[37], sep = ":"),
  paste(names(catsort[36]), catsort[36], sep = ":"),
  paste(names(catsort[35]), catsort[35], sep = ":"),
  paste(names(catsort[34]), catsort[34], sep = ":"),
  sep = "|")))


write.csv(cats, "hotel_top5_cat.csv", sep=",", row.names = FALSE)

top3rating <- read.csv("~/Documents/Columbia University/Data Visualization/hotel_top3rating_restaurant.csv", stringsAsFactors=FALSE)

top3 <- top3rating %>%
  group_by(hotel_ID) %>%
  top_n(n = 3, wt = X60_40)

top3_restaurant <- data.frame()
top3_food_ID <- data.frame()

hotelID <- unique(top3$hotel_ID)
#data.frame(hotel_ID = hotelID[1], 
#           top3restaurant = paste(unname(unlist(top3[top3$hotel_ID==hotelID[1], "Restaurant"])), collapse = "|"))

for (i in 1:length(hotelID)){
  top3_2 <- data.frame(hotel_ID = hotelID[i], 
             top3restaurant = paste(unname(unlist(top3[top3$hotel_ID==hotelID[i], "food_ID"])), 
                                    collapse = "|"))
  top3_food_ID <- rbind(top3_food_ID, top3_2)
}

write.csv(top3_food_ID, "top3foodID.csv", sep=",", row.names = FALSE)
