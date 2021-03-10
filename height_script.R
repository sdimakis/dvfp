library(rio)
library(here)
library(tidyverse)
data <- import(here("data", "flying-etiquette.csv"))

data <- data %>% filter(travel_frequency != "Never")


height_inches = NULL
height <- function(x){
  for(i in 1:length(x)){
    if(x[i] == "Under 5 ft." | x[i] == "6'6\"\" and above"){
      height_inches <- append(height_inches, x[i])
    }
    else{
      temp <- str_split_fixed(x[i], "\"\"", n = 2)
      temp2 <- str_split_fixed(temp[1], "'", n = 2)
      temp3 <- as.numeric(temp2[1])*12 + as.numeric(temp2[2])
      height_inches <- append(height_inches, temp3)
      i <- i +1}
  }
  return(height_inches)
}

data <- data %>% 
  mutate(height_inches = height(data$height),
        height_cat = case_when(
          height_inches %in% 
            c("Under 5 ft. ", "60", "61","62") ~ "< 5'3",
          height_inches %in% 
            c("63", "64", "65","66") ~ "5'3 to 5'6",
          height_inches %in%
            c("67", "68", "69", "70") ~ "5'7 to 5'10",
          height_inches %in%
            c("71", "72", "73", "74") ~ "5'11 to 6'2", 
          height_inches %in%
            c("75", "76", "77", "78", "6'6\"\" and above") ~ "> 6'2"),
          height_inches_numeric = 
            ifelse(height_inches %in% c("Under 5 ft.", "6'6\"\" and above", ""),
                                        NA, height_inches)) %>% 
  mutate(height_inches_numeric = as.numeric(height_inches_numeric))

export(data, "flying-etiquette-heights.csv")

