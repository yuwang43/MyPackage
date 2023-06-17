# EDA ----
## Import The Data ----
getData <- function(){
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(tidyverse)
  library(DiagrammeR)
  options(scipen = 999)
  #specify a fixed date from 20200101, with a upper limit of 100,0000 rows
  vol = "1000000"
  while (as.numeric(vol) > 0) {
    url <- paste0("https://data.cityofchicago.org/resource/crimes.json?$where=date%20>=%20%222020-01-01%22&$limit=", vol)
    response <- jsonlite::fromJSON(url)
    df <- as.data.frame(response)
    if (nrow(df) == as.numeric(vol)) {
      vol = as.character(as.numeric(vol) + 500000)
      next
    } else {
      break
    }
  }

  # Convert the JSON to a data frame
  df <- as.data.frame(response)
  head(df)
  return(df)
}

## Data Preprocessing ----
RDPrepro <- function(df){
  df$date = as.POSIXct(df$date)
  df$block = as.factor(df$block)
  df$iucr = as.factor(df$iucr)
  df$primary_type = as.factor(df$primary_type)
  df$arrest = as.factor(df$arrest)
  df$domestic = as.factor(df$domestic)
  df$beat =  as.factor(df$beat)
  df$district = as.factor(df$district)
  df$ward = as.factor(df$ward)
  df$community_area = as.factor(df$community_area)
  df$latitude = as.numeric(df$latitude)
  df$longitude = as.numeric(df$longitude)

  # Identify duplicate identifiers
  df %>%
    group_by(case_number) %>%
    mutate(count=n()) %>%
    filter(count>1)

  # Remove duplicates
  df_no_dup<-filter(distinct(df,case_number,.keep_all=TRUE))

  # Check to make sure
  df_no_dup %>%
    group_by(case_number) %>%
    summarize(count=n()) %>%
    filter(count>1)

  # Specific crime types
  t <- df_no_dup %>%
    group_by(primary_type) %>%
    summarize(count=n()) %>%
    arrange(desc(count))
  # Convert “crime sexual assault”, “prostitution”, “Sex Offense” to Sex Cases.
  # Convert “narcotics", “other narcotic violation” to Drug.
  # Convert "human trafficking", "RITUALISM", "public indecency" to Other.
  df_clean <- df_no_dup %>%
    mutate(
      crime_type = fct_recode(primary_type,
                              "SEX"="CRIM SEXUAL ASSAULT",
                              "SEX"="PROSTITUTION",
                              "SEX"="SEX OFFENSE",
                              "SEX"="CRIMINAL SEXUAL ASSAULT",
                              "DRUG"="NARCOTICS",
                              "DRUG"= "OTHER NARCOTIC VIOLATION",
                              "OTHER"="RITUALISM",
                              "OTHER"="PUBLIC INDECENCY",
                              "OTHER"="HUMAN TRAFFICKING",
                              "OTHER"="OTHER OFFENSE")
    ) %>%
    mutate(season = case_when(month(date) %in% 3:5 ~ "Spring",
                              month(date) %in% 6:8 ~ "Summer",
                              month(date) %in% 9:11 ~ "Fall",
                              TRUE ~ "Winter"))

  df_clean %>%
    group_by(crime_type) %>%
    summarize(count=n()) %>%
    arrange(desc(count))

  # Summarize the data
  summary(df_clean)
  return(df_clean)
}

## Visualizations ----
### Plot of crimes and arrest by date ----
CADPlot <- function(df_clean){
  # Group the data by date and count the number of crimes for each date
  df_daily_crimes <- df_clean %>%
    group_by(date = as.Date(date)) %>%
    summarize(num_crimes = n())

  df_daily_arrests <- df_clean  %>%
    filter(arrest == TRUE) %>%
    group_by(date) %>%
    summarize(num_arrests = n())
  # Create a plot of the number of reported crimes by date
  df_daily_counts <- left_join(df_daily_arrests, df_daily_crimes, by = "date")

  ggplot(df_daily_counts, aes(x = date)) +
    geom_line(aes(y = num_crimes, color = "#619CFF"), size = 1) +
    geom_line(aes(y = num_arrests, color = "#F8766D"), size = 1) +
    labs(title = "Trends of Crimes and Arrests", x = "Date", y = "Count")
}

### Plot of crimes frequency ----
CFPlot <- function(df_clean){
  df_crime_counts <- df_clean %>%
    group_by(crime_type) %>%
    summarize(count=n()) %>%
    arrange(desc(count))

  ggplot(df_crime_counts,aes(x = reorder(crime_type,count), y = count)) +
    geom_bar(stat = "identity", fill = "#756bb1") +
    labs(title = "Frequency of Crime", x = "Crime Type", y = "Number of Crimes") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    coord_flip()
}

### Plot of number of top reported crimes location ----
TRCLPlot <- function(df_clean){
  df_location_counts <- as.data.frame(table(df_clean$location_description))%>%
    filter(Freq > 10000) %>%
    arrange(desc(Freq))

  ggplot(df_location_counts, aes(x = reorder(Var1, Freq), y = Freq, fill = Freq)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "pink", high = "purple") +
    labs(title = "Number of Crimes by Location", x = "Location Type", y = "Number of Crimes") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

### Plot of number of crimes by season and year ----
CSYPlot <- function(df_clean){
  #df_clean$year = as.factor(df_clean$year)
  #df_clean$season = as.factor(df_clean$season)

  # Group the data by season and count the number of crimes for each season
  df_season_counts <- df_clean %>%
    count(year, season)
  # Create a line chart with points of the number of crimes by season and by year
  ggplot(df_season_counts, aes(x = year, y = n, group = season)) +
    geom_line(aes(color = season), linewidth = 1) +
    geom_point(aes(color = season), size = 2, shape = 21, fill = "white") +
    scale_color_discrete(name = "Season") +
    labs(title = "Number of Crimes by Season and Year",
         x = "Year",
         y = "Number of Crimes") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

### Plot of crimes types by season ----
CSPlot <- function(df_clean){
  df_season_type <- df_clean %>%
    group_by(crime_type,season) %>%
    summarise(count=n(),.groups = 'drop')

  # Create a plot of the number of each individual crime type by season
  ggplot(df_season_type,aes(x = crime_type, y = season)) +
    geom_tile(aes(fill=count)) +
    labs(x="Crime", y = "Season", title="Summer is popular for crimes") +
    scale_fill_viridis_c("Number of Crimes") +
    coord_flip()
}

### Plot of number of each individual crime type by season ----
ICTSPlot <- function(df_clean){
  library(RColorBrewer)
  df_location_counts <- as.data.frame(table(df_clean$location_description))%>%
    filter(Freq > 10000) %>%
    arrange(desc(Freq))
  # Group the data by primary_type and location
  df_location_type <- df_clean %>%
    group_by(crime_type, location_description) %>%
    summarize(num_crimes = n(), .groups = 'drop') %>%
    filter(location_description %in% df_location_counts$Var1) %>%
    ungroup()

  # Create a plot of the number of each individual crime type by season
  ggplot(df_location_type,aes(x = crime_type, y = location_description)) +
    geom_tile(aes(fill=num_crimes)) +
    labs(x="Crime", y = "Location", title="Crime Frequencies by Crime and Location") +
    scale_fill_viridis_c("Number of Crimes") +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 90))
}



##### Predictive Analysis ----
## Data Preprocessing and Feature Engineering ----
CDPrepro <- function(df_clean){
  library(dplyr)
  library(tidyr)
  library(CatEncoders)
  # Select the columns for the features
  # Use location types from Plot 3
  df_location_counts <- as.data.frame(table(df_clean$location_description))%>%
    filter(Freq > 10000) %>%
    arrange(desc(Freq))

  df_new <- df_clean %>%
    select(location_description, beat, district, ward, community_area, latitude, longitude, crime_type, season, arrest) %>%
    filter(location_description %in% df_location_counts$Var1)
  df_new$arrest = ifelse(df_new$arrest == TRUE, 1, 0)

  # Remove missing records
  df_new <- na.omit(df_new)

  # Convert the character strings to factors
  df_new$location_description <- as.factor(df_new$location_description)

  # One-hot encode the categorical variables into dummy variables
  df_new <- df_new %>%
    ungroup() %>%
    mutate(season_summer=ifelse(season=='Summer',1,0),
           season_winter=ifelse(season=='Winter',1,0),
           season_spring=ifelse(season=='Spring',1,0),
           season_fall=ifelse(season=='Fall',1,0))
  # Encode categorical variables into numerical ones
  labs1 = LabelEncoder.fit(df_new$location_description)
  #convert labels to numeric values
  df_new$location_description = transform(labs1, df_new$location_description)

  labs6 = LabelEncoder.fit(df_new$crime_type)
  df_new$crime_type = transform(labs6, df_new$crime_type)

  # Ordinal Encoding
  df_new$beat <- as.numeric(factor(df_new$beat, levels = unique(df_new$beat), exclude = NULL))

  df_new$district <- as.numeric(factor(df_new$district, levels = unique(df_new$district), exclude = NULL))

  df_new$ward <- as.numeric(factor(df_new$ward, levels = unique(df_new$ward), exclude = NULL))
  df_new$community_area <- as.numeric(factor(df_new$community_area, levels = unique(df_new$community_area), exclude = NULL))

  df_new <- df_new[,-9]

  head(df_new)
  return(df_new)
}

## Correlation Matrix ----
TCMat <- function(df_new){
  library(corrplot)
  M <- df_new %>%
    ungroup() %>%
    select(location_description, beat,district, ward, community_area,latitude, longitude, crime_type) %>%
    cor()

  # correlation plot
  corrplot.mixed(M, order = "hclust", number.cex = 0.9)
}

## Logistic Regression ----
### Data partition ----
DP <- function(df_new){
  library(lattice)
  library(caret)
  library(dplyr)
  library(MASS)

  # Split the dataset into training and testing sets
  set.seed(123)
  train_index <- createDataPartition(df_new$arrest, p = 0.8, list = FALSE)
  train <- df_new[train_index, ]
  test <- df_new[-train_index, ]
  return(list(train = train, test = test))
}

### Logistic regression fit ----
LRFit <- function(train, myformula = as.formula("arrest~.")){
  # Fit a logistic regression model with selected features
  glm_fit <- glm(myformula, data = train, family = "binomial")
  return(glm_fit)
}

### Logistic regression prediction ----
LRPred <- function(glm_fit, test){
  # Make predictions on the testing set
  predictions <- predict(glm_fit, newdata = test, type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  return(predicted_classes)
}

### Logistic regression confusion matrix ----
LRCMat <- function(predicted_classes, test){
  # Evaluate the model performance on the testing set
  confusionMatrix(data = as.factor(predicted_classes), reference = as.factor(test$arrest))
}


