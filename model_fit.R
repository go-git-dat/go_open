
library("dplyr")
library("tidyr" )
library("readr")
library("lubridate")
library("clock")
library("magrittr") 
library("olsrr" )
library("broom" )
library("Metrics")


##########################################################################
## script start
# read in data

df <- read_csv("df.csv") %>% 
  mutate(Wochentag = date_weekday_factor(ymd(Datum), #  get weekdays as ordered factor; ymd() -> format: year,month,day
                                         abbreviate = F, # weekdays written out
                                         encoding = "iso"),
         Warengruppe = factor(x = Warengruppe,      # override 'Warengruppe' variable to factor
                              levels = c(1:6),      # with levels 1 through 6
                              labels = c('Brot',    # + labels
                                         'Brötchen',
                                         'Croissant',
                                         'Konditorei',
                                         'Kuchen',
                                         'Saisonbrot')
                              ),
         KielerWoche = replace_na(KielerWoche, 0),  # recode  NAs to 0
         #KielerWoche = as.factor(KielerWoche),
         Bewoelkung = as.factor(Bewoelkung),
         Wettercode = as.factor(Wettercode),
         Feiertag = as.factor(Feiertag),
         Ferien = as.factor(Ferien)
         ) %>% 
  # mutate(KielerWoche = factor(x = KielerWoche,      # override 'Warengruppe' variable to factor
  #                      levels = c(0,1),      # with levels 1 through 6
  #                      labels = c('nein',    # + labels
  #                                 'ja'))
  #        ) %>%
  arrange(Datum)# arrange df according the date variable


str(df)
summary(df)


# Trainingsdatensatz vom 01.07.2013 bis 31.03.2019, 
# Validierungsdatensatz vom 01.04. bis 07.06.2019 und 
# Testdatensatz vom 08.06. bis 30.07.2019


# Split the data into training, validation, and test datasets
df_train <- df %>% 
  filter(Datum <= "2019-03-31")
df_validate <- df %>% 
  filter(Datum > "2019-03-31" & Datum <= "2019-06-07")
df_test <- df %>% 
  filter(Datum > "2019-06-07" & Datum <= "2019-07-30")

# Check the dimensions of the datasets
cat("Training dataset dimensions:", dim(df_train), "\n")
cat("Validation dataset dimensions:", dim(df_validate), "\n")
cat("Test dataset dimensions:", dim(df_test), "\n")




# Estimating (Training) Models
mod_0 <- lm(Umsatz ~ as.factor(Warengruppe) 
          + KielerWoche 
          + as.factor(Bewoelkung) 
          + Temperatur 
          + Windgeschwindigkeit 
          + as.factor(Wettercode)  
          + as.factor(Feiertag) 
          + as.factor(Ferien) 
          + as.factor(Wochentag)
          , df_train)


mod_1 <- lm(Umsatz ~ as.factor(Warengruppe) 
            + KielerWoche 
            + as.factor(Bewoelkung) 
            + Temperatur 
           #+ Windgeschwindigkeit 
           #+ as.factor(Wettercode)  
            + as.factor(Feiertag) 
            + as.factor(Ferien) 
            + as.factor(Wochentag)
            , df_train)

mod_2 <- lm(Umsatz ~ as.factor(Warengruppe) 
            + KielerWoche 
            + as.factor(Bewoelkung) 
            + Temperatur 
          #  #+ Windgeschwindigkeit 
          #  #+ as.factor(Wettercode)  
            + as.factor(Feiertag) 
            + as.factor(Ferien) 
           # + as.factor(Wochentag)
            , df_train)

mod_3 <- lm(Umsatz ~ as.factor(Warengruppe) 
            + KielerWoche 
           # + as.factor(Bewoelkung) 
            + Temperatur 
          #  #  #+ Windgeschwindigkeit 
          #  #  #+ as.factor(Wettercode)  
            + as.factor(Feiertag) 
            + as.factor(Ferien) 
          #  # + as.factor(Wochentag)
            , df_train)

mod_4 <- lm(Umsatz ~ as.factor(Warengruppe) 
           # + KielerWoche 
          #  # + as.factor(Bewoelkung) 
            + Temperatur 
          #  #  #  #+ Windgeschwindigkeit 
          #  #  #  #+ as.factor(Wettercode)  
            + as.factor(Feiertag) 
            + as.factor(Ferien) 
          #  #  # + as.factor(Wochentag)
            , df_train)

glance(mod_0)
glance(mod_1)
glance(mod_2)
glance(mod_3)
glance(mod_4)

### Nutzung des resultierenden Modells für eine Vohersage

# Make predictions using the test data
predicted_values <- predict(mod_4, newdata = df_validate)

# Compare the predicted values with the actual values
comparison <- data.frame(Actual = df_validate, Predicted = predicted_values)

# Calculate the mean squared error (RMSE)
rmse <- sqrt(mean((comparison$Actual - comparison$Predicted)^2))

# Display the comparison and RMSE
head(comparison)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")



