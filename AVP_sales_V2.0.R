# Version 2.0 of AVP's ticket sales from the academic year of 2016-2017.

library(tidyverse)
library(lubridate)
library(stringr)

# read in data
fall_read <- read_csv(file = "~/Desktop/Coding/R/AVP_ticket_sales/AVP_Fall2016_Ticket_Sales.csv",
                      col_types = cols(Date = col_character(),
                                       `Time (roughly) (HH:MM)` = col_character()))
spring_read <- read_csv(file = "~/Desktop/Coding/R/AVP_ticket_sales/AVP_Spring2017_Ticket_Sales.csv",
                        col_types = cols(Date = col_character(),
                                         `Time (roughly) (HH:MM)` = col_character()))

# Rename variable names so they 1) look better/more concise and 2) fit on screen better
#  add new, useful variables
fall <- fall_read %>% 
  mutate(date = mdy(Date),
         wday = wday(date, label = TRUE),
         #time = hm(`Time (roughly) (HH:MM)`),
         date_time = mdy_hm(str_c(Date, `Time (roughly) (HH:MM)`)),
         fri_stud = `Friday tickets Sold`,
         sat_stud = `Sat tickets Sold`,
         fri_deal =  `Friday Deal Tickets Sold`,
         fri_adult = `Friday Adult tickets`,
         sat_adult = `Sat Adult Tickets`,
         method = `Payment Method (cash, card, venmo, online)`,
         total = `Total Tix`,
         profit = (fri_stud + sat_stud) * 6 + (fri_adult + sat_adult) * 10 + fri_deal * 12/3) %>% 
  select(wday:profit)

spring <- spring_read %>% 
  mutate(date = mdy(Date),
         wday = wday(date, label = TRUE),
         date_time = mdy_hm(str_c(Date, `Time (roughly) (HH:MM)`, sep  = " ")),
         #time = hm(`Time (roughly) (HH:MM)`),
         fri_stud = `Friday tickets Sold`,
         sat_stud = `Sat tickets Sold`,
         fri_adult = `Friday Adult tickets`,
         sat_adult = `Sat Adult Tickets`,
         method = `Payment Method (cash, card, venmo, online)`,
         total = `Total Tix`,
         profit = (fri_stud + sat_stud) * 7 + (fri_adult + sat_adult) * 12) %>% 
  select(wday:profit)


############# Total tickets sold per night per category ############# 

# More hard-coded solution
# fall %>% 
#   summarize(fri_stud = sum(fri_stud),
#             sat_stud = sum(sat_stud),
#             fri_deal = sum(fri_deal),
#             fri_adult = sum(fri_adult),
#             st_adult = sum(sat_adult),
#             total = sum(total))

sum_cols <- function(df) {
  output <- vector("integer", ncol(df))
  for (i in seq_along(df)) {
    if (is.numeric(df[[i]])) {
      output[[i]] <- sum(df[[i]])
    }
  }
  output <- set_names(output, names(df))
  output <- output[!output == 0]
}
fall_totals <- sum_cols(fall)
spring_totals <- sum_cols(spring)


################ Gross Profit per night and per category ################ 

profit_by_tix <- function(vector, col_names, prices) {
  indiv_profit <- vector("double", length(col_names))
  if (length(col_names) != length(prices)) {
    stop("'col_names' and 'prices' must be the same length", call. = FALSE)
  }
  prices <- set_names(prices, col_names)
  for (name in col_names) {
    indiv_profit[name] <- vector[name] * as.numeric(prices[name])
  }
  sum(indiv_profit)
}

fall_fri_names <- c("fri_stud", "fri_deal", "fri_adult")
fall_sat_names <- c("sat_stud", "sat_adult")
spring_fri_names <- c("fri_stud", "fri_adult")
spring_sat_names <- c("sat_stud", "sat_adult")
# student, deal, and adult prices
fall_fri_prices <- c(6, 12/3, 10)
fall_sat_prices <- c(6, 10) # excluding deal
spring_prices <- c(7, 12)

# Fall Per Night Costs: $500 for Jerry per night and 38 for awesome; $22 for security guard (one night)
# Program price needs to be added to this cost but is currently unknown.
fall_costs <- 500 + 38 + 11

# Spring Per night: $550 for Jerry per night and 55 for awesome; $150 for security guard both nights(lmao)
# Program price needs to be added to this cost but is currently unknown. May have been free thank to the PAC.
spring_costs <- 550 + 55/2 + 150/2

# ex: profit_by_tix(fall_totals, c("fri_stud", "fri_deal", "fri_adult"), c(6, 12/3, 10))

fall_profit <- list("fall 2016", sum(fall_totals[fall_fri_names]),
                 sum(fall_totals[fall_sat_names]),
                 fall_totals["total"], 
                 profit_by_tix(fall_totals, col_names = fall_fri_names, prices = fall_fri_prices),
                 profit_by_tix(fall_totals, col_names = fall_sat_names, prices = fall_sat_prices),
                 fall_costs)
fall_profit <- set_names(fall_profit, c("concert", "friday", "saturday", "total", "fri_profit", 
                                        "sat_profit", "costs"))
spring_profit <- list("spring 2017", sum(spring_totals[spring_fri_names]),
                 sum(spring_totals[spring_sat_names]),
                 spring_totals["total"],
                 profit_by_tix(spring_totals, 
                               col_names = spring_fri_names, 
                               prices = spring_prices),
                 profit_by_tix(spring_totals, 
                               col_names = spring_sat_names, 
                               prices = spring_prices),
                 spring_costs)
spring_profit <- set_names(spring_profit, c("concert", "friday", "saturday", "total", "fri_profit", 
                                            "sat_profit", "cost"))

both_sum <- rbind(fall_profit, spring_profit) %>% 
  as.tibble() %>% 
  unnest() %>% 
  gather(friday, saturday, key = "night", value = "tickets") %>% 
  mutate(gross = ifelse(night == "friday", fri_profit, sat_profit),
         net = gross - costs) %>% 
  select(-total, -fri_profit, -sat_profit) %>% # deselect unnecessary columns
  select(concert, night:gross, costs, net) # reorder (move costs to last column)

ggplot(both_sum, aes(night, tickets, fill = concert)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Night",
       y = "Tickets Sold")

ggplot(both_sum, aes(concert, tickets, fill = night)) +
  geom_bar(stat = "identity") +
  labs(x = "Night",
       y = "Tickets Sold",
       title = "More tickets were sold in the spring \ndespite a 3-for-2 deal on Friday in the fall")

ggplot(both_sum, aes(night, gross, fill = concert)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Night",
       y = "Gross Profit")

ggplot(both_sum, aes(concert, gross, fill = night)) +
  geom_bar(stat = "identity")

############## Net Profit ###############

ggplot(both_sum, aes(night, net, fill = concert)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Night",
       y = "Net Profit")

ggplot(both_sum, aes(concert, net, fill = concert)) +
  geom_bar(stat = "identity")


########### Visualizing the different methods of payment ########### 
fall_methods <- fall %>% 
  group_by(method) %>% 
  count()
spring_methods <- spring %>% 
  filter(!is.na(method)) %>% 
  group_by(method) %>% 
  count()

pay_methods <- left_join(fall_methods, spring_methods, by = "method") %>% 
  mutate(fall = n.x, 
         spring = n.y) %>% 
  select(-n.x, -n.y)

pay_methods %>% 
  gather(fall, spring, key = "concert", value = "n") %>% 
  ggplot(aes(reorder(method, n, FUN = max), n, fill = concert)) +
    geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Payment Method",
       y = "",
       title = "2016-2017 AVP Concert Season")

pay_methods %>% 
  mutate(simple_method = if_else(method == "door", "door", "other")) %>% 
  group_by(simple_method) %>% 
  summarise(fall = sum(fall),
            spring = sum(spring)) %>% 
  gather(fall, spring, key = "concert", value = "n") %>% 
  ggplot(aes(reorder(simple_method, n, FUN = max), n, fill = concert)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Payment Method",
       y = "",
       title = "2016-2017 AVP Concert Season")
  

########## Ticket Sales per Day ########## 
fall_by_day <- fall %>% 
  group_by(wday) %>% 
  summarize(n = sum(total),
            profit = sum(profit)) %>% 
  mutate(cum_profit = cumsum(profit))
spring_by_day <- spring %>% 
  group_by(wday) %>% 
  summarize(n = sum(total),
            profit = sum(profit)) %>% 
  filter(!is.na(n)) %>% 
  mutate(cum_profit = cumsum(profit))

both_sales <- left_join(fall_by_day, spring_by_day, by = "wday") %>% 
  mutate(fall = n.x, 
         spring = n.y) %>% 
  gather(fall, spring, key = "concert", value = "n") %>%
  mutate(profit = ifelse(concert == "fall", profit.x, profit.y),
         profit = ifelse(is.na(profit), 0, profit),
         n = ifelse(is.na(n), 0, n),
         cum_profit = ifelse(concert == "fall", cum_profit.x, cum_profit.y),
         cum_profit = ifelse(is.na(cum_profit), 0, cum_profit)) %>% 
  select(-(n.x:cum_profit.y))

s <- both_sales %>% 
  ggplot(aes(wday, profit, fill = concert)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Day of the Week",
         y = "Gross Profit")
s
s + geom_bar(aes(wday, cum_profit, fill = concert),
             stat = "identity", position = "dodge", alpha = 1/3) +
  labs(title = "Cummulative Sales Overlaid on Sales by Day")

####### Tabling Ticket Sales Histograms ####### 
fall_lims <- c(ymd_hm("2016-01-01 10:00","2016-01-01 15:00"))
spring_lims <- c(ymd_hm("2017-01-01 10:00","2017-01-01 15:00"))
  
fall_tabling <- fall %>% 
  filter(!wday %in% c("Sun", "Sat")) %>% 
  mutate(date_fixed = update(date_time, yday = 1)) %>% 
  ggplot(aes(date_fixed)) +
  geom_histogram(fill="lightblue",
                 color = "grey50",
                 binwidth = 15*60) +
  labs(x = "Time (hour)", y = "Count",
       title = "Fall 2016 Tabling") +
  scale_x_datetime(limits = fall_lims, date_labels = "%I")

# tabling, combining all days
fall_tabling 
# tabling by day
fall_tabling + facet_wrap(~wday)


spring_tabling <- spring %>% 
  filter(!wday %in% c("Sun", "Sat")) %>% 
  mutate(date_fixed = update(date_time, yday = 1)) %>% 
  ggplot(aes(date_fixed)) +
  geom_histogram(fill="lightblue",
                 color = "grey50",
                 binwidth = 15*60) +
  labs(x = "Time (hour)", y = "Count",
       title = "Spring 2017 Tabling") +
  scale_x_datetime(limits = spring_lims, date_labels = "%I")
                   

# tabling, combining all days
spring_tabling 
# tabling, by day
spring_tabling + facet_wrap(~wday)


  