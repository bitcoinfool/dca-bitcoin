#Import the required packages
library(tidyverse)      # Provides 'dplyr' for data manipulation and 'ggplot2' for plotting.
library(quantmod)       # Provides 'getSymbols' to fetch financial data from Yahoo Finance.
library(lubridate)      # Provides 'years()' to manipulate date objects.
library(gganimate)      # Provides 'transition_reveal()' for creating animated plots.
library(scales)         # Provides 'scale_y_log10()' and 'label_dollar()' for axis scaling.
library(RColorBrewer)   # Provides 'brewer.pal' to generate color palettes.
library(av)             # Provides 'av_renderer()' to render animation to video.

# -------------------------------------------------------------------

# PART 1: Plot a graph to show the performance of Bitcoin DCA across multiple timeframes
# Define a function to extract and process data from Yahoo Finance
fetch_and_save_asset_data <- function(symbol, asset_name, start_date = "2000-01-01", end_date = Sys.Date()) {
  getSymbols(symbol, src = 'yahoo', from = start_date, to = end_date, auto.assign = TRUE)   # Fetch data
  data <- get(symbol)
  data_filled <- na.approx(data)   # Fill missing data
  data_df <- data.frame(date = index(data_filled), coredata(data_filled))   # Create data frame
  column_name <- paste0(asset_name,"_price")
  data_df_filtered <- data_df %>%
    select(1, column_name = ncol(.))
  # Select the first column and rename the last column to 'asset_name_price'
  last_col_name <- names(data_df)[ncol(data_df)] # Get the name of the last column
  column_name <- paste0("price_", asset_name)    # Define the new column name
  data_df_filtered <- data_df %>%
    select(1, !!column_name := all_of(last_col_name)) %>%
    filter(date >= (Sys.Date() - years(10)))
  return(data_df_filtered)
}

# Apply this function to Bitcoin priced in USD
btc_yahoo <- fetch_and_save_asset_data("BTC-USD", "btc") %>% # The BTC data frame only starts from 2014-09-17
  select(date = date, btc_price = price_btc)

# Import historical Bitcoin price data in USD for a timeframe exceeding Yahoo Finance's range
btc_historical <- read_csv("BTC-USD_historical.csv") %>%
  select(date = date, btc_price = price_usd_close) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  filter(date < "2014-09-17")

# Combine and further preprocess the data
btc <- bind_rows(btc_yahoo, btc_historical) %>%
  arrange(date) %>%
  filter(date >= as.Date("2012-11-28")) %>% # Filter for results starting from the 1st halving
  mutate(subtitle_text = paste("Date:", date, "    BTC Price: $", comma(round(btc_price, 0)))) 
# Create a new column 'subtitle_text' for dynamic subtitle content

# Get the most recent date in your dataset
last_date <- max(btc$date)

# Create an allocate function to simulate DCA allocations
allocate <- function(btc_data, percentage, monthly_savings, start_years_ago) {
  # Calculate the start_date as the day before the latest date for the past years
  start_date <- last_date - years(start_years_ago)
  
  btc_data %>%
    filter(date >= start_date) %>%
    mutate(
      # Create a flag for saving on the same day of each month as the last_date
      save_flag = if_else(day(date) == day(last_date), 1, 0),
      save_btc = if_else(save_flag == 1, percentage * monthly_savings, 0),
      save_cash = if_else(save_flag == 1, (1 - percentage) * monthly_savings, 0),
      btc_bought_month = if_else(save_flag == 1, save_btc / btc_price, 0)
    ) %>%
    mutate(
      btc_bought_ever = cumsum(btc_bought_month),
      cash_value = cumsum(save_cash),
      btc_value = btc_bought_ever * btc_price,
      total_value = btc_value + cash_value
    )
}

# Initialize allocations list
allocations_btc_only <- list()
monthly_savings <- 1000 # Edit this based on your monthly savings in US dollars

# Loop to fill the allocations list with correctly formatted labels
for (i in 1:10) {
  label <- ifelse(i == 1, "1 year ago", paste0(i, " years ago"))
  allocations_btc_only[[label]] <- allocate(btc, 1, monthly_savings, i)
}

# Combine all allocations into a single data frame
combined_allocations_btc_only <- bind_rows(allocations_btc_only, .id = "years_ago") %>%
  mutate(years_ago = factor(years_ago, levels = c(paste0(10:2, " years ago"), "1 year ago")))

# Generate a sequence of dates for x-axis breaks
start_year <- 2014
end_year <- year(max(combined_allocations_btc_only$date))
breaks_years <- seq(from = as.Date(paste0(start_year, "-01-01")), 
                    to = as.Date(paste0(end_year, "-01-01")), 
                    by = "2 years")

expanded_set1 <- c(brewer.pal(9, "Set1"), "cyan3")

# Plot the performance of a Bitcoin DCA portfolio for various timeframes on a log scale
p1 <- ggplot(combined_allocations_btc_only, aes(x = date, y = total_value, color = years_ago)) +
  geom_line(aes(group = years_ago), linewidth = 0.7) +
  geom_point(size = 2) +
  labs(x = "", y = "", 
       title = "DCA Performance: 1-10 Year Timeframes Compared",
       caption = "@bitcoinfool", # Please feel free to remove this caption
       color = "Start of Bitcoin DCA") +
  scale_y_log10(labels = scales::label_dollar()) +
  scale_color_manual(values = expanded_set1) +
  scale_x_date(
    limits = as.Date(c("2014-01-01", "2026-01-01")),
    breaks = breaks_years, # Use the custom breaks
    date_labels = "%Y" # Format labels to show only the year
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.x = element_line(colour = "#2A2A2A"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#2A2A2A"),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "white"),
    plot.title = element_text(color = "white", size = 11, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "white", size = 8),
    plot.caption = element_text(color = "darkgrey", size = 8, hjust = 1),
    plot.caption.position = "plot",  # Default value is "panel"
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
    axis.text.y = element_text(size = 6, colour = "darkgrey"),
    axis.text.x = element_text(size = 6, colour = "darkgrey"),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "cm"),
  )
p1_animation <- p1 +
  transition_reveal(date) +
  labs(subtitle = "Based on a monthly allocation of US$1,000 to Bitcoin       Date: {frame_along}") +
  geom_text(aes(label = paste0("$", scales::comma(total_value))), 
            nudge_x = 120, hjust = 0, size = 2.5, show.legend = FALSE) +
  ease_aes('linear') # Ensures smooth transition

p1_animation_video <- animate(p1_animation, height = 1200, width = 1920, fps = 40, duration = 24, end_pause = 150, res = 300, renderer = av_renderer())
anim_save("p1_animation.mp4", p1_animation_video)
# For Twitter/X upload, convert all MP4 files to H.264 format using VLC Media Player. Test the converted file in a web browser before posting.

# -------------------------------------------------------------------

# PART 2: Plot the same graph but denominated in bitcoins
# Modify the allocate function to denominate in bitcoins
allocate_in_btc <- function(btc_data, percentage, monthly_savings, start_years_ago) {
  # Reuse your existing code for the allocate function with a minor modification
  start_date <- last_date - years(start_years_ago)
  
  btc_data %>%
    filter(date >= start_date) %>%
    mutate(
      save_flag = if_else(day(date) == day(last_date), 1, 0),
      save_btc = if_else(save_flag == 1, percentage * monthly_savings / btc_price, 0), # Buying BTC directly
      save_cash = if_else(save_flag == 1, (1 - percentage) * monthly_savings / btc_price, 0), # Keeping cash in BTC equivalent
      btc_bought_month = save_btc # Already in BTC
    ) %>%
    mutate(
      btc_bought_ever = cumsum(btc_bought_month) + cumsum(save_cash), # Total BTC ever, including cash kept as BTC equivalent
      total_value = btc_bought_ever # Total value is simply the amount of BTC
    )
}

# Recalculate allocations using the updated function
allocations_btc_only_in_btc <- list()
for (i in 1:10) {
  label <- ifelse(i == 1, "1 year ago", paste0(i, " years ago"))
  allocations_btc_only_in_btc[[label]] <- allocate_in_btc(btc, 1, monthly_savings, i)
}

# Combine all allocations into a single data frame
combined_allocations_btc_only_in_btc <- bind_rows(allocations_btc_only_in_btc, .id = "years_ago") %>%
  mutate(years_ago = factor(years_ago, levels = c(paste0(10:2, " years ago"), "1 year ago")))

# Plot the updated graph denominated in bitcoins
p2 <- ggplot(combined_allocations_btc_only_in_btc, aes(x = date, y = total_value, color = years_ago)) +
  geom_line(aes(group = years_ago), linewidth = 0.7) +
  geom_point(size = 2) +
  labs(x = "", y = "Total Portfolio Value (in BTC)", 
       title = "DCA Performance in BTC: 1-10 Year Timeframes Compared",
       caption = "@bitcoinfool", # Please feel free to remove this caption
       color = "Start of DCA") +
  scale_y_log10(labels = scales::label_number()) +
  scale_color_manual(values = expanded_set1) +
  scale_x_date(
    limits = as.Date(c("2014-01-01", "2026-01-01")),
    breaks = breaks_years,
    date_labels = "%Y"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.x = element_line(colour = "#2A2A2A"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#2A2A2A"),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "white"),
    plot.title = element_text(color = "white", size = 11, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "white", size = 8),
    plot.caption = element_text(color = "darkgrey", size = 8, hjust = 1),
    plot.caption.position = "plot",  # Default value is "panel"
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
    axis.text.y = element_text(size = 8, colour = "darkgrey"),
    axis.text.x = element_text(size = 8, colour = "darkgrey"),
    axis.title.y = element_text(size = 8, colour = "darkgrey"),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "cm"),
  )
p2_animation <- p2 +
  transition_reveal(date) +
  labs(subtitle = "Based on a monthly allocation of US$1,000 to Bitcoin       Date: {frame_along}") +
  geom_text(aes(label = paste0(format(round(total_value, 2), nsmall = 2), " BTC")), 
            nudge_x = 120, hjust = 0, size = 2.5, show.legend = FALSE) +
  ease_aes('linear') # Ensures smooth transition
p2_animation_video <- animate(p2_animation, height = 1200, width = 1920, fps = 40, duration = 24, end_pause = 150, res = 300, renderer = av_renderer())
anim_save("p2_animation.mp4", p2_animation_video)

# -------------------------------------------------------------------

# PART 3: Plot the same graph but denominated as a multiple of a 100% cash portfolio
# Loop to fill the allocations list with correctly formatted labels
allocations_cash_only <- list()
for (i in 1:10) {
  label <- ifelse(i == 1, "1 year ago", paste0(i, " years ago"))
  allocations_cash_only[[label]] <- allocate(btc, 0, monthly_savings, i)
}

# Combine all allocations into a single data frame
combined_allocations_cash_only <- bind_rows(allocations_cash_only, .id = "years_ago") %>%
  mutate(years_ago = factor(years_ago, levels = c(paste0(10:2, " years ago"), "1 year ago")))

merged_df <- combined_allocations_btc_only %>%
  left_join(combined_allocations_cash_only, by = c("years_ago", "date"), suffix = c("_btc", "_cash")) %>%
  mutate(total_value_relative = total_value_btc / total_value_cash)

# Plot graph of relative multiples for various time frames
p3 <- ggplot(merged_df, aes(x = date, y = total_value_relative, color = years_ago)) +
  geom_line(aes(group = years_ago), linewidth = 0.7) +
  geom_point(size = 2) +
  labs(x = "", y = "Multiple(s) of a cash-only DCA portfolio", 
       title = "DCA Performance: 1-10 Year Timeframes Compared",
       caption = "@bitcoinfool", # Please feel free to remove this caption
       color = "Start of Bitcoin DCA") +
  scale_y_log10(labels = label_number()) +
  scale_color_manual(values = expanded_set1) +
  scale_x_date(
    limits = as.Date(c("2014-01-01", "2026-01-01")),
    breaks = breaks_years, # Use the custom breaks
    date_labels = "%Y" # Format labels to show only the year
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major.x = element_line(colour = "#2A2A2A"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#2A2A2A"),
    panel.grid.minor.y = element_blank(),
    text = element_text(color = "white"),
    plot.title = element_text(color = "white", size = 11, face = "bold"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "white", size = 8),
    plot.caption = element_text(color = "darkgrey", size = 8, hjust = 1),
    plot.caption.position = "plot",  # Default value is "panel"
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10, unit = "pt"),
    axis.text.y = element_text(size = 8, colour = "darkgrey"),
    axis.text.x = element_text(size = 8, colour = "darkgrey"),
    axis.title.y = element_text(size = 8, colour = "darkgrey"),
    legend.position = "right",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "cm"),
  )
p3_animation <- p3 +
  transition_reveal(date) +
  labs(subtitle = "Based on a monthly allocation of US$1,000 to Bitcoin       Date: {frame_along}") +
  geom_text(aes(label = paste0(format(round(total_value_relative, 2), nsmall = 2), "x")), 
            nudge_x = 120, hjust = 0, size = 2.5, show.legend = FALSE) +
  ease_aes('linear') # Ensures smooth transition
p3_animation_video <- animate(p3_animation, height = 1200, width = 1920, fps = 40, duration = 24, end_pause = 150, res = 300, renderer = av_renderer())
anim_save("p3_animation.mp4", p3_animation_video)

