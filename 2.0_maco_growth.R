library(tidyverse)
library(maddison)
library(ggplot2)
library(lubridate)
library(scales) # Recommended for better labels

str(maddison)

mad1 <- maddison |> 
  filter(iso2c == "US")
ggplot(mad1, aes(x = year, y = rgdpnapc)) +
          geom_smooth()

mad2 <- maddison |> 
  filter(year >= as.Date("1800-01-01"))
ggplot(mad2, aes(x = year, y = rgdpnapc)) +
  geom_smooth()
# this didn't work because year is in NUMeric format. Two options:

# Option 1: Change the filter to use a numeric value instead of a Date object
mad2 <- maddison |> 
  filter(year >= 1800)
ggplot(mad2, aes(x = year, y = rgdpnapc)) +
  geom_smooth()

# Option 2: To convert a numeric year into a formal Date object in R, you must provide a specific 
#   month and day (usually January 1st) because R's Date class requires a complete calendar date.

mad_date <- maddison |> 
  filter(year >= 1800) |> 
  mutate(date_obj = make_date(year, 1, 1)) # Creates YYYY-01-01

# Once your x-axis is a formal Date object, you can use scale_x_date() to precisely control labels 
#   and breaks.
  
ggplot(mad_date, aes(x = date_obj, y = rgdpnapc)) +
  geom_smooth() +
  scale_y_log10(labels = scales::label_comma()) +
  # Customise date display (e.g., every 50 years)
  scale_x_date(date_breaks = "50 years", date_labels = "%Y") +
  labs(x = "Year (Date Object)")
# note that added here is the log10 scale to the Y axis. This is particularly useful for economic data like GDP, as it visualises percentage growth 
#   rates as straight lines rather than exponential curves.


# This function adds the "carpet" of small tick marks typically seen on logarithmic graph paper, 
#   which helps the reader visualize the magnitudes between the powers of 10.

# 1. Prepare data
mad_date <- maddison |> 
  filter(year >= 1800)

# 2. Build the plot
ggplot(mad_date, aes(x = year, y = rgdpnapc)) +
  geom_smooth(color = "steelblue", se = FALSE) +
  scale_y_log10(labels = label_comma()) +
  # Add the log ticks to the left side
  annotation_logticks(sides = "l") + 
  theme_minimal() +
  labs(
    title = "Real GDP per Capita (Log Scale)",
    y = "RGDPNAPC",
    x = "Year"
  )


# ADD COUNTRIES

# 1. Filter for your specific countries
countries_to_plot <- c("India", "China", "Argentina", "United States", "United Kingdom")

mad_subset <- maddison |> 
  filter(year >= 1800, 
         country %in% countries_to_plot)

# 2. Plot with color aesthetic
ggplot(mad_subset, aes(x = year, y = rgdpnapc, color = country)) +
  geom_line(size = 1) + # geom_line is better than geom_smooth for direct comparison
  scale_y_log10(labels = label_comma()) +
  annotation_logticks(sides = "l") +
  theme_minimal() +
  labs(
    title = "GDP Comparison: 1800–Present",
    subtitle = "Log scale: Slopes represent growth rates",
    y = "Real GDP per Capita",
    x = "Year",
    color = "Country"
  )

# 

install.packages("pwt9")
library(pwt9)
data("pwt9.0")
pwt <- force(pwt9.0)
rm(pwt9.0)
pwt <- as_tibble(pwt)

pwt2 <- pwt |> 
  filter(year == 2014)

pwt3 <- pwt2 |> 
  select("cgdpo", "emp", "pop", "ck", "country", "isocode")

str(pwt3)

pwt4 <- pwt3 |> 
  mutate(out_per_worker = cgdpo/pop,
         cap_per_worker = ck/pop)

str(pwt4)

# 1. Extract the US values specifically
usa_val_out <- pwt4 |> filter(country == "United States of America") |> pull(out_per_worker)
usa_val_cap <- pwt4 |> filter(country == "United States of America") |> pull(cap_per_worker)

# 2. Now run your mutate using these specific numbers
pwt6 <- pwt4 |> 
  mutate(
    out  = out_per_worker / usa_val_out,
    cap  = cap_per_worker / usa_val_cap,
    pred = cap^(1/3)
  )

# 3. View results (now they should be decimals < 1 for other countries)
pwt6 |> select(country, out, pred, cap)

library(ggplot2)
library(ggrepel) # For non-overlapping labels

ggplot(pwt6, aes(x = out, y = pred)) +
  # 1. Add the 45-degree reference line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  
  # 2. Add the country points
  geom_point(color = "steelblue", size = 3) +
  
  # 3. Add country labels (so we know which point is which)
  geom_text_repel(aes(label = country), size = 3) +
  
  # 4. Standardise the axes (0 to 1 scale)
  scale_x_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
  
  # 5. Labels and Theme
  labs(
    title = "Development Accounting: Actual vs. Predicted Output",
    subtitle = "Relative to the United States (1.0)",
    x = "Actual Output (GDP per Worker)",
    y = "Predicted Output (Capital per Worker ^ 1/3)",
    caption = "Dashed line represents the Solow model's perfect prediction (TFP = 1)"
  ) +
  theme_minimal() +
  coord_fixed() # Makes the plot a perfect square so the 45-degree line is actually 45 degrees


# SOLOW MODEL SIMULATION

a <- 1/3
A <- 2
L <- 200
Klow <- 0
Khigh <- 4000
Knumber <- 100
K <- seq(from = Klow, to = Khigh,
         length.out = Knumber)
Y <- A * K^a * L^(1-a)
prod <- data.frame(K,Y)

ggplot(prod, aes(x = K, y = Y)) +
         geom_smooth()

# having simulated the production function, we generate Solow diagram, which shows how savings and depreciation 
#   vary with capital (K)

s <- 0.25
S <- s*Y
d <- 0.1
dep <- d*K
Y <- A * K^a * L^(1-a)
Solow <- data.frame(S, dep, K)
ggplot(Solow) +
  geom_smooth(aes(x = K, y = S)) +
  geom_smooth(aes(x = K, y = dep))

# increase the savings rate (s) to produce dashed line
s2 <- 0.35
S2 <- s2*Y
Solow <- data.frame(S, S2, dep, K)
ggplot(Solow) +
  geom_smooth(aes(x = K, y = S)) +
  geom_smooth(aes(x = K, y = dep)) +
  geom_smooth(aes(x = K, y = S2),
              linetype = "dashed")

# then look at output
d <- 0.1
L <- 200
a <- 1/3
A <- 2
Kt <- numeric(100)
Kt[1] <- 500
for(i in 2:100) {
  Kt[i] <- Kt[i-1] +
    s * (A * Kt[i-1]^a * L^(1-a)) -
    d * Kt[i-1]
}

Yt <- A * Kt^a * L^(1-a)
motion <- tibble(Yt, Kt)
ggplot(motion, aes(x = 1:100 , 
                   y = Yt)) +
  geom_smooth()