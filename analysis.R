library(tidyverse)
library(socviz)
library(kjhmisc)
library(scales)

# Caption at half the theme default (0.9 of base size)
theme_set(
  myriad::theme_socviz_kjh() +
    theme(plot.caption = element_text(size = rel(0.65)))
)

source("R/ingest-data.R")

arrivals <- read_arrivals("data/monthly-arrivals.xlsx")
arrivals_yoy <- read_arrivals_yoy("data/monthly-arrivals.xlsx")

focal_countries <- c(
  "Mexico",
  "Canada",
  "United Kingdom",
  "Japan",
  "France"
)

fig_caption <- "Data: International Trade Administration. Figure: Kieran Healy / @kjhealy.co"

# Raw series
arrivals |>
  filter(
    country %in% focal_countries
  ) |>
  ggplot(aes(x = date, y = arrivals, color = country)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = label_comma(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = "Arrivals",
    color = "Country",
    title = "Tourist Visits to the US (monthly)",
    subtitle = "Raw series",
    caption = fig_caption
  )

ggsave("figures/arrivals-monthly.png", width = 8, height = 5)

# Smoothed series. A trailing 12-month mean removes the seasonal cycle and
# never moves ahead of the data, so the covid drop is not anticipated.
arrivals_smoothed <- arrivals |>
  arrange(country, date) |>
  mutate(
    arrivals_smooth = slider::slide_dbl(
      arrivals,
      mean,
      .before = 11,
      .complete = TRUE
    ),
    .by = country
  )

arrivals_smoothed |>
  filter(
    country %in% focal_countries,
    !is.na(arrivals_smooth)
  ) |>
  ggplot(aes(x = date, y = arrivals_smooth, color = country)) +
  geom_line(linewidth = 1.05) +
  scale_y_continuous(labels = label_comma(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = "Arrivals",
    color = "Country",
    title = "Tourist Visits to the US (monthly)",
    subtitle = "Trailing 12-month average",
    caption = fig_caption
  )

ggsave("figures/arrivals-monthly-smoothed.png", width = 8, height = 5)

# Yearly series. Calendar years; the data are monthly, so ISO years do not
# apply. Country-years without 12 observed months (e.g. 2026) are dropped.
arrivals_yearly <- arrivals |>
  mutate(year = year(date)) |>
  summarize(
    n_months = sum(!is.na(arrivals)),
    arrivals = sum(arrivals, na.rm = TRUE),
    .by = c(country, region, year)
  ) |>
  filter(n_months == 12) |>
  select(!n_months)

arrivals_yearly |>
  filter(
    country %in% focal_countries
  ) |>
  ggplot(aes(x = year, y = arrivals, color = country)) +
  geom_line(linewidth = 1.15) +
  scale_y_continuous(labels = label_comma(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = "Arrivals",
    color = "Country",
    title = "Tourist Visits to the US (annual)",
    subtitle = "Excludes 2026",
    caption = fig_caption
  )

ggsave("figures/arrivals-yearly.png", width = 8, height = 5)


## Monthly percents
arrivals_yoy |>
  filter(
    country %in% focal_countries
  ) |>
  ggplot(aes(x = date, y = yoy_change, color = country)) +
  geom_line(linewidth = 1.15) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = NULL,
    y = "Change in arrivals",
    color = "Country",
    title = "Tourist Visits to the US (monthly)",
    subtitle = "Year-over-year percent change by month",
    caption = fig_caption
  )

ggsave("figures/arrivals-yoy.png", width = 8, height = 5)

## Monthly arrivals relative to the same month in 2019. Year-over-year change
## is bounded at -100% but unbounded above, so the post-covid rebound swamps
## the decline. A fixed pre-covid baseline avoids that.
arrivals_vs_2019 <- arrivals |>
  mutate(month = month(date)) |>
  left_join(
    arrivals |>
      filter(year(date) == 2019) |>
      transmute(country, month = month(date), baseline = arrivals),
    by = join_by(country, month)
  ) |>
  mutate(vs_2019 = arrivals / baseline - 1) |>
  select(!month)

arrivals_vs_2019 |>
  filter(
    country %in% focal_countries,
    date >= "2020-01-01"
  ) |>
  ggplot(aes(x = date, y = vs_2019, color = country)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(linewidth = 1.15) +
  scale_y_continuous(labels = label_percent()) +
  labs(
    x = NULL,
    y = "Change in arrivals",
    color = "Country",
    title = "Tourist Visits to the US (monthly)",
    subtitle = "Percent difference from the same month in 2019",
    caption = fig_caption
  )

ggsave("figures/arrivals-vs-2019.png", width = 8, height = 5)


# Smoothed series by region.
arrivals_smoothed_byregion <- arrivals |>
  summarize(arrivals = sum(arrivals, na.rm = TRUE), .by = c(region, date)) |>
  arrange(region, date) |>
  mutate(
    arrivals_smooth = slider::slide_dbl(
      arrivals,
      mean,
      .before = 11,
      .complete = TRUE
    ),
    .by = region
  )


arrivals_smoothed_byregion |>
  filter_out(is.na(arrivals_smooth)) |>
  filter(date >= ymd("2015-01-01")) |>
  filter(
    region %in%
      c(
        "Western Europe",
        "Asia"
      )
  ) |>
  ggplot(aes(x = date, y = arrivals_smooth, color = region)) +
  geom_line(linewidth = 1.15) +
  scale_y_continuous(labels = label_comma(scale_cut = cut_short_scale())) +
  labs(
    x = NULL,
    y = "Arrivals",
    color = "Region",
    title = "Tourist Visits to the US (monthly)",
    subtitle = "Trailing 12-month average",
    caption = fig_caption
  )


# bigregion
arrivals_smoothed_by_bigregion <- arrivals |>
  mutate(
    bigregion = recode_values(
      region,
      "North America" ~ "North America",
      "Western Europe" ~ "Europe",
      "Eastern Europe" ~ "Europe",
      "Asia" ~ "Asia",
      "South America" ~ "Central and South America",
      "Central America" ~ "Central and South America",
      "Caribbean" ~ "Central and South America",
      default = "Rest of the World"
    ),
    # North America is only Canada and Mexico; show them separately
    bigregion = if_else(bigregion == "North America", country, bigregion)
  ) |>
  summarize(arrivals = sum(arrivals, na.rm = TRUE), .by = c(bigregion, date)) |>
  arrange(bigregion, date) |>
  mutate(
    arrivals_smooth = slider::slide_dbl(
      arrivals,
      mean,
      .before = 11,
      .complete = TRUE
    ),
    .by = bigregion
  )

bigregion_series <- arrivals_smoothed_by_bigregion |>
  filter_out(is.na(arrivals_smooth)) |>
  filter(date >= ymd("2015-01-01"), date <= ymd("2026-06-01"))

bigregion_plot <- bigregion_series |>
  ggplot(aes(x = date, y = arrivals_smooth, color = bigregion)) +
  geom_line(linewidth = 1.15) +
  ggrepel::geom_text_repel(
    data = bigregion_series |>
      filter(date == max(date)),
    mapping = aes(label = str_wrap(bigregion, 14)),
    size = 3.5,
    family = "Socviz Condensed",
    lineheight = 0.9,
    hjust = 0,
    direction = "y",
    nudge_x = 80,
    nudge_y = 1.5e4,
    segment.color = NA
  ) +
  scale_x_date(
    expand = expansion(mult = c(0.02, 0.2)),
    date_breaks = "3 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(labels = label_comma(scale_cut = cut_short_scale())) +
  guides(color = "none") +
  labs(
    x = NULL,
    y = "Arrivals",
    title = "Tourist Visits to the United States, January 2015 to June 2026",
    subtitle = "Monthly data, trailing twelve-month average",
    caption = fig_caption
  )

ggsave(
  "figures/arrivals-bigregion-smoothed.png",
  bigregion_plot,
  width = 8,
  height = 5
)
