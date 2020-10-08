
library(icews)
library(ggplot2)
library(dplyr)
library(tidyr)

by_month <- query_icews("
select yearmonth,
       count(*) as events,
       count(distinct(story_id)) as stories,
       min(story_id) as min_story_id,
       max(story_id) as max_story_id
from events
group by yearmonth;")

by_month$year <- as.integer(substr(by_month$yearmonth, 1, 4))
by_month$month <- as.integer(substr(by_month$yearmonth, 5, 6))
by_month$date <- as.Date(paste0(by_month$yearmonth, "01"), format = "%Y%m%d")

# current month is not complete
by_month <- head(by_month, -1)

table(by_month$year, by_month$month)

by_month_long <- by_month %>%
  pivot_longer(events:stories)

by_month_long %>%
  ggplot(aes(x = date, y = value, color = year, group = name)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_c(direction = -1)
ggsave("timeline.png", height = 5, width = 8)

by_month_long %>%
  filter(date > "2017-12-31") %>%
  ggplot(aes(x = date, y = value, color = factor(year), group = name)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(direction = -1)


by_month_long %>%
  ggplot(aes(x = month, group = year, color = year, y = value)) +
  facet_wrap(~ name) +
  geom_line(alpha = .5) +
  geom_line(data = by_month_long %>% filter(year==max(year)), size = 1) +
  theme_minimal() +
  scale_color_viridis_c(direction = -1) +
  scale_x_continuous(breaks = c(seq(1, 12, by = 2)))
ggsave("season-plot.png", height = 5, width = 8)


ggplot(by_month, aes(x = stories, y = events, color = year)) +
  geom_point() +
  scale_color_viridis_c(direction = -1) +
  theme_minimal()
ggsave("events-vs-stories.png", height = 5, width = 6)

by_month %>%
  mutate(events_per_story = events/stories) %>%
  ggplot(aes(x = date, y = events_per_story)) +
  geom_line()

by_month %>%
  mutate(events_per_story = events/stories) %>%
  ggplot(aes(x = stories, y = events_per_story)) +
  geom_point(aes(color = year)) +
  scale_color_viridis_c(direction = -1) +
  theme_minimal() +
  geom_point(data = by_month %>%
               filter(year==max(year)) %>%
               mutate(events_per_story = events/stories),
             size = 5, color = "red", shape = 1) +
  geom_path(data = by_month %>%
               filter(year==max(year)) %>%
               mutate(events_per_story = events/stories),
             color = "red", arrow = arrow(angle = 20, type = "closed", length = unit(3, "mm"))) +
  annotate("text", x = 28000, y = 1.7, label = "2020", color = "red") +
  labs(title = "Monthly number of events and referenced stories")
ggsave("events-per-story-vs-stories.png", height = 5, width = 8)

by_month %>%
  mutate(events_per_story = events/stories) %>%
  ggplot(aes(x = events, y = events_per_story, color = year)) +
  geom_point() +
  scale_color_viridis_c(direction = -1) +
  theme_minimal()

png("min-max-story-id.png", height = 800, width = 1000)
plot(by_month$date, by_month$min_story_id, type = "l", xlab = "yearmonth", ylab = "story_id")
lines(by_month$date, by_month$max_story_id, col = "red")
dev.off()


story_ids_by_month <- query_icews("
select yearmonth,
       story_id,
       count(*) as n
from events
group by yearmonth, story_id;")

story_ids_by_month <- story_ids_by_month %>%
  as_tibble() %>%
  mutate(date = as.Date(paste0(yearmonth, "01"), format = "%Y%m%d"))

# the original version above has 9 million rows and takes forever to plot
story_ids_shortened <- story_ids_by_month %>%
  mutate(year = lubridate::year(date),
         story_id = round(story_id, -6)) %>%  # round to nearest 10^x
  group_by(year, story_id) %>%
  summarize(n = sum(n))

p <- ggplot(story_ids_shortened, aes(x = year, y = story_id, fill = n)) +
  geom_tile() +
  scale_fill_distiller(direction = 1, palette = ) +
  theme_minimal()
p

ggsave("tile-plot-story-ids-by-month.png", plot = p)

story_ids_shortened %>%
  ungroup() %>%
  complete(year, story_id, fill = list(n = 0)) %>%
  mutate(category = cut(n, breaks = c(0, 1, 1e4, 1e5, 2.5e5, 4e5, max(n)), include.lowest = TRUE)) %>%
  ggplot(aes(x = year, y = story_id, fill = category)) +
  geom_tile() +
  scale_fill_viridis_d("N events in story ID bin", labels = c("0 - 1", "1 - 1,000", "1,000 - 10,000", "10,000 - 25,000", "25,000 - 40,000", "40,000 - 47,165")) +
  theme_minimal() +
  labs(x = "Year (based on event_date)", y = "Story ID, rounded to closest million")


