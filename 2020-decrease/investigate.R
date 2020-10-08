

library(icews)
library(ggplot2)

query <- "
SELECT event_date, count(*) AS records
FROM   events
GROUP BY event_date;"
daily_records <- query_icews(query)
daily_records$event_date <- as.Date(as.character(daily_records$event_date),
                                    format = "%Y%m%d", origin = "1970-01-01")

ggplot(daily_records, aes(x = event_date, y = records)) +
  geom_point()

query <- "
SELECT distinct(source_file),
       count(*) as records
FROM   events
GROUP BY source_file;"
n_by_sf <- query_icews(query)


query <- "
SELECT count(*) AS records,
       count(distinct(event_id)) AS ids
FROM events LIMIT 1;"
daily_count <- query_icews(query)


# Verify that events are correctly fetched from DVN

# Verify number of rows in files matches number of ingested records
dir(find_raw())
sf <- "20200927-icews-events.tab"
check <- readr::read_tsv(find_raw(sf))

query <- sprintf("
SELECT count(*) AS records
FROM   events
WHERE source_file = '%s'
", sf)
db_n <- query_icews(query)

nrow(check)
db_n[[1]]

# matches

# Check number of events by source file vs time period nominally covered





