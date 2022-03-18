times <- arrange(df, desc(recorded_at)) %>%
  select(recorded_at) %>%
  distinct()

today_stamp <- times[1,]


df %>%
  select(run_id, recorded_at) %>%
  distinct() %>%
  ggplot(aes(x = recorded_at)) +
  geom_histogram() +
  geom_vline(xintercept = as.numeric(today_stamp)) +
  theme_classic() +
  labs(title = "Data Counts")