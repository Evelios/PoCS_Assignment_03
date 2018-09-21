# Thomas Waters
# Assignment 3
library(tidyverse)
library(ggplot2)

google_vocab_raw = 'vocab_rawwordfreqs.txt'
vocab_freqs_raw = as.numeric(read_lines(google_vocab_raw))

vocab_freqs = data.frame(freq = vocab_freqs_raw)
vocab_freqs$rank = 1:length(vocab_freqs$freq)

vocab_freqs = vocab_freqs %>%
  mutate(log_rank = log10(rank)) %>%
  mutate(log_freq = log10(freq))

dropout_ammout = 20
max_frequency_dropout = 10^6
vocab_freqs_reduced = vocab_freqs %>%
  filter(freq > max_frequency_dropout | rank %% dropout_ammout == 0)

# Plot the CDF
ggplot(vocab_freqs_reduced,
       aes(x=log_freq, y=log_rank)) +
  labs(title='Zipf\'s Frequency Rank of Words',
       x='Nk, Log10 Frequency',
       y='Log10 Rank') +
  geom_point()

# ---- Problem 4 ---------------------------------------------------------------

scaling_boundary = 10^7.5

# ---- Lower Range ----
vocab_lower_range = vocab_freqs %>%
  filter(freq < scaling_boundary)

vocab_lower_range_reduced = vocab_lower_range %>%
  filter(rank %% dropout_ammout == 0)

# Plot the CDF
ggplot(vocab_lower_range_reduced,
       aes(x=log_freq, y=log_rank)) +
  labs(title='Lower Range of Zipf Distribution',
       x='Nk, Log10 Frequency',
       y='Log10 Rank') +
  geom_point() +
  geom_smooth(method='lm')

lower_range_regression = lm(log_freq ~ log_rank, vocab_lower_range)
confint(lower_range_regression)

# ---- Upper Range ----

vocab_upper_range = vocab_freqs %>%
  filter(freq > scaling_boundary)

# Plot the CDF
ggplot(vocab_upper_range,
       aes(x=log_freq, y=log_rank)) +
  labs(title='Upper Range of Zipf Distribution',
       x='Nk, Log10 Frequency',
       y='Log10 Rank') +
  geom_point() +
  geom_smooth(method='lm')

upper_range_regression = lm(log_freq ~ log_rank, vocab_upper_range)
confint(upper_range_regression)
