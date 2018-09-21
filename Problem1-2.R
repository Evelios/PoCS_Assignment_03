# Thomas Waters
# Assignment 1
# Problem 1, 2
library(tidyverse)
library(ggplot2)

# A function for creating even distribution of points in log space
logspace <- function( d1, d2, n) exp(log(10)*seq(d1, d2, length.out=n))

# ---- Problem 1 ---------------------------------------------------------------

google_vocab_smaller = 'vocab_k_nk.txt'
vocab_freqs_data = read.table(google_vocab_smaller, sep=' ', col.names=c('freq', 'count'))

google_vocab_raw = 'vocab_rawwordfreqs.txt'
vocab_freqs_raw = as.numeric(read_lines(google_vocab_raw))

vocab_cdf = ecdf(vocab_freqs_raw)

vocab_freqs = vocab_freqs_data %>%
  mutate(cdf       = vocab_cdf(freq)) %>%
  mutate(ccdf      = 1 - cdf) %>%
  mutate(log_freq  = log10(freq)) %>%
  mutate(log_count = log10(count)) %>%
  mutate(log_cdf   = log10(cdf)) %>%
  mutate(log_ccdf  = log10(ccdf))

# Plot the CDF
ggplot(vocab_freqs,
       aes(x=log_freq, y=log_ccdf)) +
  labs(title='CCDF of the Frequency Distribution of Words',
       x='Nk, Log10 Frequency',
       y='Log10 CCDF') +
  geom_point()

# ---- Problem 2 ---------------------------------------------------------------

scaling_boundary = 10^7.5

# ---- Lower Range ----
vocab_lower_range = vocab_freqs %>%
  filter(freq < scaling_boundary)

# Plot the CDF
ggplot(vocab_lower_range,
       aes(x=log_freq, y=log_ccdf)) +
  labs(title='Vocab CCDF Lower Range',
       x='Nk, Log10 Frequency',
       y='Log10 CCDF') +
  geom_point() +
  geom_smooth(method='lm')

lower_range_regression = lm(log_freq ~ log_ccdf, vocab_lower_range)
confint(lower_range_regression)

# ---- Upper Range ----
vocab_upper_range = vocab_freqs %>%
  filter(freq > scaling_boundary)

# Plot the CDF
ggplot(vocab_upper_range,
       aes(x=log_freq, y=log_ccdf)) +
  labs(title='Vocab CCDF Upper Range',
       x='Nk, Log10 Frequency',
       y='Log10 CCDF') +
  geom_point() +
  geom_smooth(method='lm')

upper_range_regression = lm(log_freq ~ log_ccdf, vocab_upper_range)
confint(upper_range_regression)
