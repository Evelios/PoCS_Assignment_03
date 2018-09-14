# Thomas Waters
# Assignment 2
# Problem 6, 7, and 8
library(tidyverse)
library(ggplot2)

google_vocab_filename = 'vocab_k_nk.txt'

# ---- Problem 6 ---------------------------------------------------------------

vocab_freqs = read.table(google_vocab_filename, sep=' ',
                         col.names=c('k', 'nk'))

ggplot(vocab_freqs,
       aes(x=k, y=nk)) +
  labs(title='Frequency Distribution of Words',
       x='Nk, Frequency',
       y='k, Number of Words') +
  geom_point()

# ---- Calculate it in log10 space ----

vocab_freqs_logspace = vocab_freqs %>%
  mutate(k = log10(k)) %>%
  mutate(nk = log10(nk))

ggplot(vocab_freqs_logspace,
       aes(x=k, y=nk)) +
  labs(title='Frequency Distribution of Words',
       x='log10 Nk, Frequency',
       y='log10 k, Number of Words') +
  geom_point()

# ---- Problem 7 ---------------------------------------------------------------

power_law_cutoff = 4.25
power_law_vocab_freqs_logspace = vocab_freqs_logspace %>%
  filter(k < power_law_cutoff)

lm(nk ~ k, power_law_vocab_freqs_logspace)

ggplot(power_law_vocab_freqs_logspace,
       aes(x=k, y=nk)) +
  labs(title='Frequency Distribution of Words',
       x='log10 Nk, Frequency',
       y='log10 k, Number of Words') +
  geom_point() +
  geom_smooth(method='lm')

# ---- Problem 8 ---------------------------------------------------------------

mean(vocab_freqs$nk)
sd(vocab_freqs$nk)
