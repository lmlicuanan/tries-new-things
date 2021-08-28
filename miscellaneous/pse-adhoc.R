library(pseR)
library(tidyquant)
library(tidyverse)
library(ggcorrplot)
library(GGally)

stock.dt <- pse_get(c("AEV", "AP", "AGI", "AC", "ALI", "BPI",
                      "BDO", "BLOOM", "DMC", "EMP", "FGEN",
                      "GLO", "GTCAP", "ICT", "JGS", "JFC",
                      "LTG", "MER", "MEG", "MPI", "MBT", "TEL",
                      "PGOLD", "RLC", "RRHI", "SMC", "SECB",
                      "SM", "SMPH", "URC", 
                      # wildcards
                      "CEB"))

stock.dt %>%
  ggplot(aes(x = as.Date(date), y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "PSE Stocks Daily Past 1 Year", y = "Closing Price", x = "") + 
  facet_wrap(~code, scales = "free") + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %d") + 
  theme_tq()

stock.dt %>%
  group_by(code) %>% 
  arrange(desc(date)) %>% 
  ungroup() %>% 
  select(code, date, perc_change) %>% 
  spread(key = code, value = perc_change) %>% 
  select(-date) %>% ggcorr(label = TRUE)
