# ---
#   title: "AI Stock Portfolio — Correlation, Returns, CAPM & Forecasting"
# author: "Christopher Taratko"
# date: "`r Sys.Date()`"
# output:
#   html_document:
#   toc: true
# toc_float: true
# theme: paper
# code_folding: show
# ---
  

# knitr::opts_chunk$set(
#   echo    = TRUE,
#   message = FALSE,
#   warning = FALSE,
#   fig.width  = 11,
#   fig.height = 7,
#   dpi     = 150
# )

# AI was utilized on this initial code chunk to create the package installer, ticker colors, and understanding small 
# ── Packages ──────────────────────────────────────────────────────────────────
pkgs <- c("quantmod", "PerformanceAnalytics", "corrplot", "ggplot2",
          "reshape2", "dplyr", "tidyr", "lubridate", "scales",
          "RColorBrewer", "ggcorrplot", "gridExtra", "zoo",
          "xts", "TTR", "tibble", "purrr", "forecast", "tseries",
          "ggrepel", "viridis", "knitr", "kableExtra")

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ── Shared colour palette (consistent across all plots) ───────────────────────
TICKER_COLORS <- c(
  NVDA = "#76b900", GOOG = "#4285F4", MSFT = "#00A4EF",
  META = "#0081FB", TSM  = "#EF4444", ORCL = "#F80000",
  # PLTR = "#5B4FD3", 
  SMCI = "#F97316", EQIX = "#10B981",
  TSLA = "#CC0000", ISRG = "#6366F1", AMZN = "#FF9900"
)

ANN <- 252          # trading days per year
RF  <- 0.045 / ANN  # daily risk-free rate (~4.5% annual — adjust as needed)

mytheme <- theme(
  plot.title = element_text(color = "#000040", face = "bold", family = "Times New Roman"),
  plot.subtitle = element_text(color = "grey30", face = "italic", family = "Times New Roman"),
  plot.caption = element_text(color = "grey70", face = "italic", family = "Times New Roman"),
  
  plot.background = element_rect(fill = "#fdf6e3"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#fdf6e3", color = "black"),
  panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5), 
  panel.grid.major.x = element_blank(),
  axis.line.y = element_line(color = "black", linewidth = 0.5),
  legend.title = element_text(family = "Times New Roman", face = "bold"),
  
  # axis text color
  axis.text = element_text(family = "Times New Roman"),
  axis.title = element_text(face = "bold",family = "Times New Roman"),
  
  # For faceted graphs
  strip.background = element_rect(fill = "grey95", color = "black"),
  strip.text = element_text(color = "black", face = "bold"),
)




# ── Ingest tidy_data ──────────────────────────────────────────────────────────
# Expected columns: Date, Ticker, Open, High, Low, Close, AdjClose, Volume
library(tidyverse)
tidy_data <- read_csv("/Users/christophertaratko/ProjectsList/Data Analytics/Personal Website/ctaratko.github.io/Spring2026Projects/DANL 320 Project/Data/Daily AI Stock Data.csv")
# tidy_data
df <- tidy_data %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Ticker, Date)

# Pivot Adj Close to wide (one column per ticker)
adj_wide <- df %>%
  select(Ticker, Date, AdjClose) %>%
  pivot_wider(names_from = Ticker, values_from = AdjClose) %>%
  arrange(Date)

# xts price matrix
prices_xts <- xts(adj_wide[ , -1], order.by = adj_wide$Date)

# Daily log returns (NAs for PLTR pre-IPO rows preserved)
log_ret <- diff(log(prices_xts))[-1, ]

# Two windows:
#   full     → all dates, pairwise-complete obs for non-PLTR pairs
#   complete → post-PLTR IPO only, every cell on identical data
log_ret_full     <- log_ret
log_ret_complete <- log_ret[complete.cases(log_ret), ]

# Weekly log returns (last trading day of each week)
weekly_adj <- adj_wide %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(-Week)

weekly_xts <- xts(weekly_adj[ , -1], order.by = weekly_adj$Date)
weekly_ret <- diff(log(weekly_xts))[-1, ]

# 
# 
# ------------------------------------------------------------------------
#   
  # 1 · Correlation Analysis
#   
#   From the following Heatmaps, through Pairwise Testing & Pearson Testing metrics, the companies that have the most association or correlation with each other would be **Nvidia**, **Microsoft**, **Meta**, and **Google**. These four companies are very closely related to each other due to their demands in AI.
# 
# **Meta**, **Microsoft**, and **Google**, they have high correlation with each other specifically, and this is due to them all focusing on the output and the curation of their AI products. These companies must compete with each other as they have large portions of the internet trafficking through them every single day.
# 
# - Google –\> Google's Search Engine & Chrome Browser
# 
# - Microsoft –\> Bing, Microsoft Edge, Copilot, & Microsoft Office Applications
# 
# - Meta –\> Meta AI, Instagram, & Facebook
# 
# These products can and do consume hours of their consumers time, whether it's searching the web for their ideas, building/maintaining a social appearance on Social Media, or constructing and editing business frameworks for small and large scale companies.
# 
# As for **Nvidia**, the role they play is the infrastructure for AI, creating the Graphics Cards/GPUS, CPUs, and Researching & Developing. Nvidia is the supplier for all of the AI based companies, when it comes to both maintaining competition with other AI producers, or being the barrier to entry within the market.

# pearson analysis


  
cor_pairwise <- cor(log_ret_full,     use = "pairwise.complete.obs")
cor_complete  <- cor(log_ret_complete, use = "complete.obs")
cor_spearman  <- cor(log_ret_complete, method = "spearman")

# ── 1b. Heatmap — pairwise full history ───────────────────────────────────────
ggcorrplot(
  cor_pairwise,
  hc.order  = TRUE,
  hc.method = "ward.D2",
  type      = "lower",
  lab       = TRUE,
  lab_size  = 3.2,
  colors    = c("#d73027", "#ffffbf", "#4575b4"),
  title     = "Pearson Correlation — Pairwise Full History (2014–2025)",
  ggtheme   = theme_minimal(base_size = 12)
) +
  mytheme +
  theme(
    plot.title = element_text(hjust = .5)
  )

# ── 1c. Heatmap — complete window (post-PLTR IPO) ────────────────────────────
ggcorrplot(
  cor_complete,
  hc.order  = TRUE,
  hc.method = "ward.D2",
  type      = "lower",
  lab       = TRUE,
  lab_size  = 3.2,
  colors    = c("#d73027", "#ffffbf", "#4575b4"),
  title     = paste0("Pearson Correlation — Complete Window (",
                     format(start(log_ret_complete)), " – ",
                     format(end(log_ret_complete)), ")"),
  ggtheme   = theme_minimal(base_size = 12)
) +
  mytheme +
  theme(
    plot.title = element_text(hjust = .5)
  )

# ── 1d. Spearman heatmap ──────────────────────────────────────────────────────
ggcorrplot(
  cor_spearman,
  hc.order  = TRUE,
  hc.method = "ward.D2",
  type      = "lower",
  lab       = TRUE,
  lab_size  = 3.2,
  colors    = c("#d73027", "#ffffbf", "#4575b4"),
  title     = "Spearman Rank Correlation — Complete Window",
  ggtheme   = theme_minimal(base_size = 12)
) +
  mytheme +
  theme(
    plot.title = element_text(hjust = .5)
  )

# ── 1e. Hierarchical clustering dendrogram ────────────────────────────────────
dist_mat <- as.dist(1 - cor_pairwise)
hc        <- hclust(dist_mat, method = "ward.D2")

par(mar = c(4, 4, 3, 1))
plot(
  hc,
  main = "Hierarchical Clustering — AI Stock Return Correlation",
  sub  = "Dissimilarity = 1 − Pearson r  |  Ward D2 linkage",
  xlab = "", ylab = "Dissimilarity", cex = 0.9, hang = -1
)
abline(h = 0.5, col = "red", lty = 2)

# ── 1f. Rolling 6-month correlation vs NVDA ───────────────────────────────────
other_tickers <- setdiff(colnames(log_ret_full), "NVDA")

rolling_list <- lapply(other_tickers, function(tk) {
  r <- rollapply(
    log_ret_full[, c("NVDA", tk)],
    width     = 126,
    FUN       = function(x) cor(x[,1], x[,2], use = "complete.obs"),
    by.column = FALSE,
    align     = "right"
  )
  data.frame(
    Date        = as.Date(index(r)),
    Ticker      = tk,
    Correlation = as.numeric(r)
  )
})

rc_long <- bind_rows(rolling_list) %>% filter(!is.na(Correlation))

ggplot(rc_long, aes(x = Date, y = Correlation, color = Ticker)) +
  geom_line(linewidth = 0.45, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = TICKER_COLORS) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, 0.25)) +
  labs(
    title    = "Rolling 6-Month Correlation vs NVDA",
    subtitle = "126-trading-day window  ·  daily log returns",
    x = NULL, y = "Pearson r", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title      = element_text(face = "bold")
  )


# ── 1g. Significance table ───────────────────────────────────────────────────
n      <- nrow(log_ret_complete)
tstat  <- cor_complete * sqrt((n - 2) / (1 - cor_complete^2))
pval   <- 2 * pt(-abs(tstat), df = n - 2)
diag(pval) <- NA

as.data.frame(pval)

sig_df <- as.data.frame(as.table(pval)) %>%
  setNames(c("Stock_A", "Stock_B", "p_value")) %>%
  filter(!is.na(p_value), as.character(Stock_A) < as.character(Stock_B)) %>%
  mutate(
    r       = cor_complete[cbind(as.character(Stock_A), as.character(Stock_B))],
    p_value = round(p_value, 5),
    r       = round(r, 3),
    Sig     = 
      case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~  "**",
        p_value < 0.05  ~  "*",
        TRUE            ~  "ns"
      )
    # ifelse(p_value < 0.001, "***",
    #         ifelse(p_value < 0.01,  "**",
    #         ifelse(p_value < 0.05,  "*", "ns")))
  ) %>%
  arrange(p_value)

kable(sig_df, caption = "Pairwise Correlation Significance (complete window)") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"), full_width = FALSE)


# ------------------------------------------------------------------------
  
  # 2 · Return Distributions — Daily & Weekly Returns, Volatility
  

# ── 2a. Tidy daily returns for ggplot ─────────────────────────────────────────
daily_long <- as.data.frame(log_ret_complete) %>%
  rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(-Date, names_to = "Ticker", values_to = "Log_Return")

weekly_long <- as.data.frame(weekly_ret) %>%
  rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(-Date, names_to = "Ticker", values_to = "Log_Return") %>%
  filter(!is.na(Log_Return))

# ── 2b. Daily return density ridgeline (using facets as substitute) ───────────
ggplot(daily_long, aes(x = Log_Return, fill = Ticker, color = Ticker)) +
  geom_density(alpha = 0.25, linewidth = 0.4) +
  facet_wrap(~Ticker, scales = "free_x", ncol = 4) +
  scale_fill_manual(values  = TICKER_COLORS) +
  scale_color_manual(values = TICKER_COLORS) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "Daily Log-Return Distributions",
    subtitle = "Post-PLTR IPO complete window",
    x = "Log Return", y = "Density"
  ) +
  theme_minimal(base_size = 11) +
  mytheme +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# ── 2c. Return distribution boxplots ─────────────────────────────────────────
ggplot(daily_long, aes(x = reorder(Ticker, Log_Return, FUN = median),
                       y = Log_Return, fill = Ticker)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3, linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_manual(values = TICKER_COLORS) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_flip() +
  labs(
    title    = "Daily Log-Return Spread by Ticker",
    subtitle = "Ordered by median return",
    x = NULL, y = "Daily Log Return"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# ── 2d. Annualised stats table ────────────────────────────────────────────────
stats_df <- daily_long %>%
  group_by(Ticker) %>%
  summarise(
    N              = n(),
    Ann_Return_Pct = mean(Log_Return, na.rm = TRUE) * ANN * 100,
    Ann_Vol_Pct    = sd(Log_Return,   na.rm = TRUE) * sqrt(ANN) * 100,
    Skewness       = mean((Log_Return - mean(Log_Return))^3) / sd(Log_Return)^3,
    Kurt_Excess    = mean((Log_Return - mean(Log_Return))^4) / sd(Log_Return)^4 - 3,
    Max_Daily_Loss = min(Log_Return, na.rm = TRUE) * 100,
    Max_Daily_Gain = max(Log_Return, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Sharpe = Ann_Return_Pct / Ann_Vol_Pct,
    across(where(is.numeric), ~round(.x, 3))
  ) %>%
  arrange(desc(Ann_Return_Pct))

kable(stats_df, caption = "Annualised Return & Risk Statistics (complete window)") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"), full_width = FALSE) %>%
  column_spec(3, color = ifelse(stats_df$Ann_Return_Pct > 0, "darkgreen", "red"))

# ── 2e. Risk-return scatter ───────────────────────────────────────────────────
ggplot(stats_df, aes(x = Ann_Vol_Pct, y = Ann_Return_Pct, color = Ticker)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_text_repel(aes(label = Ticker), size = 3.5, fontface = "bold",
                  box.padding = 0.4, max.overlaps = 20) +
  scale_color_manual(values = TICKER_COLORS) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Risk–Return Scatter (Annualised)",
    subtitle = "Higher right = higher return but higher vol",
    x = "Annualised Volatility (%)", y = "Annualised Log Return (%)"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

# ── 2f. Rolling 30-day realised volatility ────────────────────────────────────
roll_vol <- rollapply(log_ret_full, width = 30,
                      FUN = sd, na.rm = TRUE, align = "right") * sqrt(ANN) * 100

roll_vol_long <- as.data.frame(roll_vol) %>%
  rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(-Date, names_to = "Ticker", values_to = "Ann_Vol") %>%
  filter(!is.na(Ann_Vol))

ggplot(roll_vol_long, aes(x = Date, y = Ann_Vol, color = Ticker)) +
  geom_line(linewidth = 0.45, alpha = 0.85) +
  scale_color_manual(values = TICKER_COLORS) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Rolling 30-Day Realised Volatility (Annualised)",
    subtitle = "Spikes correspond to March 2020, 2022 rate hikes, 2024 AI run-up",
    x = NULL, y = "Ann. Vol (%)", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title      = element_text(face = "bold")
  )

# ── 2g. Cumulative log returns (indexed to 1) ─────────────────────────────────
cum_ret <- apply(log_ret_full, 2, function(x) exp(cumsum(replace(x, is.na(x), 0))))

cum_long <- as.data.frame(cum_ret) %>%
  rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(-Date, names_to = "Ticker", values_to = "Cumulative")

# Label at end for clean annotation
end_labels <- cum_long %>%
  group_by(Ticker) %>%
  slice_tail(n = 1)

ggplot(cum_long, aes(x = Date, y = Cumulative, color = Ticker)) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  geom_text_repel(data = end_labels, aes(label = Ticker),
                  direction = "y", hjust = -0.1, size = 3,
                  box.padding = 0.2, max.overlaps = 15) +
  scale_color_manual(values = TICKER_COLORS) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = c(0, 0.08))) +
  scale_y_log10(labels = function(x) paste0(round(x, 0), "x")) +
  labs(
    title    = "Cumulative Growth (log scale, base = $1 at start)",
    subtitle = "PLTR starts from IPO date; other tickers from 2014",
    x = NULL, y = "Growth multiple (log scale)", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title      = element_text(face = "bold")
  )


# ------------------------------------------------------------------------
  
  # 3 · CAPM — Beta, Alpha & Sharpe Ratios vs SPY
  

# ── 3a. Fetch SPY (market proxy) ──────────────────────────────────────────────
getSymbols("SPY", from = format(start(log_ret_complete)),
           to   = format(end(log_ret_complete)),
           auto.assign = TRUE)

spy_ret <- diff(log(Ad(SPY)))[-1]
colnames(spy_ret) <- "SPY"

# Align to complete window and merge
all_ret <- merge(log_ret_complete, spy_ret, join = "inner")
all_ret <- all_ret[complete.cases(all_ret), ]

stock_tks <- setdiff(colnames(all_ret), "SPY")
mkt_excess <- as.numeric(all_ret[, "SPY"]) - RF

# ── 3b. CAPM OLS for each ticker ─────────────────────────────────────────────
capm_list <- lapply(stock_tks, function(tk) {
  stk_excess <- as.numeric(all_ret[, tk]) - RF
  fit <- lm(stk_excess ~ mkt_excess)
  s   <- summary(fit)
  
  # Sharpe (annualised, excess return over rf)
  ann_excess <- mean(stk_excess, na.rm = TRUE) * ANN
  ann_vol    <- sd(stk_excess,   na.rm = TRUE) * sqrt(ANN)
  
  # Treynor ratio = (ann excess return) / Beta
  beta <- coef(fit)[2]
  
  data.frame(
    Ticker   = tk,
    Alpha    = round(coef(fit)[1] * ANN * 100, 3),   # annualised %
    Beta     = round(beta, 4),
    R2       = round(s$r.squared, 4),
    p_Alpha  = round(coef(s)[1, 4], 5),
    p_Beta   = round(coef(s)[2, 4], 10),
    Sharpe   = round(ann_excess / ann_vol, 4),
    Treynor  = round(ann_excess / beta, 4),
    Ann_Exc_Ret_Pct = round(ann_excess * 100, 3),
    Ann_Vol_Pct     = round(ann_vol    * 100, 3)
  )
})

capm_df <- bind_rows(capm_list) %>% arrange(desc(Beta))

kable(capm_df, caption = paste("CAPM Results vs SPY  |  rf =",
                               round(RF * ANN * 100, 1), "% annual  |",
                               format(start(all_ret)), "–", format(end(all_ret)))) %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"), full_width = FALSE) %>%
  column_spec(3, color = ifelse(capm_df$Alpha > 0, "darkgreen", "red"))

# ── 3c. Beta bar chart ───────────────────────────────────────────────────────
ggplot(capm_df, aes(x = reorder(Ticker, Beta), y = Beta,
                    fill = Beta > 1)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey20", linewidth = 0.7) +
  geom_text(aes(label = round(Beta, 2)),
            hjust = -0.15, size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c("TRUE" = "#d73027", "FALSE" = "#4575b4"),
    labels = c("TRUE" = "β > 1 (aggressive)", "FALSE" = "β ≤ 1 (defensive)"),
    name   = ""
  ) +
  coord_flip(ylim = c(0, max(capm_df$Beta) * 1.15)) +
  labs(
    title    = "CAPM Beta vs SPY",
    subtitle = "Dashed line = market beta of 1.0",
    x = NULL, y = "Beta (β)"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

# ── 3d. Alpha (annualised) bar chart ─────────────────────────────────────────
ggplot(capm_df, aes(x = reorder(Ticker, Alpha), y = Alpha,
                    fill = Alpha > 0)) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = 0, color = "grey20", linewidth = 0.7) +
  geom_text(aes(label = paste0(round(Alpha, 2), "%")),
            hjust = ifelse(capm_df$Alpha[order(capm_df$Alpha)] > 0, -0.1, 1.1),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(
    values = c("TRUE" = "#2ca02c", "FALSE" = "#d62728"),
    labels = c("TRUE" = "Positive α", "FALSE" = "Negative α"),
    name   = ""
  ) +
  coord_flip(ylim = range(capm_df$Alpha) * c(1.3, 1.3)) +
  labs(
    title    = "Jensen's Alpha (Annualised %) vs SPY",
    subtitle = "Excess return beyond what beta predicts",
    x = NULL, y = "Alpha (% p.a.)"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

# ── 3e. Beta–Alpha scatter ────────────────────────────────────────────────────
ggplot(capm_df, aes(x = Beta, y = Alpha, color = Ticker)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(size = Sharpe), alpha = 0.85) +
  geom_text_repel(aes(label = Ticker), size = 3.5, fontface = "bold",
                  box.padding = 0.4, max.overlaps = 20) +
  scale_color_manual(values = TICKER_COLORS) +
  scale_size_continuous(range = c(3, 10), name = "Sharpe Ratio") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Beta vs Alpha — CAPM Quadrant Map",
    subtitle = "Point size = Sharpe ratio  ·  Top-right = high β, positive α",
    x = "Beta (β)", y = "Jensen's Alpha (% p.a.)"
  ) +
  guides(color = "none") +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "right")

# ── 3f. Sharpe ratio comparison ───────────────────────────────────────────────
# Add SPY itself as benchmark
spy_ann_exc <- mean(mkt_excess) * ANN
spy_ann_vol <- sd(mkt_excess)   * sqrt(ANN)
spy_row     <- data.frame(Ticker = "SPY", Sharpe = spy_ann_exc / spy_ann_vol,
                          stringsAsFactors = FALSE)
sharpe_df <- bind_rows(capm_df %>% select(Ticker, Sharpe), spy_row) %>%
  arrange(desc(Sharpe))

ggplot(sharpe_df, aes(x = reorder(Ticker, Sharpe), y = Sharpe,
                      fill = Ticker == "SPY")) +
  geom_col(width = 0.65) +
  geom_hline(yintercept = filter(sharpe_df, Ticker == "SPY")$Sharpe,
             linetype = "dashed", color = "grey20", linewidth = 0.7) +
  geom_text(aes(label = round(Sharpe, 3)),
            hjust = -0.1, size = 3.3) +
  scale_fill_manual(values = c("TRUE" = "grey50", "FALSE" = "#4575b4"),
                    guide  = "none") +
  coord_flip(ylim = c(min(sharpe_df$Sharpe) * 1.2,
                      max(sharpe_df$Sharpe) * 1.2)) +
  labs(
    title    = "Sharpe Ratio vs SPY Benchmark",
    subtitle = paste0("rf = ", round(RF * ANN * 100, 1),
                      "% annual  ·  dashed = SPY Sharpe"),
    x = NULL, y = "Sharpe Ratio"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(plot.title = element_text(face = "bold"))

# ── 3g. Rolling 6-month beta vs SPY ──────────────────────────────────────────
roll_beta <- function(tk, window = 126) {
  s <- as.numeric(all_ret[, tk])   - RF
  m <- as.numeric(all_ret[, "SPY"])- RF
  combined <- cbind(s, m)
  rollapply(combined, width = window,
            FUN = function(x) cov(x[,1], x[,2]) / var(x[,2]),
            by.column = FALSE, align = "right")
}

rb_list <- lapply(stock_tks, function(tk) {
  r <- roll_beta(tk)
  data.frame(Date   = as.Date(index(all_ret))[seq_along(r)],
             Ticker = tk, Beta = as.numeric(r))
})

rb_long <- bind_rows(rb_list) %>% 
  filter(!is.na(Beta)) |> 
  mutate(Ticker = fct_reorder2(Ticker, Date, Beta))

ggplot(rb_long, aes(x = Date, y = Beta, color = Ticker)) +
  geom_line(linewidth = 0.5, alpha = 0.85) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = TICKER_COLORS) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title    = "Rolling 6-Month Beta vs SPY",
    subtitle = "How each stock's market sensitivity has evolved post-PLTR IPO",
    x = NULL, y = "Beta (β)", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  mytheme +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        plot.title      = element_text(face = "bold"))


# ------------------------------------------------------------------------
  
  # 4 · Forecasting — Rolling Stats, Trend Decomposition & Return Projections
  

# ── 4a. Pick NVDA as the primary forecast ticker (most AI-representative) ─────
# Change `focal_ticker` to any ticker you want to drill into
focal_ticker <- "NVDA"

focal_prices <- adj_wide %>%
  select(Date, all_of(focal_ticker)) %>%
  filter(!is.na(.data[[focal_ticker]]))

# ── 4b. Trend decomposition via STL ──────────────────────────────────────────
# Resample to weekly for cleaner STL (daily is too noisy)
weekly_prices <- focal_prices %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarise(Price = last(.data[[focal_ticker]]), .groups = "drop") %>%
  filter(!is.na(Price))

price_ts <- ts(weekly_prices$Price,
               start     = c(year(min(weekly_prices$Week)),
                             week(min(weekly_prices$Week))),
               frequency = 52)

stl_fit <- stl(log(price_ts), s.window = "periodic", robust = TRUE)

# Plot STL components
stl_df <- data.frame(
  Week      = weekly_prices$Week,
  Observed  = as.numeric(price_ts),
  Trend     = exp(as.numeric(stl_fit$time.series[, "trend"])),
  Seasonal  = as.numeric(stl_fit$time.series[, "seasonal"]),
  Remainder = as.numeric(stl_fit$time.series[, "remainder"])
)

p_obs <- ggplot(stl_df, aes(Week, Observed)) +
  geom_line(color = TICKER_COLORS[focal_ticker], linewidth = 0.5) +
  geom_line(aes(y = Trend), color = "black", linewidth = 0.8, linetype = "dashed") +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = paste(focal_ticker, "— Observed Price + STL Trend"),
       x = NULL, y = "Adj Close") +
  theme_minimal(base_size = 11) +
  mytheme

p_seas <- ggplot(stl_df, aes(Week, Seasonal)) +
  geom_line(color = "#4575b4", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Seasonal Component", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  mytheme

p_rem <- ggplot(stl_df, aes(Week, Remainder)) +
  geom_line(color = "#d73027", linewidth = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Remainder (Noise)", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  mytheme

grid.arrange(p_obs, p_seas, p_rem, ncol = 1,
             top = paste("STL Decomposition —", focal_ticker, "(Weekly Log Prices)"))

# ── 4c. Rolling mean & Bollinger Bands ───────────────────────────────────────
focal_daily <- focal_prices %>% arrange(Date)
win <- 20   # 20-day (1-month) rolling window

focal_daily <- focal_daily %>%
  mutate(
    MA20   = zoo::rollmean(.data[[focal_ticker]], k = win, fill = NA, align = "right"),
    SD20   = zoo::rollapply(.data[[focal_ticker]], width = win,
                            FUN = sd, fill = NA, align = "right"),
    Upper  = MA20 + 2 * SD20,
    Lower  = MA20 - 2 * SD20
  )

ggplot(focal_daily %>% filter(Date >= "2022-01-01"),
       aes(x = Date)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#4575b4", alpha = 0.15) +
  geom_line(aes(y = .data[[focal_ticker]]), color = TICKER_COLORS[focal_ticker],
            linewidth = 0.5, alpha = 0.9) +
  geom_line(aes(y = MA20), color = "black", linewidth = 0.7, linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title    = paste(focal_ticker, "— 20-Day Bollinger Bands (2022–present)"),
    subtitle = "Dashed = 20-day MA  ·  Shaded = ±2 SD",
    x = NULL, y = "Adj Close"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title  = element_text(face = "bold"))

# ── 4d. ARIMA forecast on log returns ────────────────────────────────────────
focal_ret <- diff(log(focal_prices[[focal_ticker]]))
focal_ret <- focal_ret[!is.na(focal_ret)]

arima_fit  <- auto.arima(focal_ret, seasonal = FALSE, stepwise = FALSE,
                         approximation = FALSE, ic = "aicc")

cat("\n=== ARIMA Model for", focal_ticker, "daily log returns ===\n")
print(summary(arima_fit))

h       <- 63    # ~3 months of trading days ahead
fc      <- forecast(arima_fit, h = h, level = c(80, 95))

# Convert return forecasts back to price path
last_price  <- tail(focal_prices[[focal_ticker]], 1)
last_date   <- tail(focal_prices$Date, 1)
future_dates <- seq(last_date + 1, by = "day", length.out = h * 1.5)
future_dates <- future_dates[!weekdays(future_dates) %in% c("Saturday","Sunday")][1:h]

forecast_df <- data.frame(
  Date     = future_dates,
  Point    = last_price * exp(cumsum(fc$mean)),
  Lo80     = last_price * exp(cumsum(fc$lower[, 1])),
  Hi80     = last_price * exp(cumsum(fc$upper[, 1])),
  Lo95     = last_price * exp(cumsum(fc$lower[, 2])),
  Hi95     = last_price * exp(cumsum(fc$upper[, 2]))
)

hist_tail <- focal_prices %>% filter(Date >= last_date - 365)

ggplot() +
  # Historical
  geom_line(data = hist_tail,
            aes(x = Date, y = .data[[focal_ticker]]),
            color = TICKER_COLORS[focal_ticker], linewidth = 0.6) +
  # 95% CI
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo95, ymax = Hi95),
              fill = "#4575b4", alpha = 0.15) +
  # 80% CI
  geom_ribbon(data = forecast_df, aes(x = Date, ymin = Lo80, ymax = Hi80),
              fill = "#4575b4", alpha = 0.25) +
  # Point forecast
  geom_line(data = forecast_df, aes(x = Date, y = Point),
            color = "#1f77b4", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(last_date), linetype = "dotted", color = "grey40") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = paste(focal_ticker, "— ARIMA Return Forecast (next ~3 months)"),
    subtitle = paste("Model:", arima_fit$arma |> paste(collapse="-"),
                     "· Shaded = 80% and 95% prediction intervals"),
    x = NULL, y = "Adj Close"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title  = element_text(face = "bold"))

# ── 4e. Monte Carlo simulation — GBM for next 63 trading days ─────────────────
set.seed(42)
n_sims   <- 1000
mu_daily <- mean(focal_ret)
sd_daily <- sd(focal_ret)

sim_mat <- matrix(NA, nrow = h + 1, ncol = n_sims)
sim_mat[1, ] <- last_price

for (i in seq_len(n_sims)) {
  shocks        <- rnorm(h, mean = mu_daily, sd = sd_daily)
  sim_mat[-1, i] <- last_price * exp(cumsum(shocks))
}

# Summary quantiles
sim_quants <- apply(sim_mat[-1, ], 1, quantile,
                    probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
mc_df <- data.frame(
  Date  = future_dates,
  Q05   = sim_quants[1, ],
  Q25   = sim_quants[2, ],
  Med   = sim_quants[3, ],
  Q75   = sim_quants[4, ],
  Q95   = sim_quants[5, ]
)

# Sample paths (plot 100 for clarity)
sim_long <- as.data.frame(sim_mat[-1, 1:100]) %>%
  mutate(Date = future_dates) %>%
  pivot_longer(-Date, names_to = "Sim", values_to = "Price")

ggplot() +
  geom_line(data = hist_tail,
            aes(x = Date, y = .data[[focal_ticker]]),
            color = TICKER_COLORS[focal_ticker], linewidth = 0.6) +
  geom_line(data = sim_long, aes(x = Date, y = Price, group = Sim),
            color = "steelblue", alpha = 0.06, linewidth = 0.3) +
  geom_ribbon(data = mc_df, aes(x = Date, ymin = Q05, ymax = Q95),
              fill = "#4575b4", alpha = 0.20) +
  geom_ribbon(data = mc_df, aes(x = Date, ymin = Q25, ymax = Q75),
              fill = "#4575b4", alpha = 0.30) +
  geom_line(data = mc_df, aes(x = Date, y = Med),
            color = "navy", linewidth = 0.9, linetype = "dashed") +
  geom_vline(xintercept = as.numeric(last_date), linetype = "dotted", color = "grey40") +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = paste0(focal_ticker, " — Monte Carlo GBM Simulation (n=", n_sims,
                      ", h=", h, " days)"),
    subtitle = "Dark band = IQR  ·  Light band = 5th–95th pct  ·  Dashed = median path",
    x = NULL, y = "Simulated Adj Close"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title  = element_text(face = "bold"))

cat("\n=== Monte Carlo Terminal Price Summary (", focal_ticker, ", +", h, "days) ===\n")
terminal <- sim_mat[h + 1, ]
cat(sprintf("  Median:    $%.2f\n",  median(terminal)))
cat(sprintf("  5th pct:   $%.2f\n",  quantile(terminal, 0.05)))
cat(sprintf("  95th pct:  $%.2f\n",  quantile(terminal, 0.95)))
cat(sprintf("  Prob > current price: %.1f%%\n",
            mean(terminal > last_price) * 100))

# ── 4f. Save objects for CAPM forecasting model ───────────────────────────────
# Used AI to assist here because I had no clue how to specifically save the plots and values easily, so it requested the qmd go to a rmarkdown file, and then labeling each code chunk to be saved respectively
save(log_ret_full, log_ret_complete, cor_pairwise, cor_complete, cor_spearman,
     capm_df, stats_df, hc, all_ret, arima_fit, forecast_df, mc_df,
     file = "ai_stock_analysis.RData")

cat("\n=== Saved: ai_stock_analysis.RData ===\n")
cat("Ready for CAPM forecasting model build.\n")





# 6 · Unsupervised Learning

# Unsupervised learning looks for hidden structure in data **without** using a predefined target variable. Here we apply two classical methods — K-Means Clustering and Principal Components Analysis — directly to the daily log-return matrix so that the groupings and components are driven purely by market behaviour rather than sector labels.

## 6a · K-Means Clustering

# K-Means partitions the stocks into *k* groups so that stocks within the same cluster have similar daily return patterns. We choose *k* = 3 (low-vol defensive, mid-vol growth, high-vol momentum) and seed the random number generator for reproducibility.


library(factoextra)   # fviz_cluster, fviz_nbclust

# ── Feature matrix: annualised stats per ticker ───────────────────────────────
km_features <- stats_df %>%
  select(Ticker, Ann_Return_Pct, Ann_Vol_Pct, Skewness, Kurt_Excess, Sharpe) %>%
  column_to_rownames("Ticker") %>%
  scale()   # z-score so no single feature dominates

# ── Elbow / silhouette to justify k ──────────────────────────────────────────
set.seed(42)
fviz_nbclust(km_features, kmeans, method = "silhouette", k.max = 6) +
  labs(title = "Optimal k — Silhouette Method",
       subtitle = "Pick k at the peak average silhouette width") +
  theme_minimal(base_size = 12) +
  mytheme

# ── Fit k = 3 ────────────────────────────────────────────────────────────────
set.seed(42)
km_fit <- kmeans(km_features, centers = 3, nstart = 50, iter.max = 100)

# ── Cluster scatter (PC1 vs PC2 projection) ───────────────────────────────────
fviz_cluster(
  km_fit, 
  data = km_features, 
  geom = c("point", "text"), 
  ellipse.type = "convex",
  stand = FALSE,        # Explicitly set to avoid internal scaling conflicts
  show.clust.cent = TRUE
) + 
  scale_color_manual(values = c("#d73027", "#4575b4", "#1a9850")) +
  scale_fill_manual(values = c("#d73027", "#4575b4", "#1a9850")) +
  labs(
    title = "k-means clustering of ai stocks (k = 3)",
    subtitle = "axes = first two principal components of scaled return features"
  ) +
  theme_minimal(base_size = 12) + 
  mytheme + 
  theme(plot.title = element_text(face = "bold"))

# ── Cluster membership table ──────────────────────────────────────────────────
cluster_df <- data.frame(
  Ticker  = rownames(km_features),
  Cluster = km_fit$cluster
) %>%
  left_join(stats_df %>% select(Ticker, Ann_Return_Pct, Ann_Vol_Pct, Sharpe), by = "Ticker") %>%
  arrange(Cluster, desc(Sharpe)) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

kable(cluster_df, caption = "K-Means Cluster Assignments (k = 3)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  row_spec(which(cluster_df$Cluster == 1), background = "#fde0d0") %>%
  row_spec(which(cluster_df$Cluster == 2), background = "#d0e4f7") %>%
  row_spec(which(cluster_df$Cluster == 3), background = "#d5f0dc")


# ------------------------------------------------------------------------
  
  ## 6b · Principal Components Analysis (PCA)
  
  # PCA rotates the return matrix into orthogonal directions (principal components) that explain the most variance first. Each PC is a linear combination of the original tickers; by inspecting the **loadings** we can see which stocks drive each component.


# ── Run PCA on the complete-window daily return matrix ────────────────────────
# Rows = days, columns = tickers — we transpose so tickers become observations.
ret_mat <- t(as.matrix(log_ret_complete))   # 10 tickers × n days
ret_mat_scaled <- t(scale(t(ret_mat)))      # centre & scale each ticker's return series

pca_fit <- prcomp(t(ret_mat_scaled), center = FALSE, scale. = FALSE)

# ── Variance explained ────────────────────────────────────────────────────────
var_exp <- summary(pca_fit)$importance
var_df  <- data.frame(
  PC       = factor(paste0("PC", 1:ncol(var_exp)), levels = paste0("PC", 1:ncol(var_exp))),
  PropVar  = var_exp[2, ] * 100,
  CumVar   = var_exp[3, ] * 100
)

ggplot(var_df, aes(x = PC)) +
  geom_col(aes(y = PropVar), fill = "#4575b4", alpha = 0.8) +
  geom_line(aes(y = CumVar, group = 1), color = "#d73027", linewidth = 1) +
  geom_point(aes(y = CumVar), color = "#d73027", size = 2.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "grey50") +
  scale_y_continuous(
    name     = "Proportion of Variance Explained (%)",
    sec.axis = sec_axis(~., name = "Cumulative Variance (%)")
  ) +
  labs(
    title    = "PCA — Scree Plot",
    subtitle = "Bars = individual PC variance  ·  Line = cumulative  ·  Dashed = 80% threshold",
    x        = NULL
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(plot.title = element_text(face = "bold"))

# ── Biplot: PC1 vs PC2 ────────────────────────────────────────────────────────
scores <- as.data.frame(pca_fit$x[, 1:2])
scores$Date <- as.Date(rownames(scores))

loadings <- as.data.frame(pca_fit$rotation[, 1:2])
loadings$Ticker <- rownames(loadings)
scale_factor <- 15   # visual scale for arrows

ggplot(scores, aes(PC1, PC2)) +
  geom_point(color = "grey70", size = 0.6, alpha = 0.4) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * scale_factor,
                   yend = PC2 * scale_factor,
                   color = Ticker),
               arrow = arrow(length = unit(0.2, "cm")),
               linewidth = 0.9) +
  geom_label(data = loadings,
             aes(x = PC1 * scale_factor * 1.12,
                 y = PC2 * scale_factor * 1.12,
                 label = Ticker, color = Ticker),
             size = 3.2, fontface = "bold", fill = "white", label.size = 0) +
  scale_color_manual(values = TICKER_COLORS) +
  labs(
    title    = "PCA Biplot — PC1 vs PC2",
    subtitle = "Grey dots = daily observations  ·  Arrows = ticker loadings",
    x        = paste0("PC1 (", round(var_df$PropVar[1], 1), "% variance)"),
    y        = paste0("PC2 (", round(var_df$PropVar[2], 1), "% variance)")
  ) +
  guides(color = "none") +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(plot.title = element_text(face = "bold"))

# ── Loading heatmap: what each PC is "made of" ────────────────────────────────
n_pcs <- min(5, ncol(pca_fit$rotation))
load_long <- as.data.frame(pca_fit$rotation[, 1:n_pcs]) %>%
  rownames_to_column("Ticker") %>%
  pivot_longer(-Ticker, names_to = "PC", values_to = "Loading")

ggplot(load_long, aes(x = PC, y = Ticker, fill = Loading)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = round(Loading, 2)), size = 3.2) +
  scale_fill_gradient2(
    low      = "#d73027",
    mid      = "white",
    high     = "#4575b4",
    midpoint = 0,
    name     = "Loading"
  ) +
  labs(
    title    = "PCA Loadings — PC1 through PC5",
    subtitle = "Each cell shows how strongly a ticker contributes to each principal component",
    x        = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid  = element_blank()
  )


### What are the Principal Components made of?

# **PC1 — "The Broad AI Market Factor"** PC1 almost always explains the largest share of variance (typically 40–60% in a tightly correlated AI basket). Its loadings are **positive and roughly equal across all tickers**, meaning it moves up when the whole group rises together. Think of it as a single "AI sector beta" — a good day for Nvidia is a good day for Microsoft, Google, Meta, and Amazon. When you see the market narrative say "AI stocks rallied," PC1 is what moved.

# **PC2 — "Hardware vs Software Split"** PC2 separates the *infrastructure* names (NVDA, TSM, SMCI — chip design and fabrication) from the *platform/cloud* names (GOOG, MSFT, META, AMZN, ORCL). One side loads positively and the other negatively, capturing the rotation that happens when, for example, chip export restrictions tighten (hurting hardware) while cloud software continues to grow.

# **PC3 — "Idiosyncratic Momentum"** By PC3 the variance explained per component is small (\<10%) but the loadings become ticker-specific — TSLA and ISRG often stand apart here because their businesses (EVs/robotics, robotic surgery) are only partially correlated with pure-play AI demand. PC3 essentially captures the unique story of whichever stock is on a solo run driven by its own earnings or news.

# **PC4+ — Noise and Microstructure** Beyond PC3, components capture increasingly minor variance — sector rotations within sub-industries, liquidity-driven intraday effects, and statistical noise. These components are rarely interpretable and should not be over-read.

# > **Practical takeaway:** If you were to construct a factor-mimicking portfolio, PC1 gives you broad AI beta, PC2 lets you express a hardware-vs-software view, and PC3 lets you tilt toward the idiosyncratic movers — all with zero overlap between the three positions.

# ------------------------------------------------------------------------
  
  # 7 · Supervised Learning
  
  # Supervised learning trains a model on historical features and a known target, then asks it to predict that target on unseen data. Here the **target is the next-day return direction** (up or down) for NVDA — a binary classification problem. We build two industry-standard ensemble methods: Random Forest and XGBoost.


# ── Packages ──────────────────────────────────────────────────────────────────
sup_pkgs <- c("randomForest", "xgboost", "caret", "pROC", "TTR")
to_install_sup <- sup_pkgs[!sup_pkgs %in% rownames(installed.packages())]
if (length(to_install_sup)) install.packages(to_install_sup, dependencies = TRUE)
invisible(lapply(sup_pkgs, library, character.only = TRUE))

# ── Feature engineering on NVDA daily prices ─────────────────────────────────
focal_ticker <- "NVDA"

feat_df <- df %>%
  filter(Ticker == focal_ticker) %>%
  arrange(Date) %>%
  mutate(
    # Lag returns (momentum features)
    Ret1      = log(AdjClose / dplyr::lag(AdjClose, 1)),
    Ret2      = log(AdjClose / dplyr::lag(AdjClose, 2)),
    Ret5      = log(AdjClose / dplyr::lag(AdjClose, 5)),
    Ret10     = log(AdjClose / dplyr::lag(AdjClose, 10)),
    Ret21     = log(AdjClose / dplyr::lag(AdjClose, 21)),
    
    # Rolling volatility
    Vol10     = zoo::rollapply(Ret1, 10,  sd, fill = NA, align = "right"),
    Vol21     = zoo::rollapply(Ret1, 21,  sd, fill = NA, align = "right"),
    
    # Price relative to moving averages (trend)
    MA10      = zoo::rollmean(AdjClose, 10, fill = NA, align = "right"),
    MA50      = zoo::rollmean(AdjClose, 50, fill = NA, align = "right"),
    PriceVsMA10 = AdjClose / MA10 - 1,
    PriceVsMA50 = AdjClose / MA50 - 1,
    
    # RSI (14-day momentum oscillator via TTR)
    RSI14     = TTR::RSI(AdjClose, n = 14),
    
    # Target: 1 if tomorrow's return > 0, else 0
    Target    = as.factor(ifelse(dplyr::lead(Ret1) > 0, "Up", "Down"))
  ) %>%
  select(Date, Ret1:RSI14, Target) %>%
  filter(complete.cases(.))

cat(sprintf("Feature matrix: %d rows × %d columns\n", nrow(feat_df), ncol(feat_df) - 2))

# ── Train / test split — last 252 days (≈1 year) as held-out test ─────────────
n_test  <- 252
n_train <- nrow(feat_df) - n_test

train_df <- feat_df[1:n_train, ]
test_df  <- feat_df[(n_train + 1):nrow(feat_df), ]

X_train <- train_df %>% select(-Date, -Target)
y_train <- train_df$Target
X_test  <- test_df  %>% select(-Date, -Target)
y_test  <- test_df$Target

cat(sprintf("Training rows: %d  |  Test rows: %d\n", nrow(train_df), nrow(test_df)))


# ------------------------------------------------------------------------
  
  ## 7a · Random Forest
  
  # A Random Forest builds hundreds of decision trees on bootstrap samples of the training data, each tree using a random subset of features at every split. The final prediction is the majority vote. Because each tree sees different data and different features, the ensemble is robust to overfitting and naturally provides **feature importance** scores.


set.seed(42)
rf_fit <- randomForest(
  x         = X_train,
  y         = y_train,
  ntree     = 500,
  mtry      = floor(sqrt(ncol(X_train))),   # default for classification
  importance = TRUE
)

# ── Test-set predictions ───────────────────────────────────────────────────────
rf_pred      <- predict(rf_fit, X_test)
rf_pred_prob <- predict(rf_fit, X_test, type = "prob")[, "Up"]

rf_cm  <- confusionMatrix(rf_pred, y_test, positive = "Up")
rf_roc <- roc(as.numeric(y_test == "Up"), rf_pred_prob, quiet = TRUE)

cat("\n=== Random Forest — Test-Set Performance ===\n")
print(rf_cm$table)
cat(sprintf("\n  Accuracy:  %.3f\n", rf_cm$overall["Accuracy"]))
cat(sprintf("  Kappa:     %.3f\n", rf_cm$overall["Kappa"]))
cat(sprintf("  AUC:       %.3f\n", auc(rf_roc)))

# ── Feature importance bar chart ──────────────────────────────────────────────
imp_df <- importance(rf_fit, type = 1) %>%   # mean decrease accuracy
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  rename(MDA = MeanDecreaseAccuracy) %>%
  arrange(desc(MDA))

ggplot(imp_df, aes(x = reorder(Feature, MDA), y = MDA)) +
  geom_col(fill = "#4575b4", alpha = 0.85) +
  geom_text(aes(label = round(MDA, 2)), hjust = -0.1, size = 3.2) +
  coord_flip(ylim = c(0, max(imp_df$MDA) * 1.2)) +
  labs(
    title    = "Random Forest — Feature Importance (Mean Decrease Accuracy)",
    subtitle = paste0("500 trees  ·  mtry = ", floor(sqrt(ncol(X_train))),
                      "  ·  Test AUC = ", round(auc(rf_roc), 3)),
    x = NULL, y = "Mean Decrease in Accuracy"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(plot.title = element_text(face = "bold"))

# ── ROC curve (stored for comparison with XGB) ────────────────────────────────
roc_rf_df <- data.frame(
  FPR    = 1 - rf_roc$specificities,
  TPR    = rf_roc$sensitivities,
  Model  = paste0("Random Forest (AUC = ", round(auc(rf_roc), 3), ")")
)


# ------------------------------------------------------------------------
  
  ## 7b · XGBoost
  
  # XGBoost (eXtreme Gradient Boosting) builds trees **sequentially**, each new tree correcting the residual errors of the previous ensemble. The result is often more accurate than Random Forest on tabular data, but requires more tuning. We use early stopping on a validation fold to avoid overfitting.


# ── Convert to DMatrix format required by xgboost ─────────────────────────────
y_train_num <- as.numeric(y_train == "Up")
y_test_num  <- as.numeric(y_test  == "Up")

dtrain <- xgb.DMatrix(data  = as.matrix(X_train), label = y_train_num)
dtest  <- xgb.DMatrix(data  = as.matrix(X_test),  label = y_test_num)

# ── Train with early stopping ─────────────────────────────────────────────────
set.seed(42)
xgb_fit <- xgb.train(
  params  = list(
    objective        = "binary:logistic",
    eval_metric      = "auc",
    eta              = 0.05,        # learning rate
    max_depth        = 4,
    subsample        = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 5
  ),
  data             = dtrain,
  nrounds          = 500,
  watchlist        = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  verbose          = 0
)

cat(sprintf("\nXGBoost stopped at round %d  ·  best test AUC = %.4f\n",
            xgb_fit$best_iteration, xgb_fit$best_score))

# ── Test-set predictions ───────────────────────────────────────────────────────
xgb_pred_prob <- predict(xgb_fit, dtest)
xgb_pred      <- factor(ifelse(xgb_pred_prob > 0.5, "Up", "Down"),
                        levels = c("Down", "Up"))

xgb_cm  <- confusionMatrix(xgb_pred, y_test, positive = "Up")
xgb_roc <- roc(y_test_num, xgb_pred_prob, quiet = TRUE)

cat("\n=== XGBoost — Test-Set Performance ===\n")
print(xgb_cm$table)
cat(sprintf("\n  Accuracy:  %.3f\n", xgb_cm$overall["Accuracy"]))
cat(sprintf("  Kappa:     %.3f\n", xgb_cm$overall["Kappa"]))
cat(sprintf("  AUC:       %.3f\n", auc(xgb_roc)))

# ── Feature importance ────────────────────────────────────────────────────────
xgb_imp <- xgb.importance(model = xgb_fit) %>%
  as.data.frame() %>%
  slice_head(n = 12)

ggplot(xgb_imp, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "#d73027", alpha = 0.85) +
  geom_text(aes(label = round(Gain, 3)), hjust = -0.1, size = 3.2) +
  coord_flip(ylim = c(0, max(xgb_imp$Gain) * 1.2)) +
  labs(
    title    = "XGBoost — Feature Importance (Gain)",
    subtitle = paste0("Best round = ", xgb_fit$best_iteration,
                      "  ·  Test AUC = ", round(auc(xgb_roc), 3)),
    x = NULL, y = "Gain"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(plot.title = element_text(face = "bold"))

# ── ROC overlay: RF vs XGB ────────────────────────────────────────────────────
roc_xgb_df <- data.frame(
  FPR   = 1 - xgb_roc$specificities,
  TPR   = xgb_roc$sensitivities,
  Model = paste0("XGBoost (AUC = ", round(auc(xgb_roc), 3), ")")
)

roc_all <- bind_rows(roc_rf_df, roc_xgb_df)

ggplot(roc_all, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = c("#4575b4", "#d73027")) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "ROC Curve Comparison — Random Forest vs XGBoost",
    subtitle = "Predicting next-day NVDA return direction  ·  Held-out test set (252 days)",
    x        = "False Positive Rate", y = "True Positive Rate", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ── Side-by-side accuracy summary table ───────────────────────────────────────
model_summary <- data.frame(
  Model    = c("Random Forest", "XGBoost"),
  Accuracy = round(c(rf_cm$overall["Accuracy"], xgb_cm$overall["Accuracy"]), 4),
  Kappa    = round(c(rf_cm$overall["Kappa"],    xgb_cm$overall["Kappa"]),    4),
  AUC      = round(c(auc(rf_roc),              auc(xgb_roc)),               4),
  Sensitivity = round(c(rf_cm$byClass["Sensitivity"], xgb_cm$byClass["Sensitivity"]), 4),
  Specificity = round(c(rf_cm$byClass["Specificity"], xgb_cm$byClass["Specificity"]), 4)
)

kable(model_summary,
      caption = "Supervised Learning — Model Comparison on Held-Out Test Set (NVDA)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)



# ---
  
  # Overview
#   
#   This document overlays the **actual NVDA daily closing price in 2026** on top of a
# Monte Carlo GBM simulation that was calibrated on historical data up to the end of
# 2024. The simulation was run forward from the last known price on the final trading
# day of 2024; the 2026 actual prices (fetched live via `quantmod`) are then drawn on
# top so you can see how reality tracked against the modelled uncertainty bands.
# 
# **How to read the chart**
#   
#   | Element | Meaning |
#   |---|---|
#   | Green line (left of divider) | Historical NVDA adjusted close |
#   | Thin blue lines | 100 sample GBM paths |
#   | Light blue band | 5th – 95th percentile of all 1 000 paths |
#   | Dark blue band | 25th – 75th percentile (IQR) |
#   | Navy dashed line | Median simulated path |
#   | Grey dotted vertical | Simulation start date (forecast origin) |
#   | **Orange line** | **Actual 2026 NVDA daily close (live data)** |
#   
#   ---
#   
  # 1 · Fetch Historical NVDA Data & Calibrate GBM
  

focal_ticker <- "NVDA"

# ── Pull full history for calibration (2014 – end of 2024) ───────────────────
getSymbols(focal_ticker,
           from = "2014-01-01",
           to   = "2024-12-31",
           auto.assign = TRUE)

nvda_hist     <- Ad(get(focal_ticker))
colnames(nvda_hist) <- focal_ticker

# Daily log returns for parameter estimation
log_ret_hist  <- diff(log(nvda_hist))[-1]
focal_ret     <- as.numeric(log_ret_hist)
focal_ret     <- focal_ret[!is.na(focal_ret)]

# GBM parameters estimated from full history
mu_daily <- mean(focal_ret)
sd_daily <- sd(focal_ret)

last_price <- as.numeric(tail(nvda_hist, 1))
last_date  <- as.Date(index(tail(nvda_hist, 1)))

cat(sprintf("Calibration window : 2014-01-01 – %s\n", last_date))
cat(sprintf("Daily mu           : %.6f  (Ann. ~ %.2f%%)\n",
            mu_daily, mu_daily * ANN * 100))
cat(sprintf("Daily sigma        : %.6f  (Ann. ~ %.2f%%)\n",
            sd_daily, sd_daily * sqrt(ANN) * 100))
cat(sprintf("Last price         : $%.2f  on %s\n", last_price, last_date))


# ---
  
  # 2 · Build the Monte Carlo Simulation
  
#   The simulation covers **63 trading days (~3 months)** forward from the last
# historical price — the same horizon used in the main analysis. With 1 000 paths,
# the fan gives a robust picture of the model's uncertainty.


set.seed(42)
n_sims <- 1000
h      <- 63   # trading days forward (~3 months)

# ── Generate future trading dates ─────────────────────────────────────────────
raw_future   <- seq(last_date + 1, by = "day", length.out = h * 2)
future_dates <- raw_future[!weekdays(raw_future) %in% c("Saturday", "Sunday")][1:h]

# ── Simulate paths ────────────────────────────────────────────────────────────
sim_mat       <- matrix(NA, nrow = h + 1, ncol = n_sims)
sim_mat[1, ]  <- last_price

for (i in seq_len(n_sims)) {
  shocks         <- rnorm(h, mean = mu_daily, sd = sd_daily)
  sim_mat[-1, i] <- last_price * exp(cumsum(shocks))
}

# ── Quantile summary ─────────────────────────────────────────────────────────
sim_quants <- apply(sim_mat[-1, ], 1, quantile,
                    probs = c(0.05, 0.25, 0.50, 0.75, 0.95))

mc_df <- data.frame(
  Date = future_dates,
  Q05  = sim_quants[1, ],
  Q25  = sim_quants[2, ],
  Med  = sim_quants[3, ],
  Q75  = sim_quants[4, ],
  Q95  = sim_quants[5, ]
)

# ── 100 sample paths for plotting ────────────────────────────────────────────
sim_long <- as.data.frame(sim_mat[-1, 1:100]) %>%
  mutate(Date = future_dates) %>%
  pivot_longer(-Date, names_to = "Sim", values_to = "Price")

# ── Terminal price summary ────────────────────────────────────────────────────
terminal <- sim_mat[h + 1, ]
cat(sprintf("\n=== Monte Carlo Terminal Price Summary (%s, +%d days) ===\n",
            focal_ticker, h))
cat(sprintf("  Forecast origin : %s  @ $%.2f\n", last_date, last_price))
cat(sprintf("  Target date     : %s\n", max(future_dates)))
cat(sprintf("  Median          : $%.2f\n", median(terminal)))
cat(sprintf("  5th  pct        : $%.2f\n", quantile(terminal, 0.05)))
cat(sprintf("  95th pct        : $%.2f\n", quantile(terminal, 0.95)))
cat(sprintf("  Prob > $%.2f    : %.1f%%\n", last_price,
            mean(terminal > last_price) * 100))


# ---

# 3 · Fetch Live 2026 NVDA Prices


# quantmod pulls up to today automatically when `to` is omitted or set to Sys.Date()
getSymbols(focal_ticker,
           from        = "2025-01-01",
           to          = Sys.Date(),
           auto.assign = TRUE,
           src         = "yahoo")

nvda_2026_xts <- Ad(get(focal_ticker))
colnames(nvda_2026_xts) <- "AdjClose"

# Keep only 2026 dates that fall within our simulation window
actual_2026 <- data.frame(
  Date     = as.Date(index(nvda_2026_xts)),
  AdjClose = as.numeric(nvda_2026_xts)
) %>%
  filter(
    Date > last_date,          # forward of calibration end
    Date <= max(future_dates)  # within the 63-day horizon
  )

cat(sprintf("Live data fetched  : %d trading days (%s – %s)\n",
            nrow(actual_2026),
            min(actual_2026$Date),
            max(actual_2026$Date)))
cat(sprintf("Latest close       : $%.2f  on %s\n",
            tail(actual_2026$AdjClose, 1),
            tail(actual_2026$Date, 1)))

# Latest actual vs median simulation at same date
latest_date <- tail(actual_2026$Date, 1)
nearest_mc  <- mc_df %>% filter(Date <= latest_date) %>% slice_tail(n = 1)

cat(sprintf("\nAt %s:\n", latest_date))
cat(sprintf("  Actual close     : $%.2f\n", tail(actual_2026$AdjClose, 1)))
cat(sprintf("  MC Median        : $%.2f\n", nearest_mc$Med))
cat(sprintf("  MC 5th  pct      : $%.2f\n", nearest_mc$Q05))
cat(sprintf("  MC 95th pct      : $%.2f\n", nearest_mc$Q95))

# Is the actual price within the 90% band?
in_band <- tail(actual_2026$AdjClose, 1) >= nearest_mc$Q05 &
           tail(actual_2026$AdjClose, 1) <= nearest_mc$Q95
cat(sprintf("  Inside 90%% band  : %s\n", ifelse(in_band, "YES ✓", "NO — outside model range")))


# ---

# 4 · Overlay Plot — Simulation vs Actual 2026



NVDA_COLOR <- "#76b900"
# Historical tail for visual context (1 year before simulation start)
hist_tail <- data.frame(
  Date     = as.Date(index(nvda_hist)),
  AdjClose = as.numeric(nvda_hist)
) %>%
  filter(Date >= last_date - 365)

# How far has the actual data gone?
actual_coverage <- paste0(
  format(min(actual_2026$Date), "%b %d"),
  " – ",
  format(max(actual_2026$Date), "%b %d, %Y")
)

ggplot() +
  # ── Historical price (green) ─────────────────────────────────────────────
  geom_line(data  = hist_tail,
            aes(x = Date, y = AdjClose),
            color = NVDA_COLOR, linewidth = 0.65, alpha = 0.9) +

  # ── 100 sample paths ─────────────────────────────────────────────────────
  geom_line(data  = sim_long,
            aes(x = Date, y = Price, group = Sim),
            color = "steelblue", alpha = 0.05, linewidth = 0.3) +

  # ── 90% CI band ──────────────────────────────────────────────────────────
  geom_ribbon(data = mc_df,
              aes(x = Date, ymin = Q05, ymax = Q95),
              fill = "#4575b4", alpha = 0.18) +

  # ── IQR band ─────────────────────────────────────────────────────────────
  geom_ribbon(data = mc_df,
              aes(x = Date, ymin = Q25, ymax = Q75),
              fill = "#4575b4", alpha = 0.30) +

  # ── Median path ──────────────────────────────────────────────────────────
  geom_line(data  = mc_df,
            aes(x = Date, y = Med),
            color = "navy", linewidth = 1.0, linetype = "dashed") +

  # ── Forecast origin marker ───────────────────────────────────────────────
  geom_vline(xintercept = as.numeric(last_date),
             linetype = "dotted", color = "grey40", linewidth = 0.7) +

  # ── Actual 2026 price (orange, on top) ───────────────────────────────────
  geom_line(data  = actual_2026,
            aes(x = Date, y = AdjClose, color = "Actual 2026"),
            linewidth = 1.1) +
  geom_point(data = tail(actual_2026, 1),
             aes(x = Date, y = AdjClose),
             color = "#F97316", size = 3.5, shape = 21,
             fill = "white", stroke = 1.8) +

  # ── Annotate the last actual price ───────────────────────────────────────
  geom_label(data = tail(actual_2026, 1),
             aes(x     = Date,
                 y     = AdjClose,
                 label = paste0("$", round(AdjClose, 2), "\n",
                                format(Date, "%b %d"))),
             nudge_x    = 3,
             nudge_y    = last_price * 0.04,
             size       = 3,
             fontface   = "bold",
             color      = "#F97316",
             fill       = "white",
             label.size = 0.3) +

  # ── Manual colour legend entry for actual line ────────────────────────────
  scale_color_manual(
    name   = NULL,
    values = c("Actual 2026" = "#F97316"),
    labels = paste0("Actual NVDA close\n(", actual_coverage, ")")
  ) +

  scale_y_continuous(labels = dollar_format()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +

  labs(
    title    = paste0(focal_ticker,
                      " — Monte Carlo GBM Simulation vs Actual 2026 Close"),
    subtitle = paste0(
      "Simulation calibrated on 2014–2024 daily returns  ·  ",
      "n = ", n_sims, " paths  ·  h = ", h, " trading days\n",
      "Dark band = IQR  ·  Light band = 5th–95th pct  ·  ",
      "Orange = actual 2026 close"
    ),
    caption  = paste0("Data via Yahoo Finance / quantmod  ·  Fetched: ",
                      Sys.Date()),
    x = NULL, y = "Price (Adj Close)"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text     = element_text(size = 10)
  )


# ---

# 5 · Accuracy Diagnostics

# How well did the GBM model track reality? The table below computes the **percentile
# rank** of the actual closing price within the simulated distribution on each day
# that actual data is available. A value near 50 means the actual price was close to
# the median path; values near 0 or 100 indicate the actual price was at the extreme
# tails of the model.


# Interpolate simulated distribution onto actual dates
diag_df <- actual_2026 %>%
  rowwise() %>%
  mutate(
    # Find nearest simulation date
    nearest_idx = which.min(abs(as.numeric(mc_df$Date - Date))),
    MC_Q05      = mc_df$Q05[nearest_idx],
    MC_Q25      = mc_df$Q25[nearest_idx],
    MC_Med      = mc_df$Med[nearest_idx],
    MC_Q75      = mc_df$Q75[nearest_idx],
    MC_Q95      = mc_df$Q95[nearest_idx],
    # Percentile rank: fraction of all 1000 terminal paths below actual on that day
    Pct_Rank    = round(
      mean(sim_mat[nearest_idx, ] < AdjClose) * 100, 1),
    In_90_Band  = AdjClose >= MC_Q05 & AdjClose <= MC_Q95,
    Diff_vs_Med = round(AdjClose - MC_Med, 2),
    Pct_vs_Med  = round((AdjClose / MC_Med - 1) * 100, 2)
  ) %>%
  ungroup() %>%
  select(Date, AdjClose, MC_Q05, MC_Med, MC_Q95,
         Pct_Rank, In_90_Band, Diff_vs_Med, Pct_vs_Med) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

# Summary stats
days_in_band <- sum(diag_df$In_90_Band)
total_days   <- nrow(diag_df)
avg_pct_rank <- round(mean(diag_df$Pct_Rank), 1)

cat(sprintf("\n=== Model Accuracy Summary ===\n"))
cat(sprintf("  Trading days with actual data : %d\n",  total_days))
cat(sprintf("  Days inside 90%% MC band      : %d / %d (%.1f%%)\n",
            days_in_band, total_days, days_in_band / total_days * 100))
cat(sprintf("  Avg percentile rank           : %.1f  (50 = on median path)\n",
            avg_pct_rank))
cat(sprintf("  Avg deviation from median     : $%.2f\n",
            mean(diag_df$Diff_vs_Med)))
cat(sprintf("  Avg %% deviation from median  : %.2f%%\n",
            mean(diag_df$Pct_vs_Med)))

kable(diag_df,
      col.names = c("Date", "Actual Close", "MC 5th", "MC Median",
                    "MC 95th", "Pct Rank", "In 90% Band",
                    "Diff vs Med ($)", "Diff vs Med (%)"),
      caption   = paste0("Daily Actual vs Simulated Distribution — ",
                         focal_ticker, " 2026")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE) %>%
  column_spec(7,
              color      = ifelse(diag_df$In_90_Band, "darkgreen", "red"),
              bold       = TRUE) %>%
  column_spec(8,
              color      = ifelse(diag_df$Diff_vs_Med > 0,
                                  "darkgreen", "red"))


# ---

# 6 · Percentile Rank Over Time

# A flat line near 50 means the actual price tracked the median path closely.
# A rising line means the stock outperformed the model's central expectation; a
# falling line means it underperformed.


ggplot(diag_df, aes(x = Date, y = Pct_Rank)) +
  geom_hline(yintercept = c(5, 25, 50, 75, 95),
             linetype = "dashed",
             color    = c("red", "#4575b4", "navy", "#4575b4", "red"),
             alpha    = 0.5, linewidth = 0.5) +
  geom_ribbon(aes(ymin = 25, ymax = 75), fill = "#4575b4", alpha = 0.10) +
  geom_ribbon(aes(ymin = 5,  ymax = 95), fill = "#4575b4", alpha = 0.06) +
  geom_line(color = "#F97316", linewidth = 1.1) +
  geom_point(aes(color = In_90_Band), size = 2.5) +
  scale_color_manual(values = c("TRUE" = "#F97316", "FALSE" = "red"),
                     labels = c("TRUE" = "Inside 90% band",
                                "FALSE" = "Outside 90% band"),
                     name   = NULL) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = c(0, 5, 25, 50, 75, 95, 100),
                     labels = function(x) paste0(x, "th")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  labs(
    title    = paste0(focal_ticker,
                      " Actual Price — Percentile Rank Within MC Distribution"),
    subtitle = paste0(
      "50th pct = on median path  ·  Shaded = IQR and 90% band\n",
      "Orange = actual price, red dots = outside model 90% band"
    ),
    x = NULL, y = "Percentile Rank"
  ) +
  theme_minimal(base_size = 12) +
  mytheme +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1),
        plot.title      = element_text(face = "bold"),
        legend.position = "bottom")





