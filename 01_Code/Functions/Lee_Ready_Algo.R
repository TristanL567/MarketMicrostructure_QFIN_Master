Lee_Ready_Algo <- function(Data){
  Data %>%
    arrange(datetime) %>%
    mutate(
      MIDQUOTE = (BID + ASK) / 2,
      price_change = PRICE - lag(PRICE),
      tick_direction_raw = case_when(
        price_change > 0 ~ 1,
        price_change < 0 ~ -1,
        TRUE ~ NA_real_
      )
    ) %>%
    fill(tick_direction_raw, .direction = "down") %>%
    mutate(
      d_t = case_when(
        # 1. Quote Rule: Closer to Ask -> Buy
        PRICE > MIDQUOTE ~ 1,
        # 2. Quote Rule: Closer to Bid -> Sell
        PRICE < MIDQUOTE ~ -1,
        # 3. Midpoint/Tick Test Rule 
        # If at midquote (or quote missing), use the extended tick test
        # (which now includes zero-tick logic via the fill)
        PRICE == MIDQUOTE ~ tick_direction_raw,
        TRUE ~ 0 
      )
    )
}