Lee_Ready_Algo <- function(Data){
  Data %>%
    arrange(datetime) %>%
    mutate(
      MIDQUOTE = (BID + ASK) / 2,
      prev_PRICE = lag(PRICE) # Get previous trade price
    ) %>%
    mutate(
      d_t = case_when(
        PRICE > MIDQUOTE ~ 1,   # 1. Quote Rule: Price > Midquote -> Buy
        PRICE < MIDQUOTE ~ -1,  # 2. Quote Rule: Price < Midquote -> Sell
        PRICE > prev_PRICE ~ 1,   # 3. Tick Test: Price > Prev Price -> Buy
        PRICE < prev_PRICE ~ -1,  # 4. Tick Test: Price < Prev Price -> Sell
        TRUE ~ 0                  # Unclassifiable (e.g., zero-tick)
      )
    )
}