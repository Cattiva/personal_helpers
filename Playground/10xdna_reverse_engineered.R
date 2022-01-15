library(magrittr)

con <- Rblpapi::blpConnect()

url <- "https://10xdna.com/de/offenlegungen/"

page <- rvest::read_html(url)

all_holdings <- page %>% 
  rvest::html_node(".disclosures") %>% 
  rvest::html_nodes("h2") %>% 
  rvest::html_text()

content <- page %>% 
  rvest::html_node(".disclosures") %>% 
  rvest::html_nodes(".__content")

fx <- tidyquant::tq_get("EUR=X",
                        from = lubridate::today()) %>% 
  dplyr::pull(adjusted) #get exchange rate


extract_ticker <- function(id){
  info <- content[id] %>% 
    rvest::html_node(".disclosure-table") %>% 
    rvest::html_nodes(".disclosure-column") %>% 
    rvest::html_text()
  
  isin <- info[2]
  position <- as.numeric(
    stringr::str_extract(
      stringr::str_trim(info[4]), "(?<=\\s)\\d*(?=\\s)" #Match first number sequence enclosed by whitespace
      )
    ) 
  last_trade <- lubridate::dmy(
    stringr::str_extract(
      stringr::str_trim(info[4]), "(\\d{2}).(\\d{2}).(\\d{4})" #Match first number sequence enclosed by whitespace
    )
  )
  
  ticker <- ROpenFIGI::OpenFIGI(data.frame("idType" = "ID_ISIN",
                                           "idValue" = isin),
                                preferdf = T) %>%
    dplyr::mutate(ticker_bb = stringr::str_c(ticker, " ", exchCode)) %>% 
    dplyr::distinct(ticker_bb) %>% 
    dplyr::pull(ticker_bb) #Get Bloomberg Ticker from OpenFIGI
  
  ticker <- ticker[1] 
  
  Sys.sleep(12) #For OpenFIGI download Limit
  
  return(tibble::tibble(name = all_holdings[id],
                        isin = isin,
                        ticker = ticker,
                        position = position,
                        last_trade = last_trade))
  
}

thelen_ticker <- purrr::map_df(1:length(content),
                                 extract_ticker)


extract_instrument_information <- function(id, start_date_override = NULL){
  
  organizational <- thelen_ticker[id,]
  
  if(!is.null(start_date_override)){
    organizational$last_trade <- lubridate::as_date(start_date_override)
  }
  
  price <- Rblpapi::bdh(stringr::str_c(organizational$ticker, " Equity"), 
                        c("PX_LAST"),start.date = organizational$last_trade) %>% 
    dplyr::as_tibble() 
  
  price <- price %>% 
    dplyr::filter(date == min(date) |
                    date == max(date)) %>% 
    dplyr::mutate(
      date_type = dplyr::case_when(
        date == max(date) ~ "date",
        date == min(date) ~ "last_trade"
      )
    )
  
  currency <- Rblpapi::bdp(stringr::str_c(organizational$ticker, " Equity"), 
                           c("CURRENCY"))

  if(currency$CURRENCY == "GBp"){ #If price is in GB Pence, divide by 100
    price[,2] <- price[,2] / 100
  }
  
  if(currency$CURRENCY != "EUR"){ #Convert Currency
    
    fx <- Rblpapi::bdp(stringr::str_c(toupper(currency$CURRENCY), "EUR Curncy"), #If price not in EUR, convert to
                 c("PX_LAST")) 
    
    price[,2] <- price[,2] * fx[[1]]
    
    
    
  } 
  
  output <- price %>% 
    dplyr::select(PX_LAST, date_type) %>% 
    tidyr::pivot_wider(names_from = date_type,
                       values_from = PX_LAST) %>% 
    dplyr::select("price" = date,
                  "last_trade_price" = last_trade) %>% 
    dplyr::bind_cols(organizational, .) %>% 
    dplyr::mutate(performance_since_trade = (price / last_trade_price)-1)
  
  
  return(output)
  
}

thelen_holdings <- purrr::map_df(1:length(content),
                                 extract_instrument_information,
                                 start_date_override = "20210101")

holdings_prepared <- thelen_holdings %>% 
  dplyr::mutate(mv_ptf = position * price,
                nav = sum(mv_ptf),
                wgt = mv_ptf / nav) %>% 
  dplyr::select(
    isin,
    ticker,
    name,
    position,
    "price" = price,
    "mv_ptf" = mv_ptf,
    "weight" = wgt,
    last_trade,
    last_trade_price,
    performance_since_trade
  ) %>% 
  dplyr::mutate(mv_start = position * last_trade_price,
                mv_end = position * price,
                diff_mv = mv_end - mv_start,
                diff_mv_pct = diff_mv / mv_start) 


holdings_prepared %>% 
  dplyr::select(name, mv_start,
                mv_end,
                diff_mv,
                diff_mv_pct) %>% 
  dplyr::arrange(desc(diff_mv)) %>% 
  gt::gt() %>% 
  gt::fmt_number(columns = c(2:4),
                 decimals = 0,use_seps = T) %>%
  gt::fmt_percent(columns = c(5),
                  decimals = 2,use_seps = F) %>%
  gt::opt_row_striping() %>% 
  gt::opt_table_outline() %>% 
  gt::tab_header("Profit and Loss YTD per Instrument",
                 subtitle = stringr::str_c("YTD the portfolio accumulated a net loss of ", 
                                           round(((sum(holdings_prepared$mv_start) / sum(holdings_prepared$mv_end)) - 1)*100, 2), "%")) %>%
  gt::tab_options(
    column_labels.font.weight = "bold"
  ) %>% 
  gt::data_color(
    columns = c("diff_mv"),
    colors = scales::col_numeric(
      palette = c(
        "red","white", "green"),
      domain = c(-10000000, 10000000)),
    alpha = NULL,
    apply_to = c("fill", "text"),
    autocolor_text = TRUE
  ) %>% 
  gt::data_color(
    columns = c("diff_mv_pct"),
    colors = scales::col_numeric(
      palette = c(
        "red","white", "green"),
      domain = c(-1, 1.1)),
    alpha = NULL,
    apply_to = c("fill", "text"),
    autocolor_text = TRUE
  ) %>% 
  gt::summary_rows(
    columns = c(2:4),
    fns = list(
      total = ~sum(.)),
    formatter = gt::fmt_number,
    use_seps = TRUE
  ) 
  
  

dplyr::summarise(nav_start = sum(mv_start),
                   nav_end = sum(mv_ptf))


holdings_prepared %>% 
    dplyr::arrange(desc(weight)) %>% 
    gt::gt() %>% 
  gt::fmt_number(columns = c(4),
                 decimals = 0,use_seps = T) %>%
  gt::fmt_number(columns = c(5,6,9),
                 decimals = 2,use_seps = T) %>%
  gt::fmt_percent(columns = c(7, 10),
                 decimals = 2,use_seps = F) %>%
  gt::data_color(
    columns = c("weight"),
    colors = scales::col_numeric(
      palette = c(
        "white", "blue"),
      domain = c(min(holdings_prepared$weight), max(holdings_prepared$weight))),
    alpha = NULL,
    apply_to = c("fill", "text"),
    autocolor_text = TRUE
  ) %>% 
    gt::data_color(
      columns = c("performance_since_trade"),
      colors = scales::col_numeric(
        palette = c(
          "red","white", "green"),
        domain = c(min(holdings_prepared$performance_since_trade),0, max(holdings_prepared$performance_since_trade))),
      alpha = NULL,
      apply_to = c("fill", "text"),
      autocolor_text = TRUE
    ) %>% 
  gt::opt_row_striping() %>% 
  gt::opt_table_outline() %>% 
  gt::tab_header("Asset Allocation of 10xDNA Fund",
                 subtitle = stringr::str_c("As of ", lubridate::today())) %>%
  gt::tab_options(
    column_labels.font.weight = "bold"
  )


















