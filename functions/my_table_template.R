######### my_table_template #########

library(knitr)      # for integrating computing and reporting in markdown
library(kableExtra) # for customizing appearance of tables


# Function for table template
my_table_template <- function(df, caption = NULL, digits = 2, row_names = FALSE, col_names = NULL, header_above_config = NULL, footnote_config = NULL) {
  if(!is.null(col_names)) {
    kable(df, caption = caption, digits = digits, row.names = row_names, col.names = col_names) %>%
      kable_styling(bootstrap_options = c("striped", "condensed"), font_size = 12, full_width = F, position = "left") %>%
      add_header_above(header_above_config) %>%
      footnote(footnote_config) 
  } else {
    kable(df, caption = caption, digits = digits, row.names = row_names) %>%
      kable_styling(bootstrap_options = c("striped", "condensed"), font_size = 12, full_width = F, position = "left") %>%
      add_header_above(header_above_config) %>%
      footnote(footnote_config)
  }
}