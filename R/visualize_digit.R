visualize_digit <- function(data, row_id) {
  
  data |>
    slice(row_id) |>
    select(-label) |>
    pivot_longer(everything(), names_to = "id", values_to = "cell") |>
    separate(id, into = c("nothing", "row", "column"), sep = "_") |>
    mutate(
      row = as.numeric(row),
      column = as.numeric(column)
    ) |> 
    ggplot(aes(column, row, fill = cell)) +
    geom_tile()
  
}