library(tidyverse)
library(flexmix)

# https://pjreddie.com/projects/mnist-in-csv/
mnist <- read_csv(
  "https://pjreddie.com/media/files/mnist_train.csv", 
  col_names = FALSE
)

names <- expand_grid(r = 28:1, c = 1:28) %>%
  mutate(name = str_glue("pix_{r}_{c}")) %>%
  pull(name)

names(mnist) <- c("label", names)

mnist <- mnist |> 
  mutate(across(-label, ~as.numeric((.x / 255) > 0.5)))

write_csv(mnist, "data/mnist_binary.csv")

set.seed(20230613)
mnist_sample <- mnist |>
  slice_sample(n = 5000) |>
  select(-label) |>
  as.matrix()

steps <- stepFlexmix(
  formula = mnist_sample ~ 1, 
  model = FLXMCmvbinary(), 
  control = list(iter.max = 100),
  k = 2:20, 
  nrep = 1
)

steps
