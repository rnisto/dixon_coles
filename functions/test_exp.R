n <- 500
weight_h <- 0.03

x <- tibble(
  time = seq(0,n),
  discount = exp(-0.03 * seq(0,n)),
  weight = weight_h / (1 + (n - seq(n,0,-1))*weight_h)
) %>%
  mutate(discount_hack = weight / weight_h) %>%
  select(!weight) %>%
  pivot_longer(cols = c("discount", "discount_hack"))

ggplot(data = x, aes(x = time, y = value, colour = name)) +
  geom_line()
