data <- gapminder::gapminder %>%
  filter(country %in% c("France", "Germany", "Ireland", "Italy",
                        "Japan", "Norway", "Greece", "United Kingdom",
                        "Spain")) %>%
  mutate(year = as.Date(paste(year, "-01-01", sep = "", format = '%Y-%b-%d')))

ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
  geom_area(alpha = 0.9) +
  scale_x_date(expand = c(0, 0),
               breaks = data$year,
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
  scale_fill_firstflower()+
  guides(fill="none")+
  theme_void()
  ggsave("figs/firstflower1.png")


ggplot(data = data, aes(x = year, y = gdpPercap, fill = country)) +
  geom_area(alpha = 0.9) +
  scale_x_date(expand = c(0, 0),
               breaks = data$year,
               date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
  scale_fill_finalrose()+
  guides(fill="none")+
  theme_void()
ggsave("figs/finalrose1.png")




x <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
z <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")

df <- tibble(x, z) %>%
  mutate(y = 1) %>%
  mutate(x = as_factor(x))

ggplot(df, aes(x = x, y = y, fill = z))+
  geom_col()+
  scale_fill_firstflower()+
  theme_void()+
  guides(fill="none")
ggsave("figs/firstflower2.png")

ggplot(df, aes(x = x, y = y, fill = z))+
  geom_col()+
  scale_fill_finalrose()+
  theme_void()+
  guides(fill="none")
ggsave("figs/finalrose2.png")
