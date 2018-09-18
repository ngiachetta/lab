library(tidyverse)
library(haven)

a <- read_dta("A.dta")

cor(a)

# y           x           z
# y 1.0000000  0.69746375  0.27695030
# x 0.6974638  1.00000000 -0.03553445
# z 0.2769503 -0.03553445  1.00000000

# Model 1 A
m1_a <- lm(y ~ x, data = a)

summary(m1_a)

# Variancia
std_error_x_m1_a <- broom::tidy(m1_a) %>% pull(std.error) %>% pluck(2)
var_x_m1_a <- std_error_x_m1_a^2

# Model 2 A 
m2_a <- lm(y ~ x + z, data = a)

summary(m2_a)

# Variancia
std_error_x_m2_a <- broom::tidy(m2_a) %>% pull(std.error) %>% pluck(2)
var_x_m2_a <- std_error_x_m2_a^2

#################################################################

b <- read_dta("B.dta")

cor(b)

# y         x         z
# y 1.0000000 0.6974638 0.2678829
# x 0.6974638 1.0000000 0.7265937
# z 0.2678829 0.7265937 1.0000000

# Model 1 B
m1_b <- lm(y ~ x, data = b)

summary(m1_b)

# Variancia
std_error_x_m1_b <- broom::tidy(m1_b) %>% pull(std.error) %>% pluck(2)
var_x_m1_b <- std_error_x_m1_b^2

# Model 2 B
m2_b <- lm(y ~ x + z, data = b)

summary(m1_b)

# Variancia
std_error_x_m2_b <- broom::tidy(m2_b) %>% pull(std.error) %>% pluck(2)
var_x_m2_b <- std_error_x_m2_b^2