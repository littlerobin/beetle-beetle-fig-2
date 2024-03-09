library(ggplot2)

# Data
assembly_length <- c(417140630, 615001830, 485705082, 368211209, 450479495, 292537731)
runtime_with_temp <- c("0-18:19:12", "0-20:00:53", "1-19:40:53", "0-20:19:58", "1-02:35:34", "0-18:43:17")
runtime_without_temp <- c("", "0-21:57:51", "", "0-20:39:05", "", "")

# Convert run time to seconds
to_minutes <- function(time) {
  parts <- as.numeric(strsplit(time, "-|:")[[1]])
  days <- parts[1]
  hours <- parts[2]
  minutes <- parts[3]
  seconds <- parts[4]
  total_seconds <- days * 24 * 3600 + hours * 3600 + minutes * 60 + seconds
  total_minutes <- round(total_seconds / 60)
  return(total_minutes)
}

runtime_with_temp_minutes <- sapply(runtime_with_temp, to_minutes)
runtime_without_temp_minutes <- sapply(runtime_without_temp, to_minutes)
assembly_length_mbp <- assembly_length / 1000000

# Create a data frame
data <- data.frame(
  assembly_length = assembly_length_mbp,
  runtime_with_temp_minutes = runtime_with_temp_minutes,
  runtime_without_temp_minutes = runtime_without_temp_minutes
)

# Fit linear models
lm_with_temp <- lm(runtime_with_temp_minutes ~ assembly_length, data = data)
lm_without_temp <- lm(runtime_without_temp_minutes ~ assembly_length, data = data)

# Plot
p <- ggplot(data, aes(x = assembly_length)) +
  geom_point(aes(y = runtime_with_temp_minutes, color = "With temp directory"), size = 3) +
  geom_point(aes(y = runtime_without_temp_minutes, color = "Without temp directory"), size = 3) +
  geom_smooth(aes(y = runtime_with_temp_minutes, color = "With temp directory"), method = "lm", se = FALSE, linetype = "solid", linewidth = 1) +
  geom_smooth(aes(y = runtime_without_temp_minutes, color = "Without temp directory"), method = "lm", se = FALSE, linetype = "solid", linewidth = 1) +
  labs(x = "Assembly Length (Mbp)", y = "Runtime (minutes)") +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(name = "", values = c("With temp directory" = "#17aeac", "Without temp directory" = "#bb4135")) +
  ggtitle("Runtime vs. Assembly Length with and without temp directory use")

p