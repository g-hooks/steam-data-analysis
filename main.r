library(sqldf)

# Import data
df_apps <- data.frame(read.csv("datasets\\steam.csv"))

# Convert owners column to numeric by averaging the two values
convert_owner_range <- function(x) {
  val1 <- as.numeric(strsplit(x, "-")[[1]][1])
  val2 <- as.numeric(strsplit(x, "-")[[1]][2])
  return(as.numeric((val1 + val2) / 2))
}

for (i in 1:nrow(df_apps)) {
  df_apps$owners[i] <- convert_owner_range(df_apps$owners[i])
}

# Add positive review percentage column
df_apps$positive_percent <- df_apps$positive_ratings / (df_apps$positive_ratings + df_apps$negative_ratings)

# Create two data frames - one for indie, one for publisher
df_indie <- fn$sqldf("SELECT * FROM df_apps WHERE developer LIKE publisher")[1:9000,]
df_published <- fn$sqldf("SELECT * FROM df_apps WHERE developer NOT LIKE publisher")[1:9000,]



# Two-tailed t-test alpha = 0.05
t.test(df_indie$positive_percent, df_published$positive_percent, alternative = "two.sided", var.equal = FALSE)
t.test(df_indie$positive_percent, df_published$positive_percent, alternative = "greater", var.equal = FALSE)

hist(df_indie$positive_percent, labels=TRUE, ylim=c(0, 1200), xlab="Positive Rating %", main="Histogram of Positive Rating % of Indie Games")
hist(df_published$positive_percent, labels=TRUE, ylim=c(0, 1200), xlab="Positive Rating %", main="Histogram of Positive Rating % of Published Games")

library(ggplot2)

ggplot(aes(positive_percent)) +
  geom_bar(data=df_indie, position=position_dodge()) +
  geom_bar(data=df_published, position=position_dodge())

ggplot(df_published, aes(price, positive_percent, color=positive_percent)) +
  geom_point(shape=16, position="jitter", alpha=0.1) +
  xlim(0, 60)
