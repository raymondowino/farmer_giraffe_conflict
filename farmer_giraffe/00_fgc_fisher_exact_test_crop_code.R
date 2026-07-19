################################################################################

# https://statsandr.com/blog/fisher-s-exact-test-in-r-independence-test-for-a-small-sample/

# Just comparing mangoes vs. no mangoes and invasion vs. no invasion
# 194 invasion; 13 no invasion
# 169 total - 2 or more crops (167 with invasion + 2 without invasion)

# No mangoes + invasion = 167-164 = 3
# No mangoes + no invasion = 2-1 = 1

# Mangoes + invasion = 164
# Mangoes + no invasion = 1



dat <- data.frame(
  "mango_no" = c(3, 1),
  "mango_yes" = c(164, 1),
  row.names = c("Invasion", "Non-invasion"),
  stringsAsFactors = FALSE
)
colnames(dat) <- c("No-Mango", "Mango")

dat


mosaicplot(dat,
           main = "Mosaic plot",
           color = TRUE
)

chisq.test(dat)$expected

test <- fisher.test(dat)
test$conf.int
test

# create dataframe from contingency table
x <- c()
for (row in rownames(dat)) {
  for (col in colnames(dat)) {
    x <- rbind(x, matrix(rep(c(row, col), dat[row, col]), ncol = 2, byrow = TRUE))
  }
}
df <- as.data.frame(x)
colnames(df) <- c("Giraffe Invasion", "Mango growing")
df


test <- fisher.test(table(df))

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  df, 'Giraffe Invasion', 'Mango growing',
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 3))
  )
)

################################################################################

# Many farms grew multiple crops, so there may be confounding effects we are not
# accounting for. If we just look at farms that grew only mangoes, we have the following 
# data and results.

# 27 invasion; 11 no invasion

# No mangoes + invasion = 27-17 = 10
# No mangoes + no invasion = 11-1 = 10

# Mangoes + invasion = 17 
# Mangoes + no invasion = 1

dat2 <- data.frame(
  "mango_no" = c(10, 10),
  "mango_yes" = c(17, 1),
  row.names = c("Invasion", "Non-invasion"),
  stringsAsFactors = FALSE
)
colnames(dat2) <- c("No-Mango", "Mango")

dat2


mosaicplot(dat2,
           main = "Mosaic plot",
           color = TRUE
)

chisq.test(dat2)$expected

test2 <- fisher.test(dat2)
test2


# create dataframe from contingency table
x2 <- c()
for (row in rownames(dat2)) {
  for (col in colnames(dat2)) {
    x2 <- rbind(x2, matrix(rep(c(row, col), dat2[row, col]), ncol = 2, byrow = TRUE))
  }
}
df2 <- as.data.frame(x2)
colnames(df2) <- c("Giraffe Invasion", "Mango growing")
df2


test2 <- fisher.test(table(df2))

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  df2, 'Giraffe Invasion', 'Mango growing',
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test2$p.value < 0.001, "< 0.001", round(test2$p.value, 3))
  )
)

# Mangoes WERE significant

################################################################################
# Does this same pattern occur for other crops? Look at only single crops.
# Citrus, bananas, vegetables, melons

# Citrus
# 27 invasion; 11 no invasion

# No citrus + invasion = 27
# No citrus + no invasion = 11-2 = 9

# Citrus + invasion = 0 
# Citrus + no invasion = 2

dat3 <- data.frame(
  "citrus_no" = c(27, 9),
  "citrus_yes" = c(0, 2),
  row.names = c("Invasion", "Non-invasion"),
  stringsAsFactors = FALSE
)
colnames(dat3) <- c("No-Citrus", "Citrus")

dat3


mosaicplot(dat3,
           main = "Mosaic plot",
           color = TRUE
)

chisq.test(dat3)$expected

test3 <- fisher.test(dat3)
test3


# create dataframe from contingency table
x3 <- c()
for (row in rownames(dat3)) {
  for (col in colnames(dat3)) {
    x3 <- rbind(x3, matrix(rep(c(row, col), dat3[row, col]), ncol = 2, byrow = TRUE))
  }
}
df3 <- as.data.frame(x3)
colnames(df3) <- c("Giraffe Invasion", "Citrus growing")
df3


test3 <- fisher.test(table(df3))

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  df3, 'Giraffe Invasion', 'Citrus growing',
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test3$p.value < 0.001, "< 0.001", round(test3$p.value, 3))
  )
)

### Citrus was NOT significant

################################################################################

# Bananas
# 27 invasion; 11 no invasion

# No bananas + invasion = 27-5 = 22
# No bananas + no invasion = 11-4 = 7

# bananas + invasion =  5
# bananas + no invasion = 4

dat4 <- data.frame(
  "bananas_no" = c(22, 7),
  "bananas_yes" = c(5, 4),
  row.names = c("Invasion", "Non-invasion"),
  stringsAsFactors = FALSE
)
colnames(dat4) <- c("No-bananas", "bananas")

dat4


mosaicplot(dat4,
           main = "Mosaic plot",
           color = TRUE
)

chisq.test(dat4)$expected

test4 <- fisher.test(dat4)
test4


# create dataframe from contingency table
x4 <- c()
for (row in rownames(dat4)) {
  for (col in colnames(dat4)) {
    x4 <- rbind(x4, matrix(rep(c(row, col), dat4[row, col]), ncol = 2, byrow = TRUE))
  }
}
df4 <- as.data.frame(x4)
colnames(df4) <- c("Giraffe Invasion", "Banana growing")
df4


test4 <- fisher.test(table(df4))

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  df4, 'Giraffe Invasion', 'Mango growing',
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test4$p.value < 0.001, "< 0.001", round(test4$p.value, 3))
  )
)

### Bananas were NOT significant

################################################################################

# Vegetables
# 27 invasion; 11 no invasion

# No veg + invasion = 27-4 = 23
# No veg + no invasion = 11-6 = 5

# veg + invasion = 4
# veg + no invasion = 6

dat5 <- data.frame(
  "veg_no" = c(23, 5),
  "veg_yes" = c(4, 6),
  row.names = c("Invasion", "Non-invasion"),
  stringsAsFactors = FALSE
)
colnames(dat5) <- c("No-veg", "veg")

dat5


mosaicplot(dat5,
           main = "Mosaic plot",
           color = TRUE
)

chisq.test(dat5)$expected

test5 <- fisher.test(dat5)
test5


# create dataframe from contingency table
x5 <- c()
for (row in rownames(dat5)) {
  for (col in colnames(dat5)) {
    x5 <- rbind(x5, matrix(rep(c(row, col), dat5[row, col]), ncol = 2, byrow = TRUE))
  }
}
df5 <- as.data.frame(x5)
colnames(df5) <- c("Giraffe Invasion", "Vegetable growing")
df5


test5 <- fisher.test(table(df5))

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  df5, 'Giraffe Invasion', 'Vegetable growing',
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test5$p.value < 0.001, "< 0.001", round(test5$p.value, 3))
  )
)

### vegetables WERE significant

################################################################################

# Melons
# 27 invasion; 11 no invasion

# No melon + invasion = 27-1 = 26
# No melon + no invasion = 11-3 = 8

# melon + invasion = 1
# melon + no invasion = 3

dat6 <- data.frame(
  "melon_no" = c(26, 8),
  "melon_yes" = c(1, 3),
  row.names = c("Invasion", "Non-invasion"),
  stringsAsFactors = FALSE
)
colnames(dat6) <- c("No-melon", "melon")

dat6


mosaicplot(dat6,
           main = "Mosaic plot",
           color = TRUE
)

chisq.test(dat6)$expected

test6 <- fisher.test(dat6)
test6


# create dataframe from contingency table
x6 <- c()
for (row in rownames(dat6)) {
  for (col in colnames(dat6)) {
    x6 <- rbind(x6, matrix(rep(c(row, col), dat6[row, col]), ncol = 2, byrow = TRUE))
  }
}
df6 <- as.data.frame(x6)
colnames(df6) <- c("Giraffe Invasion", "Melon growing")
df6


test6 <- fisher.test(table(df6))

# combine plot and statistical test with ggbarstats
library(ggstatsplot)
ggbarstats(
  df6, 'Giraffe Invasion', 'Melon growing',
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test6$p.value < 0.001, "< 0.001", round(test6$p.value, 3))
  )
)

### melons were NOT significant