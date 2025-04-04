filepath <- 'Downloads/AGR333-Lab9 Material/'
u2_data <- read.csv("U2_2017data.csv")
u2_data <- subset(u2_data, DBH != '' & Code != '' & Class == "O")
# Read species code data
SppCode <- read.csv("Species_Codes.csv")

trees_merge <- merge(x = u2_data, y = SppCode, by.x = 'Code', by.y = 'SppCode', all.x = TRUE) # Complete this line with your solution
str(trees_merge)

trees <- trees_merge %>%
  select(Plot, Code, Genus, Common.name, DBH, Chojnacky_Code)
str(trees)

head(trees %>% arrange(desc(DBH)))

plot_radius <- 58.5  # in feet
plot_area_acres <- (pi * plot_radius^2) / 43560
EF <- round(1 / plot_area_acres)
trees <- trees %>% mutate(EF = EF)
print(paste("EF =", EF))

trees$dia_ft <- trees$DBH / 12
trees$BA <- pi * (trees$dia_ft / 2)^2
trees$BA_pa <- trees$BA * trees$EF
trees$TPA <- 1 * trees$EF
head(trees %>% arrange(desc(BA_pa)))

sum_u2_BA <- trees %>%
  group_by(Plot) %>%
  summarise(BA = sum(BA_pa))

head(sum_u2_BA)

sum_u2_TPA <- trees %>%
  group_by(Plot) %>%
  summarise(TPA = sum(TPA))

sum_u2 <- sum_u2_BA %>%
  inner_join(sum_u2_TPA, by = "Plot")

sum_u2 %>% arrange(desc(BA))
head(sum_u2 %>% arrange(desc(TPA)))

Bm_equa <- read.csv("https://raw.githubusercontent.com/DS4Ag/Forestry_lab1/refs/heads/main/Biomass%20Equation.csv") %>%
  select(b0, b1, Chojnacky_Code)

trees <- trees %>%
  left_join(Bm_equa, by = "Chojnacky_Code")

trees <- trees %>%
  mutate(biomass = exp(b0 + b1 * log(DBH * 2.54)))

trees <- trees %>%
  filter(b0 != 0 & b1 != 0)

sum_u2_bm <- trees %>%
  group_by(Plot) %>%
  summarise(biomass = sum(biomass))

sum_u2_bm <- sum_u2_bm %>%
  mutate(bm_pa = biomass * unique(trees$EF))

sum_u2 <- sum_u2 %>%
  left_join(sum_u2_bm, by = "Plot")

sum_u2 %>%
  arrange(desc(bm_pa))
head(sum_u2 %>% arrange(desc(bm_pa)))

tree_cnt <- trees %>%
  group_by(Plot, Code) %>%
  tally()

dom_cnt <- tree_cnt %>%
  group_by(Plot) %>%
  filter(n == max(n))

tree_total <- tree_cnt %>%
  group_by(Plot) %>%
  summarise(Ttl_Trees = sum(n))

richness <- tree_cnt %>%
  group_by(Plot) %>%
  summarise(richness = n_distinct(Code))

dom_cnt <- dom_cnt %>%
  left_join(tree_total, by = "Plot") %>%
  left_join(richness, by = "Plot")

dom_cnt <- dom_cnt %>%
  mutate(rel_abd = (n / Ttl_Trees) * 100) %>%
  mutate(rel_abd = round(rel_abd, 1))

head(dom_cnt %>% arrange(desc(rel_abd)))

sum_u2 <- sum_u2 %>%
  left_join(dom_cnt, by = "Plot") %>%
  left_join(richness, by = "Plot")

sum_u2 <- sum_u2 %>%
  left_join(SppCode[,c('SppCode','Common.name')], by = c("Code" = "SppCode"))

sum_u2 <- sum_u2 %>%
  rename(Dom_species = Code, Abundance = n)

sum_u2$bm_tonpa <- sum_u2$bm_pa / 1000

sum_u2 <- sum_u2 %>%
  mutate(Dom_species = ifelse(rel_abd > 50, Dom_species, "Mixed"),
         Common.name = ifelse(rel_abd > 50, Common.name, "Mixed"))

sum_u2 <- sum_u2 %>% distinct()

write.csv(sum_u2, "sum_u2_dplyr.csv", row.names = FALSE)














