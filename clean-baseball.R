
library(Lahman)
library(dplyr)
library(lubridate)

# cleaning ----------------------------------------------

# selecting players who played at least 20 seasons and calculating wOBA
bat <- Batting %>% 
  tbl_df() %>% 
  filter(yearID >= 1920, AB > 100, !is.na(AB)) %>% 
  group_by(playerID, yearID) %>% 
  summarise_each(funs(sum), G:GIDP) %>% 
  group_by(playerID) %>% 
  mutate(n_seasons = n()) %>% 
  ungroup() %>% 
  filter(n_seasons > 19) %>% 
  mutate(IBB = ifelse(is.na(IBB), 0, IBB), 
         SF = ifelse(is.na(SF), 0, SF), 
         woba = (.699 * IBB + .888 * (H - X2B - X3B - HR) + 1.271 * X2B + 1.616 * X3B + 2.101 * HR) / 
           (AB + BB - IBB + SF + HBP)) %>% 
  select(playerID, yearID, AB, woba)

# obtaining birthdays
master <- Master %>% 
  tbl_df() %>% 
  mutate(bday = ymd(paste(birthYear, birthMonth, birthDay, sep = "-"))) %>% 
  select(playerID, bday)

# merging stats with birthdays, calculating ages
dat <- bat %>% 
  left_join(master) %>% 
  mutate(age = interval(bday, ymd(paste0(yearID, "-6-30"))) / eyears(1)) %>% 
  select(playerID, yearID, age, woba)

# orthogonal polynomial of age ------------------------------

# decomposing age into orthogonal polynomials of order 2
poly_age <- poly(dat$age, 2)
to_combine <- as.matrix(poly_age) %>% data.frame() %>% setNames(c("a1", "a2"))
dat2 <- cbind(dat, to_combine) %>% tbl_df()

# saving --------------------------------------------------

# saving data.frame of player-stats to fit in JAGS
setwd("~/Teaching/Stat-506/JAGS-lecture")
write.csv(dat2, "clean-baseball.csv", quote = F, row.names = F)

# saving orthogonal polynomial for plotting later
save(poly_age, file = "baseball-age-polynomials.RData")
