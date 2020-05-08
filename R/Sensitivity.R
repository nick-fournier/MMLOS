



Volume = seq(100, 1200, by = 100)
Speed = seq(15, 45, by = 10)
Lanes = 1:4
Signalization = c("Signalized","Stop", "Free")


combos <- expand.grid(Volume = Volume, Speed = Speed, Lanes = Lanes, Signalization = Signalization)



write.csv(combos, file = "Data/RData/combos.csv")

