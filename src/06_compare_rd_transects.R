# A script to compare prevalences between the 2ha plot dataset and road transect datasets

# Load pilot road transect data for comparison

pilot <- read.csv(file = "dat/road-transects-pilot.csv") |>
  select(-Plot) |> 
  rename(Bilby = m.lagotis, 
         Skink.Blue.tongue.lizard = t.multifasciata, 
         Bustard = turkey, 
         Cat = cat,
         Dingo = dingo,
         Legless.lizard.Sand.slider.spp = lerista,
         Camel = camel,
         Hopping.mouse.Spinifex = hopping.mouse,
         Dragon = dragon)  |> 
  mutate(Snake = (snake + a.pyrrhus)) |> 
  summarize(across(everything(), mean))

pilot.df <- as.data.frame(t(pilot))
pilot.df$species <- row.names(pilot.df)

# place together with sandplot data
comp.df <- left_join(pilot.df, species_df) |> 
  filter(!is.na(sigma))

ggplot(comp.df, aes(x = V1, y = p_baseline)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = species)) +
  geom_abline()


comp.df <- comp.df |> 
  select(species, sigma, p_baseline = V1)

saveRDS(comp.df, "out/species_baselines_road.rds")
