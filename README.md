# Titanic-prediction

tidyverse, dplyr, binomial generalized linear model(glm)
 
#select select column 

#mutate add new column 

starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)
  
#summarise calucate sum, mean, n-distinct 

 starwars %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(
    n > 1,
    mass > 50
  )
 
 #arrange -desc 
 
 starwars %>% 
  arrange(desc(mass))
  
 #ggplot
