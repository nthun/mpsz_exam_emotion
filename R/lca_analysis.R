library(tidyverse)
library(tidyLPA)
library(googlesheets)

# TODO: Rename classes to as adjectives

# Read data -------------------------------------------------------------------------

survey_raw <-
  gs_title("exam_survey_data") %>% 
  gs_read(1)


emotion_df <- read_csv("data/emotion_names.csv")

before_factors <- tibble(
  before_class = 1:6,
  before_class_hu = c(
                        "1: Izgatott",
                        "2: Készenléti állapot",
                        "3: Félelemmel teli",
                        "4: Félreértett instrukció",
                        "5: Aggodalmaskodó",
                        "6: Harcos"
  )
)

after_factors <- tibble(
  after_class = 1:4,
  after_class_hu = c(
                        "1: Félreértett instrukció",
                        "2: Pozitív kimenet",
                        "3: Negatív kimenet",
                        "4: Megkönnyebbült"
  )
)

# Process data ----------------------------------------------------------------------

survey <- 
  survey %>%
  # Keep only the first exam of each person
  distinct(id, .keep_all = TRUE) %>% 
  # Change NA-s to 0s in all emotional variables
  mutate_at(vars(before_anger:before_interest, after_anger:after_interest), 
            ~if_else(is.na(.), 0, .))


# LCA for the before data -----------------------------------------------------------

before_profiles <-  
  survey %>% 
  select(before_anger:before_interest) %>% 
  estimate_profiles(n_profiles = 1:10, 
                    models = 1)

before_profiles %>% 
  compare_solutions(statistics = c('BIC', 'AIC', 'KIC', 'CAIC')) %>% 
  plot()

before_classes <-
  get_data(before_profiles) %>% 
  filter(classes_number == 6 & model_number == 1 & Class_prob == 1) %>% 
  select(id, before_class = Class)

# Plot emotion profiles

before_long <- 
  get_data(before_profiles) %>% 
  filter(classes_number == 6 & model_number == 1 & Class_prob == 1) %>% 
  select(-model_number, -classes_number, -Class_prob) %>% 
  gather(emotion, intensity, before_anger:before_interest) %>% 
  mutate(emotion = str_remove(emotion, "before_")) %>% 
  left_join(emotion_df, by = "emotion") %>% 
  left_join(before_factors, by = c("Class" = "before_class")) %>% 
  group_by(emotion_hu, before_class_hu) %>% 
  add_count(Class, name = "class_n") %>% 
  ungroup() %>% 
  mutate(emotion_hu = str_to_sentence(emotion_hu) %>% fct_inorder(),
         before_class_hu = str_glue("{before_class_hu} (N = {class_n})")) %>% 
  group_by(emotion_hu, before_class_hu) %>% 
  summarise(intensity_mean = mean(intensity),
            intensity_se = sd(intensity)/sqrt(n())) %>% 
  ungroup()
  
before_long %>% 
  ggplot() +
  aes(x = emotion_hu, y = intensity_mean, ymin = intensity_mean - intensity_se, ymax = intensity_mean + intensity_se, group = before_class_hu) +
  geom_line() +
  facet_wrap(~before_class_hu) +
  geom_pointrange() +
  coord_flip()+
  labs(y = "Intenzitás", x = NULL) +
  theme_light()
  
# LCA for the after data -----------------------------------------------------------

after_profiles <-  
  survey %>% 
  select(after_anger:after_interest) %>% 
  estimate_profiles(n_profiles = 1:10, 
                    models = 1)

after_profiles %>% 
  compare_solutions(statistics = c('BIC', 'AIC', 'KIC', 'CAIC')) %>% 
  plot()


after_classes <-
  get_data(after_profiles) %>%
  filter(classes_number == 4 & model_number == 1 & Class_prob == 1) %>% 
  select(id, after_class = Class)


after_long <- 
  get_data(after_profiles) %>% 
  filter(classes_number == 4 & model_number == 1 & Class_prob == 1) %>% 
  select(-model_number, -classes_number, -Class_prob) %>% 
  gather(emotion, intensity, after_anger:after_interest) %>% 
  mutate(emotion = str_remove(emotion, "after_")) %>% 
  left_join(emotion_df, by = "emotion") %>% 
  left_join(after_factors, by = c("Class" = "after_class")) %>% 
  group_by(emotion_hu, after_class_hu) %>% 
  add_count(Class, name = "class_n") %>% 
  ungroup() %>% 
  mutate(emotion_hu = str_to_sentence(emotion_hu) %>% fct_inorder(),
         after_class_hu = str_glue("{after_class_hu} (N = {class_n})")) %>% 
  group_by(emotion_hu, after_class_hu) %>% 
  summarise(intensity_mean = mean(intensity),
            intensity_se = sd(intensity)/sqrt(n())) %>% 
  ungroup()

after_long %>% 
  ggplot() +
  aes(x = emotion_hu, y = intensity_mean, ymin = intensity_mean - intensity_se, ymax = intensity_mean + intensity_se, group = after_class_hu) +
  geom_line() +
  facet_wrap(~after_class_hu) +
  geom_pointrange() +
  coord_flip()+
  labs(y = "Intenzitás", x = NULL) +
  theme_light()


# Alluvial plot ---------------------------------------------------------------------
library(ggalluvial)

left_join(before_classes, after_classes, by = "id") %>% 
  left_join(before_factors, by = "before_class") %>% 
  left_join(after_factors, by = "after_class") %>% 
  count(before_class_hu, after_class_hu) %>% 
  ggplot() +
    aes(axis1 = before_class_hu, axis2 = after_class_hu, y = n) +
    geom_flow(aes(fill = before_class_hu), width = 1/4) +
    geom_stratum(width = 1/4, fill = "darkgray", color = "white") +
    geom_text(stat = "stratum", label.strata = TRUE) +
    # scale_fill_viridis_d(option = "magma") +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    guides(fill = FALSE) +
    theme_void()
  

  
  