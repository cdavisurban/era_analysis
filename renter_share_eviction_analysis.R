
## The purpose of this script is to calculate the percent of renter households that live in areas with eviction moratoria, a temporary delay for processing an ERA application, or neither. 
## States may have eviction protections, as may counties/places. There are places/counties that have eviction moratoria in states that do not. 
library(tidyverse)
library(tidycensus)
library(urbnthemes)

#this isn't using mapping data, but this is the style we wanted
set_urbn_defaults("map")
#states that had moratoriums
#JA: I'm just going to assume that these manually-assigned eviction designations for 
# states, counties, and places are all correct throughout
states <- c( 
  "NJ", #def #stops 9/1, but below 80% median income, covid-related hardship
  "NY", #def pauses judgements where tenant can show covid-19 hardship
  "NM" #def demonstrated inability to pay rent by preponderance of evidence

)

#states that didnt have moratoriums but had a temporary delay due to ERA
states_not_evic <- c("DC",
                     "MI",
                     "MN",
                     "IN",
                     "CT", 
                     "VA", 
                     "OR", 
                     "MA",
                     "NV"
)
#counties with moratoriums
cty_fips <- c(
  "06037", #la county
  "06001", #alemeda county
  "06113" #yolo county
)

#places with moratoriums
place_fips <- c("0653000", #oakland
                "0656000", #pasadena
                "0606308", #beverly hills #JA: Not being downloaded below
                "0626000", #fremont
                "0627000", #fresno
                "0643000", #long beach
                "0649138", #moorpark #JA: Not being downloaded below
                "0648648", #monrovia #JA: Not being downloaded below
                "0681666", #vallejo
                "0681204", #union city
                "4805000", #austin 
                "5363000", #seattle
                "2562535" #somerville
                
) 


#counties without moratoria but have delay
cty_not_evic <- c("42017") #bucks county

#places without moratoria but have delay
plc_not_evic <- c("4260000", #philadelphia
                  "4819000"  #dallas
)


#JA: Might be worth just adding a note that you need a Census API Key to use this function (just for reproducability)
#download number of renter households by state
renter_states <- get_acs(geography = "state", variables = "B25003_003", year = 2019, survey = "acs1")

#download number of renter households by county and keep only ones that have some form of moratoria or delay
renter_counties <- get_acs(geography = "county", variables = "B25003_003", year = 2019, survey = "acs1") %>% 
  filter(GEOID %in% c(cty_fips, cty_not_evic)) %>% 
  mutate(is_protected_l = case_when(
    GEOID %in% cty_fips ~ "Some form of moratoria", 
    GEOID %in% cty_not_evic ~ "Temporary suspension or delay during rental assistance application", 
    T ~ NA_character_))

#download number of renter households by place and keep only ones that have some form of moratoria or delay
renter_places <- get_acs(geography = "place", variables = "B25003_003", year = 2019, survey = "acs1") %>% 
  filter(GEOID %in% c(place_fips, plc_not_evic)) %>% 
  mutate(is_protected_l = case_when(
    GEOID %in% place_fips ~ "Some form of moratoria", 
    GEOID %in% plc_not_evic ~ "Temporary suspension or delay during rental assistance application", 
    T ~ NA_character_
  ))

#add abbreviations to state data, remove puerto rico, and create eviction moratoria category
renter_states_1 <- fips_codes %>% 
  select(state_code, state) %>% 
  distinct() %>% 
  left_join(renter_states, ., by = c("GEOID" = "state_code" )) %>% 
  filter(GEOID != "72") %>% 
  mutate(
    is_protected = case_when(
      state %in% states ~ "Some form of moratoria", 
      state %in% states_not_evic ~ "Temporary suspension or delay during rental assistance application", 
      TRUE ~ "Limited or no protections"
    )
  )


##this section is to account for states that have localities in them that have different categories of eviction than the state

#sum renter households by eviction category, for states
states_initial <- renter_states_1 %>% 
  group_by(is_protected) %>% 
  summarise(initial = sum(estimate)) %>% 
  ungroup()


#by state and category, sum locality population that have some form of moratoria
local_estimates<- bind_rows(renter_places, renter_counties) %>% 
  mutate(st = str_sub(GEOID, 1, 2)) %>% 
  group_by(st, is_protected_l) %>% 
  summarise(lcl_est = sum(estimate, na.rm = TRUE))

#join state data with local data
renter_states_adj <- renter_states_1 %>% 
  left_join(local_estimates, by = c("GEOID" = "st"))

#generate how much to subtract by category
states_initial_1 <- renter_states_adj %>% 
  group_by(is_protected) %>% 
  summarise(to_subtract = sum(lcl_est, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(states_initial,.,  by = "is_protected")

#generate how much to add by category, join to data, add and subract locality information by category to get the accurate count by category
final_vals <- renter_states_adj %>% 
  group_by(is_protected_l) %>% 
  summarise(to_add = sum(lcl_est, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(states_initial_1, ., by = c("is_protected"= "is_protected_l")) %>% 
  mutate(to_add = if_else(is.na(to_add), 0, to_add), 
         final = initial - to_subtract + to_add) %>% 
  select(is_protected, final) %>% 
  janitor::adorn_percentages("col") %>% 
  mutate(is_protected = factor(is_protected, levels = c("Some form of moratoria", #JA: I think the singular "moratorium" reads better here
                                                        "Temporary suspension or delay during rental assistance application", 
                                                        "Limited or no protections"))) %>% 
  arrange(is_protected) %>% 
  mutate(temp = 1)

#write out data #JA: Is this written out data being used anywhere? If not, maybe can be deleted
final_vals %>% 
  write_csv("share_renter_households.csv")


#plot data
my_plot_5<- final_vals %>% 
  ggplot() + 
  geom_col(mapping= aes(x = temp, y = final, fill = is_protected), position = "fill") + 
  geom_text(aes(x = temp, y = final, label = scales::percent(final), group = is_protected), color = "white", size = 11, position = position_stack(vjust = .5)) + 
  labs(fill = NULL, 
       title = "Share of Renter Households in States or Jurisdictions with Different Types of Eviction Protections") + 
  scale_fill_manual( values = as.character(c(palette_urbn_cyan[c(7, 2)], palette_urbn_gray[4])))+
  scale_y_reverse() +
  theme(legend.text = element_text(size = 20), #JA: I think this line is overwritten below and can be removed
        legend.position = "right") + # JA: Can maybe add this to the theme() below just for ease of following along
  coord_flip() + 
  theme(legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10), 
        plot.title = element_text(size = 18), 
        plot.subtitle = element_text(size = 14))


ggsave("my_plot_6.png",my_plot_5, width = 13.3, height = 5.21)
