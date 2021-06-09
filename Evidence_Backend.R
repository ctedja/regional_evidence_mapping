# Work in progress.


# Libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(httr)
library(rlist)
library(xlsx)
library(sf)



# 1. Manual Evidence Database ----

# First let's read in a corporate shapefile which has all subnational boundaries down to administrative level 2 (we use these to then build our maps later)
geography <- read_sf("C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\rbb_adm2_may_2021\\adm2.shp")


# Load our manually-input master database of evidence products, which dates back to 2017. Importantly, we need to pivot the subnational level tags into long data, as it's currently comma separated within singular cells.
evidence <- read_excel('C:\\Users\\clinton.tedja\\World Food Programme\\Regional Bureau Bangkok - Programme\\Programme cycle, Reports & KM\\Knowledge Management\\CSP Evidence Master List.xlsx', sheet = 'Evidence', skip = 3)


# Pivot to allow for tidier long format of coverage at subnational levels which are manually comma separated in the original database
evidence <- evidence %>% 
  # Tag regional first in a separate column; we need to have this identification in our country/coverage disaggregated data later to identify which are regional products
  mutate(Coverage_Regional = case_when(Coverage == "Asia-Pacific" ~ "Regional",
                                TRUE ~ "Specific"),
         #  Split out for areas of interest
         Coverage = case_when(Coverage == "Asia-Pacific" ~ paste0(unique(geography$adm0_name), collapse = ", "),
                              Coverage == "South Asia" ~ "India, Sri Lanka, Bangladesh, Pakistan",
                              TRUE ~ Coverage)) %>%
  # Do the same for countries
  mutate(Country_Regional = case_when(Country == "Asia-Pacific" ~ "Regional",
                                TRUE ~ "Specific"),
         Country = case_when(Country == "Asia-Pacific" ~ paste0(unique(geography$adm0_name), collapse = ", "),
                              Country == "South Asia" ~ "India, Sri Lanka, Bangladesh, Pakistan",
                              TRUE ~ Country))


nmax <- max(str_count(evidence$Coverage, "\\,"))+1

evidence <- separate(data = evidence, col = Coverage, 
                  into = paste0("c", seq_len(nmax)),
                  sep = "\\,")


evidence <- evidence %>% pivot_longer(cols = paste0("c", seq_len(nmax)),
                      names_to = "Coverage_N",
                      values_to = "Coverage",
                      values_drop_na = TRUE) %>%
  # We also are needing to make it match our API data source
  select(-c(Coverage_N, Index, Theme, Source)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate(Source = "Manual Entry", Origin_Link = NA, Image = NA) %>%
  mutate(Coverage = str_trim(Coverage, side = c("left"))) 

# Then we gotta repeat that pivot for any places where country titles were comma separated in the same way
nmax <- max(str_count(evidence$Country, "\\,"))+1

evidence <- separate(data = evidence, col = Country, 
                     into = paste0("c", seq_len(nmax)),
                     sep = "\\,")

evidence <- evidence %>% pivot_longer(cols = paste0("c", seq_len(nmax)),
                                      names_to = "Country_N",
                                      values_to = "Country",
                                      values_drop_na = TRUE) %>%
  select(-c(Country_N)) %>%
  mutate(Country = str_trim(Country))



# And a little formula to get rid of any superfluously pivoted rows
evidence <- filter(evidence, !((Coverage %in% unique(geography$adm0_name) |
                                  Coverage == "Regional") &
                        Coverage != Country))



#
#
#


# 2. ReliefWeb API ----

# Using the API from ReliefWeb, we can also expand the scope of our search substantially

# First let's define the countries of interest and produce a URL call that is easier to read.

api_country_query <- c("Afghanistan",
          "Bangladesh",
          "Bhutan",
          "Cambodia",
          "Democratic%20People's%20Republic%20of%20Korea",
          "Fiji",
          "Indonesia",
          "Kyrgyzstan",
          "Lao%20People's%20Democratic%20Republic%20(the)",
          "Myanmar",
          "Nepal",
          "Pakistan",
          "Papua%20New%20Guinea",
          "Philippines",
          "Samoa",
          "Sri%20Lanka",
          "Tajikistan",
          "Timor-Leste",
          "Tonga",
          "Tuvalu")

api_country_query <- paste0("&filter[conditions][0][conditions][",
       0:(length(api_country_query)-1),
       "][field]=primary_country&filter[conditions][0][conditions][", 
       0:(length(api_country_query)-1), 
       "][value]=",
       api_country_query,
       collapse = "")

# Then bring the country query [condition 0] together with a filter of:
#   maximum limit of 1,000 [limit]
#   time [condition 1]
#   everything with WFP as the author [condition 2]
api_url_2021 <- paste0("https://api.reliefweb.int/v1/reports?appname=apidoc&profile=full&limit=1000&filter[operator]=AND&filter[conditions][0][operator]=OR", 
                  api_country_query,
                  "&filter[conditions][1][field]=date.created&filter[conditions][1][value][from]=2021-01-01T00:00:00%2B00:00&filter[conditions][2][field]=source.shortname&filter[conditions][2][value]=WFP")

api_url_2020 <- paste0("https://api.reliefweb.int/v1/reports?appname=apidoc&profile=full&limit=1000&filter[operator]=AND&filter[conditions][0][operator]=OR", 
                  api_country_query,
                  "&filter[conditions][1][field]=date.created&filter[conditions][1][value][from]=2020-01-01T00:00:00%2B00:00&filter[conditions][1][value][to]=2020-12-31T23:59:59%2B00:00&filter[conditions][2][field]=source.shortname&filter[conditions][2][value]=WFP")


api_url_2019 <- paste0("https://api.reliefweb.int/v1/reports?appname=apidoc&profile=full&limit=1000&filter[operator]=AND&filter[conditions][0][operator]=OR", 
                       api_country_query,
                       "&filter[conditions][1][field]=date.created&filter[conditions][1][value][from]=2019-01-01T00:00:00%2B00:00&filter[conditions][1][value][to]=2019-12-31T23:59:59%2B00:00&filter[conditions][2][field]=source.shortname&filter[conditions][2][value]=WFP")


api_url_2018 <- paste0("https://api.reliefweb.int/v1/reports?appname=apidoc&profile=full&limit=1000&filter[operator]=AND&filter[conditions][0][operator]=OR", 
                       api_country_query,
                       "&filter[conditions][1][field]=date.created&filter[conditions][1][value][from]=2018-01-01T00:00:00%2B00:00&filter[conditions][1][value][to]=2018-12-31T23:59:59%2B00:00&filter[conditions][2][field]=source.shortname&filter[conditions][2][value]=WFP")


api_url_2017 <- paste0("https://api.reliefweb.int/v1/reports?appname=apidoc&profile=full&limit=1000&filter[operator]=AND&filter[conditions][0][operator]=OR", 
                       api_country_query,
                       "&filter[conditions][1][field]=date.created&filter[conditions][1][value][from]=2017-01-01T00:00:00%2B00:00&filter[conditions][1][value][to]=2017-12-31T23:59:59%2B00:00&filter[conditions][2][field]=source.shortname&filter[conditions][2][value]=WFP")



# Now we load the APIs here.We have to build multiple because the limit for this API is 1,000.
reliefweb_raw_2021 <- GET(api_url_2021)
reliefweb_raw_2020 <- GET(api_url_2020)
reliefweb_raw_2019 <- GET(api_url_2019)
reliefweb_raw_2018 <- GET(api_url_2018)
reliefweb_raw_2017 <- GET(api_url_2017)


reliefweb_list <- c(content(reliefweb_raw_2021)$data,
                    content(reliefweb_raw_2020)$data,
                    content(reliefweb_raw_2019)$data,
                    content(reliefweb_raw_2018)$data,
                    content(reliefweb_raw_2017)$data)


# Now let's build our dataframe.
reliefweb_df <- bind_rows(lapply(reliefweb_list, as.data.frame)) %>%
  select(c(fields.date.created,
           fields.primary_country.name,
           fields.format.name,
           fields.source.shortname,
           fields.title,
           fields.origin,
           fields.file.url,
           fields.body,
           fields.url_alias,
           fields.file.preview.url.thumb)) %>%
  mutate(fields.date.created = as.Date(fields.date.created, 
                                          format = "%Y-%m-%d", 
                                          tz = "UTC")) %>%
  # Let's rename these to match our manual dataset
  rename(Country = fields.primary_country.name,
         Category = fields.format.name,
         Title = fields.title,
         Date = fields.date.created,
         Link = fields.url_alias,
         Alt_Link = fields.file.url,
         Author = fields.source.shortname,
         Summary = fields.body,
         Origin_Link = fields.origin,
         Image = fields.file.preview.url.thumb) %>%
  # Then we want to make sure the categories match our manual database.
  mutate(Category = case_when(Category == "Map" ~ "Dashboards/Maps/Infographics",
                              Category == "Infographic" ~ "Dashboards/Maps/Infographics",
                              Category == "Evaluation and Lessons Learned" ~ "Evaluations",
                              Category == "Assessment" ~ "Assessments (Food Security and Nutrition)",
                              Category == "Analysis" ~ "Analysis/Research",
                              Category == "News and Press Release" ~ "Stories/Articles",
                              TRUE ~ Category)) %>%
  # And that some certain values are correctly categorized.
  mutate(Category = case_when(grepl("market", Title, ignore.case = TRUE) ~ "Market/Price Monitoring",
                              grepl("price", Title, ignore.case = TRUE) ~ "Market/Price Monitoring",
                              grepl("annual country report", Title, ignore.case = TRUE) ~ "Annual Country Report",
                              TRUE ~ Category)) %>%
  # Add some columns to match the manual database
  mutate(Sharing = "Public", 
         Recommendations = NA, 
         Source = "ReliefWeb API",
         Coverage_Regional = "Specific",
         Country_Regional = "Specific") %>%
  # Ensure any abberations in country names match our spatial file later
  mutate(Country = case_when(
    Country == "Lao People's Democratic Republic (the)" ~ "Lao People's Democratic Republic",
    Country == "American Samoa" ~ "Samoa",
    TRUE ~ Country)) %>%
  # And these country briefs appear to have been picked up so intermittently by ReliefWeb so we'll leave it for now.
  filter(!grepl("country brief", Title, ignore.case = TRUE)) %>%
  # And we want to also get rid of all the superfluous asterisks, for consistency.
  mutate(Summary = gsub("\\*", "", Summary)) 




# Then test for duplicates between the datasets and filter duplicates out
duplicates <- as.character(na.omit(unique(c(
  unique(evidence$Link[evidence$Link %in% reliefweb_df$Link]),
  unique(evidence$Link[evidence$Link %in% reliefweb_df$Alt_Link]),
  unique(evidence$Link[evidence$Link %in% reliefweb_df$Origin_Link]),
  unique(evidence$Alt_Link[evidence$Alt_Link %in% reliefweb_df$Alt_Link]),
  unique(evidence$Alt_Link[evidence$Alt_Link %in% reliefweb_df$Link]),
  unique(evidence$Alt_Link[evidence$Alt_Link %in% reliefweb_df$Origin_Link]),
  unique(evidence$Title[evidence$Title %in% reliefweb_df$Title])))))


reliefweb_df <- reliefweb_df %>% 
  filter(!Title %in% duplicates &
           !Link %in% duplicates &
           !Alt_Link %in% duplicates &
           !Origin_Link %in% duplicates)

unique(reliefweb_df$Country)

#
#
#


# 3. Subnational Allocation ----

# Now we're searching within the body summaries of our API-pulled text to automate any mentions of subnational data.

# The following function does the work of automating the allocation of subnational level data for each item. This is coded with the  assumption that any mention of administrative levels for the primary country within the title means that the item indeed has a subnational focus there. This logic seems to check out for the samples tested. 

# First, we use map_df to apply a function to every single row in our dataset and subsequently bind these into a dataframe.
reliefweb_df <- map_df(seq(nrow(reliefweb_df)), function(x) {
  # The function applies a logical T/F vector for all rows in our df; here it's checking if any admin names exist within our titles
  matches <- (str_detect(
        reliefweb_df$Title[x],
        (filter(geography, adm0_name == reliefweb_df$Country[x]))$adm1_name) | 
      str_detect(
        reliefweb_df$Title[x], 
        (filter(geography, adm0_name == reliefweb_df$Country[x]))$adm2_name)
    )
  # Then the function returns the subnational location for every instance of T.
  if(any(matches, na.rm = TRUE)) 
    tibble(reliefweb_df[x, ], 
           Coverage = (filter(geography, adm0_name == reliefweb_df$Country[x]))$adm2_name[matches])
  else tibble(reliefweb_df[x, ], Coverage = Country)}) %>%
# Then, we have to filter out a few select subnational level locations, that were picked up simply because they are common in English words, such as a town called "Ba" in Fiji, and a district called "Reg" in Afghanistan,and which don't actually have any subnational level items. We can manually review using the following: sort(table((filter(admin_matching, Locations != Country))$Locations), decreasing = TRUE)[1:100]
  filter(!Coverage %in% c("Ba", "Reg", "La")) 



#
#
#


# 4. Combined Data ----

# Then let's bind our reliefweb database with the manual database
all_equal(evidence, reliefweb_df)

combined_data <- rbind(evidence, reliefweb_df) %>%
  # But we also need to consolidate the links so that there's only one, preferably giving priority to Origin_Link which provides us with any of the original non-ReliefWeb links.
  mutate(Link = ifelse(is.na(Origin_Link), 
                       ifelse(is.na(Link), Alt_Link, Link), 
                       Origin_Link),
         Coverage_Regional = case_when(Coverage_Regional == "Specific" ~ Coverage,
                                       TRUE ~ "Regional"),
         Country_Regional = case_when(Country_Regional == "Specific" ~ Country,
                                      TRUE ~ "Regional")) %>%
  select(-c("Alt_Link", "Origin_Link")) %>%
  arrange(Country, Category)



#
#
#



# 5. Tests and Export ----
test <- anti_join(combined_data, geography, by = c("Country" = "adm0_name"))
rm(test)

plot_data <- combined_data %>% filter(grepl("WFP", Author)) %>%
  distinct(Title, .keep_all = TRUE)

ggplot(plot_data, aes(x = Date, fill = Source)) + 
  geom_histogram(binwidth = 180)


# Now we'll add it to a OneDrive location that will allow for immediate syncing with our Tableau dashboard
write.xlsx(as.data.frame(filter(combined_data, Category != "External assessments" &
                                  Category != "External Assessments")), 
           sheetName = "Combined_Reports",
           showNA = FALSE,
           row.names = FALSE,
           'C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Evidence_Backend\\Evidence_Mapping.xlsx')


# And also appending it to the original excel sheet on our Sharepoint though you'll want to double check that this sheet isn't already there.
write.xlsx(as.data.frame(plot_data), 
           sheetName = "ReliefWeb_Merged_API", 
           append = TRUE, 
           showNA = FALSE,
           row.names = FALSE,
           'C:\\Users\\clinton.tedja\\World Food Programme\\Regional Bureau Bangkok - Programme\\Programme cycle, Reports & KM\\Knowledge Management\\CSP Evidence Master List.xlsx')



# And this list of adm2_level locations will help colleagues to crosscheck against their manual entry matches
geography_reference <- geography %>% 
  as.data.frame() %>%
  select(c(objectid, iso3, adm0_id, adm0_name, adm1_name, adm1_id, adm2_name, adm2_altnm, adm2_id)) %>%
  arrange(adm0_name, adm1_name, adm2_name)


write.xlsx(geography_reference, 
           sheetName = "geography_reference",
           showNA = FALSE,
           row.names = FALSE,
           append = TRUE,
           "C:\\Users\\clinton.tedja\\OneDrive - World Food Programme\\Documents (OneDrive)\\Data\\Admin Boundaries RBB.xlsx")



# fin








# //// Temporary formula testing area -----


test_admin <- data.frame(adm0_name = c("Pakistan", "Pakistan", "Pakistan", "Pakistan", "Pakistan", "Pakistan", "Myanmar", "Pakistan", "Afghanistan", "Afghanistan"),
                         adm1_name = c("Sindh", "Sindh", "Sindh", "Sindh", "Sindh", "Sindh", "Bago", "Sindh", "Afg_adm1", "Afg_adm1"),
                         adm2_name = c("Central Karachi", "Dadu", "East Karachi", "Ghotki", "Sujawal", "Sukkur", "Bago", "hap", "Kabul", "Khost"))




test_dataset <- data.frame(Country =c("Bangladesh", "Myanmar", "Pakistan", "Pakistan", "Pakistan", "Pakistan", "Afghanistan", "Afghanistan"),
                           Summary = c("In Cox's Bazar, this and that happened.",
                                       "In Yangon, something else happened",
                                       "In Central Karachi, this happened",
                                       "In Sindh, this happened",
                                       "In Dadu AND East Karachi, this happened",
                                       "In Bago, this happened",
                                       "In bla bla bla",
                                       "In Khost this thing"),
                           Title = c("Lame title",
                                     "Lame title",
                                     "Lame title",
                                     "Lame title",
                                     "Lame title",
                                     "Lame title",
                                     "In Kabul",
                                     "IN Kabul"),
                           Other_Variable_1 = 1:8,
                           Other_Variable_2 = 1:8)




test_admin_matching <- map_df(seq(nrow(test_dataset)), function(x) {
  matches <- (str_detect(
    test_dataset$Summary[x],
    (filter(test_admin, adm0_name == test_dataset$Country[x]))$adm1_name) | 
      str_detect(
        test_dataset$Summary[x], 
        (filter(test_admin, adm0_name == test_dataset$Country[x]))$adm2_name) |
      str_detect(
        test_dataset$Title[x],
        (filter(test_admin, adm0_name == test_dataset$Country[x]))$adm1_name) | 
      str_detect(
        test_dataset$Title[x], 
        (filter(test_admin, adm0_name == test_dataset$Country[x]))$adm2_name))
  if(any(matches, na.rm = TRUE)) tibble(test_dataset[x, ], Locations = (filter(test_admin, adm0_name == test_dataset$Country[x]))$adm2_name[matches])
  else tibble(test_dataset[x, ], Locations = Country)})

rm(test_admin)
rm(test_dataset)
rm(test_admin_matching)
