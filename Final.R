# Import libraries
library(stargazer)
library(tidyverse)
library(data.table)
library(wordcloud2)
library(dplyr)  
library(RCurl)  
library(tidyr)
library(stringr)
library(jsonlite)
library(scales)
library(gridtext)
library(ggtext) 
library(RColorBrewer)
library(gridExtra) 
library(tidytext)
library(kableExtra)
library(ggforce)
library(VennDiagram)
library(png) 
library(robotstxt)
library(patchwork)
library(viridis)
library(gganimate)
library(plotly)
library(priceR)
library(png)
library(httr) 
library(rnaturalearth)
library(purrr)
library(heatmaply)
library(corrplot)
library(leaflet)
library(lubridate)
library(maps)
library(ggridges)
library(urltools)
library(tufte)
library(rvest)
library(skimr)
library(gifski) 
library(magick) 

# Read data from local path
listings <- read.csv("/Users/yangchaoran/Desktop/Final Project/Data/listings2019_2022.csv", stringsAsFactors = FALSE)
listings %>% skim()
head(listings)
str(listings)
# Define functions for later use
# a function that collects colours for each programming language
get_colour <- function(language) {
  
  language <- tolower(language)
  
  tryCatch({
    colour <-
      which((language_colours %>% names %>% tolower) %in% language) %>%
      language_colours[[.]] %>% .$color
  }
  ,
  error = function(e) {
    colour <<- "#82E0AA" # Default if colour not found in list
  }) 
  
  if(is.null(colour)) { colour <- "#82E0AA" } # Sets the colour if colour is NULL
  if(language == "hadoop") { colour <- "#EEE72F"}
  if(language == "sql") { colour <- "#4B4B4B"}
  if(language == "tableau") { colour <- "#001F4E"}
  if(language == "spark") { colour <- "#DC581B"}
  if(language == "spss") { colour <- "#D70033"}
  if(language == "d3") { colour <- "#F1993E"}
  if(language == "stata") { colour <- "#125C8D"}
  if(language == "golang") { colour <- "#00A7D1"}
  if(language == "knime") { colour <- "#F8D200"}
  if(language == "minitab") { colour <- "#6CB33F"}
  
  return(colour)
}

languages <- listings %>% select(R:Fortran) %>% colnames 
language_colours <- fromJSON("https://raw.githubusercontent.com/ozh/github-colors/master/colors.json")
colours <- languages %>% sapply(get_colour) %>% unlist %>% {
  data.frame(language=names(.), colour_for_plot=unname(.), stringsAsFactors = FALSE)
} # show_col(colours$colour_for_plot)
colours_vec <- languages %>% sapply(get_colour)

# Define RColorBrewer helpers
show_all_colours <- function(colour_set_name) {
  info <- brewer.pal.info[colour_set_name, ]
  brewer.pal(n = info$maxcolors, name = colour_set_name)
}


low_colour <- function(colour_set_name) {
  colours <- show_all_colours(colour_set_name)
  colours[1]
}

high_colour <- function(colour_set_name) {
  colours <- show_all_colours(colour_set_name)
  colours[length(colours)]
}

# Define ggplot theme and helpers
tidy_name <- function(name, n_char) {
  ifelse(nchar(name) > (n_char - 2), 
         {substr(name, 1, n_char) %>% paste0(., "..")},
         name)
}

theme_precision <- theme_classic() + 
  theme(axis.text = element_text(size=7.5),
        axis.title = element_text(size=8),
        plot.title.position = "plot",
        axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")
  )

# Fix for wordcloud2() javascript conflict
wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
                         fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
                         minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
                         rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
                         widgetsize = NULL, figPath = NULL, hoverFunction = NULL) 
{
  if ("table" %in% class(data)) {
    dataOut = data.frame(name = names(data), freq = as.vector(data))
  }
  else {
    data = as.data.frame(data)
    dataOut = data[, 1:2]
    names(dataOut) = c("name", "freq")
  }
  if (!is.null(figPath)) {
    if (!file.exists(figPath)) {
      stop("cannot find fig in the figPath")
    }
    spPath = strsplit(figPath, "\\.")[[1]]
    len = length(spPath)
    figClass = spPath[len]
    if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
      stop("file should be a jpeg, jpg, png, bmp or gif file!")
    }
    base64 = base64enc::base64encode(figPath)
    base64 = paste0("data:image/", figClass, ";base64,", 
                    base64)
  }
  else {
    base64 = NULL
  }
  weightFactor = size * 180/max(dataOut$freq)
  settings <- list(word = dataOut$name, freq = dataOut$freq, 
                   fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
                   minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
                   gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
                   shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
                   ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
  chart = htmlwidgets::createWidget("wordcloud2", settings, 
                                    width = widgetsize[1], height = widgetsize[2], 
                                    sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0,
                                                                             browser.padding = 0, browser.fill = TRUE))
  chart
}


# Data Cleaning and Preparing
# 1.Convert listingDate and expiryDate to POSIXct format
listings$listingDate <- as.POSIXct(listings$listingDate, format="%Y-%m-%d %H:%M:%OS")
listings$expiryDate <- as.POSIXct(listings$expiryDate, format="%Y-%m-%d %H:%M:%OS")

# 2.Rename column "F." to "F#"
colnames(listings)[which(colnames(listings) == "F.")] <- "F#"
# 3.Fixing Job Description
listings %>%
  select(desktopAdTemplate, mobileAdTemplate) %>%
  mutate(char_length_desktop = nchar(desktopAdTemplate)) %>%
  mutate(char_length_mobile = nchar(mobileAdTemplate)) %>%
  select(char_length_desktop, char_length_mobile) %>%
  pivot_longer(cols = char_length_desktop:char_length_mobile, names_to = "desktop_or_mobile") %>%
  ggplot(aes(value, fill = desktop_or_mobile)) +
  geom_histogram(bins = 30, position = "dodge") +
  theme_minimal() +  
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-500, NA)) +  # -500 so as to not lose zero.
  labs(title = "Characters in Job Description - Desktop / Mobile") +
  theme(legend.position = 'none') +
  xlab("Characters") +
  theme(plot.title = element_markdown(lineheight = 1.1))

# 4.Creating new variable "jobDscription"
listings <- listings %>% 
  mutate(jobDescription = ifelse(nchar(desktopAdTemplate) == 0, mobileAdTemplate, desktopAdTemplate)) 

# 5.Creating ‘Recruiter Vs Employer’ Variable
listings %>%
  select(jobId, mobileAdTemplate, advertiserName) %>%
  mutate(advertiserName = tolower(advertiserName)) %>%
  distinct(advertiserName, .keep_all = TRUE) %>%
  mutate(recruiter = ifelse(str_detect(tolower(advertiserName), "recruit"), 1, 0)) %>% 
  pull(recruiter) %>%
  table %>% as.vector %>% `names<-`(c("Does not contain 'recruit'", "Contains 'recruit'"))


# Visualization and EDA
## 1.Most Used Programming Languages & Software
p1 <- listings %>% 
  select(jobId, R:Fortran) %>% 
  pivot_longer(cols = R:Fortran, names_to = "language") %>% 
  group_by(language) %>% 
  summarise(n=sum(value)) %>% 
  arrange(desc(n)) %>% 
  left_join(colours, by = c("language")) %>% 
  mutate(language = factor(language, levels = unique(language))) %>% 
  mutate(language = reorder(language, n)) %>% 
  mutate(colour_for_plot = factor(colour_for_plot)) %>% 
  ggplot(aes(language, n, fill = colour_for_plot)) + 
  geom_col() +
  scale_fill_identity() +
  theme(axis.text.x = element_text(hjust = 1)) + 
  ggtitle("Data Scientist Jobs Listings by Language (March 2019 - Jan 2022)") +
  xlab("Language") + 
  scale_y_continuous(label=comma) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position='none',
        plot.title = element_text(size = 10)) +
  coord_flip() 

p1 %>%  ggplotly(tooltip = c("language", "n"))

p2020 <- listings %>% 
  filter(first_seen >= "2020-01-25" & first_seen < "2021-01-25") %>% 
  select(jobId, R:Fortran) %>% 
  pivot_longer(cols = R:Fortran, names_to = "language") %>% 
  group_by(language) %>% 
  summarise(n=sum(value)) %>% 
  arrange(desc(n)) %>% 
  left_join(colours, by = c("language")) %>% 
  mutate(language = factor(language, levels = unique(language))) %>% 
  mutate(language = reorder(language, n)) %>% 
  mutate(colour_for_plot = factor(colour_for_plot)) %>% 
  ggplot(aes(language, n, fill = colour_for_plot)) + 
  geom_col() +
  scale_fill_identity() +
  theme(axis.text.x = element_text(hjust = 1)) + 
  ggtitle("Australian Data Scientist Job Listings by Language (Year to 2021-01-25)") + 
  xlab("Language") + 
  scale_y_continuous(label=comma) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position='none',
        plot.title = element_text(size = 10)) +
  coord_flip() 


p2021 <- listings %>% 
  filter(first_seen >= "2021-01-25" & first_seen < "2022-01-25") %>% 
  select(jobId, R:Fortran) %>% 
  pivot_longer(cols = R:Fortran, names_to = "language") %>% 
  group_by(language) %>% 
  summarise(n=sum(value)) %>% 
  arrange(desc(n)) %>% 
  left_join(colours, by = c("language")) %>% 
  mutate(language = factor(language, levels = unique(language))) %>% 
  mutate(language = reorder(language, n)) %>% 
  mutate(colour_for_plot = factor(colour_for_plot)) %>% 
  ggplot(aes(language, n, fill = colour_for_plot)) + 
  geom_col() +
  scale_fill_identity() +
  theme(axis.text.x = element_text(hjust = 1)) + 
  ggtitle("Data Scientist Job Listings by Language (years ending Jan 2021, Jan 2022)") + 
  xlab("Language") + 
  scale_y_continuous(label=comma) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        legend.position='none',
        plot.title = element_text(size = 8)) +
  coord_flip() 

subplot(p2020, p2021)

## 2.Job Descriptions Analysis
### WordCount Plot
stop_words_except_languages <- stop_words %>% 
  select(word) %>% 
  filter(!word %in% c("r", "c", "f", "d", "q"))

listing_words <- listings %>%
  unnest_tokens(word, jobDescription) %>%
  anti_join(stop_words_except_languages) %>%
  distinct()

p <- listing_words %>%
  count(word, sort = TRUE) %>%
  top_n(35) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = "pink") +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  xlab("") +
  ylab("Number of Job Listings Containing Term") +
  ggtitle("Most Common Words in Data Scientist Job") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_flip() +
  theme_precision

p %>% ggplotly

### WordCloud
listing_words %>% 
  count(word, sort = TRUE) %>% 
  head(200) %>%
  wordcloud2a(size = .2, 
              fontFamily ="Arial", 
              # The choice of a two-scale colour oddly works
              color=brewer.pal(n = 11, name = "Pastel1"), # RdYlGn
              shape = 'cloud',
              ellipticity = 1)

## 3.What is Each Programming Language Used for?
library(tidyverse)

stop_words_except_languages <- stop_words %>% 
  select(word) %>% 
  filter(!word %in% c("r", "c", "f", "d", "q"))

generic_words_to_exclude <- c("experience", "data", "scientist",
                              "skills", "team", "role", "opportunity", "knowledge", 
                              "ability", "projects", "highly", "complex", "environment",
                              "including", "required", "technical", "management",
                              "support", "strong", "excellent", "opportunities", "develop",
                              "degree", "people", "key", "innovative", "industry", "technologies",
                              "client", "information", "services", "provide",
                              "seeking", "stakeholders", "position", "software", "applications",
                              "developing", "culture", "considered", "based", "leading", "world",
                              "time", "sets", "relevant", "organisation", "company", "identify", 
                              "expertise", "datasets", "results", "global", "diverse", "clients",
                              "solve", "understanding", "multiple", "proficiency", "life", "work",
                              "career", "successful", "communicate", "passion", "delivering",
                              "candidate", "applying", "field", "real", "proven", "project", 
                              "driven", "apply", "passionate", "deliver", "platforms", "create", 
                              "written", "outcomes", "offer", "operations", "flexible", "existing",
                              "include", "responsible", "e.g", "build", "languages", "join", 
                              "arrangements", as.character(1:10), "bring", "employer", "future",
                              "qualifications", "requirements", "solving", "thrive",
                              "superannuation", "diversity", "committed", "assess", "cover", 
                              "qualification", "specialist", "responsibilities", "contact", "advanced",
                              "senior", "sydney", "australian"
)

generic_words_to_exclude <- c(tolower(languages), generic_words_to_exclude)

languages_to_include <- listings %>% 
  pivot_longer(cols = R:Fortran, names_to = "Language") %>% 
  filter(value == 1) %>% 
  group_by(Language) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n > 4) %>% # n > 0
  pull(Language) %>% 
  unname

plot <- listings %>% 
  select(jobDescription, R:Fortran) %>% 
  unnest_tokens(word, jobDescription) %>% 
  anti_join(stop_words_except_languages) %>% 
  distinct() %>% 
  pivot_longer(cols = R:Fortran, names_to = "Language") %>%
  filter(value == 1) %>% 
  filter(!word %in% generic_words_to_exclude) %>% 
  filter(Language %in% languages_to_include) %>% 
  select(-value) %>% 
  select(word, Language) %>% 
  group_by(Language) %>% 
  count(word, sort = TRUE) %>%
  top_n(28) %>%
  mutate(word = reorder(word, n)) %>%
  select(word, Language, n) %>%   
  mutate(word = reorder(word, n)) %>% 
  group_by(Language, word) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(word = factor(paste(word, Language, sep="___"),
                       levels = rev(paste(word, Language, sep="___")))) %>% 
  ggplot() +
  geom_col(aes(word, n, fill = Language)) + 
  scale_fill_manual(values = colours_vec) + 
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +  
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  coord_flip() 

plots <- lapply(seq_along(languages_to_include),
                function(i) {
                  plot <- plot + facet_wrap_paginate(~factor(Language, levels = languages_to_include), 
                                                     ncol = 1, nrow = 1, page = i, scales="free") +
                    scale_x_discrete(labels = function(x) gsub("___.+$", "", x))
                }
)

for (i in 1:length(languages_to_include)) {
  cat(paste0("\n\n### ", languages_to_include[i], "\n"))
  print(plots[[i]])
}

### Turn it to a dataframe
plot_data <- listings %>% 
  select(jobDescription, R:Fortran) %>% 
  unnest_tokens(word, jobDescription) %>% 
  anti_join(stop_words_except_languages) %>% 
  distinct() %>% 
  pivot_longer(cols = R:Fortran, names_to = "Language") %>%
  filter(value == 1) %>% 
  filter(!word %in% generic_words_to_exclude) %>% 
  filter(Language %in% languages_to_include) %>% 
  select(-value) %>% 
  select(word, Language) %>% 
  group_by(Language) %>% 
  count(word, sort = TRUE) %>%
  top_n(28) %>%
  mutate(word = reorder(word, n)) %>%
  select(word, Language, n) %>%   
  mutate(word = reorder(word, n)) %>% 
  group_by(Language, word) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(word = factor(paste(word, Language, sep="___"),
                       levels = rev(paste(word, Language, sep="___"))))

plot_data_list <- list()
for (i in 1:length(languages_to_include)) {
  plot_data_list[[i]] <- plot_data %>% filter(Language == languages_to_include[i])
}


combined_plot_data <- combined_plot_data %>%
  separate(word, into = c("Word", "Language"), sep = "___") %>%
  select(Word, Language, n)
head(combined_plot_data)

output_path <- "/Users/yangchaoran/Desktop/Final Project/Output/What is Each Programming Language Used for?/What is each language used for.csv"
write.csv(combined_plot_data, file = output_path, row.names = FALSE)

## 4.Which sectors are best to work in?
jobClassifications <- listings %>% 
  group_by(jobClassification) %>% 
  summarise(n=n(), ave_company_rating = mean(companyRating, na.rm = TRUE)) %>% 
  arrange(desc(n)) %>% pull(jobClassification) %>% 
  {.[!. %in% c("Information & Communication Technology", "Science & Technology")]}



p1 <- listings %>% 
  group_by(jobClassification) %>% 
  summarise(n=n(), ave_company_rating = mean(companyRating, na.rm = TRUE)) %>% 
  arrange(desc(n)) %>% 
  mutate(jobClassification = factor(jobClassification, levels=rev(jobClassification))) %>% 
  ggplot(aes(jobClassification, n, fill=ave_company_rating)) + 
  geom_col() +
  scale_fill_gradient(high=high_colour("Blues"), low="#9ECAE1", na.value = "grey80") +
  theme_precision + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(axis.text.x = element_text(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none") + 
  labs(title="Number of Listings by Industry",
       subtitle = "Average Company Rating: <span style='color:#08306B';><strong>Best</strong></span> to
          <span style='color:#9ECAE1';><strong>Worst</strong></span>") +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1)) +
  coord_flip() 

p1 

## 5.When are Job Listings Posted?
newperweek <- listings %>% 
  mutate(listingDate = as.Date(listingDate)) %>% 
  group_by(week = floor_date(listingDate, "week")) %>%
  summarise(n=n()) %>% 
  filter(week > as.Date('2019-01-13'), week < as.Date('2022-01-25')) %>%  
  ggplot(aes(week, n)) +
  geom_col(fill = "navyblue") +
  scale_x_date() + 
  theme_precision +
  theme(axis.text.x = element_text(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank()) + 
  theme(legend.position = "none") +
  ggtitle("New Job Listings per Week")

newperweek %>% ggplotly

## 6.What’s the best time to find new listings?
listings %>% 
  mutate(
    listingDate = listingDate,  
    listing_hour = hour(listingDate),
    listing_day = weekdays(listingDate)
  ) %>% 
  group_by(listing_day, listing_hour) %>% 
  summarise(Freq = n()) %>% 
  mutate(lab = sprintf('%02d:00', listing_hour)) %>%
  ggplot(aes(lab, listing_day, fill = Freq)) +
  geom_tile(colour = "ivory") +
  labs(x = "Hour of the day", y = "Day of the week") +
  scale_fill_gradient(low = low_colour("YlOrRd"), high=("#FC4E2A")) +
  theme(
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    legend.position = "none",
    axis.text.x = element_text()
  )  + 
  ggtitle("When are Data Science Job Listings Posted?") + 
  coord_flip()

## 6.About Salaries
listings$salary_string[1:50]
### Data Scientist Salaries Trends
listings <- listings$salary_string %>%  # extract numeric salaries from these strings
  extract_salary(
    exclude_below = 740.80  * 48, 
    exclude_above = 600000,
    include_periodicity = TRUE,
    working_weeks_per_year = 48 
  ) %>% 
  cbind(listings, .)
listings %>% 
  group_by(month = lubridate::floor_date(as.Date(first_seen), "month")) %>%
  summarise(mean(salary, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(month = format(as.Date(month), "%Y-%m")) %>% 
  `colnames<-`(c("month", "salary")) %>% 
  ggplot(aes(x=month, y=salary, group = 1)) +
  geom_line(colour = "#E7298A", size=1.5) + 
  geom_smooth(colour = "#807DBA", method = "lm") +
  labs(title = 'Average Salary for Data Science Jobs (Mar 2019 - Jan 2022)') + 
  scale_y_continuous(labels = scales::dollar, breaks = pretty_breaks(n = 10)) + 
  theme_precision +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank())

### Salary distribution by language
listings %>% 
  select(salary, R:Fortran) %>% 
  pivot_longer(R:Fortran, names_to = "language") %>% 
  filter(value == 1) %>% 
  select(language, salary) %>% 
  filter(!is.na(salary)) %>% 
  ggplot( aes(x = salary, y = language, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) +
  scale_fill_viridis() +
  labs(title = 'Salary Distribution by Language') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  theme_precision  +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  scale_x_continuous(labels = scales::dollar, breaks = pretty_breaks()) 

listings %>% 
  select(salary, R:Fortran) %>% 
  pivot_longer(R:Fortran, names_to = "language") %>% 
  filter(value == 1) %>% 
  select(language, salary) %>% 
  group_by(language) %>% 
  summarise(ave_salary = mean(salary, na.rm = TRUE),
            median_salary = median(salary, na.rm = TRUE),
            n = n()) %>% 
  filter(!is.na(ave_salary)) %>% 
  arrange(desc(ave_salary)) %>% 
  mutate(ave_salary = format_dollars(ave_salary)) %>% 
  mutate(median_salary = format_dollars(median_salary)) %>% 
  kable(table.attr = "style = \"color: black;\"") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = T) %>% 
  kable_styling(fixed_thead = T)

listings %>%
  select(salary, R:Fortran) %>% 
  pivot_longer(R:Fortran, names_to = "Language") %>% 
  filter(value == 1) %>% 
  select(-value) %>% 
  group_by(Language) %>% 
  filter(n() > 20) %>% 
  ungroup %>% 
  ggplot(aes(salary, fill = Language)) +
  geom_histogram(bins = 30) +
  labs(title = 'Salary Distribution by Language') + 
  scale_x_continuous(labels = scales::dollar, breaks = pretty_breaks(n = 10)) +
  theme_precision 

## 7.Correlation Matrix
cor_mat_names <- c("Is Right To Work Required", "Days to Fill Role", "Job Description Length", 
                   "Company Rating", "Salary", "Python", "R", "SQL", "Hadoop") 

# Filter listings for which we cannot reliabily gauge listing duration
start_of_research_period <- as.Date(min(listings$first_seen))
end_of_research_period <- as.Date(max(listings$last_seen))

listings %>%   
  filter(
    listingDate > start_of_research_period, 
    listingDate < end_of_research_period - 30) %>% 
  mutate(
    days_to_fill = as.numeric(as.Date(last_seen)) - as.numeric(as.Date(first_seen)),
    isRightToWorkRequired = ifelse(isRightToWorkRequired == "t", 1, 0),
    listingDate = as.Date(listingDate),
    listing_expiry = as.numeric(expiryDate - listingDate),
    jobDescriptionLength = nchar(jobDescription)
  ) %>% 
  select(isRightToWorkRequired, days_to_fill, jobDescriptionLength, companyRating, salary, Python, R, SQL, Hadoop) %>% 
  cor(use = "complete.obs") %>% 
  replace(is.na(.), 0) %>% 
  as.data.frame %>% 
  `colnames<-`(cor_mat_names) %>% 
  `rownames<-`(cor_mat_names) %>%
  as.matrix %>% 
  corrplot(type = "upper", tl.col = "black", tl.srt = 45)

## 8.Correlations Between the Languages Mentioned
cooccurrence_matrix <- listings %>% 
  select(jobId, R:Fortran) %>% 
  pivot_longer(R:Fortran, names_to = "language") %>% 
  filter(value == 1) %>% 
  select(jobId, language) %>% 
  table %>% crossprod 

totals_df <- listings %>% 
  select(jobId, R:Fortran) %>% 
  pivot_longer(R:Fortran, names_to = "language") %>% 
  filter(value == 1) %>% 
  group_by(language) %>% 
  summarise(n=n())

totals <- totals_df$n
names(totals) <- totals_df$language

relative_cooccurrence_matrix <- (cooccurrence_matrix / totals) %>% `*`(100) %>% round(1)

diag(relative_cooccurrence_matrix) <- 0

smart_colours <- c("white", rev(viridis(n = 256, alpha = 1, begin = 0, end = 1, option = "viridis")))

p <- heatmaply(relative_cooccurrence_matrix, 
               dendrogram = "none",
               xlab = "", ylab = "", 
               main = "Co-occurrence - Relative (%)",
               # scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = FALSE,
               branches_lwd = 0.1,
               # label_names = c("Country", "Feature:", "Value"),
               fontsize_row = 8, fontsize_col = 8,
               labCol = colnames(relative_cooccurrence_matrix),
               labRow = rownames(relative_cooccurrence_matrix),
               heatmap_layers = theme(axis.line=element_blank()),
               #colors = rev(viridis(n = 256, alpha = 1, begin = 0, end = 1, option = "viridis"))
               #colors = rev(c("000000", heat.colors(300)))
               colors = smart_colours)

p

# Co-Occurrence (stronger to weaker)
cooccurrence_matrix <- listings %>% 
  select(jobId, R:Fortran) %>% 
  pivot_longer(R:Fortran) %>% 
  filter(value == 1) %>% 
  select(jobId, name) %>% 
  table %>% crossprod 

smart_colours <- c("white", rev(viridis(n = 256, alpha = 1, begin = 0, end = 1, option = "mako")))

p <- heatmaply(cooccurrence_matrix, 
               dendrogram = "none",
               xlab = "", ylab = "", 
               main = "Co-occurrence - Absolute",
               # scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = FALSE,
               branches_lwd = 0.1,
               # label_names = c("Country", "Feature:", "Value"),
               fontsize_row = 8, fontsize_col = 8,
               labCol = colnames(cooccurrence_matrix),
               labRow = rownames(cooccurrence_matrix),
               heatmap_layers = theme(axis.line=element_blank()),
               #colors = rev(viridis(n = 256, alpha = 1, begin = 0, end = 1, option = "viridis"))
               #colors = rev(c("000000", heat.colors(300)))
               colors = smart_colours)

p

## 9.Analysis of the two languages learned in UCLA courses: Python&R
listings %>%
  select(salary, R, Python) %>% 
  pivot_longer(R:Python, names_to = "Language") %>% 
  filter(value == 1) %>% 
  select(-value) %>% 
  group_by(Language) %>% 
  ungroup %>% 
  ggplot(aes(salary, fill = Language)) +
  geom_histogram(breaks = seq(0, 400000, 12500), position = "identity", alpha = 0.4) + 
  labs(title = '') + 
  scale_x_continuous(labels = scales::dollar, breaks = pretty_breaks(n = 10), limits = c(0, NA), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_precision + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(axis.text.x = element_text(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none") + 
  labs(title="Salary Comparison -
  <span style='color:#00BFC4';><strong>R</strong></span> Vs
  <span style='color:#F8766D';><strong>Python</strong></span>") +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1)) + 
  
  geom_text(aes(x=45000, y=65,label="Many more python jobs"), colour="grey30", angle=0, size = 3.25) +
  geom_curve(
    aes(x = 40000, y = 61.5, xend = 65000, yend = 55),
    arrow = arrow(length = unit(0.03, "npc"), type="closed"), colour = "grey", size = 0.8, angle = 70) +
  
  geom_curve(
    aes(x = 50000, y = 69, xend = 85000, yend = 73),
    arrow = arrow(length = unit(0.03, "npc"), type="closed"), colour = "grey", size = 0.8, angle = 90, 
    curvature = -0.3)

listings %>%
  select(companyRating, R, Python) %>%
  pivot_longer(R:Python, names_to = "Language") %>%
  filter(value == 1) %>%
  select(-value) %>%
  group_by(Language) %>%
  ungroup %>% 
  ggplot(aes(companyRating, fill = Language)) +
  expand_limits(x = 0, y = 0) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) + 
  scale_x_continuous(limits = c(0, 5), expand = c(0, 0), breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(0, 50)) +
  theme_precision + 
  theme(axis.text.x = element_text(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none") + 
  labs(title="Company Rating Comparison -
  <span style='color:#00BFC4';><strong>R</strong></span> Vs
  <span style='color:#F8766D';><strong>Python</strong></span>") +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1)) 

# Regression Model
re_data <- listings %>%
  select(salary_string, R:Fortran)

re_data <- re_data %>%
  mutate(salary_string = str_remove_all(salary_string, "\\D"),  
         salary = as.numeric(salary_string))

re_data <- re_data %>%
  filter(!is.na(salary))

re_model <- lm(salary ~ R + Python + SQL + Java + C++ + Scala + Tableau + Java + Hadoop + SAS + Julia + Fortran, data = re_data)
summary(re_model)

stargazer(re_model,type="html",no.space=T,align=T,report="vc*t",out="Regression result.doc")
