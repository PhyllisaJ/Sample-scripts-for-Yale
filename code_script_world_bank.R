###############################################################################
#
#                             DATA TASK: IFC
# by    :    Phyllisa Joseph
# date  :    31-03-2024
#
###############################################################################




############### Setting WD, installing and loading useful packages #############
rm(list = ls())
setwd("/Users/phyllisajoseph/Dropbox/link/sample/") 

list.of.packages <- c("dplyr", "sjmisc", "sjlabelled", "ggplot2", "gridExtra", 
                      "Hmisc", "lmtest", "sandwich", "stopwords", "tidytext", 
                      "tm", "tibble","kableExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))


############################      Importing data     ###########################

J_summary  <- read.csv("job_summary.csv"          , header = TRUE, sep = ",")
J_skills   <- read.csv("job_skills.csv"           , header = TRUE)
j_postings <- read.csv("linkedin_job_postings.csv", header = TRUE, sep = ",")

#######################   Preparing data for analysis   ########################

# 1. Using job summary to categorize clean/non-clean jobs 

J_summary$job_summary <- gsub("\n", " ", J_summary$job_summary)                  # Replace "\n" with a space for Job summary


green_words <- c("sustainab"   , "renewable"  ,"energy"      ,"green" ,"clean" , # These are the words I use to identify if the job is clean or not
                 "eco friendly", "environment","conservation","recycl","carbon", 
                 "footprint"   , "efficien"   ,"organic"    ,"climate", "solar",
                 "wind","global warming","emission", "ecolog", 
                 "technology", "waste mana", "water", "biowaste","biodiversity", 
                 "biomimicry")


calculate_score <- function(job_summary_new, keywords) {                         # Create a function to calculate the Green/ clean score for each job description
  score <- 0                                                                     # Note: I also use this function to get the education level required for the job
  for (word in keywords) {
    if (grepl(word, tolower(job_summary_new))) {
      score <- score + 1
    }
  }
  return(score)
}

J_summary$job_summary_new <- J_summary$job_summary                                                                      # creating a column with only alphabets
J_summary$job_summary_new <- gsub("[^[:alpha:] ]", "", J_summary$job_summary_new)                                       # removing special characters and numbers from the new job_desc col
J_summary$green_score     <- sapply(J_summary$job_summary_new, calculate_score, keywords = green_words)                 # Apply the function to each job description and store the scores in a new column


# 2. Using job desc to get education level required for the job

diploma<-c("diploma","certification", "high school", "license","baccalaureate", "course", "vocational","accredit", "training")
J_summary$diploma_score     <- sapply(J_summary$job_summary_new, calculate_score, keywords = diploma)                            # Apply the function to each job description and store the scores in a new column for diploma

undergraduate<-c("bachelor", "undergraduate", "nurs", "degree", "college", "university")
J_summary$undergraduate_score     <- sapply(J_summary$job_summary_new, calculate_score, keywords = undergraduate)                # Apply the function to each job description and store the scores in a new column for undergraduate education

postgraduate<-c("master", "graduate","post graduate", "doctor", "phd", "mba", "msc","msw")
J_summary$postgraduate_score     <- sapply(J_summary$job_summary_new, calculate_score, keywords = postgraduate)                  # Apply the function to each job description and store the scores in a new column for post-graduate education

## Create a new variable "education_level"
J_summary$education_level <- ifelse(J_summary$undergraduate_score == 0 & J_summary$postgraduate_score  == 0 & J_summary$diploma_score == 0, "No education",
                             ifelse(J_summary$undergraduate_score == 0 & J_summary$postgraduate_score  == 0 & J_summary$diploma_score != 0,  "Diploma",
                             ifelse(J_summary$postgraduate_score  == 0 & J_summary$undergraduate_score != 0, "Undergraduate","Graduate & above")))

stopifnot(J_summary$postgraduate_score!=0 | J_summary$education_level != "Graduate & above")  # checking


# 3. Using job location to get country and continent of job location
## as search country is not the same as the country in which the job is located, 
## I created a variable for the region (continent) and a variable for country
## in which the job is located

## Country categories : variable name= country
country_patterns <- c("United States", "Canada", "Australia|Sydney", "Mexico", 
                      "Nigeria", "Greece", "France", "Italy", "Switzerland", 
                      "Netherlands", "Germany", "Ireland", "India", "Thailand", 
                      "Turkey", "Malaysia", "Philippines", "China", "Vietnam", 
                      "Indonesia", "Hong Kong", "Wales", "Scotland", "England", 
                      "Northern Ireland")

country_names    <- c("United States", "Canada", "Australia", "Mexico", "Nigeria", 
                      "Greece", "France", "Italy", "Switzerland", "Netherlands", 
                      "Germany", "Ireland", "India", "Thailand", "Turkey", "Malaysia", 
                      "Philippines", "China", "Vietnam", "Indonesia", "Hong Kong", 
                      "Wales", "Scotland", "England", "Northern Ireland")


j_postings$country <- NA_character_
for (i in seq_along(country_patterns)) {                                                                               # Iterate through each country pattern and assign the corresponding country name
  j_postings$country <- ifelse(grepl(country_patterns[i], j_postings$job_location), 
                               country_names[i], j_postings$country)
}
j_postings$country[is.na(j_postings$country)] <- "United States"                                                        # Assign default country for other cases here it would be united states as when checking I find 246 other unique cases falling under US



## Continent categories : variable name= region
j_postings$region = NA_character_ 
j_postings$region <- ifelse(grepl("United States|Mexico|Canada", j_postings$country), "North America", j_postings$region)
j_postings$region <- ifelse(grepl("Australia|Sydney", j_postings$country), "Oceania", j_postings$region)
j_postings$region <- ifelse(grepl("Nigeria", j_postings$country), "Africa", j_postings$region)
j_postings$region <- ifelse(grepl("France|Italy|Switzerland|Netherlands|Germany|Ireland|Greece", j_postings$country), "Europe", j_postings$region)
j_postings$region <- ifelse(grepl("India|Thailand|Turkey|Malaysia|Philippines|China|Vietnam|Indonasia|Hong Kong|UAE|United Arab Emirates", j_postings$country), "Asia", j_postings$region)
j_postings$region <- ifelse(grepl("England|Scotland|Northern Ireland", j_postings$country), "United Kingdom", j_postings$region)
j_postings$region <- ifelse(grepl("Brazil", j_postings$country), "South America", j_postings$region)
j_postings$region <- ifelse(grepl("[[:upper:]][[:upper:]]$", substr(j_postings$country, nchar(j_postings$country) - 1, nchar(j_postings$country))), "North America", j_postings$region)
j_postings$region[is.na(j_postings$region)] <- "North America"     # 256 different cities of US mostly 



# 4. Using job skills to get number of skills needed for each job posts

J_skills$job_skills_new <- J_skills$job_skills                                  # creating a column with only alphabets
J_skills$job_skills_new <- gsub("[^[:alpha:] ]", "", J_skills$job_skills_new)  

stwds = stopwords::stopwords("en", source = "stopwords-iso")                    # Getting rid of English stop words as they do not add an meaning
stwds

remove_stopwords_and_count <- function(text) {                                  # Developing a function that counts the number of non-stop wards in the job skills column
  if (is.na(text) || text == "information not available in the context"  
      || text == "information not found in context"|| text == "nan"
      || text == "information not found"
      || text == "no relevant information found in the provided text" 
      || text == "no technical information was found in the provided text") {
    return(NA)
  } else {
    text <- tolower(text)                         # Convert text to lowercase
    words <- unlist(strsplit(text, "\\W+"))       # Tokenize the text
    words <- words[!words %in% stwds]             # Remove stopwords
    nb_words <- length(words)                     # Count the number of remaining words
    return(nb_words)
  }
}

J_skills$nb_skills <- sapply(J_skills$job_skills_new, remove_stopwords_and_count)    # Apply the function to each row of job_skills column
J_skills <- select(J_skills, -job_skills_new)                                        # this column not needed anymore


# 5. merging all the three data sets

linkdeln <- merge(merge(J_summary, J_skills, by = "job_link", all = TRUE), j_postings, by = "job_link", all = TRUE)     # Merge all three data sets together based on the "ID" column, I use the link as the ID column, 
linkdeln <-linkdeln[!is.na(linkdeln$job_summary) & !is.na(linkdeln$job_skills),]                                        # removing job postings with no job description and no skills available 
stopifnot(!duplicated(linkdeln$job_link))                                                                               # Note: here I am assuming each link is a different job posting, however the right way would be to first check if no duplicates in job link  all psotings are unique using  stopifnot(!duplicated(linkdeln[c("job_summary","job_title")]))
stopifnot((!is.na(linkdeln$green_score)))                                                                                 


# 6. Creating variable identifying clean and non-clean economy jobs

linkdeln$clean_economy_jobs <- ifelse(linkdeln$green_score >= 2, 1, 0)                                                  # if score is greater than 2 then clean economy jobs  # according to linkdeln report 2021 the  share of grren jobs acount for 10 percent, im gettin 17percent
stopifnot((!is.na(linkdeln$clean_economy_jobs)))
linkdeln$clean <- ifelse(linkdeln$clean_economy_jobs == 1, "Clean", "Non-clean")



###################  Comparison of clean 7 non-clean jobs  #####################


# 1.a ) country concentration of clean and non-clean economy jobs

## getting proportions
country_proportions <- linkdeln %>%
  group_by(country, clean) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

country_proportions <- country_proportions %>%
  group_by(country) %>%
  #mutate(proportion = count / sum(count)) %>%
  mutate(total = sum(count))


country_proportions <- country_proportions[country_proportions$clean == "Clean", ] 

## setting colors for plots
clean_color <- "#6FC67C"
non_clean_color <- "#666666"

##  plot
Country_plot <- ggplot(country_proportions, aes(x = reorder(country, proportion), y = proportion, fill = clean)) +
                geom_bar(stat = "identity", width = 0.5) +
                geom_text(aes(label = total), vjust = -0.5, size = 3, color = "black", fontface = "bold") +  
                labs(x = "Country", y = "Proportion") +
                scale_fill_manual(values = c("Clean" = clean_color), name = NULL) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1),  
                plot.background = element_rect(fill = "white"), legend.position = "none",  
                plot.title = element_text(hjust = 0.5)  
  ) 

Country_plot <- Country_plot +
                geom_text(x = 2, y = 0.95, label = "Note: The count on top of each bar corresponds to the total number of job listings identified for the respective country.", color = "black", size = 3.5, hjust = 0, vjust = 1, inherit.aes = FALSE)

print(Country_plot)

## Save the plot
ggsave("country_concentration.png", Country_plot, width = 10, height = 6, units = "in")

# 1.b ) Continent concentration of green and non-green jobs

## getting proportions
continent_proportions <- linkdeln %>%
  group_by(region, clean) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

continent_proportions <- continent_proportions %>%
  group_by(region) %>%
 # mutate(proportion = count / sum(count)) %>%
  mutate(total = sum(count))


continent_proportions <- continent_proportions[continent_proportions$clean == "Clean", ] 

## setting colors for plots
clean_color <- "#6FC67C"
non_clean_color <- "#666666"

##  plot
continent_plot <- ggplot(continent_proportions, aes(x = reorder(region, proportion), y = proportion, fill = clean)) +
                  geom_bar(stat = "identity", width = 0.5) +
                  geom_text(aes(label = total), vjust = -0.5, size = 3, color = "black", fontface = "bold") +  
                  labs(x = "Country", y = "Proportion") + scale_fill_manual(values = c("Clean" = clean_color), name = NULL) +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.background = element_rect(fill = "white"),
                  legend.position = "none", plot.title = element_text(hjust = 0.5))

continent_plot <- continent_plot +
  geom_text(x = 0.5, y = 0.95, label = "Note: The count on top of each bar corresponds to the total number of job listings identified for the respective continent", color = "black", size = 3.5, hjust = 0, vjust = 1, inherit.aes = FALSE)

# Save the plot
ggsave("continent_concentration.png", continent_plot, width = 10, height = 6, units = "in")


# 2 ) Do both clean and non-clean jobs have equal distribution in education level required?


## getting proportions
education <- linkdeln %>%
              group_by(education_level, clean) %>%
              summarise(count = n()) %>%
              mutate(proportion = count / sum(count))

education <- education %>%
             group_by(clean) %>%
             mutate(proportion = count / sum(count))

## setting colors
custom_colors   <- c("#238B45", "#41AB5D", "#6FC67C", "#9FD3A9", "#D1E5C0")

## setting order of bars
education_order <- c("No education","Diploma", "Undergraduate","Graduate & above"  )           # Define the order of education levels

## plotting bar
Education_plot<-ggplot(education, aes(x = clean, y = proportion, fill = factor(education_level, levels = education_order))) +
                geom_bar(stat = "identity", position = "dodge") +
                geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), position = position_dodge(width = 1), vjust = -0.5) +
                labs(x = "Education Level", y = "Proportion", fill = "Clean") +
                #ggtitle("Clean vs. Non-Clean Education Required") +
                scale_fill_manual(values = custom_colors, name=NULL) + theme_minimal() +
                theme(axis.text.x = element_text(size = 12), plot.background = element_rect(fill = "white"))

##saving plot
ggsave("education.png", Education_plot, width = 6, height = 5, units = "in")

# 3 ) Do clean jobs require more skills: t-test


clean_jobs     <- subset(linkdeln, clean_economy_jobs == 1)$nb_skills           # Subsetting the data into two groups: clean jobs and non-clean jobs
non_clean_jobs <- subset(linkdeln, clean_economy_jobs == 0)$nb_skills
t_test_result  <- t.test(clean_jobs, non_clean_jobs)                             # Performing t-test

results_df     <- tibble(                                                           # Create a tibble with the t-test results                                             
                  Statistics = c("t-statistic", "degrees of freedom", "p-value", "Mean number of skills (Clean Jobs)", "Mean number of skills (Non-Clean Jobs)"),
                  Value = c(t_test_result$statistic, t_test_result$parameter, t_test_result$p.value, t_test_result$estimate[1], t_test_result$estimate[2])
)

table_formatted<- kable(results_df, align = "c") %>%                           # Format the table using kable                              
                  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# 4 ) Do both clean and non-clean economy jobs have equal distribution in job type?

job_type <- linkdeln %>%                                                        # getting proportions
            group_by(clean_economy_jobs, job_type) %>%
            summarise(count = n()) %>%
            ungroup()

job_type <- job_type %>%
            group_by(clean_economy_jobs) %>%
            mutate(proportion = count / sum(count))
            unique_values <- unique(job_type$clean_economy_jobs)                # Get unique values of clean_economy_jobs

par(mfrow = c(1, length(unique_values)), mar = c(5, 5, 1, 1))                   # Set up the layout with one row and two columns and adjusting the margin to reduce space between plots

for (value in unique_values) {                                                  # Loop over unique values of clean_economy_jobs
  job <- job_type[job_type$clean_economy_jobs == value, ]                       # Subset data for the current value
  old_par <- par(pin = c(4, 4))                                                 # Increase pin size for bigger pie charts
  title <- ifelse(value == 1, "Clean Economy Jobs", "Non Clean Economy Jobs")   # Determine title based on the value of clean_economy_jobs
  colors <- terrain.colors(nrow(job))                                           # setting color scheme for pie chart, I create pie chart as very few categories 
  pie(job$proportion, labels = sprintf("%.1f%%", job$proportion * 100), main = paste(title), col = colors, clockwise = TRUE, cex = 1.2)  # Increase cex for bigger text
  par(old_par)
  legend("topright", inset = 0.05, title = "Type of Jobs", cex = 0.8, fill = colors, legend = job$job_type, bty = "n")   
}

dev.off()                                                                       # I clear dev list




# End




