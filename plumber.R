

library(plumber)
library(httr)
library(jsonlite)
library(tidyverse)


# Hent data én gang ved opstart
url <- "https://restcountries.com/v2/all"
res <- httr::GET(url)
countries <- jsonlite::fromJSON(rawToChar(res$content))

#* @apiTitle country Stats API
#* @apiDescription Plumber Exam test


#* How many countries speak Spanish English and French?
#* @get /language_counts
function() {
  # Extract all language names from all countries
  all_languages <- c()
  
  for (country_langs in countries$languages) {
    if (is.null(country_langs)) next
    
    # Handle different possible structures
    if (is.data.frame(country_langs)) {
      # If it's a data frame, extract the name column
      if ("name" %in% names(country_langs)) {
        all_languages <- c(all_languages, country_langs$name)
      }
    } else if (is.list(country_langs)) {
      # If it's a list, try to extract name from each element
      langs <- sapply(country_langs, function(lang) {
        if (is.list(lang) && "name" %in% names(lang)) {
          return(lang$name)
        }
        return(NULL)
      })
      all_languages <- c(all_languages, unlist(langs))
    }
  }
  
  # Count occurrences of specific languages
  counts <- list(
    english = sum(all_languages == "English", na.rm = TRUE),
    spanish = sum(all_languages == "Spanish", na.rm = TRUE),
    french = sum(all_languages == "French", na.rm = TRUE)
  )
  
  return(counts)
}

#* Which region has the most countries?
#* @get /top_region
function() {
  countries %>%
    filter(!is.na(region)) %>%
    count(region, sort = TRUE) %>%
    slice(1)
}

#* Top ten most and least populated countries?
#* @get /population_stats
function() {
  countries %>%
    select(name, population) %>%
    arrange(desc(population)) %>%
    {
      list(
        most_populated = head(., 10),
        least_populated = tail(., 10)
      )
    }
}

#* Ekstra endpoint: average_population_by_region
#* @get /average_population_by_region
function() {
  countries %>%
    filter(!is.na(region)) %>%
    group_by(region) %>%
    summarise(avg_population = round(mean(population, na.rm = TRUE))) %>%
    arrange(desc(avg_population))
}


#* Generate plot of top regions by number of countries
#* @get /plot_top_regions
#* @serializer png
function() {
  # Create a plot showing which region has most countries
  regions_count <- countries %>%
    filter(!is.na(region)) %>%
    count(region, sort = TRUE)
  
  # Create the plot
  plot <- ggplot(regions_count, aes(x = reorder(region, n), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Number of Countries by Region",
      x = "Region",
      y = "Number of Countries"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 10)
    )
  
  # Return the plot
  print(plot)
}

#* Generate plot of average population by region
#* @get /plot_average_population
#* @serializer png
function() {
  # Get average population by region
  avg_pop_by_region <- countries %>%
    filter(!is.na(region)) %>%
    group_by(region) %>%
    summarise(avg_population = round(mean(population, na.rm = TRUE))) %>%
    arrange(desc(avg_population))
  
  # Create the plot
  plot <- ggplot(avg_pop_by_region, aes(x = reorder(region, avg_population), y = avg_population/1000000)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    coord_flip() +
    labs(
      title = "Average Population by Region",
      x = "Region",
      y = "Average Population (millions)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 10)
    )
  
  # Return the plot
  print(plot)
}


# Programmatically alter your API
#* @plumber
function(pr) {
  pr %>%
    # Overwrite the default serializer to return unboxed JSON
    pr_set_serializer(serializer_unboxed_json())
}