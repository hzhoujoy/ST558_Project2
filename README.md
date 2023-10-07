Project2_JoyZhou
================
Joy Zhou
2023-10-03

- <a href="#1-diseasesh-open-disease-data-api-vigenette"
  id="toc-1-diseasesh-open-disease-data-api-vigenette">1 disease.sh-Open
  Disease Data API VIGENETTE</a>

``` r
library(rmarkdown)

rmarkdown::render("Project2_JoyZhoy.Rmd", 
                  output_format = "github_document", 
                  output_file = "README.md",
                  output_options = list(
                    name_value_pairs = "value",
                    toc = TRUE,
                    toc_depth = 3,
                    number_sections = TRUE,
                    df_print = "tibble"
                  )
)
```

# 1 disease.sh-Open Disease Data API VIGENETTE

In this vignette, I will describe how to read and summarize data from an
APIs. \# Requirements To use the functions for interacting with the
`NHL` API, I used the following packages: The following packages are
required to develop the vignette and to excute the functions and engage
with the API.

. [`tidyverse`](https://www.tidyverse.org/): .
[`jaonlite'](https://cran.r-project.org/web/packages/jsonlite/): is particularly powerful for building pipelines and interacting with a web API. . [`httr2\`](https://httr2.r-lib.org/):

load the library that I needed to build this vigenette

``` r
library(httr2)
library(jsonlite)
library(tidyverse)
library(sjmisc)
library(countrycode)
library(ggplot2)
```

`CountryVaccine`

I wrote a function `CountryVaccine` for a user to interact with the
`countries` endpoint of the `Open Disease API`. It returns a data frame
with key metrics (country and timeline) for every country. The
`timeline` column represents the number of vaccine doses collected
today. It accepts one argument, country, and the default value is “all”.
The user may enter a country’s name to get only data for a specific
country.

``` r
CountryVaccine <- function(country = "all") {
  ###
  # This function returns raw JSON data on vaccine coverage from the Open Disease API.
  ###
  
  # Argument Validation
  if (!is.character(country)) {
    stop("ERROR: 'country' argument should be a character.")
  }
  country <- trimws(country)  # Remove leading/trailing whitespace
  
  # Construct the API URL
  api_url <- "https://disease.sh/v3/covid-19/vaccine/coverage/countries?lastdays=1"
  
  # Get the data from the API
  response <- httr::GET(api_url)
  
  # Check if the response is successful
  if (httr::http_status(response)$category != "Success") {
    stop("ERROR: Failed to retrieve data from the API.")
  }
  
  # Parse the JSON data
  data <- jsonlite::fromJSON(httr::content(response, as = "text"))
  
  # Check if the API response contains the "country" data
  if (!"country" %in% names(data)) {
    stop("ERROR: The API response does not contain the 'country' data.")
  }
  
  # If country does not equal "all", filter the data for the specified country
  if (country != "all") {
    # Check if the specified country is in the data
    if (country %in% data$country) {
      # Filter the data for the specified country
      data <- data[data$country == country, ]
    } else {
      # If the specified country is not found, return an empty data frame
      data <- data.frame(country = character(0), timeline = list())
    }
  }
  
  # Return the raw JSON data
  return(data)
}

#call the function
#ConVacdata <- CountryVaccine(country = "China")
ConVacdata <- CountryVaccine()
```

`CountryVaccine30day`

I’ve created the `CountryVaccinefor30day` function for a user to
interact with the `countries` endpoint of the `Open Disease API` to
retrieve data for countries that have been actively administering
vaccines for a 30-day period. This function returns a data frame
containing essential metrics such as the country’s name and its
vaccination timeline spanning 30 days. By default, the function collects
data for all countries, but users can specify a particular country’s
name to obtain data exclusively for that country.

``` r
CountryVaccinefor30day <- function(country = "all") {
  ###
  # This function returns raw JSON data on vaccine coverage from the Open Disease API.
  ###
  
  # Argument Validation
  if (!is.character(country)) {
    stop("ERROR: 'country' argument should be a character.")
  }
  country <- trimws(country)  # Remove leading/trailing whitespace
  
  # Construct the API URL
  api_url <- "https://disease.sh/v3/covid-19/vaccine/coverage/countries?lastdays=30&fullData=false"
  
  # Get the data from the API
  response <- httr::GET(api_url)
  
  # Check if the response is successful
  if (httr::http_status(response)$category != "Success") {
    stop("ERROR: Failed to retrieve data from the API.")
  }
  
  # Parse the JSON data
  data <- jsonlite::fromJSON(httr::content(response, as = "text"))
  
  # Check if the API response contains the "country" data
  if (!"country" %in% names(data)) {
    stop("ERROR: The API response does not contain the 'country' data.")
  }
  
  # If country does not equal "all", filter the data for the specified country
  if (country != "all") {
    # Check if the specified country is in the data
    if (country %in% data$country) {
      # Filter the data for the specified country
      data <- data[data$country == country, ]
    } else {
      # If the specified country is not found, return an empty data frame
      data <- data.frame(country = character(0), timeline = list())
    }
  }
  
  # Return the raw JSON data
  return(data)
}

#call the function
countrydata30day <- CountryVaccinefor30day()
```

`extractTimeline` I wrote a helper function `extractTimeline` to extract
both the date and the corresponding number from the `timeline` data
frame

``` r
# Helper function to extract date and number from timeline data frame
extractTimeline <- function(timeline_df) {
  # Check if the "timeline" column contains a list
  if (is.list(timeline_df$timeline)) {
    # Flatten the list into a data frame
    timeline_df <- do.call(rbind, timeline_df$timeline)
    
    # Rename the columns for clarity
    colnames(timeline_df) <- c("date", "number")
    
    # Convert the Date column to Date class using the specified format
    date_format <- "%m/%d/%Y"
    timeline_df$date <- as.Date(timeline_df$date, format = date_format)
  }
  
  return(timeline_df)
}

# Call the helper function to extract date and number
# call the function
ConVacdata <- CountryVaccine()

timeline_data <- ConVacdata$timeline
timeline_extracted <- extractTimeline(timeline_data)

## timeline_data data frame
col_names <- names(timeline_data)  # Get the column names
values <- as.numeric(unlist(timeline_data))  # Convert values to numeric

# Create a new data frame with two columns: Date and Number
timeline_extracted <- data.frame(
  date = as.Date(col_names, format = "%m/%d/%y"),  # Convert to Date format
  number = values
)
timeline_extracted$country <- ConVacdata$country

timeline_extracted 
```

call the `CountryVaccine` function and `extractTimeline` function

``` r
#call the functions

ConVacdata <- CountryVaccine()
timeline_data <- ConVacdata$timeline
timeline_extracted <- extractTimeline(timeline_data)

## timeline_data data frame
col_names <- names(timeline_data)  # Get the column names
values <- as.numeric(unlist(timeline_data))  # Convert values to numeric

# Create a new data frame with two columns: Date and Number
timeline_extracted <- data.frame(
  date = as.Date(col_names, format = "%m/%d/%y"),  # Convert to Date format
  number = values
)
timeline_extracted$country <- ConVacdata$country 

timeline_extracted #contains three columns: country, date, and number
extracted <- timeline_extracted[c("date", "number", "country")]
```

extracted30

``` r
ConVacdata30 <- CountryVaccinefor30day(country = "India")
timeline_data <- ConVacdata30$timeline
timeline_extracted <- extractTimeline(timeline_data)

## timeline_data data frame
col_names <- names(timeline_data)  # Get the column names
values <- as.numeric(unlist(timeline_data))  # Convert values to numeric

# Create a new data frame with two columns: Date and Number
timeline_extracted <- data.frame(
  date = as.Date(col_names, format = "%m/%d/%y"),  # Convert to Date format
  number = values
)
timeline_extracted$country <- ConVacdata30$country 

timeline_extracted #contains three columns: country, date, and number
extracted30 <- timeline_extracted[c("date", "number", "country")]
```

`longSheet` I created the `longSheet` helper function to transform data
on vaccine doses for either states or countries into a long-format
structure. This conversion is intended to simplify and streamline
subsequent data analysis processes. (note: this function is not working
now because of unstable data stracture)

``` r
# longSheet <- function(data, location_column) {
#   longSheet <- data %>%
#     pivot_longer(
#       cols = -c({{ location_column }}),
#       names_to = "date",
#       values_to = "number"
#     )
#   return(longSheet)
# }
# 
#  #call the function
# # For data with a "country" column
# longdata <- longSheet(extracted30, location_column = "country")

# For data with a "state" column
#longSheet(data_with_state, location_column = "state")
```

`stateDoses` I’ve created a `stateDoses` function to retrieve data for
states and territories in the United States that have reported 30 days
of vaccination records. It returns a data frame containing essential
metrics (state and timeline) for each state. The function accepts one
argument, `state`, with a default value of ‘`all`.’ Users can specify a
state’s name to retrieve data for a specific state.

``` r
stateVaccine <- function(state = "all") {
  ###
  # This function returns raw JSON data on vaccine coverage from the Open Disease API.
  ###
  
  # Argument Validation
  if (!is.character(state)) {
    stop("ERROR: 'state' argument should be a character.")
  }
  country <- trimws(state)  # Remove leading/trailing whitespace
  
  # Construct the API URL
  api_url <- "https://disease.sh/v3/covid-19/vaccine/coverage/states?lastdays=1"
  
  
  # Get the data from the API
  response <- httr::GET(api_url)
  
  # Check if the response is successful
  if (httr::http_status(response)$category != "Success") {
    stop("ERROR: Failed to retrieve data from the API.")
  }
  
  # Parse the JSON data
  data <- jsonlite::fromJSON(httr::content(response, as = "text"))
  
  # Check if the API response contains the "state" data
  if (!"state" %in% names(data)) {
    stop("ERROR: The API response does not contain the 'state' data.")
  }
  
  # If state does not equal "all", filter the data for the specified state
  if (state != "all") {
    # Check if the specified state is in the data
    if (state %in% data$state) {
      # Filter the data for the specified state
      data <- data[data$state == state, ]
    } else {
      # If the specified country is not found, return an empty data frame
      data <- data.frame(state = character(0), timeline = list())
    }
  }
  
  # Return the raw JSON data
  return(data)
}

#call the function
stateVac <- stateVaccine()
```

`countryCovidcase` I wrote this function to interact with the
`countries` endpoint of the total Covid-19 case records for all
countries. It returns a `data.frame` containing 231 observations and 23
variables, including country, countryInfo, cases, deaths, population,
and more.By default, the function collects data for all countries, but
users can specify a particular country’s name to obtain data exclusively
for that country.

``` r
countryCovidcase <- function(country="all") {
  # Construct the API URL
  api_url <- "https://disease.sh/v3/covid-19/countries"
  
  # Get the data from the API
  response <- httr::GET(api_url)
  
  # Check if the response is successful
  if (httr::http_status(response)$category != "Success") {
    stop("ERROR: Failed to retrieve data from the API.")
  }
  
  # Parse the JSON data
  data <- jsonlite::fromJSON(httr::content(response, as = "text"))
  
  # Return the data frame
  return(data)
}
# call the function
covid_data <- countryCovidcase()
```

`countryPop` I’ve written a helper function called `countryPop` to
facilitate interactions with the `countries` endpoint of the
`disease.sh-Open Disease Data API`. This function allows users to input
either a specific country’s name or choose “all” to obtain population
data for all countries. It will prove useful in other functions that
require population data from the API.

``` r
countryPop <- function(country = "all") {
  if (country == "all") {
    # If country is "all," return populations of all countries
    api_url <- "https://disease.sh/v3/covid-19/countries"
    
    # Fetch data from the API
    response <- httr::GET(api_url)
    
    # Check if the response is successful
    if (httr::http_status(response)$category != "Success") {
      stop("ERROR: Failed to retrieve data from the API.")
    }
    
    # Parse the JSON data
    data <- jsonlite::fromJSON(httr::content(response, as = "text"))
    
    # Extract and return a data frame with country names and populations
    population_data <- data.frame(
      Country = data$country,
      Population = data$population
    )
    
    return(population_data)
  } else {
    # Construct the API URL to get population data for a specific country
    api_url <- paste0("https://disease.sh/v3/covid-19/countries/", country)
    
    # Fetch data from the API
    response <- httr::GET(api_url)
    
    # Check if the response is successful
    if (httr::http_status(response)$category != "Success") {
      stop("ERROR: Failed to retrieve data from the API.")
    }
    
    # Parse the JSON data
    data <- jsonlite::fromJSON(httr::content(response, as = "text"))
    
    # Extract and return the population of the specified country
    population <- data$population
    
    return(population)
  }
}

# call the function
data1 <- countryPop()
data2 <- countryPop(country = "China")
```

`stateCovidCase` I wrote this function to interact with the `states`
endpoint of the total real time Covid-19 case records updated by every
10 minutes for all states and territories.It accepts one argument,
state, and the default value is “all”. The user may enter a state’s name
to get only data for a specific state.

``` r
library(httr)
library(jsonlite)

stateCovidCase <- function(state = "all") {
  ###
  # This function returns COVID-19 data for all states and territories or a specific state.
  ###
  
  # Argument Validation
  if (!is.character(state)) {
    stop("ERROR: 'state' argument should be a character.")
  }
  state <- trimws(state)  # Remove leading/trailing whitespace
  
  # Construct the API URL
  api_url <- "https://disease.sh/v3/covid-19/states"
  
  # Get the data from the API
  response <- httr::GET(api_url)
  
  # Check if the response is successful
  if (httr::http_status(response)$category != "Success") {
    stop("ERROR: Failed to retrieve data from the API.")
  }
  
  # Parse the JSON data
  data <- jsonlite::fromJSON(httr::content(response, as = "text"))
  
  # If state does not equal "all", filter the data for the specified state
  if (state != "all") {
    data <- data[data$state == state, ]
  }
  
  # Return the data frame
  return(data)
}

# Example usage:
# To get data for a specific state (e.g., California):
california_data <- stateCovidCase(state = "California")
# To get data for all states and territories:
all_states_data <- stateCovidCase()
```

`continentCovid` I wrote a helper function named `continent` to interact
with the `countries` endpoint of the `disease.sh-Open Disease Data API`.
This function enables users to retrieve data specific to a particular
continent.

``` r
continent <- function(continent_name) {
  # Fetch data from the URL
  output <- fromJSON("https://disease.sh/v3/covid-19/countries/")
  
  # Check if the continent_name argument is provided
  if (!missing(continent_name)) {
    # Filter the data for the specified continent
    output <- output[output$continent == continent_name, ]
  }
  
  return(output)
}

contientcases <- continent(continent_name = "Asia")
```

`Cleanvaccinedata` Next, I wanted to create a `Cleanvaccinedata`
function to deal with retrieved vaccine data.

``` r
# Cleanvaccinedata <- function(data, location_column){
#                   timeline_data <- data$timeline
#                   timeline_extracted <- extractTimeline(timeline_data)
#                   # Get the column names
#                   col_names <- names(timeline_data)  # Get the column names
#                   values <- as.numeric(unlist(timeline_data))  # Convert values to numeric  
#                   # Create a new data frame with two columns: Date and Number
#                     timeline_extracted <- data.frame(
#                       date = as.Date(col_names, format = "%m/%d/%y"),  # Convert to Date format
#                       number = values
#                     )
#                     
#                   timeline_extracted$location_column <- data$location_column
#                   longdata <- longSheet(timeline_extracted, location_column)          
#   
#   return(longdata)
# 
# }
# 
# # call the function
# data <- CountryVaccine()
# clean <- Cleanvaccinedata(data, location_column = "country")


#CountryVaccinefor30day()
```

``` r
Cleanvaccinedata <- function(data, location_column) {
  timeline_data <- data$timeline
  timeline_extracted <- extractTimeline(timeline_data)
  
  # Get the column names
  col_names <- names(timeline_data)
  values <- as.numeric(unlist(timeline_data))
  
  # Create a new data frame with two columns: Date and Number
  timeline_extracted <- data.frame(
    date = as.Date(col_names, format = "%m/%d/%y"),  # Convert to Date format
    number = values
  )
  
  # Assign the location_column to the data frame
  timeline_extracted[[location_column]] <- data[[location_column]] 

 timeline_extracted #contains three columns: country, date, and number
 extracted <- timeline_extracted[c("date", "number", "location_column")]

 
  return(extracted)
}
# call the function
data <- CountryVaccine()
```

Data Exploration Call the `CountryVaccine` and `countryCovidcase`
functions to get data sets then combine them by country. I will do the
further analysis based on the combined data

Wrap up The major challenge I encountered was the volatility of the
dataset obtained from the API.
