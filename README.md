::: {style="color: dodgerblue"}
# **Airwars Scrapping (Repository) Project**
:::

Thank you for visiting this repository which started as a class project for a UMD data science course and has taken a life of its own. The purpose is to scrape data from [https://airwars.org/](#0) and create analytics to enhance our understanding of the conflict in Gaza since the October 2023 war with Israel begun. [Aljazeera](#0) has a detailed look into this conflict along with casualty tracker.

::: {style="color: darkgray"}
### Background

On October 7, 2023, Hamas and other Palestinian armed groups orchestrated a deadly attack on Israel. The horrific attack killed 1,200 people, with over 200 hostages seized and over 100 still unaccounted for. Israeli forces began airstrikes and ground operations in response

-   The ongoing conflict has devastated the civilian population of Gaza. Seventy five percent of the population of Gaza has been displaced, most multiple times, and the entire population is in need of humanitarian assistance. The ongoing conflict, bombardment and blockade has led to catastrophic humanitarian suffering for more than 2 million Palestinians---half of them children---who are now without clean water, food and vital medical services.

-   Since the beginning of the war in October 7th, 2023, Airwars has monitored open source civilian harm incidents in Gaza. Incident is defined as an explosive weapon or ground battle operation that produced civilian casualties or harm. Civilian status of victims is assumed unless there is information determining their militant status. Data is derived from tweets or Facebook postings, translated from Arabic-to-English and when names are provided they are counted as a casualty. These names are corroborated with the Hamas MoH when they are released. All data is presented on their website, and each incident has its own web-page.
:::

\

### Documentation

#### **Please find the documentation for this repo → [here](https://klinares.github.io/airwars_scraping_project/documentation/scraping_airwars.html).**

This document outlines all of the pieces of this project in order for others to be able to collaborates. Given that some of the packages used in the code, such as text, is easier to just follow the website's instruction on how to install. Otherwise we have written out our anaconda environment to a yml file here to be used more as a reference for package/library reference. Some packages will not be able to be built from conda and would require cran. In the near future, we will export out a full bash script that installs this environment using both the anaconda repository and cran.

The structure of this repository lives in two folder; "`code`" and "`database`."

-   Code folder: [scrape_process_incidence.R](https://github.com/klinares/airwars_scraping_project/blob/main/code/scrape_process_incidence.R) r script will scrape and process all data in this repository. The documentation file above this paragraph goes over the workflow to achieve a final SQLite database. The second accompanying script is the [helpful_functions.R](https://github.com/klinares/airwars_scraping_project/blob/main/code/helpful_functions.R) that gets sourced in the scrape_process_incidence.R script. These are basically custom functions that optimizes the workflow.

-   Database folder: During the worflow all of the web URL information gets stored within the [webpages](https://github.com/klinares/airwars_scraping_project/tree/main/database/webpages) sub folder. This is in the event that we need to look at the raw data more closely. The [airwars_db.sqlite](https://github.com/klinares/airwars_scraping_project/blob/main/database/airwars_db.sqlite) contains all of the data we have scraped, processed, and enriched. More details found in the documentation link above.

\

::: {style="color: darkgray"}
### Findings

**Given the comprehensive nature of Airwars\' data, this study aims to explore the potential of open-source information to identify patterns in the targeting of Palestinian civilians in Gaza**. We scraped and processed all available incident reports from the Airwars website, and storing in a publicly accessible SQLite database. To enrich the dataset, we reversed geocoded incident coordinates to query via the Nominatim API to return the type of location in which the attack took place (i.e., school, mosque, park, etc.). Additionally, to understand the general emotional tone in these assessments we used sentiment analysis to classify text into various emotional states. Finally, we hierarchically clustered incidents based on casualties and emotional scores to identify patterns in targeting and geographic associations with children and women casualties in Gaza. Below we report any findings containing data from this repository.

#### **The slide deck for this project is found → [here](https://klinares.github.io/airwars_scraping_project/documentation/airwars_project_slides#/title-slide).**
:::
