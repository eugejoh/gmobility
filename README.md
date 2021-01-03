
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Get Canadian COVID-19 Google Mobility Reports Data

Pet project to understand more of SQLite, R Shiny, GitHub Pages, and data
visualization on Windows 10 x64. Contains R code to download and process
[Google Mobility data](https://www.google.com/covid19/mobility/) for
only [Canadian
regions](https://www.canada.ca/en/immigration-refugees-citizenship/services/new-immigrants/prepare-life-canada/provinces-territories.html).
The scripts are to simplify the download, clean, and process steps.

1.  Download .zip and only extract Canadian regions .csv files
2.  Process raw .csv into national, provincial/territorial, and
    municipal levels
3.  Store into local SQLite database
4.  Read tables from SQLite and feed into downstream workflows

Last updated: 2021-01-02
