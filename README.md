# Google Location data during the Covid-19 period

Archive of data extracted from Google's [Community Mobility
Reports](https://www.google.com/covid19/mobility/).  All countries are included.

## Data download

For now, download a [dated
file](https://raw.githubusercontent.com/nacnudus/google-location-coronavirus/master/2020-03-29.tsv). It is tab-separated.

## Method

Based on similar work by the [Office for National Statistics Data Science
Campus](https://github.com/datasciencecampus/mobility-report-data-extractor).

1. Convert the PDF files to SVG format, and extract the trend lines.
1. Extract text from the PDF.
1. Pair up the text with the trends.

The differences are:

1. All countries are included.
1. Using R, instead of Python
1. Scripting pdf->svg file conversion with
   [`pdf2svg`](https://github.com/dawbarton/pdf2svg), rather than doing it
   manually.
