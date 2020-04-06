
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Google Location data during the Covid-19 period

Archive of data extracted from Google’s [Community Mobility
Reports](https://www.google.com/covid19/mobility/). All countries are
included.

<img src="man/figures/README-plot-countries-by-category-1.png" width="100%" />

## Data download

For now, download a [dated
file](https://raw.githubusercontent.com/nacnudus/google-location-coronavirus/master/2020-03-29.tsv).
It is tab-separated.

## Method

Based on similar work by the [Office for National Statistics Data
Science
Campus](https://github.com/datasciencecampus/mobility-report-data-extractor).

1.  Convert the PDF files to SVG format, and extract the trend lines.
2.  Extract text from the PDF.
3.  Pair up the text with the trends.

The differences are:

1.  All countries are included.
2.  Using R, instead of Python
3.  Scripting pdf-\>svg file conversion with
    [`pdf2svg`](https://github.com/dawbarton/pdf2svg), rather than doing
    it manually.

### Don’t compare changes between countries or regions

Google’s note:

> Location accuracy and the understanding of categorized places varies
> from region to region, so we don’t recommend using this data to
> compare changes between countries, or between regions with different
> characteristics (e.g. rural versus urban areas).
