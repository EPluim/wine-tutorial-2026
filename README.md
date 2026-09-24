### README 
# Wine Dataset

A small curated dataset of 20 wines from 12 countries, with chemical properties, price and quality rating. It is suitable for teaching, quick exploratory analysis, or testing data pipelines.

## File

| File | Format | Rows | Columns |
|------|--------|------|---------|
| `wines.tsv` | Tab-separated, header row | 20 | 13 |

## Columns

| Column | Type | Unit | Description |
|--------|------|------|-------------|
| *(index)* | integer | – | Row identifier (1–20) |
| `name` | string | – | Wine name |
| `country` | string | – | Country of origin |
| `region` | string | – | Wine region |
| `grape` | string | – | Main grape variety |
| `color` | categorical | – | One of `red`, `white`, `rose`, `sparkling` |
| `vintage` | integer | year | Harvest year |
| `alcohol` | float | % ABV | Alcohol by volume |
| `acidity` | float | g/L | Titratable acidity |
| `residual_sugar` | float | g/L | Sugar remaining after fermentation |
| `ph` | float | – | pH |
| `price_eur` | float | EUR | Retail price per bottle |
| `rating` | integer | points | Quality score (100-point scale) |

## Overview

**By color**

| Color | Count |
|-------|-------|
| red | 10 |
| white | 7 |
| sparkling | 2 |
| rose | 1 |

**By country:** France (5), Spain (3), Italy (2), USA (2), and one each from Germany, New Zealand, Australia, Portugal, Austria, Argentina, Hungary and South Africa.

**Value ranges**

| Variable | Min | Max |
|----------|-----|-----|
| vintage | 2015 | 2023 |
| alcohol | 8.5 | 15.0 |
| acidity | 5.2 | 9.5 |
| residual_sugar | 1.2 | 180.0 |
| ph | 3.05 | 3.70 |
| price_eur | 13.00 | 85.00 |
| rating | 85 | 95 |

Note that `residual_sugar` is heavily skewed. Most wines are dry (under 5 g/L), while the Tokaji Aszú (180 g/L) and the Mosel Kabinett (38 g/L) are sweet outliers. Consider a log transform before modelling.

## Data

| # | name | country | region | grape | color | vintage | alcohol | acidity | residual_sugar | ph | price_eur | rating |
|---|------|---------|--------|-------|-------|---------|---------|---------|----------------|----|-----------|--------|
| 1 | Chateau Lune | France | Bordeaux | Merlot | red | 2018 | 13.5 | 5.8 | 2.1 | 3.55 | 32.00 | 91 |
| 2 | Rio Verde Reserva | Spain | Rioja | Tempranillo | red | 2016 | 14.0 | 5.5 | 1.9 | 3.60 | 24.50 | 89 |
| 3 | Mosel Kabinett | Germany | Mosel | Riesling | white | 2021 | 8.5 | 8.9 | 38.0 | 3.05 | 18.00 | 90 |
| 4 | Colline Rosso | Italy | Tuscany | Sangiovese | red | 2019 | 13.8 | 6.1 | 1.5 | 3.45 | 27.00 | 88 |
| 5 | Kiwi Coast | New Zealand | Marlborough | Sauvignon Blanc | white | 2023 | 12.5 | 7.4 | 3.2 | 3.20 | 16.50 | 87 |
| 6 | Barossa Shiraz | Australia | Barossa Valley | Shiraz | red | 2017 | 14.8 | 5.4 | 2.6 | 3.70 | 35.00 | 92 |
| 7 | Provence Blush | France | Provence | Grenache | rose | 2023 | 12.8 | 6.3 | 1.8 | 3.35 | 19.00 | 86 |
| 8 | Douro Tinto | Portugal | Douro | Touriga Nacional | red | 2018 | 14.2 | 5.7 | 2.0 | 3.62 | 21.00 | 90 |
| 9 | Napa Reserve | USA | Napa Valley | Cabernet Sauvignon | red | 2016 | 14.5 | 5.9 | 2.4 | 3.68 | 85.00 | 94 |
| 10 | Wachau Federspiel | Austria | Wachau | Gruner Veltliner | white | 2022 | 12.0 | 6.8 | 2.2 | 3.30 | 22.00 | 89 |
| 11 | Andes Malbec | Argentina | Mendoza | Malbec | red | 2020 | 14.1 | 5.6 | 2.3 | 3.65 | 15.00 | 88 |
| 12 | Cava Brut | Spain | Penedes | Macabeo | sparkling | 2021 | 11.5 | 7.1 | 8.0 | 3.15 | 13.00 | 85 |
| 13 | Chablis Premier | France | Burgundy | Chardonnay | white | 2020 | 12.5 | 7.0 | 1.2 | 3.25 | 38.00 | 92 |
| 14 | Sonoma Pinot | USA | Sonoma | Pinot Noir | red | 2021 | 13.9 | 6.0 | 1.7 | 3.52 | 42.00 | 91 |
| 15 | Tokaji Aszu | Hungary | Tokaj | Furmint | white | 2015 | 11.0 | 9.5 | 180.0 | 3.40 | 55.00 | 95 |
| 16 | Stellen Chenin | South Africa | Stellenbosch | Chenin Blanc | white | 2022 | 13.0 | 6.9 | 4.5 | 3.28 | 14.00 | 87 |
| 17 | Champagne Brut | France | Champagne | Chardonnay | sparkling | 2018 | 12.0 | 7.8 | 9.0 | 3.05 | 60.00 | 93 |
| 18 | Priorat Garnacha | Spain | Priorat | Grenache | red | 2017 | 15.0 | 5.3 | 2.2 | 3.58 | 48.00 | 93 |
| 19 | Alsace Gewurz | France | Alsace | Gewurztraminer | white | 2021 | 13.5 | 5.2 | 12.0 | 3.45 | 26.00 | 88 |
| 20 | Chianti Classico | Italy | Tuscany | Sangiovese | red | 2020 | 13.5 | 6.2 | 1.6 | 3.48 | 23.00 | 89 |

## Loading the data

**Python (pandas)**
```python
import pandas as pd
wines = pd.read_csv("wines.tsv", sep="\t", index_col=0)
```

**R**
```r
wines <- read.delim("wines.tsv", row.names = 1)
```