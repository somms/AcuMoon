# R Lunar Analysis Functions

This set of R functions is designed for analyzing and calculating lunar illumination during a specific time period. Leveraging the `lunar` and `suncalc` libraries, these functions enable various operations related to the position and duration of the moon in relation to nights and days.

## Key Functions:

### 1. `nightjaring.date_btw`

This function determines if a given date is within a specific period, useful for evaluating event occurrences within a date range.

### 2. `nightjaring.acu_moon`

Calculates the accumulation of lunar illumination between two dates, allowing adjustments for nighttime lunar hours and geographic location.

### 3. `nightjaring.mean_moon`

Returns the average amount of accumulated lunar illumination per day between two dates, facilitating statistical analysis of lunar light.

### 4. `nightjaring.moon_night_overlap`

Determines the duration and overlap of lunar illumination during the night, considering the positions of the moon and the sun.

## Example Usage:

```r
# Define geographic location
lat = 37.3449745
lon = -6.0634118

# Calculate average lunar illumination between two dates
nightjaring.mean_moon(as.Date('2020-07-01'), as.Date('2020-07-31'))

# Calculate accumulation of lunar illumination between two dates
nightjaring.acu_moon(as.Date('2020-07-01'), as.Date('2020-07-31'))

# Calculate accumulation adjusted with lunar nighttime duration
nightjaring.acu_moon(as.Date('2020-07-01'), as.Date('2020-07-31'), TRUE, lat, lon)

# Create and analyze a dataset
nightjars.data <- data.frame(
  individual = c('331234', '1B50802', '320032'),
  firstDate = as.Date(c('2020-06-01', '2020-05-03', '2020-06-25')),
  lastDate = as.Date(c('2020-08-23', '2020-09-10', '2020-07-22'))
)

# Calculate accumulation of lunar illumination for each row of the dataset
nightjars.data['moon'] <- apply(nightjars.data, 1, function(x) nightjaring.acu_moon(as.Date(x['firstDate']), as.Date(x['lastDate']), 1))

# Calculate accumulation adjusted with lunar duration for each row
nightjars.data['real_moon'] <- apply(nightjars.data, 1, function(x) nightjaring.acu_moon(as.Date(x['firstDate']), as.Date(x['lastDate']), 1, TRUE, lat, lon))
```

These functions provide versatile tools for lunar analysis, allowing exploration and understanding of lunar illumination in different contexts and locations. Enjoy exploring the fascinating lunar world with these R functions! 🌕📊

