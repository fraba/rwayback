rwayback: An R package to search and parse different versions of the same page from the 
Internet Archive's Wayback Machine
================
Francesco Bailo

## Installation
```
library(devtools) 
install_github('fraba/rwayback')
```

## Use
```
retrieve(webpage = 'corriere.it',
from = "2010-01-01 00:00:00", to = "2010-01-05 00:00:00",
by = 'day', tz = 'CET', filename = 'corriere.RData')
```
