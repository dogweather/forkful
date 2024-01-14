---
title:                "Ruby: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer kan være en viktig del av programmering, spesielt når man jobber med tidssensitive applikasjoner. Ved å sammenligne datoer kan man enkelt finne ut om en dato er før, etter eller lik en annen. Dette kan være nyttig for å utføre forskjellige handlinger eller filtrere data basert på datoer.

## Hvordan gjøre det

Det er flere måter å sammenligne datoer på i Ruby, avhengig av hvordan datoene er lagret. Her er tre eksempler på hvordan dette kan gjøres:

### Med Date objekter

```Ruby
require 'date'

date1 = Date.new(2020, 9, 1)
date2 = Date.new(2020, 8, 1)

if date1 > date2
  puts "Date1 er etter Date2"
elsif date1 < date2
  puts "Date1 er før Date2"
else
  puts "Date1 er lik Date2"
end
```

### Med Time objekter

```Ruby
require 'time'

time1 = Time.new(2020, 9, 1, 12, 0, 0)
time2 = Time.new(2020, 9, 1, 10, 0, 0)

if time1 > time2
  puts "Time1 er etter Time2"
elsif time1 < time2
  puts "Time1 er før Time2"
else
  puts "Time1 er lik Time2"
end
```

### Med DateTime objekter

```Ruby
require 'date'

date_time1 = DateTime.new(2020, 9, 1, 12, 0, 0)
date_time2 = DateTime.new(2020, 9, 1, 10, 0, 0)

if date_time1 > date_time2
  puts "DateTime1 er etter DateTime2"
elsif date_time1 < date_time2
  puts "DateTime1 er før DateTime2"
else
  puts "DateTime1 er lik DateTime2"
end
```

Output for alle tre eksemplene vil være:

```Ruby
Date1 er etter Date2 (eller Time1 er etter Time2 / DateTime1 er etter DateTime2)
```

## Dypdykk

Det er viktig å merke seg at når man sammenligner datoer i Ruby, så vil kun dato, måned og år bli tatt i betraktning. Timer, minutter og sekunder blir ikke sammenlignet. Dette kan føre til uventet oppførsel dersom man jobber med datoer i ulike formater eller tidsmerking. Det er derfor viktig å være oppmerksom på disse forskjellene når man sammenligner datoer i Ruby.

## Se også

- [Date klasse i Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Time klasse i Ruby](https://ruby-doc.org/core-2.7.1/Time.html)
- [DateTime klasse i Ruby](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/DateTime.html)