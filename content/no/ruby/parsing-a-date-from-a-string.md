---
title:                "Å analysere en dato fra en streng"
html_title:           "Ruby: Å analysere en dato fra en streng"
simple_title:         "Å analysere en dato fra en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente ut en dato fra en tekststreng betyr å konvertere en tekst som representerer en dato til et datatypen som kan brukes i programmene våre. Dette gjøres for å gjøre det enklere å arbeide med datoer og utføre operasjoner som å sortere, filtrere eller sammenligne dem.

## Hvordan:

```Ruby 
require 'date'

puts Date.parse("18-02-2021")
```

Resultat: 2021-02-18

## Dypdykk:

Å konvertere datoer fra en streng er en vanlig oppgave i programmering, spesielt når man jobber med data fra forskjellige kilder. I Ruby kan man bruke Date.parse metoden eller Date.strptime metoden for å hente ut datoer fra en streng. Det finnes også alternative metoder som Date.strptime fra standardbiblioteket eller Date._parse fra gemmen "date_parse".

## Se også:

- [Date.parse dokumentasjon](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html#method-c-_parse)
- [Date.strptime dokumentasjon](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html#method-c-strptime)
- [date_parse gem](https://github.com/date/date-parse)