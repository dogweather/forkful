---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:47.474915-07:00
description: "Hvordan: I Ruby tilbyr standardbiblioteket direkte m\xE5ter \xE5 analysere\
  \ datoer fra strenger ved hjelp av `Date` og `DateTime`-klassene. Her er hvordan\
  \ du\u2026"
lastmod: '2024-03-13T22:44:41.341675-06:00'
model: gpt-4-0125-preview
summary: "I Ruby tilbyr standardbiblioteket direkte m\xE5ter \xE5 analysere datoer\
  \ fra strenger ved hjelp av `Date` og `DateTime`-klassene."
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:
I Ruby tilbyr standardbiblioteket direkte måter å analysere datoer fra strenger ved hjelp av `Date` og `DateTime`-klassene. Her er hvordan du gjør det ved bruk av Rubys innebygde metoder:

```ruby
require 'date'

# Analysere en dato fra en streng
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime for mer detaljert tidsrepresentasjon
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

For mer kontroll eller for å håndtere formater som `parse` kanskje ikke direkte forstår, kan du bruke `strptime` (string parse time), ved å spesifisere formatet eksplisitt:

```ruby
# Bruke strptime for tilpassede formater
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Bruke tredjepartsbiblioteker:
Selv om Rubys innebygde funksjoner er kraftige, kan du noen ganger foretrekke tredjepartsbiblioteker for ekstra funksjoner eller enklere syntaks. Et populært valg er `Chronic`-gemen for naturlig språkanalyse:

1. Legg først til Chronic i Gemfileen din og kjør `bundle install`:
```ruby
gem 'chronic'
```

2. Bruk det deretter slik:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('neste tirsdag')
puts parsed_chronic
# Utdata vil variere avhengig av nåværende dato; antar parsing på 2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` er svært nyttig for brukerinput ettersom det kan forstå et bredt spekter av naturlige språkdatoformater, noe som gjør det til et kraftig verktøy for applikasjoner som krever fleksibel datoinngang.
