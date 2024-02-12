---
title:                "Analysering av en dato fra en streng"
aliases:
- no/ruby/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:47.474915-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av en dato fra en streng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å analysere en dato fra en streng handler om å konvertere tekst som representerer en dato til et `Date` eller `DateTime`-objekt som Ruby forstår. Programmerere gjør dette for å utføre operasjoner som sammenligninger, kalkulasjoner eller formatering på datoer, som er vanlige oppgaver i applikasjoner som håndterer planlegging, analytikk eller databehandling.

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
