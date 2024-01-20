---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer er prosessen med å bestemme hvilken av to spesifikke datoer er tidligst eller senest. Programmerere gjør dette for å uthente eller sortere data basert på tid og dato i applikasjoner og databaser. 

## Hvordan:

Ruby, som mange andre programmeringsspråk, tilbyr innebygde metoder for å gjøre datoforvaltning enklere. Du kan for eksempel sammenligne to datoer i Ruby på følgende måte:

```Ruby
require 'date'

date1 = Date.new(2022, 5, 3)
date2 = Date.new(2023, 5, 3)

if date1 > date2
  puts "date1 is later"
elsif date1 < date2
  puts "date2 is later"
else
  puts "Both dates are the same"
end
```
Når du kjører dette kodesnippet, vil outputten være "date2 is later" fordi 3. mai 2023 er etter 3. mai 2022.

## Dypdykk:

Før dagens digitale tidsalder brukte folk forskjellige måter å sammenligne datoer på - som å bruke kalendere, steintavler, eller solklokker! Heldigvis har programmeringsspråk som Ruby gjort livene våre litt enklere.

Alternativt kan Ruby's `Date` klasse også bruke 'ajd' metode (absolutt juledagsdating) for å lage en sammenlignbar skala for å sammenligne datoer. Denne metoden gir et enkelt tall for hver dato, noe som gjør sammenligningene litt mer håndterlige.

```Ruby
date1 = Date.new(2022, 5, 3).ajd
date2 = Date.new(2023, 5, 3).ajd
puts date1 < date2
```
Den overnevnte koden vil også returnere "true", fordi ajd-verdien til date2 er høyere (senere) enn ajd-verdien til date1.

## Se Også:

Hvis du er interessert i Ruby eller dato-håndtering, kan du prøve å lese mer om følgende temaer og ressurser:
- Ruby's offisielle dokumentasjon på 'Date' klassen: [Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)