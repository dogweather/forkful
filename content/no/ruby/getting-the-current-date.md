---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:16:29.846369-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I programmering betyr "å få dagens dato" enkelt og greit å finne ut nøyaktig hvilken dato det er i dag. Det er nyttig for funksjoner som logger, rapporterer og datofunksjoner i webapplikasjoner.

## Hvordan gjøre det:
```Ruby
require 'date'

# Hent dagens dato
dagens_dato = Date.today
puts dagens_dato
```
Output:
```
2023-04-02 # Dette output kan variere basert på dagens dato.
```

## Dypdykk
Å få tak i dagens dato i Ruby er takket være `Date` og `Time`-bibliotekene en smal sak. Faktisk har Ruby hatt innebygd støtte for dato og tid siden de tidlige versjonene. Alternativer inkluderer bruk av `Time.now` for å få et `Time`-objekt som også inneholder tidsinformasjonen, eller `DateTime.now` for en kombinasjon av dato og tid.

Implementeringsdetaljene i `Date.today` er at det faktisk oppretter et nytt `Date`-objekt som representerer dagens dato basert på systemklokken. Det er verdt å nevne at alle disse metodene reflekterer den lokale tidszonen som systemet ditt bruker.

## Se også
- En guide til Ruby's `Time`-klasse: [Ruby Time documentation](https://ruby-doc.org/core-2.7.0/Time.html)
- Stack Overflow diskusjoner om Ruby, dato og tid: [Stack Overflow: Ruby Date & Time](https://stackoverflow.com/questions/tagged/ruby+date+time)
