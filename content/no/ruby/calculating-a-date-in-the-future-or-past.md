---
title:                "Ruby: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor?

Det kan være mange grunner til at man ønsker å kunne beregne en dato i fremtiden eller fortiden ved hjelp av Ruby-programmering. Noen eksempler kan være å lage en kalender-applikasjon, planlegge fremtidige hendelser eller datofeste innlegg på en blogg. Uansett hva årsaken måtte være, så kan dette være et nyttig verktøy å ha i verktøykassen din som Ruby-utvikler.

## Slik gjør du det

Å beregne en dato i fremtiden eller fortiden i Ruby er enkelt, takket være `Date` klassen. Du kan benytte følgende kode for å beregne en dato 5 dager frem i tid fra dagens dato:

```Ruby
require 'date' # Importerer Date-klassen

today = Date.today # Henter dagens dato
future_date = today + 5 # Legger til 5 dager
puts future_date # Skriver ut resultatet
```

Dette vil gi følgende utskrift:

```Ruby
#<Date: 2020-06-15 ((2459034j,0s,0n),+0s,2299161j)>
```

Som du kan se, returnerer Ruby en instans av `Date` klassen med ønsket dato. Du kan også beregne datoer i fortiden ved å trekke fra et antall dager fra dagens dato.

## Dypdykk

Det er verdt å merke seg at `Date` klassen i Ruby har mange nyttige metoder for å arbeide med datoer. Du kan for eksempel sjekke hvilken dag i uken en bestemt dato faller på, sjekke om et år er et skuddår og mye mer. Ved å utforske dokumentasjonen til `Date` klassen, kan du oppdage enda flere måter å manipulere datoer på.

En annen viktig ting å huske på er at Ruby også har klasser for å håndtere tid og dato sammen, slik som `DateTime` og `Time`. Disse klassene kan være nyttige hvis du også trenger å håndtere tidspunkt i tillegg til datoer.

## Se også

Her er noen nyttige ressurser for å hjelpe deg med å lære mer om beregning av datoer i Ruby:

- [Dokumentasjon for `Date` klassen](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Beregning av dato og tid i Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)
- [Dato- og tidsbehandling i Ruby](http://zetcode.com/ruby/datetime/)