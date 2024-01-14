---
title:                "Gleam: Sammenligning av to datoer"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
I denne bloggposten vil vi utforske hvordan vi kan sammenligne to datoer ved hjelp av Gleam. Dette kan være nyttig i mange situasjoner, som for eksempel å finne ut om en bestemt dato kommer før eller etter en annen, eller å beregne antall dager mellom to datoer. 

## Hvordan
Vi starter med å importere Gleams `Calendar` modul, som gir oss tilgang til funksjonalitet for å håndtere datoer og klokkeslett. Deretter kan vi bruke `Calendar.compare_dates` funksjonen til å sammenligne to datoer som parametere. 

```Gleam
import Gleam.Calendar

let dato_1 = Calendar.Date.new(2019, 1, 5)
let dato_2 = Calendar.Date.new(2020, 1, 5)

let resultat = Calendar.compare_dates(dato_1, dato_2)

IO.println(resultat) // Skriver ut "-1" siden dato 1 kommer før dato 2
```

Vi kan også bruke `Calendar.days_between` funksjonen for å beregne antall dager mellom to datoer: 

```Gleam
import Gleam.Calendar

let dato_1 = Calendar.Date.new(2019, 1, 5)
let dato_2 = Calendar.Date.new(2020, 1, 5)

let antall_dager = Calendar.days_between(dato_1, dato_2)

IO.println(antall_dager) // Skriver ut "365"
```

## Dypt dykk
I Gleam er datoer representert som en `Calendar.Date`-rekord med tre felter: `year`, `month` og `day`. Dette gjør det enkelt å hente ut individuelle deler av datoen og sammenligne dem. 

Det er også verdt å merke seg at fuzzy matching av datoer ikke er støttet i Gleam. Dette betyr at datoer må være nøyaktig like for å bli ansett som like når de sammenlignes. 

## Se også
- [Offisiell dokumentasjon for Gleam Calendar modulen](https://gleam.run/modules/calendar)
- [Stack Overflow: Sammenligne datoer i Gleam](https://stackoverflow.com/questions/56756472/comparing-dates-in-gleam) 
- [Github: Eksempler på bruk av Gleam Calendar modulen](https://github.com/gleam-lang/gleam/blob/master/examples/calendar.gleam)