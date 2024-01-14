---
title:                "Clojure: Sammenligning av to datoer"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

"## Hvorfor

Så, du lurer på hvorfor man ville sammenligne to datoer? Vel, det er flere grunner til det. Kanskje du vil sortere en liste med datoer i stigende rekkefølge, eller kanskje du vil finne ut hvor lang tid det er mellom to hendelser. Uansett årsak, å kunne sammenligne datoer er en viktig ferdighet å ha i din Clojure-programmeringsverktøykasse.

## Hvordan gjøre det

Det første trinnet for å sammenligne to datoer er å sørge for at de er i et format som Clojure kan forstå. Dette kan gjøres ved å bruke funksjonen `java.util.Date` som konverterer en streng til en datotypen. Her er et eksempel på hvordan du kan sammenligne to datoer og få en sann eller falsk utgang:

```Clojure
(def dato1 (java.util.Date. "2021-08-15"))
(def dato2 (java.util.Date. "2021-08-20"))
(println (<= dato1 dato2))
```

I dette tilfellet vil utgangen bli "true" siden dato2 er etter dato1. Hvis du ønsker å sammenligne datoer for likhet, kan du bruke funksjonen `==`.

## Dypdykk

Når du sammenligner datoer, er det viktig å være klar over at det også er flere underliggende faktorer som kan påvirke resultatet. For eksempel, hvis du sammenligner datoer med tidsangivelser, må du sørge for at tiden er inkludert i sammenligningen for nøyaktighet. En annen ting å vurdere er at noen tidssoner kan ha forskjellig tid for samme dato, så du må være forsiktig når du sammenligner over ulike tidszoner.

En annen nyttig funksjon i Clojure for å håndtere datoer er `java.util.Calendar` som lar deg lage en kalender for en bestemt dato og deretter bruke funksjoner som `getTime()` for å få frem tidspunkter og `get()` for å hente spesifikk informasjon om datoen.

## Se også

Her er noen linker som kan være nyttige for å lære mer om å sammenligne datoer i Clojure:

- Offisiell Clojure dokumentasjon for å jobbe med datoer: https://clojure.github.io/java.time-java-time/ 

- "Working with Dates and Times in Clojure" av Alex Miller: http://blog.cognitect.com/blog/2016/12/1/clojure-dates 

- En diskusjon om tidszoner og datoer i Clojure: https://stackoverflow.com/questions/14416468/converting-java-util-date-to-java-time-localdate#14435763