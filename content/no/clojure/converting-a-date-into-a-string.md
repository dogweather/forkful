---
title:    "Clojure: Konvertere en dato til en streng"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Mange programmerere kan trenge å konvertere datoer til strenger for å vise dem på en enklere og mer lesbar måte. Dette kan være nyttig for å presentere datoer i et brukergrensesnitt eller for å lagre dem i en database.

## Hvordan
Å konvertere datoer til strenger i Clojure er enkelt med bruk av funksjonen `(str ...)`. Dette er en innebygd funksjon som lar deg bygge en streng fra flere ulike datastrukturer, inkludert datoer.

```Clojure
(str 15 "April" 2020) ;;output: "15 April 2020"
```

Du kan også bruke funksjonen `format` for å spesifisere formatet på datoen. For eksempel, hvis du ønsker å vise måneden som en forkortet streng, kan du skrive:

```Clojure
(format "%d %b %Y" 15 :April 2020) ;;output: "15 Apr 2020"
```

Det er også mulig å konvertere datoer til andre formater, som ISO 8601-formatet, ved hjelp av biblioteket `clj-time`. Her er et eksempel på hvordan du kan gjøre det:

```Clojure
(require '[clj-time.format :as time])
(time/unparse (time/date-time 2020 4 15) "yyyy-MM-dd") ;;output: "2020-04-15"
```

## Dypdykk
Når du konverterer en dato til en streng, kan du også inkludere tid, tidssone og milliseconds hvis du ønsker det. Dette gjøres ved å bruke funksjonen `date-time`. Her er et eksempel på hvordan du kan legge til tid og milliseconds i en konvertert dato:

```Clojure
(str (time/date-time 2020 4 15 15 30 45 500)) ;;output: "2020-04-15T15:30:45.500"
```

Det er også mulig å konvertere en dato til en lokal tidssone ved hjelp av `zoned-date-time`-funksjonen. Dette kan være nyttig hvis du ønsker å vise datoer i forskjellige tidssoner, som for eksempel i et globalt applikasjonsmiljø.

```Clojure
(str (time/zoned-date-time (time/date-time 2020 4 15) "Europe/Oslo")) ;;output: "2020-04-15T00:00:00.000+02:00"
```

## Se også
- [Clojure `str`-funksjonen dokumentasjon](https://clojuredocs.org/clojure.core/str)
- [Clojure `format`-funksjonen dokumentasjon](https://clojuredocs.org/clojure.core/format)
- [Clj-time bibliotek dokumentasjon](https://github.com/clj-time/clj-time)