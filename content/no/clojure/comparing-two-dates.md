---
title:    "Clojure: Sammenligning av to datoer"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Sammenligning av to datoer er en vanlig oppgave i programmering, spesielt i Clojure. Det kan være nyttig for å identifisere forskjeller mellom to datoer, eller for å sortere en liste av datoer i kronologisk rekkefølge.

## Hvordan

For å sammenligne to datoer i Clojure, kan du bruke funksjonen `compare`. Denne funksjonen tar inn to datoer og returnerer en numerisk verdi som indikerer forholdet mellom dem.

```Clojure
(compare (java.util.Date. 2021 12 15) (java.util.Date. 2021 12 10))
```

Dette vil gi en output på `1`, som betyr at den første datoen kommer etter den andre. Hvis den første datoen hadde vært tidligere enn den andre, ville output vært `-1`, og hvis de to datoene er like, ville output vært `0`.

Du kan også bruke funksjonen `before?` og `after?` for å sjekke om en dato er før eller etter en annen.

```Clojure
(before? (java.util.Date. 2020 5 10) (java.util.Date. 2021 5 10))
```

Dette vil returnere `true`, siden den første datoen kommer før den andre.

En annen nyttig funksjon er `days-between`, som returnerer antall dager mellom to datoer.

```Clojure
(days-between (java.util.Date. 2021 5 1) (java.util.Date. 2021 5 10))
```

Dette vil gi en output på `9`.

## Dypdykk

Når du sammenligner to datoer, er det viktig å huske på at tidsforskjellen påvirker resultatet. For eksempel, hvis en dato har en nøyaktighet på bare dager, vil den ignorere forskjellen i tid på to datoer som har samme dag.

Det er også viktig å være oppmerksom på hvilken type dato du jobber med. I Clojure har vi forskjellige datatyper for datoer, som `java.util.Date` og `java.time.LocalDate`, og disse kan ha forskjellige funksjoner og egenskaper for sammenligning.

En annen ting å huske på er at datoer kan være utfordrende å håndtere på grunn av forskjellige formater og tidszoner. Det er viktig å ha en god forståelse av hvordan datoer fungerer i Clojure, og å bruke de riktige funksjonene for å få nøyaktige resultater.

## Se Også

- [Offisiell Clojure Dokumentasjon](https://clojure.org/)
- [Clojure Datatypes Cheat Sheet](https://clojure.org/guides/learn/functions#_useful_functions_on_core_data_structures)
- [Clojure Date and Time Library](https://github.com/clj-time/clj-time)