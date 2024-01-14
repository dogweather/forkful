---
title:    "Clojure: Å beregne en dato i fremtiden eller fortiden"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å kunne beregne en dato i fremtiden eller fortiden kan være svært nyttig for en utvikler, enten det er for å lage en kalenderfunksjon eller for å arbeide med tidsavhengige data. Clojure gir oss verktøyene vi trenger for å enkelt beregne slike datoer.

## Hvordan gjøre det
Vi kan bruke funksjonen `clj-time.core/plus` til å legge til en viss tidsmengde til en eksisterende dato. La oss si at vi vil beregne en dato som er 30 dager frem i tid. I Clojure ville vi gjøre følgende:

```Clojure
(require '[clj-time.core :as t])

(def nåtid (t/today))  ;Dette gir oss dagens dato
(def fremtidig-dato (t/plus nåtid (t/days 30)))
(println fremtidig-dato)
```
Dette vil gi følgende utskrift:

`#inst "2022-01-14T00:00:00.000000000-00:00"`

Vi kan også bruke funksjonen `clj-time.core/minus` for å beregne en dato i fortiden. La oss si at vi vil finne datoen for en uke siden:

```Clojure
(require '[clj-time.core :as t])

(def nåtid (t/today))
(def forrige-dato (t/minus nåtid (t/weeks 1)))
(println forrige-dato)
```
Dette vil gi følgende utskrift:

`#inst "2022-01-07T00:00:00.000000000-00:00"`

Vi kan også endre på hvor mye tidsenheten skal være, for eksempel dager, uker, år eller til og med måneder. For å lære mer om de ulike funksjonene i Clojure for å beregne datoer, kan du utforske dokumentasjonen til `clj-time` biblioteket.

## Dypdykk
Nå som vi har en grunnleggende forståelse av hvordan vi kan beregne datoer i fremtiden og fortiden i Clojure, la oss dykke litt dypere inn i hvordan dette faktisk fungerer. Clojure bruker en datatype kalt `java.time.Instant` for å representere en dato og tidspunkt. Dette gjør det enkelt å manipulere og beregne datoer ved hjelp av Clojure sine funksjoner.

Når vi bruker `t/today` funksjonen, får vi en instans av denne datatype. Og når vi bruker `t/plus` eller `t/minus` funksjonene, legger vi til eller trekker fra en viss tidsmengde til denne instanten, og returnerer en ny instant med den ønskede datoen.

Dette gjør det enkelt å arbeide med datoer i Clojure, da vi kan bruke de samme verktøyene og funksjonene for å beregne datoer i forskjellige tidsenheter.

## Se også
- [Offisiell dokumentasjon for "clj-time" biblioteket](https://github.com/clj-time/clj-time)
- [Clojure programmeringsguide](https://clojure.org/guides/getting_started) (på engelsk)
- [Hvordan bruke datoer og tider i Clojure](https://www.braveclojure.com/dates-and-times-in-clojure/) (på engelsk)