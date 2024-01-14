---
title:                "Clojure: Beregning av en dato i fremtiden eller fortiden"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å kunne regne ut en dato i fremtiden eller fortiden kan være nyttig for å planlegge hendelser, søke om visum, eller bare for å tilfredsstille nysgjerrigheten. Med Clojure kan du enkelt lage funksjoner som kan beregne fremtidige eller fortidige datoer.

## Hvordan

For å beregne en dato i fremtiden eller fortiden vil vi bruke standardbiblioteket `java.time.LocalDate` som gir oss funksjonalitet for datoer. Først må vi importere biblioteket og deretter opprette en funksjon som tar inn et antall dager og beregner datoen basert på dette tallet. For å gjøre koden mer lesbar, bruker vi også funksjonen `clj-time.core/plus-days` fra biblioteket `clj-time` som gjør det enklere å legge til dager i en dato.

```Clojure
(ns comp.compdate (:import [java.time LocalDate]))
(defn compdate [num-days]
  (let [today (LocalDate/now)]
    (clj-time.core/plus-days today num-days)))
```
Vi kan nå kalle funksjonen med et ønsket antall dager som parameter, for eksempel 7 dager frem i tid:

```Clojure
(compdate 7)
```
Outputen vil da bli `#<LocalDate 2020-12-07>`. Vi kan også bruke negative tall for å beregne en dato i fortiden, for eksempel 10 dager tilbake:

```Clojure
(compdate -10)
```
Outputen vil da bli `#<LocalDate 2020-11-18>`. Du kan også endre formatet på outputen ved å bruke funksjonen `clj-time.format/show` som tar inn en dato og et ønsket format som parametere. For eksempel vil vi ha datoen i formatet dd/MM/yyyy:

```Clojure
(clj-time.format/show (compdate 7) "dd/MM/yyyy")
```
Outputen vil da bli `07/12/2020`.

## Dypdykk

Det er også mulig å gjøre mer avanserte beregninger med datoer, som å finne ut hvilken dag en spesifikk dato vil være. Dette kan gjøres med funksjonen `clj-time.core/day-of-week` som returnerer en verdi mellom 1 og 7, hvor 1 er mandag og 7 er søndag. Vi kan også bruke funksjonen `clj-time.utils.day-of-week` for å få et mer leselig navn på ukedagen.

```Clojure
(clj-time.utils/day-of-week (day-of-week (compdate 7)))
```
Outputen vil da bli `mandag`. Dette kan være nyttig hvis du f.eks. skal planlegge en hendelse og vil vite hvilken dag den vil være på.

## Se også

- [Java Time Dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [clj-time biblioteket](https://github.com/clj-time/clj-time)