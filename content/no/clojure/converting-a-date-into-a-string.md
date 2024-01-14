---
title:                "Clojure: Konvertere en dato til en streng"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor
Konvertering av datoer til strenger er en vanlig oppgave i programmering, spesielt når man jobber med dato- og tidsbaserte applikasjoner. Dette kan være nyttig for å vise datoer i et mer leselig format eller for å sammenligne datoer med hverandre.

# Hvordan du gjør det
For å konvertere en dato til en streng i Clojure, kan du bruke funksjonen `str` sammen med `clj-time` biblioteket. Først må du importere `clj-time` biblioteket ved å legge til følgende linje i toppen av filen din:
```Clojure
(ns din-prosjekt.navn (:require [clj-time.core :as time]))
```
Deretter kan du konvertere en dato ved å bruke `str` funksjonen og spesifisere ønsket format:
```Clojure
(str (time/local-date "2019-09-14") "dd.MM.yyyy")
```
Dette vil konvertere datoen til en streng i formatet `14.09.2019`. Du kan også spesifisere andre formater, som for eksempel `yyyy-MM-dd` eller `MM/dd/yyyy`.

# Dypdykk
Videre kan du også bruke `clj-time` biblioteket til å utføre en rekke andre operasjoner på datoer, som for eksempel å legge til eller trekke fra dager, uker eller måneder. Du kan lese mer om dette i offisiell dokumentasjon for `clj-time` biblioteket.

Det er også viktig å være oppmerksom på at datotyper i Clojure er immutable, noe som betyr at de ikke kan endres. Derfor vil du alltid få en ny instans av datoen når du konverterer den til en streng.

# Se også
- [Offisiell dokumentasjon for `clj-time` biblioteket](https://clj-time.github.io/clj-time/)
- [En bloggpost om arbeid med `clj-time` i Clojure](https://yogthos.net/posts/2014-01-27-Clojure-and-date-manipulation.html)
- [En tutorial for å jobbe med datoer i Clojure](https://www.tutorialspoint.com/clojure/clojure_date_time.htm)