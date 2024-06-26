---
date: 2024-01-20 17:36:17.479521-07:00
description: "How to: Konvertering av datoer til strenger i Clojure skjer ofte via\
  \ `java.time` biblioteket, en del av Java Platform Standard Ed. 8 (JSR-310). Historien\u2026"
lastmod: '2024-04-05T21:53:41.393083-06:00'
model: gpt-4-1106-preview
summary: Konvertering av datoer til strenger i Clojure skjer ofte via `java.time`
  biblioteket, en del av Java Platform Standard Ed.
title: Konvertere en dato til en streng
weight: 28
---

## How to:
```Clojure
;; Importer java.time.format for formateringsklasser
(require '[java-time.format :as fmt])

;; Sett opp en dato som et LocalDate objekt.
(def my-date (java.time.LocalDate/of 2021 12 24))

;; Konverter LocalDate til en streng
(def date-str (fmt/format my-date (fmt/formatter "dd.MM.yyyy")))
(println date-str)
```
Output:
```
24.12.2021
```
```Clojure
;; Bruk java.time.Instant for å få nåværende tidspunkt
(def now-instant (java.time.Instant/now))

;; Konverter Instant til en streng med forhåndsbestemt format
(def instant-str (fmt/format now-instant (fmt/formatter "HH:mm:ss dd.MM.yyyy")))
(println instant-str)
```
Output:
```
21:45:01 24.12.2021
```

## Deep Dive
Konvertering av datoer til strenger i Clojure skjer ofte via `java.time` biblioteket, en del av Java Platform Standard Ed. 8 (JSR-310). Historien bak `java.time` inkluderer en kritikk av de forrige dato og tid APIene i Java for deres mangler og compleksitet.

Alternativer inkluderer å lage din egen formaterer eller å bruke biblioteker som `clj-time` som bygger på `Joda-Time` – et prosjekt som inspirerte `java.time`. Når du implementerer en konvertering, tenk på tidssonebehandling. Standard `Instant` antar UTC. Bruk `ZonedDateTime` om nødvendig.

## See Also
- Clojure's `java-time` bibliotek: https://clojure.github.io/clojure/javadoc/clojure/java-time.html
- Oracle's guide til `java.time`: https://docs.oracle.com/javase/tutorial/datetime
- GitHub siden til `clj-time`: https://github.com/clj-time/clj-time
