---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:16.363112-07:00
description: "Hvordan: Clojures s\xF8ml\xF8se interoperabilitet med Java lar deg tappe\
  \ direkte inn i Java Date-Time API-et. Slik kan du f\xE5 den n\xE5v\xE6rende datoen."
lastmod: '2024-03-13T22:44:40.412405-06:00'
model: gpt-4-0125-preview
summary: "Clojures s\xF8ml\xF8se interoperabilitet med Java lar deg tappe direkte\
  \ inn i Java Date-Time API-et."
title: "F\xE5 dagens dato"
weight: 29
---

## Hvordan:


### Ved bruk av Java Interop
Clojures sømløse interoperabilitet med Java lar deg tappe direkte inn i Java Date-Time API-et. Slik kan du få den nåværende datoen:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; Eksempel på utdata
(get-current-date) ; "2023-04-15"
```

### Ved å bruke clj-time-biblioteket
For en mer idiomatisk Clojure-løsning, kan du velge `clj-time`-biblioteket, en innpakning rundt Joda-Time, selv om det for de fleste nye prosjekter anbefales å bruke det innebygde Java 8 Date-Time API-et. Men, skulle du foretrekke eller trenge `clj-time`:

Først, legg til `clj-time` i prosjektavhengighetene dine. I din `project.clj`, inkluder:

```clojure
[clj-time "0.15.2"]
```

Deretter, bruk det for å få den nåværende datoen:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; Eksempel på utdata
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

Begge metodene gir raske, effektive måter å få den nåværende datoen på i Clojure, ved å utnytte kraften fra den underliggende Java-plattformen eller bekvemmeligheten av et Clojure-spesifikt bibliotek.
