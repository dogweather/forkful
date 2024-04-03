---
date: 2024-01-20 17:32:30.657457-07:00
description: 'Hvordan: .'
lastmod: '2024-03-13T22:44:40.414237-06:00'
model: gpt-4-1106-preview
summary: .
title: Sammenlikning av to datoer
weight: 27
---

## Hvordan:
```Clojure
;; Importer nødvendige biblioteker først
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])

;; La oss ta to eksemplariske datoer
(def date1 (t/date-time 2023 3 25))
(def date2 (t/date-time 2023 4 10))

;; Sjekk om datoene er like
(= date1 date2) ; => false

;; Finn ut hvilken som kommer først
(t/before? date1 date2) ; => true
(t/after? date1 date2) ; => false

;; Dager mellom datoene
(t/in-days (t/interval date1 date2)) ; => 16
```
Her ser vi at `date1` kommer før `date2` og det er 16 dager mellom dem.

## Dypdykk:
Før `clj-time`, Clojures egen bibliotek for dato- og tidsmanipulasjon, brukte programmerere Java sin `java.util.Date` direkte—ikke alltid pent eller enkelt. I dag gir `clj-time`, en wrapper til Jodas `DateTime` objekter, en rik sett med funksjoner for arbeid med datoer og tider på en idiomatic Clojure måte.

Alternativer til `clj-time` inkluderer `java.time` (java.time.LocalDateTime) biblioteket som også kan brukes direkte i Clojure. Men siden `clj-time` er såpass integrert i Clojure økosystemet, er det ofte foretrukket.

Viktig i implementasjon er forståelsen av tidssoner. `clj-time` håndterer dette elegant, gjør det enkelt å sammenligne datoer på tvers av tidssoner. Husk at sammenligning av datoer kan gi forskjellig resultat avhengig av tidssonen de sammenlignes i.

## Se Også:
- [clj-time GitHub repo](https://github.com/clj-time/clj-time)
- Joda-Time [hjemmeside](http://www.joda.org/joda-time/)
- Oracle dokumentasjon for [java.time pakken](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
