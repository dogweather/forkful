---
date: 2024-01-20 17:30:43.527734-07:00
description: "Hvordan Gj\xF8re Det: Beregner av datoer har v\xE6rt nyttig siden starten\
  \ av programmering. P\xE5 den tiden var det vanlig \xE5 gj\xF8re alt fra grunnen\
  \ av. N\xE5 har vi\u2026"
lastmod: '2024-04-05T22:50:54.425694-06:00'
model: gpt-4-1106-preview
summary: "Beregner av datoer har v\xE6rt nyttig siden starten av programmering."
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Hvordan Gjøre Det:
```clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])
(require '[clj-time.periodic :as p])

;; Beregner en dato 10 dager fra nå
(let [today (t/now)
      future-date (t/plus today (t/days 10))]
  (c/to-string future-date))
;;=> "2023-04-10T14:59:59.999Z"

;; Beregner en dato 5 år tilbake i tid
(let [today (t/now)
      past-date (t/minus today (t/years 5))]
  (c/to-string past-date))
;;=> "2018-03-31T14:59:59.999Z"
```

## Dypdykk
Beregner av datoer har vært nyttig siden starten av programmering. På den tiden var det vanlig å gjøre alt fra grunnen av. Nå har vi biblioteker som `clj-time`, som bygger på Jodas `java.time` biblioteket. I Clojure gir `clj-time` enklere, Clojure-vennlig syntaks for datohåndtering og operasjoner som å legge til eller trekke fra tidsperioder.

Alternativer til `clj-time` inkluderer innebygde funksjoner i `java.time`, eller for eldre prosjekter, klasser som `java.util.Date` og `java.util.Calendar`. Men disse kan være mer klossete og verbøse.

Et viktig detalj å huske når du jobber med tid og datoer er tidszoner. Alltid vurdere tidssonekonsekvenser, spesielt når du planlegger noe over forskjellige geografiske regioner.

## Se Også
- clj-time GitHub side: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
